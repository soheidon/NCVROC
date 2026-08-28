# test-repeated-cv-policies.R — Phase 6: Repeated K-Fold Policies & Validation Restrictions

test_that("Phase 6 (A, B, C, D): OOF schema, exact coverage per repeat, and seed reproducibility", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 24, replace = TRUE),
    Q1 = sample(0:2, 24, replace = TRUE),
    Q2 = sample(0:2, 24, replace = TRUE),
    Q3 = sample(0:2, 24, replace = TRUE)
  )
  n <- nrow(d)
  r <- 3

  res1 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold",
                        folds = 4, repeats = r, selection_metric = "youden", seed = 100)

  # A. repeats = 3 produces N * 3 OOF rows
  expect_equal(nrow(res1$oof_predictions), n * r)

  # B. Each repeat contains each subject index exactly once
  for (rep_i in 1:r) {
    sub_oof <- res1$oof_predictions[res1$oof_predictions$repeat_id == rep_i, ]
    expect_equal(sort(sub_oof$row_index), seq_len(n))
  }

  # C. Same seed reproducibility
  res2 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold",
                        folds = 4, repeats = r, selection_metric = "youden", seed = 100)
  expect_identical(res1$final_selected_model, res2$final_selected_model)
  expect_identical(res1$oof_predictions$applied_cutoff, res2$oof_predictions$applied_cutoff)
  expect_identical(res1$oof_predictions$predicted_class, res2$oof_predictions$predicted_class)

  # D. Different seed produces different fold partition
  res_diff <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold",
                            folds = 4, repeats = r, selection_metric = "youden", seed = 999)
  expect_false(identical(res1$oof_predictions$fold_id, res_diff$oof_predictions$fold_id))
})

test_that("Phase 6 (E, F, G, I, J): Independent repeated-CV reference equality for Youden and Accuracy", {
  set.seed(456)
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 14)), # N = 24
    Q1 = sample(0:2, 24, replace = TRUE),
    Q2 = sample(0:2, 24, replace = TRUE),
    Q3 = sample(0:2, 24, replace = TRUE),
    Q4 = sample(0:2, 24, replace = TRUE)
  )
  item_pool <- paste0("Q", 1:4)
  n <- nrow(d)
  r_count <- 3
  k_count <- 4
  sizes <- c(1, 2)

  folds_rep <- .build_cv_folds(d$y, cv_method = "kfold", folds = k_count, repeats = r_count, seed = 888)

  eval_rep_reference <- function(metric_name) {
    all_res <- list()
    cum_idx <- 0L
    for (s in sizes) {
      cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
      for (gi in seq_along(cmb_mat)) {
        combo <- cmb_mat[[gi]]
        rep_metrics <- numeric(r_count)

        for (r in seq_len(r_count)) {
          pred_cls_r <- integer(n)
          for (k in seq_len(k_count)) {
            f_name <- sprintf("Rep%d_Fold%d", r, k)
            test_idx  <- folds_rep[[f_name]]
            train_idx <- setdiff(seq_len(n), test_idx)

            tr_dat <- d[train_idx, , drop = FALSE]
            te_dat <- d[test_idx, , drop = FALSE]

            tr_sc <- rowSums(tr_dat[, combo, drop = FALSE])
            tr_fr <- compute_score_frequencies(tr_sc, tr_dat$y)
            tr_roc <- compute_roc_metrics_from_table(tr_fr$pos_counts, tr_fr$neg_counts)
            tr_cut <- find_optimal_cutoff(tr_roc, method = "youden")$cutoff

            te_sc <- rowSums(te_dat[, combo, drop = FALSE])
            pred_cls_r[test_idx] <- as.integer(te_sc >= tr_cut)
          }

          tp <- sum(pred_cls_r == 1L & d$y == 1L)
          tn <- sum(pred_cls_r == 0L & d$y == 0L)
          fp <- sum(pred_cls_r == 1L & d$y == 0L)
          fn <- sum(pred_cls_r == 0L & d$y == 1L)
          sens <- tp / (tp + fn)
          spec <- tn / (tn + fp)
          acc  <- (tp + tn) / n
          youd <- sens + spec - 1

          rep_metrics[r] <- switch(
            metric_name,
            "youden"   = youd,
            "accuracy" = acc
          )
        }

        all_res[[length(all_res) + 1L]] <- data.frame(
          items      = paste(combo, collapse = ", "),
          n_items    = s,
          metric_mean = mean(rep_metrics),
          metric_sd   = stats::sd(rep_metrics),
          .global_combo_index = cum_idx + gi,
          stringsAsFactors = FALSE
        )
      }
      cum_idx <- cum_idx + choose(length(item_pool), s)
    }
    df <- do.call(rbind, all_res)
    df[order(-df$metric_mean, df$n_items, df$.global_combo_index), ]
  }

  for (m in c("youden", "accuracy")) {
    ref_df <- eval_rep_reference(m)
    pkg_res <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 2), cv_method = "kfold",
                             selection_metric = m, folds = k_count, repeats = r_count, top_n = 10, seed = 888)
    pkg_col <- paste0("cv_", m)

    # I. Candidate ranking matches mean repeat metric
    expect_identical(pkg_res$final_selected_model$items, ref_df$items[1])
    expect_equal(pkg_res$final_selected_model[[pkg_col]], ref_df$metric_mean[1], tolerance = 1e-10)
    expect_identical(pkg_res$candidate_ranking$items, ref_df$items)
    expect_equal(pkg_res$candidate_ranking[[pkg_col]], ref_df$metric_mean, tolerance = 1e-10)

    # J. SD calculated across repeat metrics
    expect_equal(pkg_res$cv_performance$sd[pkg_res$cv_performance$metric == m], ref_df$metric_sd[1], tolerance = 1e-10)
  }
})

test_that("Phase 6.1 (H): Deterministic Non-pooling test for both PPV and NPV (unconditional)", {
  # Deterministic dataset where repeat-level PPV/NPV averages strictly differ from N*R pooled metrics
  d_det <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 10)),
    Q1 = c(1L, 3L, 0L, 1L, 3L, 2L, 2L, 2L, 0L, 2L, 1L, 1L, 2L, 0L, 3L, 0L, 1L, 1L, 1L, 3L),
    Q2 = c(3L, 0L, 1L, 2L, 3L, 0L, 0L, 1L, 0L, 3L, 2L, 3L, 2L, 3L, 1L, 3L, 0L, 2L, 0L, 2L)
  )

  res_rep <- cross_size_cv(d_det, y, Q1:Q2, model_sizes = 1, cv_method = "kfold",
                           folds = 2, repeats = 2, selection_metric = "youden", seed = 1)

  rep_df <- res_rep$repeat_metrics
  expect_equal(nrow(rep_df), 2)

  # Analytical repeat-level metrics:
  # Repeat 1: TP=8, FP=8 -> PPV1 = 8/16 = 0.5000000; TN=2, FN=2 -> NPV1 = 2/4  = 0.5000000
  # Repeat 2: TP=6, FP=3 -> PPV2 = 6/9  = 0.6666667; TN=7, FN=4 -> NPV2 = 7/11 = 0.6363636
  expect_equal(rep_df$ppv[1], 0.5)
  expect_equal(rep_df$ppv[2], 6 / 9)
  expect_equal(rep_df$npv[1], 0.5)
  expect_equal(rep_df$npv[2], 7 / 11)

  # Expected unpooled repeat-mean values:
  expected_mean_ppv <- mean(rep_df$ppv) # (1/2 + 2/3) / 2 = 7/12 = 0.583333333333
  expected_mean_npv <- mean(rep_df$npv) # (1/2 + 7/11) / 2 = 25/44 = 0.568181818182

  # Expected pooled values across all N*R = 40 rows:
  # Pooled TP=14, FP=11 -> Pooled PPV = 14/25 = 0.56
  # Pooled TN=9, FN=6   -> Pooled NPV = 9/15  = 0.60
  oof <- res_rep$oof_predictions
  pooled_tp <- sum(oof$predicted_class == 1L & oof$true_outcome == 1L)
  pooled_fp <- sum(oof$predicted_class == 1L & oof$true_outcome == 0L)
  pooled_tn <- sum(oof$predicted_class == 0L & oof$true_outcome == 0L)
  pooled_fn <- sum(oof$predicted_class == 0L & oof$true_outcome == 1L)

  pooled_ppv <- pooled_tp / (pooled_tp + pooled_fp)
  pooled_npv <- pooled_tn / (pooled_tn + pooled_fn)

  expect_equal(pooled_ppv, 0.56)
  expect_equal(pooled_npv, 0.60)

  # 1. Unconditional PPV assertions:
  expect_false(isTRUE(all.equal(expected_mean_ppv, pooled_ppv)))
  expect_equal(res_rep$final_selected_model$cv_ppv, expected_mean_ppv, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(res_rep$final_selected_model$cv_ppv, pooled_ppv)))

  # 2. Unconditional NPV assertions:
  expect_false(isTRUE(all.equal(expected_mean_npv, pooled_npv)))
  expect_equal(res_rep$final_selected_model$cv_npv, expected_mean_npv, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(res_rep$final_selected_model$cv_npv, pooled_npv)))
})

test_that("Phase 6 (K, L): AUC repeat identity and zero theoretical SD", {
  set.seed(888)
  d <- data.frame(
    y  = sample(0:1, 30, replace = TRUE),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )

  res <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold",
                       folds = 5, repeats = 5, selection_metric = "auc", seed = 123)

  # K. All repeats have identical predicted scores and identical AUC
  best_items <- .parse_itemset(res$final_selected_model$items)
  full_scores <- rowSums(d[, best_items, drop = FALSE])

  for (r in 1:5) {
    sub_oof <- res$oof_predictions[res$oof_predictions$repeat_id == r, ]
    ordered_scores <- sub_oof$predicted_score[order(sub_oof$row_index)]
    expect_equal(ordered_scores, full_scores)
    expect_equal(res$repeat_metrics$auc[r], res$final_selected_model$cv_auc, tolerance = 1e-12)
  }

  # L. AUC SD = 0
  expect_equal(res$cv_performance$sd[res$cv_performance$metric == "auc"], 0.0, tolerance = 1e-12)
})

test_that("Phase 6 (M): LOOCV repeats > 1 error guard", {
  d <- data.frame(
    y  = c(rep(1L, 5), rep(0L, 5)),
    Q1 = 1:10
  )
  expect_error(
    cross_size_cv(d, y, Q1, cv_method = "loocv", repeats = 2),
    "LOOCV is deterministic and unique; repeats > 1 is not supported."
  )
})

test_that("Phase 6 (N): Parallel exactness across none, threads, and chunks with repeats = 3", {
  set.seed(999)
  d <- data.frame(
    y  = sample(0:1, 28, replace = TRUE),
    Q1 = sample(0:2, 28, replace = TRUE),
    Q2 = sample(0:2, 28, replace = TRUE),
    Q3 = sample(0:2, 28, replace = TRUE),
    Q4 = sample(0:2, 28, replace = TRUE)
  )

  res_none <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold",
                            folds = 4, repeats = 3, selection_metric = "youden", parallel = "none", seed = 42)
  res_th   <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold",
                            folds = 4, repeats = 3, selection_metric = "youden", parallel = "threads", n_workers = 2, seed = 42)
  res_ch   <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold",
                            folds = 4, repeats = 3, selection_metric = "youden", parallel = "chunks", n_workers = 2, seed = 42)

  expect_identical(res_none$final_selected_model$items, res_th$final_selected_model$items)
  expect_identical(res_none$final_selected_model$items, res_ch$final_selected_model$items)
  expect_equal(res_none$final_selected_model$cv_youden, res_th$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_equal(res_none$final_selected_model$cv_youden, res_ch$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_identical(res_none$candidate_ranking$items, res_th$candidate_ranking$items)
  expect_identical(res_none$candidate_ranking$items, res_ch$candidate_ranking$items)
  expect_equal(res_none$repeat_metrics, res_th$repeat_metrics)
  expect_equal(res_none$repeat_metrics, res_ch$repeat_metrics)
})

test_that("Phase 6 (O, P, Q, R): Thin wrappers, cv_sum_roc, compare_cv_selection, and nested regression", {
  set.seed(111)
  d <- data.frame(
    y  = sample(0:1, 24, replace = TRUE),
    Q1 = sample(0:2, 24, replace = TRUE),
    Q2 = sample(0:2, 24, replace = TRUE),
    Q3 = sample(0:2, 24, replace = TRUE)
  )

  # O. cv_select_sum_roc repeated regression
  res_sel <- cv_select_sum_roc(d, y, Q1:Q3, item_count = 2, folds = 4, repeats = 3, seed = 777)
  expect_s3_class(res_sel, "cv_select_sum_roc_result")
  expect_equal(nrow(res_sel$oof_predictions), 24 * 3)

  # P. cv_sum_roc repeated semantics
  res_fixed <- cv_sum_roc(d, y, c("Q1", "Q2"), folds = 4, repeats = 3, seed = 777)
  expect_equal(nrow(res_fixed$oof_predictions), 24 * 3)
  expect_equal(nrow(res_fixed$repeat_metrics), 3)

  # Q. compare_cv_selection repeated ordinary semantics
  res_comp <- compare_cv_selection(d, y, Q1:Q3, model_sizes = 1:2,
                                   folds = 3, repeats = 2,
                                   outer_folds = 3, inner_folds = 2,
                                   outer_repeats = 1, inner_repeats = 1, seed = 321)
  expect_s3_class(res_comp, "compare_cv_selection_result")
  expect_true("ordinary" %in% names(res_comp))
  expect_true("nested" %in% names(res_comp))
  expect_equal(res_comp$comparison$ordinary[res_comp$comparison$metric == "youden"],
               res_comp$ordinary$final_selected_model$cv_youden)
})
