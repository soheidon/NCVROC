# test-cross-size-audit.R — Phase 4: Cross-Size Correctness & Generalization Audit

test_that("Phase 4: model_sizes normalization, sorting, and deduplication", {
  set.seed(111)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE),
    Q5 = sample(0:2, 40, replace = TRUE)
  )

  # A. Canonical continuous: model_sizes = 1:3
  res_1_3 <- cross_size_cv(d, y, Q1:Q5, model_sizes = 1:3, folds = 4, seed = 123)
  expect_equal(res_1_3$model_sizes, 1:3)
  expect_equal(res_1_3$total_combinations, choose(5, 1) + choose(5, 2) + choose(5, 3)) # 5 + 10 + 10 = 25

  # B. Discontinuous sizes: model_sizes = c(1, 3, 5)
  res_1_3_5 <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), folds = 4, seed = 123)
  expect_equal(res_1_3_5$model_sizes, c(1L, 3L, 5L))
  expect_equal(res_1_3_5$total_combinations, choose(5, 1) + choose(5, 3) + choose(5, 5)) # 5 + 10 + 1 = 16
  expect_false(2L %in% res_1_3_5$candidate_ranking$n_items)
  expect_false(4L %in% res_1_3_5$candidate_ranking$n_items)

  # C. Unordered input: model_sizes = c(5, 1, 3) produces identical results to c(1, 3, 5)
  res_unord <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(5, 1, 3), folds = 4, seed = 123)
  expect_identical(res_1_3_5$final_selected_model, res_unord$final_selected_model)
  expect_identical(res_1_3_5$candidate_ranking, res_unord$candidate_ranking)
  expect_identical(res_1_3_5$model_size_summary, res_unord$model_size_summary)

  # D. Duplicated input: model_sizes = c(1, 3, 3, 5) deduplicates cleanly
  res_dup <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 3, 5), folds = 4, seed = 123)
  expect_identical(res_1_3_5$final_selected_model, res_dup$final_selected_model)
  expect_identical(res_1_3_5$candidate_ranking, res_dup$candidate_ranking)

  # Invalid inputs: Inf, -Inf, NaN, float
  expect_error(cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, Inf)), "non-empty numeric/integer vector without NA")
  expect_error(cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, -Inf)), "non-empty numeric/integer vector without NA")
  expect_error(cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, NaN)), "non-empty numeric/integer vector without NA")
  expect_error(cross_size_cv(d, y, Q1:Q5, model_sizes = c(1.5, 3)), "non-empty numeric/integer vector without NA")
  expect_error(cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 6)), "exceed number of available items")
})

test_that("Phase 4: candidate counts and model_size_summary audit including top_n boundary", {
  set.seed(222)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE),
    Q5 = sample(0:2, 40, replace = TRUE)
  )

  # Standard audit
  res <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), top_n = 10, folds = 4, seed = 444)
  sum_df <- res$model_size_summary

  # Check structure
  expect_equal(sum_df$n_items, c(1L, 3L, 5L))
  expect_equal(sum_df$n_candidates_total, c(choose(5, 1), choose(5, 3), choose(5, 5))) # 5, 10, 1
  expect_equal(sum(sum_df$n_candidates_total), res$total_combinations) # 16

  # Each size's best item matches candidate ranking
  for (i in seq_along(sum_df$n_items)) {
    s <- sum_df$n_items[i]
    sub_rank <- res$candidate_ranking[res$candidate_ranking$n_items == s, ]
    if (nrow(sub_rank) > 0) {
      expect_identical(sum_df$best_items[i], sub_rank$items[1])
      expect_equal(sum_df$best_metric_value[i], sub_rank$auc[1])
      expect_equal(sum_df$n_evaluated_in_top[i], nrow(sub_rank))
    }
  }

  # Edge case: top_n = 2 so that size 5 is not present in returned candidate_ranking
  res_top2 <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), top_n = 2, folds = 4, seed = 444)
  sum_df2 <- res_top2$model_size_summary
  expect_equal(sum_df2$n_items, c(1L, 3L, 5L))
  expect_equal(sum_df2$n_candidates_total, c(5L, 10L, 1L))
  # If size 5 is absent from top_n:
  if (!5L %in% res_top2$candidate_ranking$n_items) {
    row5 <- sum_df2[sum_df2$n_items == 5L, ]
    expect_equal(row5$n_evaluated_in_top, 0L)
    expect_true(is.na(row5$best_items))
    expect_true(is.na(row5$best_metric_value))
  }
})

test_that("Phase 4: prefer_fewer_items tie-only semantics and global index tie-breaking", {
  # Construct synthetic data where item combinations produce exact ties
  d_tie <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 10)),
    # Q1 and Q2 are identical
    Q1 = c(rep(2L, 10), rep(0L, 10)),
    Q2 = c(rep(2L, 10), rep(0L, 10)),
    # Q3 is slightly worse
    Q3 = c(rep(1L, 10), rep(0L, 10))
  )

  # 1. Size 1 vs Size 2: Both have AUC = 1.0. With prefer_fewer_items = TRUE, size 1 MUST win
  res_tie_pref <- cross_size_cv(d_tie, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", prefer_fewer_items = TRUE, folds = 4)
  expect_equal(res_tie_pref$final_selected_model$n_items, 1L)

  # 2. If metric is higher for a larger model, prefer_fewer_items DOES NOT override metric
  d_high <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 10)),
    Q1 = c(rep(1L, 8), rep(0L, 2), rep(0L, 10)), # AUC < 1.0 (size 1)
    Q2 = c(rep(0L, 8), rep(1L, 2), rep(0L, 10))  # Q1 + Q2 gives perfect AUC = 1.0 (size 2)
  )
  res_high <- cross_size_cv(d_high, y, Q1:Q2, model_sizes = 1:2, selection_metric = "auc", prefer_fewer_items = TRUE, folds = 4)
  expect_equal(res_high$final_selected_model$n_items, 2L)

  # 3. .global_combo_index direct tie-break test:
  # With prefer_fewer_items = FALSE and identical size models (Q1 vs Q2),
  # Q1 (.global_combo_index = 1) MUST win over Q2 (.global_combo_index = 2) deterministically
  res_tie_nofewer <- cross_size_cv(d_tie, y, Q1:Q2, model_sizes = 1, selection_metric = "auc", prefer_fewer_items = FALSE, folds = 4)
  expect_identical(res_tie_nofewer$final_selected_model$items, "Q1")
  expect_identical(res_tie_nofewer$candidate_ranking$items, c("Q1", "Q2"))

  # Parallel consistency under exact ties: none, threads, chunks all produce identical "Q1"
  res_tie_th <- cross_size_cv(d_tie, y, Q1:Q2, model_sizes = 1, selection_metric = "auc", prefer_fewer_items = FALSE, parallel = "threads", n_workers = 2)
  res_tie_ch <- cross_size_cv(d_tie, y, Q1:Q2, model_sizes = 1, selection_metric = "auc", prefer_fewer_items = FALSE, parallel = "chunks", n_workers = 2)
  expect_identical(res_tie_nofewer$final_selected_model$items, res_tie_th$final_selected_model$items)
  expect_identical(res_tie_nofewer$final_selected_model$items, res_tie_ch$final_selected_model$items)
})

test_that("Phase 4: AUC & Youden independent brute-force cross-size reference equality (K-fold & LOOCV)", {
  set.seed(777)
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 15)),
    Q1 = sample(0:2, 25, replace = TRUE),
    Q2 = sample(0:2, 25, replace = TRUE),
    Q3 = sample(0:2, 25, replace = TRUE),
    Q4 = sample(0:2, 25, replace = TRUE)
  )
  item_pool <- paste0("Q", 1:4)
  n <- nrow(d)
  sizes <- c(1, 3) # discontinuous

  # 1. AUC Independent Brute Force Reference
  all_auc <- list()
  cum_idx <- 0L
  for (s in sizes) {
    cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
    for (gi in seq_along(cmb_mat)) {
      combo <- cmb_mat[[gi]]
      sc <- rowSums(d[, combo, drop = FALSE])
      fr <- compute_score_frequencies(sc, d$y)
      auc_val <- compute_auc_from_table(fr$pos_counts, fr$neg_counts)
      all_auc[[length(all_auc) + 1L]] <- data.frame(
        items = paste(combo, collapse = ", "),
        n_items = s,
        auc = auc_val,
        .global_combo_index = cum_idx + gi,
        stringsAsFactors = FALSE
      )
    }
    cum_idx <- cum_idx + choose(length(item_pool), s)
  }
  ref_auc_df <- do.call(rbind, all_auc)
  # Contract: -auc, n_items, .global_combo_index
  ref_auc_sorted <- ref_auc_df[order(-ref_auc_df$auc, ref_auc_df$n_items, ref_auc_df$.global_combo_index), ]
  rownames(ref_auc_sorted) <- NULL

  res_auc <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), selection_metric = "auc", top_n = 8, folds = 4, seed = 123)
  expect_identical(res_auc$final_selected_model$items, ref_auc_sorted$items[1])
  expect_equal(res_auc$final_selected_model$cv_auc, ref_auc_sorted$auc[1], tolerance = 1e-10)
  expect_identical(res_auc$candidate_ranking$items, ref_auc_sorted$items)
  expect_equal(res_auc$candidate_ranking$auc, ref_auc_sorted$auc, tolerance = 1e-10)

  # 2. Youden Independent Brute Force Reference (K-fold)
  folds_k <- .build_cv_folds(d$y, cv_method = "kfold", folds = 4, seed = 321)
  all_youd_k <- list()
  cum_idx <- 0L
  for (s in sizes) {
    cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
    for (gi in seq_along(cmb_mat)) {
      combo <- cmb_mat[[gi]]
      pred_cls <- integer(n)
      for (f_name in names(folds_k)) {
        test_idx  <- folds_k[[f_name]]
        train_idx <- setdiff(seq_len(n), test_idx)

        tr_dat <- d[train_idx, , drop = FALSE]
        te_dat <- d[test_idx, , drop = FALSE]

        tr_sc <- rowSums(tr_dat[, combo, drop = FALSE])
        tr_fr <- compute_score_frequencies(tr_sc, tr_dat$y)
        tr_roc <- compute_roc_metrics_from_table(tr_fr$pos_counts, tr_fr$neg_counts)
        tr_cut <- find_optimal_cutoff(tr_roc, method = "youden")$cutoff

        te_sc <- rowSums(te_dat[, combo, drop = FALSE])
        pred_cls[test_idx] <- as.integer(te_sc >= tr_cut)
      }

      tp <- sum(pred_cls == 1L & d$y == 1L)
      tn <- sum(pred_cls == 0L & d$y == 0L)
      fp <- sum(pred_cls == 1L & d$y == 0L)
      fn <- sum(pred_cls == 0L & d$y == 1L)
      sens <- tp / (tp + fn)
      spec <- tn / (tn + fp)
      youd <- sens + spec - 1

      all_youd_k[[length(all_youd_k) + 1L]] <- data.frame(
        items = paste(combo, collapse = ", "),
        n_items = s,
        youden = youd,
        .global_combo_index = cum_idx + gi,
        stringsAsFactors = FALSE
      )
    }
    cum_idx <- cum_idx + choose(length(item_pool), s)
  }
  ref_youd_k_df <- do.call(rbind, all_youd_k)
  ref_youd_k_sorted <- ref_youd_k_df[order(-ref_youd_k_df$youden, ref_youd_k_df$n_items, ref_youd_k_df$.global_combo_index), ]
  rownames(ref_youd_k_sorted) <- NULL

  res_youd_k <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold", selection_metric = "youden",
                              folds = 4, top_n = 8, seed = 321)
  expect_identical(res_youd_k$final_selected_model$items, ref_youd_k_sorted$items[1])
  expect_equal(res_youd_k$final_selected_model$cv_youden, ref_youd_k_sorted$youden[1], tolerance = 1e-10)
  expect_identical(res_youd_k$candidate_ranking$items, ref_youd_k_sorted$items)
  expect_equal(res_youd_k$candidate_ranking$cv_youden, ref_youd_k_sorted$youden, tolerance = 1e-10)

  # 3. Youden Independent Brute Force Reference (LOOCV)
  all_youd_loo <- list()
  cum_idx <- 0L
  for (s in sizes) {
    cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
    for (gi in seq_along(cmb_mat)) {
      combo <- cmb_mat[[gi]]
      pred_cls <- integer(n)
      for (i in seq_len(n)) {
        tr_dat <- d[-i, , drop = FALSE]
        te_dat <- d[i, , drop = FALSE]

        tr_sc <- rowSums(tr_dat[, combo, drop = FALSE])
        tr_fr <- compute_score_frequencies(tr_sc, tr_dat$y)
        tr_roc <- compute_roc_metrics_from_table(tr_fr$pos_counts, tr_fr$neg_counts)
        tr_cut <- find_optimal_cutoff(tr_roc, method = "youden")$cutoff

        te_sc <- sum(te_dat[1, combo])
        pred_cls[i] <- as.integer(te_sc >= tr_cut)
      }

      tp <- sum(pred_cls == 1L & d$y == 1L)
      tn <- sum(pred_cls == 0L & d$y == 0L)
      fp <- sum(pred_cls == 1L & d$y == 0L)
      fn <- sum(pred_cls == 0L & d$y == 1L)
      sens <- tp / (tp + fn)
      spec <- tn / (tn + fp)
      youd <- sens + spec - 1

      all_youd_loo[[length(all_youd_loo) + 1L]] <- data.frame(
        items = paste(combo, collapse = ", "),
        n_items = s,
        youden = youd,
        .global_combo_index = cum_idx + gi,
        stringsAsFactors = FALSE
      )
    }
    cum_idx <- cum_idx + choose(length(item_pool), s)
  }
  ref_youd_loo_df <- do.call(rbind, all_youd_loo)
  ref_youd_loo_sorted <- ref_youd_loo_df[order(-ref_youd_loo_df$youden, ref_youd_loo_df$n_items, ref_youd_loo_df$.global_combo_index), ]
  rownames(ref_youd_loo_sorted) <- NULL

  res_youd_loo <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "loocv", selection_metric = "youden", top_n = 8)
  expect_identical(res_youd_loo$final_selected_model$items, ref_youd_loo_sorted$items[1])
  expect_equal(res_youd_loo$final_selected_model$cv_youden, ref_youd_loo_sorted$youden[1], tolerance = 1e-10)
  expect_identical(res_youd_loo$candidate_ranking$items, ref_youd_loo_sorted$items)
  expect_equal(res_youd_loo$candidate_ranking$cv_youden, ref_youd_loo_sorted$youden, tolerance = 1e-10)
})

test_that("Phase 4: Parallel exactness matrix across none / threads / chunks on discontinuous sizes", {
  set.seed(888)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE),
    Q5 = sample(0:2, 40, replace = TRUE)
  )

  # AUC: serial vs threads vs chunks on model_sizes = c(1, 3, 5)
  res_auc_none    <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), selection_metric = "auc", parallel = "none", seed = 555)
  res_auc_threads <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), selection_metric = "auc", parallel = "threads", n_workers = 2, seed = 555)
  res_auc_chunks  <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3, 5), selection_metric = "auc", parallel = "chunks", n_workers = 2, seed = 555)

  expect_identical(res_auc_none$final_selected_model$items, res_auc_threads$final_selected_model$items)
  expect_identical(res_auc_none$final_selected_model$items, res_auc_chunks$final_selected_model$items)
  expect_equal(res_auc_none$candidate_ranking$auc, res_auc_threads$candidate_ranking$auc, tolerance = 1e-10)
  expect_equal(res_auc_none$candidate_ranking$auc, res_auc_chunks$candidate_ranking$auc, tolerance = 1e-10)

  # Youden: serial vs threads vs chunks on model_sizes = c(1, 3)
  res_youd_none    <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden", parallel = "none", folds = 4, seed = 555)
  res_youd_threads <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden", parallel = "threads", n_workers = 2, folds = 4, seed = 555)
  res_youd_chunks  <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden", parallel = "chunks", n_workers = 2, folds = 4, seed = 555)

  expect_identical(res_youd_none$final_selected_model$items, res_youd_threads$final_selected_model$items)
  expect_identical(res_youd_none$final_selected_model$items, res_youd_chunks$final_selected_model$items)
  expect_identical(res_youd_none$candidate_ranking$items, res_youd_threads$candidate_ranking$items)
  expect_identical(res_youd_none$candidate_ranking$items, res_youd_chunks$candidate_ranking$items)
  expect_equal(res_youd_none$candidate_ranking$cv_youden, res_youd_threads$candidate_ranking$cv_youden, tolerance = 1e-10)
  expect_equal(res_youd_none$candidate_ranking$cv_youden, res_youd_chunks$candidate_ranking$cv_youden, tolerance = 1e-10)
})

test_that("Phase 4: K-fold and LOOCV identify same exact AUC winner (Mathematical Identity)", {
  set.seed(999)
  d <- data.frame(
    y  = sample(0:1, 30, replace = TRUE),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE),
    Q4 = sample(0:2, 30, replace = TRUE)
  )

  # Fixed unweighted sum score AUC mathematical identity:
  # Under selection_metric = "auc", K-fold and LOOCV must pick the exact same winner model and AUC
  res_kfold <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold", folds = 5, selection_metric = "auc", seed = 123)
  res_loocv <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "loocv", selection_metric = "auc")

  expect_identical(res_kfold$final_selected_model$items, res_loocv$final_selected_model$items)
  expect_equal(res_kfold$final_selected_model$cv_auc, res_loocv$final_selected_model$cv_auc, tolerance = 1e-10)
  expect_identical(res_kfold$candidate_ranking$items, res_loocv$candidate_ranking$items)
  expect_equal(res_kfold$candidate_ranking$auc, res_loocv$candidate_ranking$auc, tolerance = 1e-10)
})
