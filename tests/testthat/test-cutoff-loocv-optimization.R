# test-cutoff-loocv-optimization.R — Phase 5: Cutoff-Dependent LOOCV Optimization Tests

test_that("Phase 5: Strategy 2 serial C++ vs independent R reference across all selection metrics (Youden cutoff method)", {
  set.seed(333)
  d <- data.frame(
    y  = c(rep(1L, 12), rep(0L, 16)), # N = 28
    Q1 = sample(0:2, 28, replace = TRUE),
    Q2 = sample(0:2, 28, replace = TRUE),
    Q3 = sample(0:2, 28, replace = TRUE),
    Q4 = sample(0:2, 28, replace = TRUE)
  )
  item_pool <- paste0("Q", 1:4)
  n <- nrow(d)
  sizes <- c(1, 3)

  # 1. K-Fold References for Youden, Accuracy, Sensitivity, Specificity
  folds_k <- .build_cv_folds(d$y, cv_method = "kfold", folds = 4, seed = 543)

  eval_kfold_ref <- function(metric_name, cut_method = "youden") {
    all_res <- list()
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
          tr_cut <- find_optimal_cutoff(tr_roc, method = cut_method)$cutoff

          te_sc <- rowSums(te_dat[, combo, drop = FALSE])
          pred_cls[test_idx] <- as.integer(te_sc >= tr_cut)
        }

        tp <- sum(pred_cls == 1L & d$y == 1L)
        tn <- sum(pred_cls == 0L & d$y == 0L)
        fp <- sum(pred_cls == 1L & d$y == 0L)
        fn <- sum(pred_cls == 0L & d$y == 1L)
        sens <- tp / (tp + fn)
        spec <- tn / (tn + fp)
        acc  <- (tp + tn) / n
        youd <- sens + spec - 1

        val <- switch(
          metric_name,
          "youden"      = youd,
          "accuracy"    = acc,
          "sensitivity" = sens,
          "specificity" = spec
        )

        all_res[[length(all_res) + 1L]] <- data.frame(
          items = paste(combo, collapse = ", "),
          n_items = s,
          metric_val = val,
          .global_combo_index = cum_idx + gi,
          stringsAsFactors = FALSE
        )
      }
      cum_idx <- cum_idx + choose(length(item_pool), s)
    }
    df <- do.call(rbind, all_res)
    df[order(-df$metric_val, df$n_items, df$.global_combo_index), ]
  }

  for (m in c("youden", "accuracy", "sensitivity", "specificity")) {
    ref_k <- eval_kfold_ref(m, cut_method = "youden")
    pkg_k <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "kfold", selection_metric = m,
                           cutoff_method = "youden", folds = 4, top_n = 8, seed = 543)
    pkg_metric_col <- paste0("cv_", m)
    expect_identical(pkg_k$final_selected_model$items, ref_k$items[1])
    expect_equal(pkg_k$final_selected_model[[pkg_metric_col]], ref_k$metric_val[1], tolerance = 1e-10)
    expect_identical(pkg_k$candidate_ranking$items, ref_k$items)
    expect_equal(pkg_k$candidate_ranking[[pkg_metric_col]], ref_k$metric_val, tolerance = 1e-10)
  }

  # 2. LOOCV References for Youden, Accuracy, Sensitivity, Specificity
  eval_loocv_ref <- function(metric_name, cut_method = "youden") {
    all_res <- list()
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
          tr_cut <- find_optimal_cutoff(tr_roc, method = cut_method)$cutoff

          te_sc <- sum(te_dat[1, combo])
          pred_cls[i] <- as.integer(te_sc >= tr_cut)
        }

        tp <- sum(pred_cls == 1L & d$y == 1L)
        tn <- sum(pred_cls == 0L & d$y == 0L)
        fp <- sum(pred_cls == 1L & d$y == 0L)
        fn <- sum(pred_cls == 0L & d$y == 1L)
        sens <- tp / (tp + fn)
        spec <- tn / (tn + fp)
        acc  <- (tp + tn) / n
        youd <- sens + spec - 1

        val <- switch(
          metric_name,
          "youden"      = youd,
          "accuracy"    = acc,
          "sensitivity" = sens,
          "specificity" = spec
        )

        all_res[[length(all_res) + 1L]] <- data.frame(
          items = paste(combo, collapse = ", "),
          n_items = s,
          metric_val = val,
          .global_combo_index = cum_idx + gi,
          stringsAsFactors = FALSE
        )
      }
      cum_idx <- cum_idx + choose(length(item_pool), s)
    }
    df <- do.call(rbind, all_res)
    df[order(-df$metric_val, df$n_items, df$.global_combo_index), ]
  }

  for (m in c("youden", "accuracy", "sensitivity", "specificity")) {
    ref_loo <- eval_loocv_ref(m, cut_method = "youden")
    pkg_loo <- cross_size_cv(d, y, Q1:Q4, model_sizes = c(1, 3), cv_method = "loocv", selection_metric = m,
                             cutoff_method = "youden", top_n = 8)
    pkg_metric_col <- paste0("cv_", m)
    expect_identical(pkg_loo$final_selected_model$items, ref_loo$items[1])
    expect_equal(pkg_loo$final_selected_model[[pkg_metric_col]], ref_loo$metric_val[1], tolerance = 1e-10)
    expect_identical(pkg_loo$candidate_ranking$items, ref_loo$items)
    expect_equal(pkg_loo$candidate_ranking[[pkg_metric_col]], ref_loo$metric_val, tolerance = 1e-10)
  }
})

test_that("Phase 5.1 & 5.2: cutoff_method = 'closest_topleft' matches independent reference loop (metrics and fold cutoffs)", {
  set.seed(555)
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 14)),
    Q1 = sample(0:2, 24, replace = TRUE),
    Q2 = sample(0:2, 24, replace = TRUE),
    Q3 = sample(0:2, 24, replace = TRUE),
    Q4 = sample(0:2, 24, replace = TRUE)
  )
  item_pool <- paste0("Q", 1:4)
  n <- nrow(d)
  sizes <- c(1, 2)

  # LOOCV with closest_topleft
  all_res <- list()
  all_cutoffs <- list()
  cum_idx <- 0L
  for (s in sizes) {
    cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
    for (gi in seq_along(cmb_mat)) {
      combo <- cmb_mat[[gi]]
      pred_cls <- integer(n)
      cutoffs  <- numeric(n)
      for (i in seq_len(n)) {
        tr_dat <- d[-i, , drop = FALSE]
        te_dat <- d[i, , drop = FALSE]

        tr_sc <- rowSums(tr_dat[, combo, drop = FALSE])
        tr_fr <- compute_score_frequencies(tr_sc, tr_dat$y)
        tr_roc <- compute_roc_metrics_from_table(tr_fr$pos_counts, tr_fr$neg_counts)
        tr_cut <- find_optimal_cutoff(tr_roc, method = "closest_topleft")$cutoff
        cutoffs[i] <- tr_cut

        te_sc <- sum(te_dat[1, combo])
        pred_cls[i] <- as.integer(te_sc >= tr_cut)
      }

      tp <- sum(pred_cls == 1L & d$y == 1L)
      tn <- sum(pred_cls == 0L & d$y == 0L)
      fp <- sum(pred_cls == 1L & d$y == 0L)
      fn <- sum(pred_cls == 0L & d$y == 1L)
      sens <- tp / (tp + fn)
      spec <- tn / (tn + fp)
      acc  <- (tp + tn) / n

      combo_name <- paste(combo, collapse = ", ")
      all_res[[length(all_res) + 1L]] <- data.frame(
        items = combo_name,
        n_items = s,
        accuracy = acc,
        .global_combo_index = cum_idx + gi,
        stringsAsFactors = FALSE
      )
      all_cutoffs[[combo_name]] <- cutoffs
    }
    cum_idx <- cum_idx + choose(length(item_pool), s)
  }
  ref_topleft_df <- do.call(rbind, all_res)
  ref_topleft_ranked <- ref_topleft_df[order(-ref_topleft_df$accuracy, ref_topleft_df$n_items, ref_topleft_df$.global_combo_index), ]

  res_topleft <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv",
                               selection_metric = "accuracy", cutoff_method = "closest_topleft", top_n = 10)

  # 1. Candidate ranking and accuracy match
  expect_identical(res_topleft$final_selected_model$items, ref_topleft_ranked$items[1])
  expect_equal(res_topleft$final_selected_model$cv_accuracy, ref_topleft_ranked$accuracy[1], tolerance = 1e-10)
  expect_identical(res_topleft$candidate_ranking$items, ref_topleft_ranked$items)
  expect_equal(res_topleft$candidate_ranking$cv_accuracy, ref_topleft_ranked$accuracy, tolerance = 1e-10)

  # 2. Winning model fold cutoffs match reference
  best_items <- res_topleft$final_selected_model$items
  ref_best_cutoffs <- all_cutoffs[[best_items]]
  expect_equal(res_topleft$oof_predictions$applied_cutoff, ref_best_cutoffs, tolerance = 1e-12)
})

test_that("Phase 5.2: Direct Youden and closest_topleft tie-breaking contract & classification boundary", {
  # 1. Direct Youden tie-breaking test with explicit expected value
  # Construct training frequency table with 3 score levels where Youden is 0 for all cutoffs
  # Score 1: pos=1, neg=1; Score 2: pos=1, neg=1
  # Cutoff 1: Sens=1.0, Spec=0.0 -> Youden=0.0
  # Cutoff 2: Sens=0.5, Spec=0.5 -> Youden=0.0
  # Cutoff 3: Sens=0.0, Spec=1.0 -> Youden=0.0
  # Tie chain: max Youden (tie 0.0) -> max Sens (1.0 at cutoff 1) -> Cutoff 1 wins!
  fr_tie <- compute_score_frequencies(c(2, 1, 2, 1), c(1, 1, 0, 0))
  roc_tie <- compute_roc_metrics_from_table(fr_tie$pos_counts, fr_tie$neg_counts)
  cut_youd_opt <- find_optimal_cutoff(roc_tie, method = "youden")
  expect_equal(cut_youd_opt$cutoff, 1)
  expect_equal(cut_youd_opt$sensitivity, 1.0)
  expect_equal(cut_youd_opt$specificity, 0.0)
  expect_equal(cut_youd_opt$youden, 0.0)

  # 2. Direct closest_topleft tie-breaking test with explicit expected value
  # On the same ROC table:
  # Dist to (0,1):
  # Cutoff 1: (1-1.0)^2 + (1-0.0)^2 = 1.0
  # Cutoff 2: (1-0.5)^2 + (1-0.5)^2 = 0.5 (min dist!)
  # Cutoff 3: (1-0.0)^2 + (1-1.0)^2 = 1.0
  # Step 1: min dist -> Cutoff 2 wins!
  cut_top_opt <- find_optimal_cutoff(roc_tie, method = "closest_topleft")
  expect_equal(cut_top_opt$cutoff, 2)
  expect_equal(cut_top_opt$sensitivity, 0.5)
  expect_equal(cut_top_opt$specificity, 0.5)

  # 3. Direct R reference cutoff == C++ applied_cutoff on synthetic LOOCV dataset
  d_syn <- data.frame(
    y  = c(1L, 1L, 1L, 0L, 0L, 0L),
    Q1 = c(2, 1, 2, 2, 1, 1)
  )
  n_syn <- nrow(d_syn)

  # Compute R reference cutoffs for each LOOCV fold
  ref_syn_youd_cuts <- numeric(n_syn)
  for (i in seq_len(n_syn)) {
    tr_d <- d_syn[-i, ]
    tr_fr <- compute_score_frequencies(tr_d$Q1, tr_d$y)
    tr_roc <- compute_roc_metrics_from_table(tr_fr$pos_counts, tr_fr$neg_counts)
    ref_syn_youd_cuts[i] <- find_optimal_cutoff(tr_roc, method = "youden")$cutoff
  }

  res_syn_youd <- cross_size_cv(d_syn, y, Q1, model_sizes = 1, cv_method = "loocv",
                                selection_metric = "youden", cutoff_method = "youden")
  expect_equal(res_syn_youd$oof_predictions$applied_cutoff, ref_syn_youd_cuts, tolerance = 1e-12)

  # 4. Classification boundary: score == cutoff -> predicted_class == 1L (inclusive >= cutoff rule)
  for (r in seq_len(nrow(res_syn_youd$oof_predictions))) {
    row_score <- res_syn_youd$oof_predictions$predicted_score[r]
    row_cut   <- res_syn_youd$oof_predictions$applied_cutoff[r]
    row_pred  <- res_syn_youd$oof_predictions$predicted_class[r]
    if (row_score >= row_cut) {
      expect_equal(row_pred, 1L)
    } else {
      expect_equal(row_pred, 0L)
    }
  }
})

test_that("Phase 5: Routing instrumentation and backend exactness (none, threads, chunks)", {
  set.seed(444)
  d <- data.frame(
    y  = sample(0:1, 30, replace = TRUE),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE),
    Q4 = sample(0:2, 30, replace = TRUE),
    Q5 = sample(0:2, 30, replace = TRUE)
  )

  # 1. Strategy 2 Serial routing
  .reset_routing_counters()
  res_none <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden",
                            parallel = "none", folds = 4, seed = 777)
  expect_gt(.NCVROC_ROUTING_COUNTERS$strategy2_serial_count, 0L)
  expect_equal(.NCVROC_ROUTING_COUNTERS$strategy2_threads_count, 0L)
  expect_equal(.NCVROC_ROUTING_COUNTERS$strategy2_chunks_count, 0L)

  # 2. Strategy 2 Threads routing (True C++ RcppParallel backend)
  .reset_routing_counters()
  res_th <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden",
                          parallel = "threads", n_workers = 2, folds = 4, seed = 777)
  expect_gt(.NCVROC_ROUTING_COUNTERS$strategy2_threads_count, 0L)
  expect_equal(.NCVROC_ROUTING_COUNTERS$strategy2_serial_count, 0L)
  expect_equal(.NCVROC_ROUTING_COUNTERS$strategy2_chunks_count, 0L)

  # 3. Strategy 2 Chunks routing (PSOCK cluster)
  .reset_routing_counters()
  res_ch <- cross_size_cv(d, y, Q1:Q5, model_sizes = c(1, 3), selection_metric = "youden",
                          parallel = "chunks", n_workers = 2, folds = 4, seed = 777)
  expect_gt(.NCVROC_ROUTING_COUNTERS$strategy2_chunks_count, 0L)
  expect_equal(.NCVROC_ROUTING_COUNTERS$strategy2_threads_count, 0L)

  # Exactness assertion across all 3 modes
  expect_identical(res_none$final_selected_model$items, res_th$final_selected_model$items)
  expect_identical(res_none$final_selected_model$items, res_ch$final_selected_model$items)
  expect_equal(res_none$final_selected_model$cv_youden, res_th$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_equal(res_none$final_selected_model$cv_youden, res_ch$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_identical(res_none$candidate_ranking$items, res_th$candidate_ranking$items)
  expect_identical(res_none$candidate_ranking$items, res_ch$candidate_ranking$items)
  expect_equal(res_none$candidate_ranking$cv_youden, res_th$candidate_ranking$cv_youden, tolerance = 1e-10)
  expect_equal(res_none$candidate_ranking$cv_youden, res_ch$candidate_ranking$cv_youden, tolerance = 1e-10)
  expect_equal(res_none$oof_predictions$applied_cutoff, res_th$oof_predictions$applied_cutoff)
  expect_equal(res_none$oof_predictions$predicted_class, res_th$oof_predictions$predicted_class)
})
