# tests/testthat/test-candidate-stability-bootstrap.R — Comprehensive tests for Bootstrap Resampling in candidate_stability_roc()

test_that("Deterministic closest_topleft and youden cutoff tie-breaking matches existing NCVROC contract", {
  # Synthetic ROC table with tied distances and different Youden / cutoffs
  roc_synth <- data.frame(
    cutoff      = c(5.0, 3.0, 7.0),
    sensitivity = c(0.8, 0.6, 0.6),
    specificity = c(0.6, 0.8, 0.8),
    youden      = c(0.4, 0.4, 0.4),
    accuracy    = c(0.7, 0.7, 0.7),
    ppv         = c(0.7, 0.7, 0.7),
    npv         = c(0.7, 0.7, 0.7),
    stringsAsFactors = FALSE
  )

  # Closest topleft on roc_synth: distance is equal for all three (sqrt(0.2)), youden is equal (0.4)
  # Contract: min cutoff (3.0) must win
  opt_topleft <- find_optimal_cutoff(roc_synth, method = "closest_topleft")
  expect_equal(opt_topleft$cutoff, 3.0)

  # Youden on roc_synth: youden is equal (0.4), higher sensitivity (0.8 vs 0.6) must win -> cutoff 5.0
  opt_youden <- find_optimal_cutoff(roc_synth, method = "youden")
  expect_equal(opt_youden$cutoff, 5.0)

  # Deterministic dataset for candidate_stability_roc with closest_topleft
  dat_det <- data.frame(
    y  = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    Q1 = c(5, 5, 4, 3, 2, 4, 3, 2, 1, 1),
    Q2 = c(3, 2, 2, 1, 1, 2, 1, 1, 0, 0)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res_topleft <- candidate_stability_roc(
    data           = dat_det,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 10L,
    bootstrap_test = "original",
    cutoff_method  = "closest_topleft",
    parallel       = "none",
    seed           = 123
  )

  res_topleft_threads <- candidate_stability_roc(
    data           = dat_det,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 10L,
    bootstrap_test = "original",
    cutoff_method  = "closest_topleft",
    parallel       = "threads",
    n_workers      = 2L,
    seed           = 123
  )

  res_topleft_chunks <- candidate_stability_roc(
    data           = dat_det,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 10L,
    bootstrap_test = "original",
    cutoff_method  = "closest_topleft",
    parallel       = "chunks",
    n_workers      = 2L,
    seed           = 123
  )

  expect_equal(res_topleft$candidate_summary, res_topleft_threads$candidate_summary)
  expect_equal(res_topleft$candidate_summary, res_topleft_chunks$candidate_summary)
  expect_equal(res_topleft$resample_results, res_topleft_threads$resample_results)
  expect_equal(res_topleft$resample_results, res_topleft_chunks$resample_results)
})

test_that("Independent pure-R serial bootstrap reference matches C++ results exactly for both original and OOB modes", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q3 = sample(0:2, N, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"), "M3" = c("Q1", "Q3"))

  B <- 15L
  seed_val <- 777

  # 1. Evaluate via candidate_stability_roc (original mode)
  res_orig <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = B,
    bootstrap_test = "original",
    cutoff_method  = "youden",
    parallel       = "none",
    seed           = seed_val
  )

  # 2. Independent pure R reference implementation for original mode
  set.seed(seed_val)
  r_ref_rows_orig <- list()
  for (b in seq_len(B)) {
    tr_idx <- sample.int(N, size = N, replace = TRUE)
    tr_y <- dat$y[tr_idx]
    is_tr_valid <- (sum(tr_y == 1L) > 0L && sum(tr_y == 0L) > 0L)

    for (i in seq_along(cands)) {
      items_i <- cands[[i]]
      tr_scores <- rowSums(dat[tr_idx, items_i, drop = FALSE])
      full_scores <- rowSums(dat[, items_i, drop = FALSE])

      if (!is_tr_valid) {
        r_ref_rows_orig[[length(r_ref_rows_orig) + 1L]] <- list(
          candidate_id = i, replicate_id = b,
          train_auc = NA_real_, train_cutoff = NA_real_,
          test_auc = NA_real_, test_sensitivity = NA_real_, test_specificity = NA_real_
        )
      } else {
        tr_freq <- compute_score_frequencies(tr_scores, tr_y)
        tr_auc <- compute_auc_from_table(tr_freq$pos_counts, tr_freq$neg_counts)
        tr_roc <- compute_roc_metrics_from_table(tr_freq$pos_counts, tr_freq$neg_counts)
        tr_opt <- find_optimal_cutoff(tr_roc, method = "youden")
        cut_b <- tr_opt$cutoff

        pred_full <- ifelse(full_scores >= cut_b, 1L, 0L)
        tp <- sum(pred_full == 1L & dat$y == 1L)
        tn <- sum(pred_full == 0L & dat$y == 0L)
        fp <- sum(pred_full == 1L & dat$y == 0L)
        fn <- sum(pred_full == 0L & dat$y == 1L)

        ts_sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
        ts_spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
        ts_youd <- if (!is.na(ts_sens) && !is.na(ts_spec)) ts_sens + ts_spec - 1.0 else NA_real_
        ts_acc  <- (tp + tn) / N
        ts_ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
        ts_npv  <- if (tn + fn > 0) tn / (tn + fn) else NA_real_

        full_freq <- compute_score_frequencies(full_scores, dat$y)
        ts_auc <- compute_auc_from_table(full_freq$pos_counts, full_freq$neg_counts)

        r_ref_rows_orig[[length(r_ref_rows_orig) + 1L]] <- list(
          candidate_id      = i,
          replicate_id      = b,
          train_auc         = tr_auc,
          train_cutoff      = cut_b,
          train_sensitivity = tr_opt$sensitivity,
          train_specificity = tr_opt$specificity,
          train_youden      = tr_opt$youden,
          train_accuracy    = tr_opt$accuracy,
          train_ppv         = tr_opt$ppv,
          train_npv         = tr_opt$npv,
          test_auc          = ts_auc,
          test_sensitivity  = ts_sens,
          test_specificity  = ts_spec,
          test_youden       = ts_youd,
          test_accuracy     = ts_acc,
          test_ppv          = ts_ppv,
          test_npv          = ts_npv
        )
      }
    }
  }

  r_ref_df_orig <- do.call(rbind.data.frame, r_ref_rows_orig)
  ord_ref <- order(r_ref_df_orig$candidate_id, r_ref_df_orig$replicate_id)
  r_ref_df_orig <- r_ref_df_orig[ord_ref, ]
  rownames(r_ref_df_orig) <- NULL

  c_res <- res_orig$resample_results
  expect_equal(c_res$train_auc, r_ref_df_orig$train_auc)
  expect_equal(c_res$train_cutoff, r_ref_df_orig$train_cutoff)
  expect_equal(c_res$train_sensitivity, r_ref_df_orig$train_sensitivity)
  expect_equal(c_res$train_specificity, r_ref_df_orig$train_specificity)
  expect_equal(c_res$train_youden, r_ref_df_orig$train_youden)
  expect_equal(c_res$train_accuracy, r_ref_df_orig$train_accuracy)
  expect_equal(c_res$train_ppv, r_ref_df_orig$train_ppv)
  expect_equal(c_res$train_npv, r_ref_df_orig$train_npv)

  expect_equal(c_res$test_auc, r_ref_df_orig$test_auc)
  expect_equal(c_res$test_sensitivity, r_ref_df_orig$test_sensitivity)
  expect_equal(c_res$test_specificity, r_ref_df_orig$test_specificity)
  expect_equal(c_res$test_youden, r_ref_df_orig$test_youden)
  expect_equal(c_res$test_accuracy, r_ref_df_orig$test_accuracy)
  expect_equal(c_res$test_ppv, r_ref_df_orig$test_ppv)
  expect_equal(c_res$test_npv, r_ref_df_orig$test_npv)

  # 3. Independent pure R reference implementation for OOB mode
  res_oob <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = B,
    bootstrap_test = "oob",
    cutoff_method  = "youden",
    parallel       = "none",
    seed           = seed_val
  )

  set.seed(seed_val)
  r_ref_rows_oob <- list()
  for (b in seq_len(B)) {
    tr_idx <- sample.int(N, size = N, replace = TRUE)
    oob_idx <- setdiff(seq_len(N), unique(tr_idx))
    tr_y <- dat$y[tr_idx]
    is_tr_valid <- (sum(tr_y == 1L) > 0L && sum(tr_y == 0L) > 0L)

    for (i in seq_along(cands)) {
      items_i <- cands[[i]]
      tr_scores <- rowSums(dat[tr_idx, items_i, drop = FALSE])
      oob_scores <- rowSums(dat[oob_idx, items_i, drop = FALSE])
      oob_y <- dat$y[oob_idx]

      if (!is_tr_valid) {
        r_ref_rows_oob[[length(r_ref_rows_oob) + 1L]] <- list(
          candidate_id = i, replicate_id = b,
          test_auc = NA_real_, test_sensitivity = NA_real_, test_specificity = NA_real_
        )
      } else {
        tr_freq <- compute_score_frequencies(tr_scores, tr_y)
        tr_roc <- compute_roc_metrics_from_table(tr_freq$pos_counts, tr_freq$neg_counts)
        tr_opt <- find_optimal_cutoff(tr_roc, method = "youden")
        cut_b <- tr_opt$cutoff

        pred_oob <- ifelse(oob_scores >= cut_b, 1L, 0L)
        tp <- sum(pred_oob == 1L & oob_y == 1L)
        tn <- sum(pred_oob == 0L & oob_y == 0L)
        fp <- sum(pred_oob == 1L & oob_y == 0L)
        fn <- sum(pred_oob == 0L & oob_y == 1L)

        ts_sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
        ts_spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
        ts_youd <- if (!is.na(ts_sens) && !is.na(ts_spec)) ts_sens + ts_spec - 1.0 else NA_real_
        ts_acc  <- if (length(oob_idx) > 0) (tp + tn) / length(oob_idx) else NA_real_
        ts_ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
        ts_npv  <- if (tn + fn > 0) tn / (tn + fn) else NA_real_

        n_oob_pos <- sum(oob_y == 1L)
        n_oob_neg <- sum(oob_y == 0L)
        ts_auc <- if (n_oob_pos > 0 && n_oob_neg > 0) {
          oob_freq <- compute_score_frequencies(oob_scores, oob_y)
          compute_auc_from_table(oob_freq$pos_counts, oob_freq$neg_counts)
        } else {
          NA_real_
        }

        r_ref_rows_oob[[length(r_ref_rows_oob) + 1L]] <- list(
          candidate_id      = i,
          replicate_id      = b,
          test_auc          = ts_auc,
          test_sensitivity  = ts_sens,
          test_specificity  = ts_spec,
          test_youden       = ts_youd,
          test_accuracy     = ts_acc,
          test_ppv          = ts_ppv,
          test_npv          = ts_npv
        )
      }
    }
  }

  r_ref_df_oob <- do.call(rbind.data.frame, r_ref_rows_oob)
  ord_oob <- order(r_ref_df_oob$candidate_id, r_ref_df_oob$replicate_id)
  r_ref_df_oob <- r_ref_df_oob[ord_oob, ]
  rownames(r_ref_df_oob) <- NULL

  c_oob_res <- res_oob$resample_results
  expect_equal(c_oob_res$test_auc, r_ref_df_oob$test_auc)
  expect_equal(c_oob_res$test_sensitivity, r_ref_df_oob$test_sensitivity)
  expect_equal(c_oob_res$test_specificity, r_ref_df_oob$test_specificity)
  expect_equal(c_oob_res$test_youden, r_ref_df_oob$test_youden)
  expect_equal(c_oob_res$test_accuracy, r_ref_df_oob$test_accuracy)
  expect_equal(c_oob_res$test_ppv, r_ref_df_oob$test_ppv)
  expect_equal(c_oob_res$test_npv, r_ref_df_oob$test_npv)
})

test_that("Reproducible bootstrap index generation and seed dependency", {
  set.seed(123)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"), "M3" = c("Q1", "Q3"))

  res1 <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 30L,
    bootstrap_test = "original",
    seed           = 42
  )

  res2 <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 30L,
    bootstrap_test = "original",
    seed           = 42
  )

  expect_equal(res1$candidate_summary, res2$candidate_summary)
  expect_equal(res1$resample_results, res2$resample_results)
  expect_equal(res1$replicate_metadata, res2$replicate_metadata)

  res3 <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 30L,
    bootstrap_test = "original",
    seed           = 999
  )

  expect_false(isTRUE(all.equal(res1$candidate_summary$resampled_youden_mean,
                                res3$candidate_summary$resampled_youden_mean)))
})

test_that("Bootstrap reps validation strictly forbids non-integers, NA, Inf, <=0", {
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = rnorm(30)
  )
  cands <- list("M1" = "Q1")

  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = 0), "positive integer")
  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = -5), "positive integer")
  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = 2.5), "positive integer")
  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = NA), "positive integer")
  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = Inf), "positive integer")
  expect_error(candidate_stability_roc(dat, y, cands, resampling = "bootstrap", bootstrap_reps = c(50, 100)), "positive integer")
})

test_that("Bootstrap training sample multiplicity is strictly preserved", {
  dat <- data.frame(
    y  = c(1, 0, 1, 0, 1, 0, 1, 0),
    Q1 = c(10, 0, 8, 2, 9, 1, 7, 3)
  )
  cands <- list("M1" = "Q1")

  res <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 20L,
    bootstrap_test = "original",
    seed           = 42
  )

  expect_true(all(res$resample_results$n_train == 8L))
  expect_true(all(res$replicate_metadata$n_train == 8L))
})

test_that("Critical PPV and NPV edge cases evaluate correctly without false NA or fabricated values", {
  dat_edge <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0),
    Q1 = c(5, 4, 3, 2, 1, 0)
  )
  cands <- list("M1" = "Q1")

  expect_warning(
    res_oob <- candidate_stability_roc(
      data           = dat_edge,
      outcome        = y,
      candidate_sets = cands,
      resampling     = "bootstrap",
      bootstrap_reps = 50L,
      bootstrap_test = "oob",
      seed           = 123
    ),
    "Resampling validity rate < 80%"
  )

  one_class_reps <- which(res_oob$replicate_metadata$n_oob_positive == 0L |
                          res_oob$replicate_metadata$n_oob_negative == 0L)
  if (length(one_class_reps) > 0) {
    sub_res <- res_oob$resample_results[res_oob$resample_results$replicate_id %in% one_class_reps, ]
    expect_true(all(is.na(sub_res$test_auc)))
  }
})

test_that("bootstrap_test='original' preserves test_auc == apparent_auc while train_auc varies", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q3 = sample(0:2, N, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"), "M3" = c("Q1", "Q2", "Q3"))

  res <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 40L,
    bootstrap_test = "original",
    seed           = 42
  )

  for (i in 1:3) {
    app_auc <- res$apparent_performance$apparent_auc[i]
    cand_res <- res$resample_results[res$resample_results$candidate_id == i & res$resample_results$test_valid, ]
    expect_equal(cand_res$test_auc, rep(app_auc, nrow(cand_res)))
    expect_equal(res$candidate_summary$resampled_auc_sd[res$candidate_summary$candidate_id == i], 0.0)
    expect_equal(res$candidate_summary$resampled_auc_mean[res$candidate_summary$candidate_id == i], app_auc)
    expect_gt(stats::sd(cand_res$train_auc), 0.0)
  }
})

test_that("Efron optimism, optimism-corrected metrics, and paired denominators match exact math", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE))
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 30L,
    bootstrap_test = "original",
    seed           = 42
  )

  for (i in 1:2) {
    cand_df <- res$resample_results[res$resample_results$candidate_id == i, ]
    sum_row <- res$candidate_summary[res$candidate_summary$candidate_id == i, ]

    valid_opt_pairs <- !is.na(cand_df$train_youden) & !is.na(cand_df$test_youden)
    diffs_yd <- cand_df$train_youden[valid_opt_pairs] - cand_df$test_youden[valid_opt_pairs]
    expect_equal(sum_row$n_valid_optimism_youden, sum(valid_opt_pairs))
    expect_equal(sum_row$optimism_youden_mean, mean(diffs_yd))
    expect_equal(sum_row$optimism_corrected_youden, sum_row$apparent_youden - mean(diffs_yd))

    valid_opt_sens <- !is.na(cand_df$train_sensitivity) & !is.na(cand_df$test_sensitivity)
    diffs_se <- cand_df$train_sensitivity[valid_opt_sens] - cand_df$test_sensitivity[valid_opt_sens]
    expect_equal(sum_row$n_valid_optimism_sensitivity, sum(valid_opt_sens))
    expect_equal(sum_row$optimism_sensitivity_mean, mean(diffs_se))
    expect_equal(sum_row$optimism_corrected_sensitivity, sum_row$apparent_sensitivity - mean(diffs_se))
  }
})

test_that("Partition identity strictly holds for bootstrap mode", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE))
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = cands,
    resampling      = "bootstrap",
    bootstrap_reps  = 50L,
    bootstrap_test  = "original",
    sensitivity_min = 0.85,
    specificity_min = 0.85,
    seed            = 42
  )

  sum_sel_freq <- sum(res$selection_frequency$selection_frequency_all)
  no_feas_freq <- res$settings$no_feasible_frequency
  invalid_freq <- res$settings$invalid_evaluation_frequency

  expect_equal(sum_sel_freq + no_feas_freq + invalid_freq, 1.0, tolerance = 1e-12)
})

test_that("Parallel backends (none, threads, chunks) produce exact identical results and route to correct engine for bootstrap", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q3 = sample(0:2, N, replace = TRUE),
    Q4 = sample(0:2, N, replace = TRUE)
  )
  cands <- list(
    "M1" = "Q1",
    "M2" = c("Q1", "Q2"),
    "M3" = c("Q1", "Q3"),
    "M4" = c("Q1", "Q2", "Q3", "Q4")
  )

  NCVROC:::.reset_candidate_stability_routing_counters()
  c0 <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(c0$serial, 0L)
  expect_equal(c0$threads, 0L)
  expect_equal(c0$chunks, 0L)

  # 1. Serial (none)
  res_none <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "original",
    parallel       = "none",
    seed           = 42
  )
  c1 <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(c1$serial, 1L)
  expect_equal(c1$threads, 0L)
  expect_equal(c1$chunks, 0L)

  # 2. Threads (RcppParallel single process)
  res_threads <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "original",
    parallel       = "threads",
    n_workers      = 2L,
    seed           = 42
  )
  c2 <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(c2$serial, 1L)
  expect_equal(c2$threads, 1L)
  expect_equal(c2$chunks, 0L)

  # 3. Chunks (PSOCK multi-process)
  res_chunks <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "original",
    parallel       = "chunks",
    n_workers      = 2L,
    seed           = 42
  )
  c3 <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(c3$serial, 1L)
  expect_equal(c3$threads, 1L)
  expect_equal(c3$chunks, 1L)

  # Numerical exactness across all backends
  expect_equal(res_none$candidate_summary, res_threads$candidate_summary)
  expect_equal(res_none$candidate_summary, res_chunks$candidate_summary)
  expect_equal(res_none$resample_results, res_threads$resample_results)
  expect_equal(res_none$resample_results, res_chunks$resample_results)
  expect_equal(res_none$replicate_metadata, res_threads$replicate_metadata)
  expect_equal(res_none$replicate_metadata, res_chunks$replicate_metadata)

  # Also test OOB mode across backends
  res_oob_none <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "oob",
    parallel       = "none",
    seed           = 42
  )
  res_oob_threads <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "oob",
    parallel       = "threads",
    n_workers      = 2L,
    seed           = 42
  )
  res_oob_chunks <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 25L,
    bootstrap_test = "oob",
    parallel       = "chunks",
    n_workers      = 2L,
    seed           = 42
  )
  expect_equal(res_oob_none$candidate_summary, res_oob_threads$candidate_summary)
  expect_equal(res_oob_none$candidate_summary, res_oob_chunks$candidate_summary)
})

test_that("OOB cutoff application matches independent R reference exactly", {
  set.seed(42)
  N <- 50
  y_vec <- sample(0:1, N, replace = TRUE)
  dat <- data.frame(
    y  = y_vec,
    Q1 = ifelse(y_vec == 1, sample(2:4, N, replace = TRUE), sample(0:2, N, replace = TRUE)),
    Q2 = ifelse(y_vec == 1, sample(1:3, N, replace = TRUE), sample(0:2, N, replace = TRUE))
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    resampling     = "bootstrap",
    bootstrap_reps = 10L,
    bootstrap_test = "oob",
    seed           = 42
  )

  expect_true(nrow(res$resample_results) == 20L)
  expect_true(all(res$resample_results$test_sensitivity >= 0 | is.na(res$resample_results$test_sensitivity)))
  expect_true(all(res$resample_results$test_specificity >= 0 | is.na(res$resample_results$test_specificity)))
})
