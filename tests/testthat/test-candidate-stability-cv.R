# test-candidate-stability-cv.R — Rigorous tests for candidate_stability_roc (Repeated K-Fold CV)

test_that("candidate_sets validation handles invalid and duplicate inputs correctly", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  # NULL or empty candidate_sets
  expect_error(candidate_stability_roc(d, y, candidate_sets = NULL), "must be provided")
  expect_error(candidate_stability_roc(d, y, candidate_sets = list()), "must be a non-empty named list")

  # Missing / unnamed / duplicate labels
  expect_error(candidate_stability_roc(d, y, candidate_sets = list("Q1", c("Q1", "Q2"))), "named list")
  expect_error(candidate_stability_roc(d, y, candidate_sets = list("A" = "Q1", "A" = c("Q1", "Q2"))), "unique, non-empty names")
  expect_error(candidate_stability_roc(d, y, candidate_sets = setNames(list("Q1"), "")), "unique, non-empty names")

  # Missing items in data
  expect_error(candidate_stability_roc(d, y, candidate_sets = list("M1" = c("Q1", "Q99"))), "not found in `data`")

  # Duplicated items within candidate
  expect_error(candidate_stability_roc(d, y, candidate_sets = list("M1" = c("Q1", "Q1"))), "duplicated items")

  # Canonical duplicate candidate sets (e.g. Q1+Q2 and Q2+Q1)
  expect_error(
    candidate_stability_roc(d, y, candidate_sets = list("ModelA" = c("Q1", "Q2"), "ModelB" = c("Q2", "Q1"))),
    "Duplicate candidate definitions found"
  )

  # Invalid resampling mode error
  expect_error(
    candidate_stability_roc(d, y, candidate_sets = list("M1" = "Q1"), resampling = "invalid_mode"),
    "should be one of"
  )

  # Outcome validation
  d_one_class <- data.frame(y = rep(1, 20), Q1 = sample(0:2, 20, replace = TRUE))
  expect_error(candidate_stability_roc(d_one_class, y, candidate_sets = list("M1" = "Q1")), "both positive and negative")

  # Outcome NA error
  d_na_y <- d
  d_na_y$y[1] <- NA
  expect_error(candidate_stability_roc(d_na_y, y, candidate_sets = list("M1" = "Q1")), "contains missing \\(NA\\) values")
})

test_that("Strict integer scalar validation for folds, repeats, and n_workers", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  # folds non-integer / invalid validation
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = 3.5), "integer-valued scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = NA), "integer-valued scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = Inf), "integer-valued scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = c(3, 4)), "integer-valued scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = 1), "2 <= folds < n_obs")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, folds = 40), "2 <= folds < n_obs")

  # repeats non-integer / invalid validation
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, repeats = 2.5), "positive integer scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, repeats = NA), "positive integer scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, repeats = Inf), "positive integer scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, repeats = c(2, 3)), "positive integer scalar")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, repeats = 0), "positive integer scalar")

  # n_workers invalid validation
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = 0), "positive integer scalar or NULL")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = -1), "positive integer scalar or NULL")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = 2.5), "positive integer scalar or NULL")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = NA), "positive integer scalar or NULL")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = Inf), "positive integer scalar or NULL")
  expect_error(candidate_stability_roc(d, y, candidate_sets = cands, n_workers = c(1, 2)), "positive integer scalar or NULL")
})

test_that("Candidate item data validation enforces numeric, non-missing, finite values", {
  set.seed(42)
  d_char <- data.frame(y = sample(0:1, 30, replace = TRUE), Q1 = as.character(sample(0:2, 30, replace = TRUE)))
  expect_error(candidate_stability_roc(d_char, y, candidate_sets = list("M1" = "Q1")), "must be numeric")

  d_na <- data.frame(y = sample(0:1, 30, replace = TRUE), Q1 = sample(0:2, 30, replace = TRUE))
  d_na$Q1[5] <- NA
  expect_error(candidate_stability_roc(d_na, y, candidate_sets = list("M1" = "Q1")), "contains NA values")

  d_inf <- data.frame(y = sample(0:1, 30, replace = TRUE), Q1 = sample(0:2, 30, replace = TRUE))
  d_inf$Q1[5] <- Inf
  expect_error(candidate_stability_roc(d_inf, y, candidate_sets = list("M1" = "Q1")), "contains non-finite values")
})

test_that("Repeated CV candidate stability matches independent explicit R reference", {
  set.seed(123)
  N <- 50
  y <- c(rep(1L, 25), rep(0L, 25))
  Q1 <- ifelse(y == 1, rbinom(N, 2, 0.7), rbinom(N, 2, 0.3))
  Q2 <- ifelse(y == 1, rbinom(N, 2, 0.6), rbinom(N, 2, 0.4))
  Q3 <- ifelse(y == 1, rbinom(N, 2, 0.5), rbinom(N, 2, 0.5))
  Q4 <- rbinom(N, 2, 0.5)

  dat <- data.frame(y = y, Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4)

  cands <- list(
    "M1" = "Q1",
    "M2" = c("Q1", "Q2"),
    "M3" = c("Q1", "Q3", "Q4"),
    "M4" = c("Q2", "Q3")
  )

  K <- 3L
  R <- 4L
  seed_val <- 777

  # Run candidate_stability_roc
  res <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    folds          = K,
    repeats        = R,
    cutoff_method  = "youden",
    rank_by        = "youden",
    prefer_fewer_items = TRUE,
    parallel       = "none",
    seed           = seed_val,
    progress       = FALSE
  )

  expect_s3_class(res, "candidate_stability_result")

  # 1. Independent reference implementation
  ref_folds <- NCVROC:::.build_cv_folds(y, folds = K, repeats = R, stratified = TRUE, seed = seed_val)

  # Check Apparent Performance for each candidate
  for (i in seq_along(cands)) {
    items_i <- cands[[i]]
    scores_i <- rowSums(dat[, items_i, drop = FALSE])
    freq_i <- NCVROC:::compute_score_frequencies(scores_i, y)
    ref_auc <- NCVROC:::compute_auc_from_table(freq_i$pos_counts, freq_i$neg_counts)
    ref_roc <- NCVROC:::compute_roc_metrics_from_table(freq_i$pos_counts, freq_i$neg_counts)
    ref_opt <- NCVROC:::find_optimal_cutoff(ref_roc, method = "youden")

    cand_row <- res$apparent_performance[res$apparent_performance$candidate_id == i, ]
    expect_equal(cand_row$apparent_auc, ref_auc, tolerance = 1e-9)
    expect_equal(cand_row$apparent_cutoff, ref_opt$cutoff, tolerance = 1e-9)
    expect_equal(cand_row$apparent_sensitivity, ref_opt$sensitivity, tolerance = 1e-9)
    expect_equal(cand_row$apparent_specificity, ref_opt$specificity, tolerance = 1e-9)
    expect_equal(cand_row$apparent_youden, ref_opt$youden, tolerance = 1e-9)
    expect_equal(cand_row$apparent_accuracy, ref_opt$accuracy, tolerance = 1e-9)
  }

  # Check Repeat-level OOF metrics against independent loop
  for (i in seq_along(cands)) {
    items_i <- cands[[i]]
    for (r in seq_len(R)) {
      oof_preds <- integer(N)
      for (k in seq_len(K)) {
        fold_name <- paste0("Rep", r, "_Fold", k)
        test_idx <- ref_folds[[fold_name]]
        train_idx <- setdiff(seq_len(N), test_idx)

        train_s <- rowSums(dat[train_idx, items_i, drop = FALSE])
        train_y <- y[train_idx]
        t_freq <- NCVROC:::compute_score_frequencies(train_s, train_y)
        t_roc  <- NCVROC:::compute_roc_metrics_from_table(t_freq$pos_counts, t_freq$neg_counts)
        t_opt  <- NCVROC:::find_optimal_cutoff(t_roc, method = "youden")

        test_s <- rowSums(dat[test_idx, items_i, drop = FALSE])
        oof_preds[test_idx] <- ifelse(test_s >= t_opt$cutoff, 1L, 0L)
      }

      tp <- sum(oof_preds == 1L & y == 1L)
      tn <- sum(oof_preds == 0L & y == 0L)
      fp <- sum(oof_preds == 1L & y == 0L)
      fn <- sum(oof_preds == 0L & y == 1L)

      ref_sens <- tp / (tp + fn)
      ref_spec <- tn / (tn + fp)
      ref_acc  <- (tp + tn) / N
      ref_youd <- ref_sens + ref_spec - 1

      res_sub <- res$resample_results[res$resample_results$candidate_id == i & res$resample_results$repeat_id == r, ]

      # AUC identity check
      expect_equal(res_sub$auc, res$apparent_performance$apparent_auc[i], tolerance = 1e-9)
      expect_equal(res_sub$sensitivity, ref_sens, tolerance = 1e-9)
      expect_equal(res_sub$specificity, ref_spec, tolerance = 1e-9)
      expect_equal(res_sub$accuracy, ref_acc, tolerance = 1e-9)
      expect_equal(res_sub$youden, ref_youd, tolerance = 1e-9)
    }
  }

  # 2. Check Candidate Summary Aggregations
  for (i in seq_along(cands)) {
    sub_res <- res$resample_results[res$resample_results$candidate_id == i, ]
    sum_row <- res$candidate_summary[res$candidate_summary$candidate_id == i, ]

    expect_equal(sum_row$resampled_auc_mean, sum_row$apparent_auc, tolerance = 1e-9)
    expect_equal(sum_row$resampled_auc_sd, 0.0, tolerance = 1e-9)
    expect_equal(sum_row$resampled_youden_mean, mean(sub_res$youden), tolerance = 1e-9)
    expect_equal(sum_row$resampled_youden_sd, sd(sub_res$youden), tolerance = 1e-9)
    expect_equal(sum_row$resampled_youden_median, median(sub_res$youden), tolerance = 1e-9)
    expect_equal(sum_row$resampled_sensitivity_mean, mean(sub_res$sensitivity), tolerance = 1e-9)
    expect_equal(sum_row$resampled_specificity_mean, mean(sub_res$specificity), tolerance = 1e-9)

    # Apparent minus resampled gap
    expect_equal(sum_row$apparent_minus_resampled_youden, sum_row$apparent_youden - sum_row$resampled_youden_mean, tolerance = 1e-9)
    expect_equal(sum_row$apparent_minus_resampled_sens, sum_row$apparent_sensitivity - sum_row$resampled_sensitivity_mean, tolerance = 1e-9)
    expect_equal(sum_row$apparent_minus_resampled_spec, sum_row$apparent_specificity - sum_row$resampled_specificity_mean, tolerance = 1e-9)
  }

  # 3. Selection Frequency Partition Identity
  total_sel_freq <- sum(res$selection_frequency$selection_frequency_all)
  no_feas_freq   <- res$settings$no_feasible_frequency
  inv_eval_freq  <- res$settings$invalid_evaluation_frequency
  expect_equal(total_sel_freq + no_feas_freq + inv_eval_freq, 1.0, tolerance = 1e-9)
})

test_that("rank_by = 'auc' without constraints produces identical rank across all CV repeats", {
  set.seed(999)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  cands <- list(
    "M1" = "Q1",
    "M2" = c("Q1", "Q2"),
    "M3" = c("Q2", "Q3", "Q4")
  )

  res_auc <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = cands,
    folds          = 3,
    repeats        = 5,
    rank_by        = "auc",
    seed           = 42
  )

  # Because apparent AUC is constant across repeats, rank must be constant
  for (i in 1:3) {
    sub <- res_auc$resample_results[res_auc$resample_results$candidate_id == i, ]
    expect_equal(length(unique(sub$rank)), 1L)
    expect_equal(res_auc$candidate_summary$rank_sd[res_auc$candidate_summary$candidate_id == i], 0.0)
  }
})

test_that("rank_sd is NA_real_ when exactly one repeat is evaluated", {
  set.seed(42)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res_1rep <- candidate_stability_roc(
    dat, y, candidate_sets = cands,
    folds = 3, repeats = 1,
    seed = 42
  )

  # With 1 repeat, rank_sd must be NA_real_
  expect_true(all(is.na(res_1rep$candidate_summary$rank_sd)))
  expect_true(all(is.na(res_1rep$candidate_summary$resampled_youden_sd)))
  expect_true(all(is.na(res_1rep$candidate_summary$resampled_sensitivity_sd)))
})

test_that("Constraint feasibility and selection frequency handle no-winner conditions properly", {
  set.seed(42)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = "Q2")

  # Impossible feasibility constraint (Sens >= 1.01)
  res_nofeas <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = cands,
    folds           = 3,
    repeats         = 4,
    sensitivity_min = 1.0,
    specificity_min = 1.0,
    seed            = 42
  )

  expect_true(all(res_nofeas$replicate_metadata$selection_status == "no_feasible_candidate"))
  expect_true(all(is.na(res_nofeas$replicate_metadata$winner_candidate_id)))
  expect_equal(res_nofeas$settings$no_feasible_frequency, 1.0)
  expect_equal(sum(res_nofeas$selection_frequency$selection_count), 0L)
  expect_true(all(is.na(res_nofeas$selection_frequency$selection_frequency_conditional)))
  expect_equal(sum(res_nofeas$selection_frequency$selection_frequency_all) + res_nofeas$settings$no_feasible_frequency, 1.0)
})

test_that("Tie breaking honors prefer_fewer_items and deterministic candidate_id", {
  set.seed(42)
  # Construct data where two models have identical predictions
  d <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = c(rep(2, 15), rep(0, 15)),
    Q2 = rep(0, 30),
    Q3 = rep(0, 30)
  )

  cands <- list(
    "M1" = "Q1",
    "M2" = c("Q1", "Q2"),
    "M3" = c("Q1", "Q2", "Q3")
  )

  # prefer_fewer_items = TRUE -> M1 rank 1, M2 rank 2, M3 rank 3
  res_few <- candidate_stability_roc(
    d, y, candidate_sets = cands, folds = 3, repeats = 2,
    prefer_fewer_items = TRUE, seed = 42
  )
  expect_equal(res_few$candidate_summary$candidate_id[res_few$candidate_summary$mean_rank == 1], 1L)
  expect_equal(res_few$candidate_summary$candidate_id[res_few$candidate_summary$mean_rank == 2], 2L)
  expect_equal(res_few$candidate_summary$candidate_id[res_few$candidate_summary$mean_rank == 3], 3L)

  # prefer_fewer_items = FALSE -> ties broken by candidate_id in input order
  res_notfew <- candidate_stability_roc(
    d, y, candidate_sets = cands, folds = 3, repeats = 2,
    prefer_fewer_items = FALSE, seed = 42
  )
  expect_equal(res_notfew$candidate_summary$candidate_id[res_notfew$candidate_summary$mean_rank == 1], 1L)
  expect_equal(res_notfew$candidate_summary$candidate_id[res_notfew$candidate_summary$mean_rank == 2], 2L)
})

test_that("Parallel backends (none, threads, chunks) produce exact identical results and route to correct engine", {
  set.seed(42)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("A" = "Q1", "B" = c("Q1", "Q2"), "C" = c("Q2", "Q3"))

  NCVROC:::.reset_candidate_stability_routing_counters()

  # 1. Test serial (none)
  res_none <- candidate_stability_roc(dat, y, cands, folds = 3, repeats = 3, parallel = "none", seed = 123)
  counters <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(counters$serial, 1L)
  expect_equal(counters$threads, 0L)
  expect_equal(counters$chunks, 0L)

  # 2. Test threads (single-process RcppParallel multithreading)
  res_threads <- candidate_stability_roc(dat, y, cands, folds = 3, repeats = 3, parallel = "threads", n_workers = 2, seed = 123)
  counters <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(counters$serial, 1L)
  expect_equal(counters$threads, 1L)
  expect_equal(counters$chunks, 0L)

  # 3. Test chunks (PSOCK multi-process parallelism)
  res_chunks <- candidate_stability_roc(dat, y, cands, folds = 3, repeats = 3, parallel = "chunks", n_workers = 2, seed = 123)
  counters <- NCVROC:::.get_candidate_stability_routing_counters()
  expect_equal(counters$serial, 1L)
  expect_equal(counters$threads, 1L)
  expect_equal(counters$chunks, 1L)

  # Assert exact numeric equality across all execution modes
  expect_equal(res_none$candidate_summary, res_threads$candidate_summary)
  expect_equal(res_none$candidate_summary, res_chunks$candidate_summary)
  expect_equal(res_none$resample_results, res_threads$resample_results)
  expect_equal(res_none$resample_results, res_chunks$resample_results)
  expect_equal(res_none$replicate_metadata, res_threads$replicate_metadata)
  expect_equal(res_none$replicate_metadata, res_chunks$replicate_metadata)
})

test_that("S3 print and summary methods execute cleanly without errors", {
  set.seed(42)
  dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  res <- candidate_stability_roc(
    dat, y, cands, folds = 3, repeats = 2,
    sensitivity_min = 0.5, specificity_min = 0.5, seed = 42
  )

  # Test print method
  out_p <- capture.output(print(res))
  expect_true(any(grepl("Candidate-Level Resampling Stability Audit", out_p)))
  expect_true(any(grepl("Repeated K-Fold Cross-Validation", out_p)))

  # Test summary method
  sm <- summary(res)
  expect_s3_class(sm, "summary_candidate_stability_result")
  out_s <- capture.output(print(sm))
  expect_true(any(grepl("Summary of Candidate Stability", out_s)))
})
