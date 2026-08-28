# test-candidate-stability-screen.R — Combinatorial Screening Mode Tests for candidate_stability_roc()

test_that("Mode 1 vs Mode 2 input validation and mutual exclusivity", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )

  # Both NULL
  expect_error(
    candidate_stability_roc(dat, outcome = y, candidate_sets = NULL, items = NULL),
    "Either `candidate_sets` \\(Mode 1\\) or `items` \\(Mode 2\\) must be provided"
  )

  # Both non-NULL
  expect_error(
    candidate_stability_roc(
      dat, outcome = y,
      candidate_sets = list("M1" = "Q1"),
      items = Q1:Q3
    ),
    "`candidate_sets` and `items` are mutually exclusive"
  )
})

test_that("model_sizes and screen_top_n validation in Mode 2", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )

  # Invalid model_sizes
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 0), "All values in `model_sizes` must be >= 1")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = -1), "All values in `model_sizes` must be >= 1")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1.5), "must be a non-empty numeric/integer vector")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = NA), "must be a non-empty numeric/integer vector")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = Inf), "must be a non-empty numeric/integer vector")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 4), "exceed number of available items")

  # Invalid screen_top_n
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 0), "`screen_top_n` must be a positive integer scalar")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = -1), "`screen_top_n` must be a positive integer scalar")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 1.5), "`screen_top_n` must be a positive integer scalar")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = NA), "`screen_top_n` must be a positive integer scalar")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = Inf), "`screen_top_n` must be a positive integer scalar")
  expect_error(candidate_stability_roc(dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = c(1, 2)), "`screen_top_n` must be a positive integer scalar")
})

test_that("Stage-1 independent exactness test against complete combination loop", {
  set.seed(123)
  dat <- data.frame(
    y  = c(rep(1, 20), rep(0, 20)),
    Q1 = sample(0:3, 40, replace = TRUE),
    Q2 = sample(0:3, 40, replace = TRUE),
    Q3 = sample(0:3, 40, replace = TRUE),
    Q4 = sample(0:3, 40, replace = TRUE),
    Q5 = sample(0:3, 40, replace = TRUE)
  )

  item_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")
  sizes <- c(1, 3)

  # Manual independent enumeration of all candidates across size 1 and size 3
  manual_combos <- list()
  manual_metrics <- list()
  cum_idx <- 1L

  for (s in sizes) {
    combs_s <- utils::combn(item_names, s, simplify = FALSE)
    for (cand in combs_s) {
      sc <- rowSums(dat[, cand, drop = FALSE])
      fr <- compute_score_frequencies(sc, dat$y)
      auc_val <- compute_auc_from_table(fr$pos_counts, fr$neg_counts)
      roc_tbl <- compute_roc_metrics_from_table(fr$pos_counts, fr$neg_counts)
      best_cut <- find_optimal_cutoff(roc_tbl, method = "youden")

      manual_combos[[length(manual_combos) + 1L]] <- cand
      manual_metrics[[length(manual_metrics) + 1L]] <- data.frame(
        global_combo_index = cum_idx,
        n_items            = s,
        items              = paste(cand, collapse = ", "),
        auc                = auc_val,
        youden             = best_cut$youden,
        accuracy           = best_cut$accuracy,
        sensitivity        = best_cut$sensitivity,
        specificity        = best_cut$specificity,
        cutoff             = best_cut$cutoff,
        stringsAsFactors   = FALSE
      )
      cum_idx <- cum_idx + 1L
    }
  }

  all_manual_df <- do.call(rbind, manual_metrics)
  ord_manual_auc <- order(-all_manual_df$auc, all_manual_df$n_items, all_manual_df$global_combo_index)
  manual_ranked_auc <- all_manual_df[ord_manual_auc, , drop = FALSE]

  # Run candidate_stability_roc in Mode 2 with rank_by = 'auc', screen_top_n = 5
  res_screen_auc <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q5,
    model_sizes  = c(1, 3),
    screen_top_n = 5L,
    rank_by      = "auc",
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )

  # Check that the top 5 candidates in Stage 1 match manual ranking
  screened_items <- res_screen_auc$apparent_performance$items
  expected_items <- manual_ranked_auc$items[1:5]
  expect_identical(screened_items, expected_items)

  screened_g_idx <- res_screen_auc$apparent_performance$global_combo_index
  expected_g_idx <- manual_ranked_auc$global_combo_index[1:5]
  expect_identical(screened_g_idx, expected_g_idx)

  # Check for rank_by = 'youden'
  ord_manual_yd <- order(-all_manual_df$youden, all_manual_df$n_items, all_manual_df$global_combo_index)
  manual_ranked_yd <- all_manual_df[ord_manual_yd, , drop = FALSE]

  res_screen_yd <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q5,
    model_sizes  = c(1, 3),
    screen_top_n = 5L,
    rank_by      = "youden",
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )

  expect_identical(res_screen_yd$apparent_performance$items, manual_ranked_yd$items[1:5])
  expect_identical(res_screen_yd$apparent_performance$global_combo_index, manual_ranked_yd$global_combo_index[1:5])

  # Check for rank_by = 'sensitivity'
  ord_manual_sens <- order(-all_manual_df$sensitivity, all_manual_df$n_items, all_manual_df$global_combo_index)
  manual_ranked_sens <- all_manual_df[ord_manual_sens, , drop = FALSE]

  res_screen_sens <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q5,
    model_sizes  = c(1, 3),
    screen_top_n = 5L,
    rank_by      = "sensitivity",
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )
  expect_identical(res_screen_sens$apparent_performance$items, manual_ranked_sens$items[1:5])
  expect_identical(res_screen_sens$apparent_performance$global_combo_index, manual_ranked_sens$global_combo_index[1:5])

  # Check for rank_by = 'specificity'
  ord_manual_spec <- order(-all_manual_df$specificity, all_manual_df$n_items, all_manual_df$global_combo_index)
  manual_ranked_spec <- all_manual_df[ord_manual_spec, , drop = FALSE]

  res_screen_spec <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q5,
    model_sizes  = c(1, 3),
    screen_top_n = 5L,
    rank_by      = "specificity",
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )
  expect_identical(res_screen_spec$apparent_performance$items, manual_ranked_spec$items[1:5])
  expect_identical(res_screen_spec$apparent_performance$global_combo_index, manual_ranked_spec$global_combo_index[1:5])

  # Check for rank_by = 'accuracy'
  ord_manual_acc <- order(-all_manual_df$accuracy, all_manual_df$n_items, all_manual_df$global_combo_index)
  manual_ranked_acc <- all_manual_df[ord_manual_acc, , drop = FALSE]

  res_screen_acc <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q5,
    model_sizes  = c(1, 3),
    screen_top_n = 5L,
    rank_by      = "accuracy",
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )
  expect_identical(res_screen_acc$apparent_performance$items, manual_ranked_acc$items[1:5])
  expect_identical(res_screen_acc$apparent_performance$global_combo_index, manual_ranked_acc$global_combo_index[1:5])
})

test_that("screen_top_n edge cases work correctly", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )

  # 3 items, sizes = 1:2 -> choose(3,1) + choose(3,2) = 3 + 3 = 6 candidates
  # screen_top_n = 1
  res_1 <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 1L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_1$settings$screened_candidate_count, 1L)
  expect_equal(nrow(res_1$candidate_summary), 1L)

  # screen_top_n = 2
  res_2 <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 2L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_2$settings$screened_candidate_count, 2L)
  expect_equal(nrow(res_2$candidate_summary), 2L)

  # screen_top_n = total (6)
  res_6 <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 6L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_6$settings$screened_candidate_count, 6L)
  expect_equal(nrow(res_6$candidate_summary), 6L)

  # screen_top_n > total (100)
  res_100 <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q3, model_sizes = 1:2, screen_top_n = 100L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_100$settings$screened_candidate_count, 6L)
  expect_equal(nrow(res_100$candidate_summary), 6L)
})

test_that("discontinuous model_sizes and unsorted input canonicalization", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE),
    Q4 = sample(0:2, 30, replace = TRUE),
    Q5 = sample(0:2, 30, replace = TRUE)
  )

  # model_sizes = c(1, 3) -> 5 + 10 = 15 combos
  res_disc <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q5, model_sizes = c(1, 3), screen_top_n = 100L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_disc$settings$total_candidate_count, 15L)
  expect_false(any(res_disc$candidate_summary$n_items == 2L))
  expect_true(all(res_disc$candidate_summary$n_items %in% c(1L, 3L)))

  # unsorted with duplicates: c(3, 1, 3)
  res_unsort <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q5, model_sizes = c(3, 1, 3), screen_top_n = 100L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_equal(res_unsort$settings$model_sizes, c(1L, 3L))
  expect_equal(res_unsort$settings$total_candidate_count, 15L)
  expect_identical(res_disc$apparent_performance$items, res_unsort$apparent_performance$items)
})

test_that("clinical constraints must NOT leak into Stage 1 screening", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 20), rep(0, 20)),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Run A: unconstrained
  res_uncon <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    items           = Q1:Q4,
    model_sizes     = 1:3,
    screen_top_n    = 8L,
    rank_by         = "youden",
    sensitivity_min = NULL,
    specificity_min = NULL,
    resampling      = "repeated_cv",
    folds           = 3,
    repeats         = 2,
    seed            = 123
  )

  # Run B: highly constrained
  res_con <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    items           = Q1:Q4,
    model_sizes     = 1:3,
    screen_top_n    = 8L,
    rank_by         = "youden",
    sensitivity_min = 0.90,
    specificity_min = 0.90,
    resampling      = "repeated_cv",
    folds           = 3,
    repeats         = 2,
    seed            = 123
  )

  # Stage 1 screened candidates and their apparent metrics must be 100% IDENTICAL
  expect_identical(res_uncon$candidate_definitions, res_con$candidate_definitions)
  expect_identical(res_uncon$apparent_performance$items, res_con$apparent_performance$items)
  expect_identical(res_uncon$apparent_performance$global_combo_index, res_con$apparent_performance$global_combo_index)
  expect_identical(res_uncon$apparent_performance$apparent_youden, res_con$apparent_performance$apparent_youden)

  # But Stage 2 feasibility and selection results may differ
  expect_false(identical(res_uncon$selection_frequency$selection_count, res_con$selection_frequency$selection_count))
})

test_that("Stage 1 -> Stage 2 reuse: Mode 2 equals Mode 1 with screened candidate_sets", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 25), rep(0, 25)),
    Q1 = sample(0:2, 50, replace = TRUE),
    Q2 = sample(0:2, 50, replace = TRUE),
    Q3 = sample(0:2, 50, replace = TRUE),
    Q4 = sample(0:2, 50, replace = TRUE)
  )

  # Run Mode 2
  res_mode2 <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    items           = Q1:Q4,
    model_sizes     = 1:2,
    screen_top_n    = 5L,
    rank_by         = "youden",
    sensitivity_min = 0.60,
    specificity_min = 0.60,
    resampling      = "repeated_cv",
    folds           = 3,
    repeats         = 3,
    seed            = 999
  )

  # Extract the screened candidates from Mode 2
  screened_cands <- res_mode2$candidate_definitions

  # Run Mode 1 with the exact same candidate sets and resampling configuration
  res_mode1 <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = screened_cands,
    rank_by         = "youden",
    sensitivity_min = 0.60,
    specificity_min = 0.60,
    resampling      = "repeated_cv",
    folds           = 3,
    repeats         = 3,
    seed            = 999
  )

  # Assert equality of resample results, ranking, selection frequencies, and constraint pass rates
  expect_equal(res_mode2$resample_results$test_youden, res_mode1$resample_results$test_youden)
  expect_equal(res_mode2$resample_results$test_sensitivity, res_mode1$resample_results$test_sensitivity)
  expect_equal(res_mode2$resample_results$test_specificity, res_mode1$resample_results$test_specificity)
  expect_equal(res_mode2$resample_results$rank, res_mode1$resample_results$rank)
  expect_equal(res_mode2$replicate_metadata$winner_candidate_id, res_mode1$replicate_metadata$winner_candidate_id)
  expect_equal(res_mode2$candidate_summary$selection_frequency_all, res_mode1$candidate_summary$selection_frequency_all)
  expect_equal(res_mode2$candidate_summary$mean_rank, res_mode1$candidate_summary$mean_rank)
  expect_equal(res_mode2$candidate_summary$both_pass_frequency, res_mode1$candidate_summary$both_pass_frequency)
})

test_that("conditionality metadata and print/summary notices in Mode 2 vs Mode 1", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )

  # Mode 1: conditional_on_screen == FALSE
  res_m1 <- candidate_stability_roc(
    dat, outcome = y,
    candidate_sets = list("M1" = "Q1", "M2" = c("Q1", "Q2")),
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_false(res_m1$settings$conditional_on_screen)
  out_m1 <- paste(capture.output(print(res_m1)), collapse = "\n")
  expect_false(grepl("Screening Metric", out_m1))
  expect_false(grepl("Stage-1 screened", out_m1))

  # Mode 2: conditional_on_screen == TRUE
  res_m2 <- candidate_stability_roc(
    dat, outcome = y,
    items = Q1:Q3, model_sizes = 1:2, screen_top_n = 4L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42
  )
  expect_true(res_m2$settings$conditional_on_screen)
  expect_equal(res_m2$settings$screening_metric, "youden")
  expect_equal(res_m2$settings$screen_top_n_requested, 4L)
  expect_equal(res_m2$settings$screened_candidate_count, 4L)
  expect_equal(res_m2$settings$total_candidate_count, 6L)

  out_m2 <- paste(capture.output(print(res_m2)), collapse = "\n")
  expect_true(grepl("Screening Metric", out_m2))
  expect_true(grepl("Stage-1 screened", out_m2))

  # Summary method inspection
  sm_m2 <- summary(res_m2)
  expect_true(sm_m2$settings$conditional_on_screen)
  out_sm <- paste(capture.output(print(sm_m2)), collapse = "\n")
  expect_true(grepl("Stage-1 screened", out_sm))
})

test_that("parallel execution routing in Mode 2 across serial, threads, chunks", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 20), rep(0, 20)),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Serial
  res_none <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q4, model_sizes = 1:2, screen_top_n = 5L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42, parallel = "none"
  )

  # Threads
  res_th <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q4, model_sizes = 1:2, screen_top_n = 5L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42, parallel = "threads", n_workers = 2
  )

  # Chunks
  res_ch <- candidate_stability_roc(
    dat, outcome = y, items = Q1:Q4, model_sizes = 1:2, screen_top_n = 5L,
    resampling = "repeated_cv", folds = 3, repeats = 2, seed = 42, parallel = "chunks", n_workers = 2
  )

  expect_equal(res_none$candidate_summary$mean_rank, res_th$candidate_summary$mean_rank)
  expect_equal(res_none$candidate_summary$mean_rank, res_ch$candidate_summary$mean_rank)
  expect_equal(res_none$candidate_summary$selection_frequency_all, res_th$candidate_summary$selection_frequency_all)
  expect_equal(res_none$candidate_summary$selection_frequency_all, res_ch$candidate_summary$selection_frequency_all)
})
