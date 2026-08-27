# test-cross-size-nested-cv.R — Unit tests for cross_size_nested_cv()

test_that("cross_size_nested_cv basic execution and schema validation", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  res <- cross_size_nested_cv(
    d, y, Q1:Q3,
    model_sizes   = 1:2,
    outer_folds   = 3,
    inner_folds   = 2,
    outer_repeats = 2,
    seed          = 42,
    progress      = FALSE
  )

  expect_s3_class(res, "cross_size_nested_cv_result")
  expect_equal(nrow(res$outer_fold_results), 6L) # 3 folds * 2 repeats
  expect_length(res$selected_models_by_outer_fold, 6L)
  expect_equal(nrow(res$summary), 7L) # auc, sens, spec, youden, acc, ppv, npv
  expect_true(all(!is.na(res$summary$mean)))
  expect_true(all(!is.na(res$summary$sd)))

  # Frequencies
  expect_equal(sum(res$model_size_selection_frequency$n_selections), 6L)
  expect_equal(sum(res$model_size_selection_frequency$frequency), 1.0)
  expect_equal(sum(res$item_combination_selection_frequency$n_selections), 6L)
  expect_equal(sum(res$item_combination_selection_frequency$frequency), 1.0)

  # Cutoff distribution
  expect_length(res$cutoff_distribution$per_fold_values, 6L)

  # Outer predictions
  expect_equal(nrow(res$outer_predictions), 40L * 2L)

  # Print / summary methods work without error
  expect_output(print(res), "Cross-Size Nested Cross-Validation")
  expect_output(summary(res), "Outer-Test Generalization Performance")
})

test_that("Strict direct outer-fold leakage test: inverting outer test outcome does not alter inner selection", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  train_idx <- 1:30
  test_idx  <- 31:40

  train_data <- d[train_idx, , drop = FALSE]
  test_data  <- d[test_idx, , drop = FALSE]
  train_y    <- d$y[train_idx]
  test_y_orig <- d$y[test_idx]
  test_y_inv  <- 1L - test_y_orig

  eval_A <- NCVROC:::.evaluate_cross_size_outer_fold(
    train_data         = train_data,
    test_data          = test_data,
    train_y            = train_y,
    test_y             = test_y_orig,
    outcome_name       = "y",
    item_names         = c("Q1", "Q2", "Q3"),
    sizes              = 1:2,
    inner_folds        = 3,
    inner_repeats      = 1,
    stratified         = TRUE,
    selection_metric   = "auc",
    cutoff_method      = "youden",
    sensitivity_min    = NULL,
    specificity_min    = NULL,
    prefer_fewer_items = TRUE,
    positive_label     = 1,
    negative_label     = 0,
    engine             = "Rcpp",
    parallel_mode      = "none",
    n_workers_res      = 1L,
    threads_per_worker = 1L,
    seed               = 123,
    fold_name          = "Fold1",
    rep_id             = 1L,
    f_id               = 1L,
    test_idx           = test_idx
  )

  eval_B <- NCVROC:::.evaluate_cross_size_outer_fold(
    train_data         = train_data,
    test_data          = test_data,
    train_y            = train_y,
    test_y             = test_y_inv,
    outcome_name       = "y",
    item_names         = c("Q1", "Q2", "Q3"),
    sizes              = 1:2,
    inner_folds        = 3,
    inner_repeats      = 1,
    stratified         = TRUE,
    selection_metric   = "auc",
    cutoff_method      = "youden",
    sensitivity_min    = NULL,
    specificity_min    = NULL,
    prefer_fewer_items = TRUE,
    positive_label     = 1,
    negative_label     = 0,
    engine             = "Rcpp",
    parallel_mode      = "none",
    n_workers_res      = 1L,
    threads_per_worker = 1L,
    seed               = 123,
    fold_name          = "Fold1",
    rep_id             = 1L,
    f_id               = 1L,
    test_idx           = test_idx
  )

  expect_identical(eval_A$fold_result$selected_items, eval_B$fold_result$selected_items)
  expect_identical(eval_A$fold_result$selected_n_items, eval_B$fold_result$selected_n_items)
  expect_identical(eval_A$fold_result$selected_cutoff, eval_B$fold_result$selected_cutoff)

  expect_false(isTRUE(all.equal(eval_A$fold_result$outer_auc, eval_B$fold_result$outer_auc)))
  expect_equal(eval_A$fold_result$outer_accuracy, 1 - eval_B$fold_result$outer_accuracy)
})

# =========================================================================
# COMPREHENSIVE NESTED TEST MATRIX (3 PATHS x 5 MODES)
# =========================================================================

test_that("nested CV 5-mode exactness matrix: Path 1 - AUC unconstrained [none vs outer vs threads vs chunks vs hybrid]", {
  set.seed(555)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  .reset_routing_counters()
  res_none    <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", outer_folds = 2, inner_folds = 2,
                                      outer_repeats = 1, parallel = "none", seed = 555, progress = FALSE)
  res_outer   <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", outer_folds = 2, inner_folds = 2,
                                      outer_repeats = 1, parallel = "outer", n_workers = 2, seed = 555, progress = FALSE)
  res_threads <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", outer_folds = 2, inner_folds = 2,
                                      outer_repeats = 1, parallel = "threads", n_workers = 2, seed = 555, progress = FALSE)
  res_chunks  <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", outer_folds = 2, inner_folds = 2,
                                      outer_repeats = 1, parallel = "chunks", n_workers = 2, seed = 555, progress = FALSE)
  res_hybrid  <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", outer_folds = 2, inner_folds = 2,
                                      outer_repeats = 1, parallel = "hybrid", n_workers = 2, threads_per_worker = 1,
                                      seed = 555, progress = FALSE)

  # Check settings
  expect_equal(res_none$settings$parallel, "none")
  expect_equal(res_outer$settings$parallel, "outer")
  expect_equal(res_threads$settings$parallel, "threads")
  expect_equal(res_chunks$settings$parallel, "chunks")
  expect_equal(res_hybrid$settings$parallel, "hybrid")

  # 1. Selected models by outer fold are 100% identical across all 5 modes
  expect_identical(res_none$selected_models_by_outer_fold, res_outer$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_threads$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_chunks$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_hybrid$selected_models_by_outer_fold)

  # 2. Selected cutoffs are 100% identical
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_outer$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_threads$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_chunks$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_hybrid$outer_fold_results$selected_cutoff)

  # 3. Outer test summary performance is 100% identical
  expect_equal(res_none$summary$mean, res_outer$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_threads$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_chunks$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_hybrid$summary$mean, tolerance = 1e-10)

  # 4. Predictions are identical
  expect_equal(res_none$outer_predictions$predicted_score, res_outer$outer_predictions$predicted_score)
  expect_equal(res_none$outer_predictions$predicted_score, res_hybrid$outer_predictions$predicted_score)

  # 5. Hybrid oversubscription assertion: inner PSOCK cluster creations == 0
  expect_equal(.NCVROC_ROUTING_COUNTERS$inner_psock_count, 0L)
  expect_gt(.NCVROC_ROUTING_COUNTERS$outer_psock_count, 0L)
})

test_that("nested CV 5-mode exactness matrix: Path 2 - AUC + OOF constraint [none vs outer vs threads vs chunks vs hybrid]", {
  set.seed(777)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  .reset_routing_counters()
  res_none    <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", sensitivity_min = 0.30,
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1, parallel = "none", seed = 777, progress = FALSE)
  res_outer   <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", sensitivity_min = 0.30,
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1, parallel = "outer", n_workers = 2, seed = 777, progress = FALSE)
  res_threads <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", sensitivity_min = 0.30,
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1, parallel = "threads", n_workers = 2, seed = 777, progress = FALSE)
  res_chunks  <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", sensitivity_min = 0.30,
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1, parallel = "chunks", n_workers = 2, seed = 777, progress = FALSE)
  res_hybrid  <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "auc", sensitivity_min = 0.30,
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1, parallel = "hybrid", n_workers = 2, threads_per_worker = 1,
                                      seed = 777, progress = FALSE)

  expect_identical(res_none$selected_models_by_outer_fold, res_outer$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_threads$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_chunks$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_hybrid$selected_models_by_outer_fold)

  expect_equal(res_none$summary$mean, res_outer$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_threads$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_chunks$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_hybrid$summary$mean, tolerance = 1e-10)
})

test_that("nested CV 5-mode exactness matrix: Path 3 - Cutoff-dependent (Youden) [none vs outer vs threads vs chunks vs hybrid]", {
  set.seed(666)
  d <- data.frame(
    y  = sample(0:1, 36, replace = TRUE),
    Q1 = sample(0:2, 36, replace = TRUE),
    Q2 = sample(0:2, 36, replace = TRUE),
    Q3 = sample(0:2, 36, replace = TRUE)
  )

  .reset_routing_counters()
  res_none    <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "youden",
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1,
                                      parallel = "none", seed = 666, progress = FALSE)
  res_outer   <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "youden",
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1,
                                      parallel = "outer", n_workers = 2, seed = 666, progress = FALSE)
  res_threads <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "youden",
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1,
                                      parallel = "threads", n_workers = 2, seed = 666, progress = FALSE)
  res_chunks  <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "youden",
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1,
                                      parallel = "chunks", n_workers = 2, seed = 666, progress = FALSE)
  res_hybrid  <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2, selection_metric = "youden",
                                      outer_folds = 2, inner_folds = 2, outer_repeats = 1,
                                      parallel = "hybrid", n_workers = 2, threads_per_worker = 1,
                                      seed = 666, progress = FALSE)

  # All 5 modes identical on all components
  expect_identical(res_none$selected_models_by_outer_fold, res_outer$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_threads$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_chunks$selected_models_by_outer_fold)
  expect_identical(res_none$selected_models_by_outer_fold, res_hybrid$selected_models_by_outer_fold)

  expect_equal(res_none$outer_fold_results$selected_cutoff, res_outer$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_threads$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_chunks$outer_fold_results$selected_cutoff)
  expect_equal(res_none$outer_fold_results$selected_cutoff, res_hybrid$outer_fold_results$selected_cutoff)

  expect_equal(res_none$summary$mean, res_outer$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_threads$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_chunks$summary$mean, tolerance = 1e-10)
  expect_equal(res_none$summary$mean, res_hybrid$summary$mean, tolerance = 1e-10)

  expect_identical(res_none$model_size_selection_frequency, res_outer$model_size_selection_frequency)
  expect_identical(res_none$item_combination_selection_frequency, res_outer$item_combination_selection_frequency)
})

test_that("Seed determinism verification: distinct seeds yield distinct fold assignments and model selections", {
  set.seed(111)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  res_seed1 <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, outer_folds = 3, inner_folds = 2, seed = 111, progress = FALSE)
  res_seed2 <- cross_size_nested_cv(d, y, Q1:Q4, model_sizes = 1:2, outer_folds = 3, inner_folds = 2, seed = 999, progress = FALSE)

  # Different seed produces different outer test partitions
  expect_false(identical(res_seed1$outer_predictions$fold_id, res_seed2$outer_predictions$fold_id))
})

test_that("cross_size_nested_cv argument validation and engine compatibility", {
  d <- data.frame(y = c(1,0,1,0), Q1 = c(1,0,1,0), Q2 = c(0,1,1,0))

  # engine = 'R' cannot be used with threads or hybrid
  expect_error(cross_size_nested_cv(d, y, Q1:Q2, model_sizes = 1:2, engine = "R", parallel = "threads"), "requires `engine = 'Rcpp'`")
  expect_error(cross_size_nested_cv(d, y, Q1:Q2, model_sizes = 1:2, engine = "R", parallel = "hybrid"), "requires `engine = 'Rcpp'`")

  # threads_per_worker > 1 only allowed in hybrid mode
  expect_error(cross_size_nested_cv(d, y, Q1:Q2, model_sizes = 1:2, parallel = "outer", threads_per_worker = 2), "can only exceed 1 when `parallel = 'hybrid'`")

  # auto is reserved
  expect_error(cross_size_nested_cv(d, y, Q1:Q2, model_sizes = 1:2, parallel = "auto"), "reserved")
})
