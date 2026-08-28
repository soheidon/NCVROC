# test-cross-size-cv-execution-planner.R - Phase 1.2 cross_size_cv execution planner tests

.planner_cv_test_data <- function(n_items = 6L, n_pos = 15L, n_neg = 15L) {
  set.seed(42L)
  n_total <- n_pos + n_neg
  mat <- matrix(sample(0:2, n_total * n_items, replace = TRUE), nrow = n_total, ncol = n_items)
  df <- as.data.frame(mat)
  names(df) <- paste0("Q", seq_len(n_items))
  df$y <- rep(c(0L, 1L), c(n_neg, n_pos))
  df
}

test_that("evaluate_combos_cv_cpp grain_size default vs explicit 64 produce identical output", {
  d <- .planner_cv_test_data(6L)
  x_mat <- as.matrix(d[, paste0("Q", 1:6)])
  mode(x_mat) <- "double"
  y_int <- as.integer(d$y)

  cv_folds <- .build_cv_folds(y_int, cv_method = "kfold", folds = 5L, repeats = 1L, seed = 42L)
  test_indices_0based <- lapply(cv_folds, function(f) as.integer(f - 1L))

  combo_indices <- list(
    0:1, 0:2, 1:3, 2:4, c(0L, 2L, 4L), c(1L, 3L, 5L)
  )

  res_default <- evaluate_combos_cv_cpp(
    x               = x_mat,
    y               = y_int,
    combo_indices   = combo_indices,
    test_indices    = test_indices_0based,
    n_folds         = length(cv_folds),
    repeats         = 1L,
    cutoff_method   = "youden",
    num_threads     = 2L
  )

  res_explicit_64 <- evaluate_combos_cv_cpp(
    x               = x_mat,
    y               = y_int,
    combo_indices   = combo_indices,
    test_indices    = test_indices_0based,
    n_folds         = length(cv_folds),
    repeats         = 1L,
    cutoff_method   = "youden",
    num_threads     = 2L,
    grain_size      = 64L
  )

  expect_equal(res_default$cv_auc, res_explicit_64$cv_auc)
  expect_equal(res_default$cv_youden, res_explicit_64$cv_youden)
  expect_equal(res_default$cv_sensitivity, res_explicit_64$cv_sensitivity)
  expect_equal(res_default$cv_specificity, res_explicit_64$cv_specificity)
  expect_equal(res_default$cv_accuracy, res_explicit_64$cv_accuracy)
  expect_equal(res_default$cv_ppv, res_explicit_64$cv_ppv)
  expect_equal(res_default$cv_npv, res_explicit_64$cv_npv)
  expect_equal(res_default$cv_cutoff_mean, res_explicit_64$cv_cutoff_mean)
  expect_equal(res_default$cv_cutoff_sd, res_explicit_64$cv_cutoff_sd)
  expect_equal(res_default$final_full_data_cutoff, res_explicit_64$final_full_data_cutoff)
  expect_identical(res_default$valid, res_explicit_64$valid)
})

test_that("evaluate_combos_cv_cpp different legal grain sizes produce identical output", {
  d <- .planner_cv_test_data(6L)
  x_mat <- as.matrix(d[, paste0("Q", 1:6)])
  mode(x_mat) <- "double"
  y_int <- as.integer(d$y)

  cv_folds <- .build_cv_folds(y_int, cv_method = "kfold", folds = 5L, repeats = 1L, seed = 123L)
  test_indices_0based <- lapply(cv_folds, function(f) as.integer(f - 1L))

  # 15 combinations
  combo_indices <- utils::combn(0:5, 2, simplify = FALSE)

  res_g1 <- evaluate_combos_cv_cpp(
    x_mat, y_int, combo_indices, test_indices_0based,
    n_folds = 5L, repeats = 1L, cutoff_method = "youden",
    num_threads = 2L, grain_size = 1L
  )

  res_g4 <- evaluate_combos_cv_cpp(
    x_mat, y_int, combo_indices, test_indices_0based,
    n_folds = 5L, repeats = 1L, cutoff_method = "youden",
    num_threads = 2L, grain_size = 4L
  )

  res_g16 <- evaluate_combos_cv_cpp(
    x_mat, y_int, combo_indices, test_indices_0based,
    n_folds = 5L, repeats = 1L, cutoff_method = "youden",
    num_threads = 2L, grain_size = 16L
  )

  expect_equal(res_g1$cv_auc, res_g4$cv_auc)
  expect_equal(res_g1$cv_youden, res_g4$cv_youden)
  expect_equal(res_g1$cv_auc, res_g16$cv_auc)
  expect_equal(res_g1$cv_youden, res_g16$cv_youden)
  expect_equal(res_g1$cv_cutoff_mean, res_g16$cv_cutoff_mean)
})

test_that("cross_size_cv tuning='off' preserves legacy behavior and settings structure", {
  d <- .planner_cv_test_data(5L)

  res_off <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    selection_metric = "youden",
    folds            = 5,
    seed             = 42,
    tuning           = "off",
    progress         = FALSE
  )

  expect_s3_class(res_off, "cross_size_cv_result")
  expect_null(res_off$settings$execution_plan)
  expect_false("execution_plan" %in% names(res_off$settings))
  expect_identical(res_off$settings$parallel, "none")
  expect_identical(res_off$settings$n_workers, 1L)
})

test_that("cross_size_cv tuning='auto' skips benchmark on small workloads and matches off exactly", {
  d <- .planner_cv_test_data(5L)

  res_off <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    selection_metric = "youden",
    folds            = 5,
    seed             = 42,
    tuning           = "off",
    progress         = FALSE
  )

  res_auto <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    selection_metric = "youden",
    folds            = 5,
    seed             = 42,
    tuning           = "auto",
    progress         = FALSE
  )

  # Exact statistical equality
  expect_identical(res_auto$final_selected_model, res_off$final_selected_model)
  expect_identical(res_auto$candidate_ranking, res_off$candidate_ranking)
  expect_identical(res_auto$model_size_summary, res_off$model_size_summary)
  expect_identical(res_auto$oof_predictions, res_off$oof_predictions)
  expect_identical(res_auto$cv_performance, res_off$cv_performance)

  # Execution plan metadata presence
  plan <- res_auto$settings$execution_plan
  expect_type(plan, "list")
  expect_true(plan$tuning_performed)
  expect_false(plan$backend_benchmark_performed)
  expect_identical(plan$tuning_mode, "auto")
  expect_identical(plan$target_api, "cross_size_cv")
  expect_identical(plan$total_candidates, 15)
  expect_type(plan$micro_pilot_candidates, "list")
  expect_true(plan$micro_pilot_candidates$total > 0L)
  expect_true(is.numeric(plan$micro_pilot_candidates$by_size))
  expect_identical(plan$selected_parallel, "none")
  expect_identical(plan$selected_n_workers, 1L)
})

test_that("cross_size_cv tuning='always' degenerate workload uses manual plan", {
  d <- .planner_cv_test_data(5L)

  res_always_degen <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    selection_metric = "youden",
    folds            = 5,
    seed             = 99,
    tuning           = "always",
    progress         = FALSE
  )

  plan <- res_always_degen$settings$execution_plan
  expect_true(plan$tuning_performed)
  expect_false(plan$backend_benchmark_performed)
  expect_match(plan$decision_reason, "degenerate workload")
})

test_that("cross_size_cv tuning='always' on non-degenerate workload runs benchmark and matches off exactly", {
  d <- .planner_cv_test_data(8L)

  res_off <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:8),
    model_sizes      = 1:4,
    selection_metric = "youden",
    folds            = 5,
    seed             = 99,
    tuning           = "off",
    progress         = FALSE
  )

  res_always <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:8),
    model_sizes      = 1:4,
    selection_metric = "youden",
    folds            = 5,
    seed             = 99,
    tuning           = "always",
    progress         = FALSE
  )

  # Exact statistical equivalence
  expect_identical(res_always$final_selected_model, res_off$final_selected_model)
  expect_identical(res_always$candidate_ranking, res_off$candidate_ranking)
  expect_identical(res_always$model_size_summary, res_off$model_size_summary)
  expect_identical(res_always$oof_predictions, res_off$oof_predictions)
  expect_identical(res_always$cv_performance, res_off$cv_performance)

  plan <- res_always$settings$execution_plan
  expect_true(plan$tuning_performed)
  expect_true(plan$backend_benchmark_performed)
  expect_identical(plan$tuning_mode, "always")
  expect_true(nrow(plan$benchmark_table) >= 1L)
  expect_identical(plan$decision_reason, "selected near-best benchmark plan")
  expect_identical(plan$cv_method, "kfold")
  expect_identical(plan$k, 5L)
  expect_identical(plan$repeats, 1L)
})

test_that("cross_size_cv RNG stream is not disturbed by planner", {
  d <- .planner_cv_test_data(5L)

  set.seed(2026)
  res_off <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden", folds = 5, seed = 42,
    tuning = "off", progress = FALSE
  )
  rng_post_off <- .Random.seed

  set.seed(2026)
  res_auto <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden", folds = 5, seed = 42,
    tuning = "auto", progress = FALSE
  )
  rng_post_auto <- .Random.seed

  set.seed(2026)
  res_always <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden", folds = 5, seed = 42,
    tuning = "always", progress = FALSE
  )
  rng_post_always <- .Random.seed

  expect_identical(rng_post_off, rng_post_auto)
  expect_identical(rng_post_off, rng_post_always)
})

test_that("cross_size_cv Strategy 1 (AUC) supports tuning without changing statistical results", {
  d <- .planner_cv_test_data(6L)

  res_off <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:6),
    model_sizes      = 1:2,
    selection_metric = "auc",
    folds            = 5,
    seed             = 101,
    tuning           = "off",
    progress         = FALSE
  )

  res_auto <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:6),
    model_sizes      = 1:2,
    selection_metric = "auc",
    folds            = 5,
    seed             = 101,
    tuning           = "auto",
    progress         = FALSE
  )

  expect_identical(res_auto$final_selected_model, res_off$final_selected_model)
  expect_identical(res_auto$candidate_ranking, res_off$candidate_ranking)
  expect_identical(res_auto$oof_predictions, res_off$oof_predictions)
  expect_true(res_auto$settings$execution_plan$tuning_performed)
})

test_that("planner controller handles mocked benchmark failure and fallback gracefully", {
  d <- .planner_cv_test_data(5L)
  dat_mat <- as.matrix(d[, paste0("Q", 1:5)])
  y_int <- as.integer(d$y)
  cv_folds <- .build_cv_folds(y_int, cv_method = "kfold", folds = 5L, repeats = 1L, seed = 42L)

  # Failing benchmark executor for all plans
  failing_executor <- function(...) {
    list(elapsed = NA_real_, success = FALSE, failure_reason = "mocked cluster failure")
  }

  outcome <- .planner_cross_size_cv_controller(
    data_matrix          = dat_mat,
    y                    = y_int,
    item_names           = paste0("Q", 1:5),
    sizes                = 1:2,
    cv_folds             = cv_folds,
    folds                = 5L,
    repeats              = 1L,
    stratified           = TRUE,
    cv_method            = "kfold",
    selection_metric     = "youden",
    cutoff_method        = "youden",
    sensitivity_min      = NULL,
    specificity_min      = NULL,
    engine               = "Rcpp",
    tuning               = "always",
    manual_parallel_mode = "none",
    manual_n_workers     = NULL,
    dependencies         = list(benchmark_executor = failing_executor)
  )

  expect_true(outcome$warn)
  expect_identical(outcome$plan$parallel[[1L]], "none")
  expect_identical(outcome$plan$n_workers[[1L]], 1L)
  expect_match(outcome$metadata$fallback_reason, "manual plan")
})

test_that("planner controller respects mock timer and budget timeout", {
  d <- .planner_cv_test_data(8L)
  dat_mat <- as.matrix(d[, paste0("Q", 1:8)])
  y_int <- as.integer(d$y)
  cv_folds <- .build_cv_folds(y_int, cv_method = "kfold", folds = 5L, repeats = 1L, seed = 42L)

  # Advancing clock
  cur_time <- 0.0
  mock_clock <- function() {
    cur_time <<- cur_time + 5.0
    cur_time
  }

  outcome <- .planner_cross_size_cv_controller(
    data_matrix          = dat_mat,
    y                    = y_int,
    item_names           = paste0("Q", 1:8),
    sizes                = 1:4,
    cv_folds             = cv_folds,
    folds                = 5L,
    repeats              = 1L,
    stratified           = TRUE,
    cv_method            = "kfold",
    selection_metric     = "youden",
    cutoff_method        = "youden",
    sensitivity_min      = NULL,
    specificity_min      = NULL,
    engine               = "Rcpp",
    tuning               = "always",
    manual_parallel_mode = "none",
    manual_n_workers     = NULL,
    dependencies         = list(clock = mock_clock)
  )

  expect_type(outcome$metadata, "list")
  expect_true(outcome$metadata$tuning_budget_exhausted)
})

test_that(".planner_evaluate_cv_pilot_combos faithfully handles R-engine with repeats and constraints", {
  d <- .planner_cv_test_data(6L, n_pos = 10L, n_neg = 10L)
  x_mat <- as.matrix(d[, paste0("Q", 1:6)])
  mode(x_mat) <- "double"
  y_int <- as.integer(d$y)
  cv_folds <- .build_cv_folds(y_int, cv_method = "kfold", folds = 3L, repeats = 2L, seed = 42L)
  test_indices_0based <- lapply(cv_folds, function(f) as.integer(f - 1L))

  combo_indices <- list(0:1, 0:2, c(0L, 2L, 4L))

  # Should run cleanly without error
  expect_silent(.planner_evaluate_cv_pilot_combos(
    x_mat               = x_mat,
    y                   = y_int,
    combo_indices_list  = combo_indices,
    test_indices_0based = test_indices_0based,
    n_folds_total       = length(cv_folds),
    repeats             = 2L,
    cutoff_method       = "youden",
    sens_min            = 0.5,
    spec_min            = 0.5,
    engine              = "R",
    parallel            = "none",
    n_workers           = 1L
  ))
})

test_that("cross_size_cv with R engine, repeated CV, constraints and tuning preserves statistical identity and RNG", {
  d <- .planner_cv_test_data(6L, n_pos = 12L, n_neg = 12L)

  set.seed(123L)
  seed_before_off <- .Random.seed
  res_off <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    folds            = 3L,
    repeats          = 2L,
    selection_metric = "youden",
    sensitivity_min  = 0.3,
    specificity_min  = 0.3,
    engine           = "R",
    tuning           = "off",
    seed             = 42L,
    progress         = FALSE
  )
  seed_after_off <- .Random.seed

  set.seed(123L)
  seed_before_always <- .Random.seed
  res_always <- cross_size_cv(
    data             = d,
    outcome          = y,
    items            = paste0("Q", 1:5),
    model_sizes      = 1:2,
    folds            = 3L,
    repeats          = 2L,
    selection_metric = "youden",
    sensitivity_min  = 0.3,
    specificity_min  = 0.3,
    engine           = "R",
    tuning           = "always",
    seed             = 42L,
    progress         = FALSE
  )
  seed_after_always <- .Random.seed

  # Statistical identity
  expect_equal(res_off$final_selected_model, res_always$final_selected_model)
  expect_equal(res_off$candidate_ranking, res_always$candidate_ranking)
  expect_equal(res_off$cv_performance, res_always$cv_performance)
  expect_equal(res_off$oof_predictions, res_always$oof_predictions)
  expect_equal(res_off$repeat_metrics, res_always$repeat_metrics)

  # RNG determinism
  expect_identical(seed_after_off, seed_after_always)
})
