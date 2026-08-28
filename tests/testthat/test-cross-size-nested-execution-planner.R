# test-cross-size-nested-execution-planner.R

testthat::test_that("cross_size_nested_cv statistical exactness across tuning modes", {
  set.seed(42)
  n <- 80
  d <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE)
  )

  # tuning = "off"
  set.seed(123)
  res_off <- cross_size_nested_cv(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_folds = 3, inner_folds = 2,
    outer_repeats = 1, inner_repeats = 1,
    seed = 100, engine = "R", tuning = "off",
    progress = FALSE, verbose = FALSE
  )
  rng_off <- .Random.seed

  # tuning = "auto"
  set.seed(123)
  res_auto <- cross_size_nested_cv(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_folds = 3, inner_folds = 2,
    outer_repeats = 1, inner_repeats = 1,
    seed = 100, engine = "R", tuning = "auto",
    progress = FALSE, verbose = FALSE
  )
  rng_auto <- .Random.seed

  # tuning = "always"
  set.seed(123)
  res_always <- cross_size_nested_cv(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_folds = 3, inner_folds = 2,
    outer_repeats = 1, inner_repeats = 1,
    seed = 100, engine = "R", tuning = "always",
    progress = FALSE, verbose = FALSE
  )
  rng_always <- .Random.seed

  # Statistical payload exactness checks
  testthat::expect_equal(res_off$summary, res_auto$summary)
  testthat::expect_equal(res_off$summary, res_always$summary)

  testthat::expect_equal(res_off$outer_fold_results, res_auto$outer_fold_results)
  testthat::expect_equal(res_off$outer_fold_results, res_always$outer_fold_results)

  testthat::expect_equal(res_off$model_size_selection_frequency, res_auto$model_size_selection_frequency)
  testthat::expect_equal(res_off$model_size_selection_frequency, res_always$model_size_selection_frequency)

  testthat::expect_equal(res_off$item_combination_selection_frequency, res_auto$item_combination_selection_frequency)
  testthat::expect_equal(res_off$item_combination_selection_frequency, res_always$item_combination_selection_frequency)

  testthat::expect_equal(res_off$cutoff_distribution, res_auto$cutoff_distribution)
  testthat::expect_equal(res_off$cutoff_distribution, res_always$cutoff_distribution)

  testthat::expect_equal(res_off$outer_predictions, res_auto$outer_predictions)
  testthat::expect_equal(res_off$outer_predictions, res_always$outer_predictions)

  # RNG state preservation check
  testthat::expect_equal(rng_off, rng_auto)
  testthat::expect_equal(rng_off, rng_always)

  # Metadata attachment contract
  testthat::expect_null(res_off$settings$execution_plan)

  testthat::expect_true(is.list(res_auto$settings$execution_plan))
  testthat::expect_equal(res_auto$settings$execution_plan$target_api, "cross_size_nested_cv")
  testthat::expect_equal(res_auto$settings$execution_plan$tuning_mode, "auto")

  testthat::expect_true(is.list(res_always$settings$execution_plan))
  testthat::expect_equal(res_always$settings$execution_plan$target_api, "cross_size_nested_cv")
  testthat::expect_equal(res_always$settings$execution_plan$tuning_mode, "always")
})

testthat::test_that("cross_size_nested_cv with Rcpp engine and automatic execution plan", {
  set.seed(42)
  n <- 60
  d <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE)
  )

  res <- cross_size_nested_cv(
    data = d, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2, outer_folds = 2, inner_folds = 2,
    outer_repeats = 1, inner_repeats = 1,
    seed = 42, engine = "Rcpp", tuning = "always",
    progress = FALSE, verbose = FALSE
  )

  testthat::expect_s3_class(res, "cross_size_nested_cv_result")
  testthat::expect_true(is.list(res$settings$execution_plan))
  testthat::expect_equal(res$settings$execution_plan$target_api, "cross_size_nested_cv")
  testthat::expect_equal(res$settings$execution_plan$outer_folds, 2L)
  testthat::expect_equal(res$settings$execution_plan$inner_folds, 2L)

  # Formatting helper check
  formatted <- .planner_format_execution_plan(res)
  testthat::expect_true(grepl("Target API:\\s+cross_size_nested_cv", formatted))
  testthat::expect_true(grepl("Nested CV Structure:", formatted))
})
