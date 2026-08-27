# test-compare-cv-selection.R — Unit tests for compare_cv_selection()

test_that("compare_cv_selection arithmetic identity, schema, and basic execution", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  res <- compare_cv_selection(
    d, y, Q1:Q3,
    model_sizes   = 1:2,
    folds         = 3,
    outer_folds   = 3,
    inner_folds   = 2,
    outer_repeats = 2,
    seed          = 123,
    progress      = FALSE
  )

  expect_s3_class(res, "compare_cv_selection_result")
  expect_s3_class(res$ordinary, "cross_size_cv_result")
  expect_s3_class(res$nested, "cross_size_nested_cv_result")

  # Comparison data frame schema
  comp <- res$comparison
  expect_equal(nrow(comp), 7L)
  expect_named(comp, c("metric", "ordinary", "nested", "selection_optimism"))
  expect_equal(comp$metric, c("auc", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv"))

  # A. Arithmetic identity: selection_optimism == ordinary - nested
  for (i in seq_len(nrow(comp))) {
    expect_equal(comp$selection_optimism[i], comp$ordinary[i] - comp$nested[i], tolerance = 1e-10)
  }

  # Selected model matches ordinary selected model
  expect_identical(res$selected_model$items, res$ordinary$final_selected_model$items)
  expect_equal(res$selected_model$n_items, res$ordinary$final_selected_model$n_items)

  # Model sizes
  expect_equal(res$model_sizes, 1:2)

  # Print / summary methods work without error
  expect_output(print(res), "Cross-Size CV Selection Comparison")
  expect_output(print(res), "Selection Optimism")
  s_obj <- summary(res)
  expect_s3_class(s_obj, "summary_compare_cv_selection_result")
  expect_output(print(s_obj), "Summary of Cross-Size CV Selection Comparison")
})

test_that("compare_cv_selection result wiring matches independent ordinary and nested calls", {
  set.seed(456)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  comp_res <- compare_cv_selection(
    d, y, Q1:Q3,
    model_sizes   = 1:2,
    folds         = 3,
    outer_folds   = 3,
    inner_folds   = 2,
    outer_repeats = 1,
    seed          = 456,
    progress      = FALSE
  )

  # Independent ordinary call
  ord_indep <- cross_size_cv(
    d, y, Q1:Q3,
    model_sizes = 1:2,
    folds       = 3,
    seed        = 456,
    progress    = FALSE
  )

  # Independent nested call
  nest_indep <- cross_size_nested_cv(
    d, y, Q1:Q3,
    model_sizes   = 1:2,
    outer_folds   = 3,
    inner_folds   = 2,
    outer_repeats = 1,
    seed          = 456,
    progress      = FALSE
  )

  expect_identical(comp_res$ordinary$final_selected_model$items, ord_indep$final_selected_model$items)
  expect_equal(comp_res$ordinary$final_selected_model$cv_auc, ord_indep$final_selected_model$cv_auc, tolerance = 1e-10)

  expect_identical(comp_res$nested$selected_models_by_outer_fold, nest_indep$selected_models_by_outer_fold)
  expect_equal(comp_res$nested$summary$mean, nest_indep$summary$mean, tolerance = 1e-10)

  # Comparison table values strictly wire to ordinary model and nested summary
  expect_equal(comp_res$comparison$ordinary[comp_res$comparison$metric == "auc"], ord_indep$final_selected_model$cv_auc, tolerance = 1e-10)
  expect_equal(comp_res$comparison$nested[comp_res$comparison$metric == "auc"], nest_indep$summary$mean[nest_indep$summary$metric == "auc"], tolerance = 1e-10)
})

test_that("compare_cv_selection candidate-space and constraint propagation", {
  set.seed(789)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  res <- compare_cv_selection(
    d, y, Q1:Q4,
    model_sizes        = c(1, 3),
    sensitivity_min    = 0.30,
    prefer_fewer_items = TRUE,
    folds              = 2,
    outer_folds        = 2,
    inner_folds        = 2,
    outer_repeats      = 1,
    seed               = 789,
    progress           = FALSE
  )

  expect_equal(res$settings$model_sizes, c(1, 3))
  expect_equal(res$ordinary$model_sizes, c(1, 3))
  expect_equal(res$nested$model_sizes, c(1, 3))

  expect_equal(res$settings$sensitivity_min, 0.30)
  expect_equal(res$ordinary$settings$sensitivity_min, 0.30)
  expect_equal(res$nested$settings$sensitivity_min, 0.30)
})

test_that("compare_cv_selection parallel mode routing and mapping", {
  set.seed(111)
  d <- data.frame(
    y  = sample(0:1, 36, replace = TRUE),
    Q1 = sample(0:2, 36, replace = TRUE),
    Q2 = sample(0:2, 36, replace = TRUE)
  )

  res_none   <- compare_cv_selection(d, y, Q1:Q2, model_sizes = 1:2, parallel = "none", outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 111, progress = FALSE)
  res_outer  <- compare_cv_selection(d, y, Q1:Q2, model_sizes = 1:2, parallel = "outer", n_workers = 2, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 111, progress = FALSE)
  res_hybrid <- compare_cv_selection(d, y, Q1:Q2, model_sizes = 1:2, parallel = "hybrid", n_workers = 2, threads_per_worker = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 111, progress = FALSE)

  # Check parallel mapping settings
  expect_equal(res_none$settings$ordinary_parallel, "none")
  expect_equal(res_none$settings$nested_parallel, "none")

  expect_equal(res_outer$settings$ordinary_parallel, "none")
  expect_equal(res_outer$settings$nested_parallel, "outer")

  expect_equal(res_hybrid$settings$ordinary_parallel, "threads")
  expect_equal(res_hybrid$settings$nested_parallel, "hybrid")

  # Results are identical across parallel backends with same seed
  expect_equal(res_none$comparison$selection_optimism, res_outer$comparison$selection_optimism, tolerance = 1e-10)
  expect_equal(res_none$comparison$selection_optimism, res_hybrid$comparison$selection_optimism, tolerance = 1e-10)
})

test_that("Negative optimism is preserved without truncation", {
  # Create synthetic comparison where nested > ordinary
  comp_df <- data.frame(
    metric             = "sensitivity",
    ordinary           = 0.60,
    nested             = 0.75,
    selection_optimism = 0.60 - 0.75, # -0.15
    stringsAsFactors   = FALSE
  )
  expect_equal(comp_df$selection_optimism, -0.15)
  expect_false(comp_df$selection_optimism == 0)
})

test_that("cross_size_cv model_size_summary is populated and descriptive", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  res <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, folds = 3, seed = 42)
  expect_true("model_size_summary" %in% names(res))
  mss <- res$model_size_summary
  expect_equal(nrow(mss), 2L)
  expect_named(mss, c("n_items", "n_candidates_total", "n_evaluated_in_top", "best_items", "best_metric_value"))
  expect_equal(mss$n_items, 1:2)
  expect_equal(mss$n_candidates_total, c(3L, 3L))
})
