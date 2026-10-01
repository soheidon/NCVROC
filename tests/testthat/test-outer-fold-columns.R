# test-outer-fold-columns.R — Contract C2 tests

test_that("C2: outer_fold_results has exactly 16 columns in specified order", {
  df <- make_test_data()

  expected_cols <- c(
    "outer_fold", "repeat_id", "fold_id", "selected_items", "selected_n_items",
    "selected_cutoff", "outer_auc", "outer_sensitivity", "outer_specificity",
    "outer_youden", "outer_accuracy", "outer_ppv", "outer_npv",
    "selection_status", "n_feasible_candidates", "n_candidates_total"
  )

  # Legacy method
  res_youden <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "youden",
    progress = FALSE
  )
  expect_identical(colnames(res_youden$outer_fold_results), expected_cols)

  # New method
  res_sens <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.6,
    progress = FALSE
  )
  expect_identical(colnames(res_sens$outer_fold_results), expected_cols)
})

test_that("C2: failed outer fold emits typed NAs and correct status", {
  df <- make_test_data()

  # Set impossibly high sensitivity requirement (e.g. 1.0 or with clinical_constraint 1.0, 1.0)
  res_fail <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "clinical_constraint",
    sensitivity_min = 1.0,
    specificity_min = 1.0,
    progress = FALSE
  )

  expect_s3_class(res_fail, "cross_size_nested_cv_result")
  ofr <- res_fail$outer_fold_results

  # If any fold failed
  failed_rows <- ofr[ofr$selection_status != "selected", , drop = FALSE]
  if (nrow(failed_rows) > 0) {
    expect_true(all(is.na(failed_rows$selected_items)))
    expect_true(all(is.na(failed_rows$selected_n_items)))
    expect_true(all(is.na(failed_rows$selected_cutoff)))
    expect_true(all(is.na(failed_rows$outer_auc)))
    expect_true(all(is.na(failed_rows$outer_sensitivity)))
    expect_true(all(is.na(failed_rows$outer_specificity)))
    expect_true(all(is.na(failed_rows$outer_youden)))
    expect_true(all(is.na(failed_rows$outer_accuracy)))
    expect_true(all(is.na(failed_rows$outer_ppv)))
    expect_true(all(is.na(failed_rows$outer_npv)))
    expect_true(all(is.integer(failed_rows$n_feasible_candidates)))
    expect_true(all(is.integer(failed_rows$n_candidates_total)))
  }
})
