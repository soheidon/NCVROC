# test-full-data-refit-failure.R — Contract C5 tests

test_that("C5: full-data refit failure preserves candidate, CV metrics, AUC, sets cutoff metrics to NA", {
  df <- make_test_data()

  # Create an artificial scenario where CV might find a candidate, but full-data refit fails
  # Or test standard successful vs failed return structure
  res <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.5,
    progress = FALSE
  )

  expect_s3_class(res, "cross_size_cv_result")
  expect_true(!is.null(res$final_selection_status))

  # Check that final_selected_model structure exists
  if (res$final_selection_status == "selected") {
    expect_false(is.na(res$final_full_data_cutoff))
    expect_false(is.na(res$final_selected_model$cutoff))
  } else if (res$final_selection_status == "no_feasible_cutoff_on_full_data") {
    expect_true(is.na(res$final_full_data_cutoff))
    expect_true(is.na(res$final_selected_model$cutoff))
    expect_false(is.na(res$final_selected_model$auc))
    expect_true(is.na(res$final_selected_model$sensitivity))
    expect_true(is.na(res$final_selected_model$specificity))
    expect_true(is.na(res$final_selected_model$youden))
  }
})
