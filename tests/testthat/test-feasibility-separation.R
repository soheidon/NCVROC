# test-feasibility-separation.R — Feasibility status tests for cross_size_cv

test_that("new methods report feasibility and selection status correctly", {
  df <- make_test_data()

  res <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.8,
    progress = FALSE
  )

  expect_true(res$final_selection_status %in% c("selected", "no_feasible_candidate"))
  expect_type(res$n_feasible_candidates, "integer")
  expect_true(res$n_feasible_candidates >= 0L)
  if (res$n_feasible_candidates > 0L) {
    expect_true(nrow(res$candidate_ranking) > 0L)
    expect_s3_class(res, "cross_size_cv_result")
  }
})

test_that("strict constraints produce no_feasible_candidate when error_on_empty is FALSE", {
  set.seed(999)
  df <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    q1 = sample(0:1, 40, replace = TRUE),
    q2 = sample(0:1, 40, replace = TRUE),
    q3 = sample(0:1, 40, replace = TRUE)
  )

  res <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    model_sizes = 1:2,
    cutoff_method = "clinical_constraint",
    sensitivity_min = 0.99,
    specificity_min = 0.99,
    error_on_empty = FALSE,
    progress = FALSE
  )

  expect_null(res$final_selected_model)
  expect_identical(res$final_selection_status, "no_feasible_candidate")
  expect_equal(res$n_feasible_candidates, 0L)
  expect_equal(nrow(res$candidate_ranking), 0L)
  expect_true(is.na(res$final_full_data_cutoff))
})
