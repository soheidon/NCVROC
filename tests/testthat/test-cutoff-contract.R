# test-cutoff-contract.R — Contract C1 tests

test_that("C1: sensitivity_target requires sensitivity_min and prohibits specificity_min", {
  df <- make_test_data()

  # Missing sensitivity_min
  expect_error(
    cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                  min_items = 1, max_items = 2,
                  cutoff_method = "sensitivity_target",
                  sensitivity_min = NULL,
                  progress = FALSE),
    regexp = "sensitivity_min.*required"
  )

  # Providing specificity_min when prohibited
  expect_error(
    cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                  min_items = 1, max_items = 2,
                  cutoff_method = "sensitivity_target",
                  sensitivity_min = 0.8,
                  specificity_min = 0.5,
                  progress = FALSE),
    regexp = "specificity_min.*prohibited|specificity_min.*must not be specified"
  )
})

test_that("C1: clinical_constraint requires both sensitivity_min and specificity_min", {
  df <- make_test_data()

  # Missing specificity_min
  expect_error(
    cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                  min_items = 1, max_items = 2,
                  cutoff_method = "clinical_constraint",
                  sensitivity_min = 0.8,
                  specificity_min = NULL,
                  progress = FALSE),
    regexp = "specificity_min.*required"
  )

  # Missing sensitivity_min
  expect_error(
    cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                  min_items = 1, max_items = 2,
                  cutoff_method = "clinical_constraint",
                  sensitivity_min = NULL,
                  specificity_min = 0.8,
                  progress = FALSE),
    regexp = "sensitivity_min.*required"
  )
})

test_that("C1: legacy methods allow optional sensitivity_min/specificity_min", {
  df <- make_test_data()

  res <- cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                       min_items = 1, max_items = 2,
                       cutoff_method = "youden",
                       sensitivity_min = 0.5,
                       progress = FALSE)
  expect_s3_class(res, "cross_size_cv_result")
})

test_that("C1: no double application of constraints for new methods", {
  df <- make_test_data()

  # Run sensitivity_target and check that candidate-level constraint is not applied twice
  res <- cross_size_cv(df, outcome = "y", items = c("q1", "q2", "q3"),
                       min_items = 1, max_items = 2,
                       cutoff_method = "sensitivity_target",
                       sensitivity_min = 0.5,
                       progress = FALSE)
  expect_s3_class(res, "cross_size_cv_result")
  expect_true(res$final_selection_status %in% c("selected", "no_feasible_candidate", "no_feasible_cutoff_on_full_data"))
})
