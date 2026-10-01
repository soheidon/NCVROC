# test-select-operating-point.R — Unit tests for select_operating_point()

test_that("select_operating_point handles sensitivity_target correctly", {
  # Synthetic metrics table
  # cutoff: 1, 2, 3, 4
  # sens: 1.0, 0.8, 0.6, 0.2
  # spec: 0.1, 0.7, 0.9, 1.0
  # youd: 0.1, 0.5, 0.5, 0.2
  metrics <- data.frame(
    cutoff      = c(1, 2, 3, 4),
    tp          = c(10, 8, 6, 2),
    fp          = c(9, 3, 1, 0),
    fn          = c(0, 2, 4, 8),
    tn          = c(1, 7, 9, 10),
    sensitivity = c(1.0, 0.8, 0.6, 0.2),
    specificity = c(0.1, 0.7, 0.9, 1.0),
    youden      = c(0.1, 0.5, 0.5, 0.2),
    accuracy    = c(0.55, 0.75, 0.75, 0.6),
    ppv         = c(10/19, 8/11, 6/7, 1.0),
    npv         = c(1.0, 7/9, 9/13, 10/18),
    stringsAsFactors = FALSE
  )

  # With sensitivity_min = 0.8, candidates are cutoff 1 (sens 1.0, spec 0.1) and cutoff 2 (sens 0.8, spec 0.7).
  # Best specificity is cutoff 2 (spec 0.7).
  res <- NCVROC:::select_operating_point(metrics, method = "sensitivity_target", sensitivity_min = 0.8)
  expect_equal(res$cutoff, 2)
  expect_equal(res$sensitivity, 0.8)
  expect_equal(res$specificity, 0.7)

  # With sensitivity_min = 0.95, only cutoff 1 satisfies.
  res2 <- NCVROC:::select_operating_point(metrics, method = "sensitivity_target", sensitivity_min = 0.95)
  expect_equal(res2$cutoff, 1)

  # Infeasible: sensitivity_min = 1.01 -> returns NULL
  res_infeasible <- NCVROC:::select_operating_point(metrics, method = "sensitivity_target", sensitivity_min = 1.01)
  expect_null(res_infeasible)
})

test_that("select_operating_point handles clinical_constraint correctly", {
  metrics <- data.frame(
    cutoff      = c(1, 2, 3, 4),
    tp          = c(10, 8, 6, 2),
    fp          = c(9, 3, 1, 0),
    fn          = c(0, 2, 4, 8),
    tn          = c(1, 7, 9, 10),
    sensitivity = c(1.0, 0.8, 0.6, 0.2),
    specificity = c(0.1, 0.7, 0.9, 1.0),
    youden      = c(0.1, 0.5, 0.5, 0.2),
    accuracy    = c(0.55, 0.75, 0.75, 0.6),
    ppv         = c(10/19, 8/11, 6/7, 1.0),
    npv         = c(1.0, 7/9, 9/13, 10/18),
    stringsAsFactors = FALSE
  )

  # Both sens >= 0.7 and spec >= 0.6 -> only cutoff 2 satisfies (sens 0.8, spec 0.7)
  res <- NCVROC:::select_operating_point(metrics, method = "clinical_constraint", sensitivity_min = 0.7, specificity_min = 0.6)
  expect_equal(res$cutoff, 2)

  # Infeasible: sens >= 0.9 and spec >= 0.8 -> NULL
  res_infeasible <- NCVROC:::select_operating_point(metrics, method = "clinical_constraint", sensitivity_min = 0.9, specificity_min = 0.8)
  expect_null(res_infeasible)
})
