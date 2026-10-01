# test-parity-exact-vs-tolerance.R — Real R vs C++ evaluation parity and contract C6 tests

test_that("C6: normalize_candidate_payload enforces exact equality on discrete fields and tolerance on continuous metrics", {
  payload1 <- list(
    selected_items = c("q1", "q2"),
    canonical_rank = 1L,
    cutoff = 2.5,
    n_cutoff_feasible_folds = 5L,
    n_total_folds = 5L,
    cutoff_feasible = TRUE,
    candidate_feasible = TRUE,
    selection_status = "selected",
    auc = 0.85000000001,
    sensitivity = 0.80000000001,
    specificity = 0.75000000001,
    youden = 0.55000000002,
    accuracy = 0.77000000001,
    ppv = 0.76000000001,
    npv = 0.78000000001
  )

  payload2 <- list(
    selected_items = c("q1", "q2"),
    canonical_rank = 1L,
    cutoff = 2.5,
    n_cutoff_feasible_folds = 5L,
    n_total_folds = 5L,
    cutoff_feasible = TRUE,
    candidate_feasible = TRUE,
    selection_status = "selected",
    auc = 0.85,
    sensitivity = 0.80,
    specificity = 0.75,
    youden = 0.55,
    accuracy = 0.77,
    ppv = 0.76,
    npv = 0.78
  )

  p1_norm <- NCVROC:::normalize_candidate_payload(payload1)
  p2_norm <- NCVROC:::normalize_candidate_payload(payload2)

  # Discrete fields identical
  expect_identical(p1_norm$selected_items, p2_norm$selected_items)
  expect_identical(p1_norm$canonical_rank, p2_norm$canonical_rank)
  expect_identical(p1_norm$cutoff, p2_norm$cutoff)
  expect_identical(p1_norm$n_cutoff_feasible_folds, p2_norm$n_cutoff_feasible_folds)
  expect_identical(p1_norm$n_total_folds, p2_norm$n_total_folds)
  expect_identical(p1_norm$cutoff_feasible, p2_norm$cutoff_feasible)
  expect_identical(p1_norm$candidate_feasible, p2_norm$candidate_feasible)
  expect_identical(p1_norm$selection_status, p2_norm$selection_status)

  # Continuous metrics within 1e-10 tolerance
  expect_equal(p1_norm$auc, p2_norm$auc, tolerance = 1e-10)
  expect_equal(p1_norm$sensitivity, p2_norm$sensitivity, tolerance = 1e-10)
  expect_equal(p1_norm$specificity, p2_norm$specificity, tolerance = 1e-10)
  expect_equal(p1_norm$youden, p2_norm$youden, tolerance = 1e-10)
})

test_that("Real evaluation parity: Rcpp vs R engine for sensitivity_target", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    Q1 = sample(0:2, 60, replace = TRUE),
    Q2 = sample(0:2, 60, replace = TRUE),
    Q3 = sample(0:2, 60, replace = TRUE),
    Q4 = sample(0:2, 60, replace = TRUE),
    Q5 = sample(0:2, 60, replace = TRUE)
  )

  res_cpp <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    folds              = 3,
    repeats            = 1,
    selection_metric   = "specificity",
    cutoff_method      = "sensitivity_target",
    sensitivity_min    = 0.70,
    engine             = "Rcpp",
    seed               = 42,
    progress           = FALSE
  )

  res_r <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    folds              = 3,
    repeats            = 1,
    selection_metric   = "specificity",
    cutoff_method      = "sensitivity_target",
    sensitivity_min    = 0.70,
    engine             = "R",
    seed               = 42,
    progress           = FALSE
  )

  # Selected model identity and cutoff
  expect_identical(res_cpp$final_selected_model$items, res_r$final_selected_model$items)
  expect_identical(res_cpp$final_selected_model$n_items, res_r$final_selected_model$n_items)
  expect_identical(res_cpp$final_full_data_cutoff, res_r$final_full_data_cutoff)
  expect_identical(res_cpp$final_selection_status, res_r$final_selection_status)
  expect_equal(res_cpp$n_feasible_candidates, res_r$n_feasible_candidates)

  # Metrics parity within numerical tolerance
  expect_equal(res_cpp$final_selected_model$cv_sensitivity, res_r$final_selected_model$cv_sensitivity, tolerance = 1e-7)
  expect_equal(res_cpp$final_selected_model$cv_specificity, res_r$final_selected_model$cv_specificity, tolerance = 1e-7)
  expect_equal(res_cpp$final_selected_model$cv_auc, res_r$final_selected_model$cv_auc, tolerance = 1e-7)

  # Ranking table parity
  expect_identical(res_cpp$candidate_ranking$items, res_r$candidate_ranking$items)
  expect_equal(res_cpp$candidate_ranking$cv_specificity, res_r$candidate_ranking$cv_specificity, tolerance = 1e-7)
  expect_equal(res_cpp$candidate_ranking$cv_sensitivity, res_r$candidate_ranking$cv_sensitivity, tolerance = 1e-7)
})

test_that("Real evaluation parity: Rcpp vs R engine for clinical_constraint", {
  set.seed(456)
  y <- sample(0:1, 60, replace = TRUE)
  d <- data.frame(
    y  = y,
    Q1 = y * 2 + sample(0:1, 60, replace = TRUE),
    Q2 = y * 2 + sample(0:1, 60, replace = TRUE),
    Q3 = sample(0:2, 60, replace = TRUE),
    Q4 = sample(0:2, 60, replace = TRUE),
    Q5 = sample(0:2, 60, replace = TRUE)
  )

  res_cpp <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    folds              = 3,
    repeats            = 1,
    selection_metric   = "youden",
    cutoff_method      = "clinical_constraint",
    sensitivity_min    = 0.65,
    specificity_min    = 0.60,
    engine             = "Rcpp",
    seed               = 42,
    progress           = FALSE
  )

  res_r <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    folds              = 3,
    repeats            = 1,
    selection_metric   = "youden",
    cutoff_method      = "clinical_constraint",
    sensitivity_min    = 0.65,
    specificity_min    = 0.60,
    engine             = "R",
    seed               = 42,
    progress           = FALSE
  )

  expect_identical(res_cpp$final_selected_model$items, res_r$final_selected_model$items)
  expect_identical(res_cpp$final_full_data_cutoff, res_r$final_full_data_cutoff)
  expect_identical(res_cpp$final_selection_status, res_r$final_selection_status)
  expect_equal(res_cpp$n_feasible_candidates, res_r$n_feasible_candidates)
  expect_equal(res_cpp$final_selected_model$cv_youden, res_r$final_selected_model$cv_youden, tolerance = 1e-7)
  expect_equal(res_cpp$final_selected_model$cv_sensitivity, res_r$final_selected_model$cv_sensitivity, tolerance = 1e-7)
  expect_equal(res_cpp$final_selected_model$cv_specificity, res_r$final_selected_model$cv_specificity, tolerance = 1e-7)
})

test_that("Real evaluation parity: Rcpp vs R engine when no candidate is feasible", {
  set.seed(789)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Impossible constraint
  res_cpp <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:4),
    model_sizes        = 1:2,
    folds              = 3,
    repeats            = 1,
    cutoff_method      = "clinical_constraint",
    sensitivity_min    = 0.99,
    specificity_min    = 0.99,
    engine             = "Rcpp",
    error_on_empty     = FALSE,
    seed               = 42,
    progress           = FALSE
  )

  res_r <- cross_size_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:4),
    model_sizes        = 1:2,
    folds              = 3,
    repeats            = 1,
    cutoff_method      = "clinical_constraint",
    sensitivity_min    = 0.99,
    specificity_min    = 0.99,
    engine             = "R",
    error_on_empty     = FALSE,
    seed               = 42,
    progress           = FALSE
  )

  expect_null(res_cpp$final_selected_model)
  expect_null(res_r$final_selected_model)
  expect_identical(res_cpp$final_selection_status, "no_feasible_candidate")
  expect_identical(res_r$final_selection_status, "no_feasible_candidate")
  expect_equal(res_cpp$n_feasible_candidates, 0L)
  expect_equal(res_r$n_feasible_candidates, 0L)
  expect_equal(nrow(res_cpp$candidate_ranking), 0L)
  expect_equal(nrow(res_r$candidate_ranking), 0L)
  expect_true(is.na(res_cpp$final_full_data_cutoff))
  expect_true(is.na(res_r$final_full_data_cutoff))
})
