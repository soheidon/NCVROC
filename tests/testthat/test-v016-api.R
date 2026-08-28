# test-v016-api.R — Phase 7: Unified API, S3 Methods, & Regression Test Suite

test_that("v0.16.0 Public API exports audit", {
  ns_exports <- getNamespaceExports("NCVROC")

  expected_public_fns <- c(
    # Fixed model evaluation
    "cv_sum_roc", "loocv_sum_roc",
    # Within-size selection
    "cv_select_sum_roc", "loocv_select_sum_roc",
    # Cross-size selection
    "cross_size_cv", "cross_size_loocv",
    # Selection-procedure validation
    "cross_size_nested_cv",
    # Comparison
    "compare_cv_selection",
    # Legacy & helpers
    "ncvroc", "nested_sum_roc", "exhaustive_sum_roc",
    "roc_bruteforce", "roc_bf", "ncvroc_config",
    "run_ncvroc", "fit_final_sum_scale", "make_stratified_folds",
    "count_item_combinations", "suggest_preselect_top_n", "ncvroc_results"
  )

  for (fn in expected_public_fns) {
    expect_true(fn %in% ns_exports, info = sprintf("Expected %s to be exported", fn))
  }

  # Ensure internal helpers are NOT exported
  internal_helpers <- c(
    "evaluate_combos_cv_cpp",
    ".build_cv_folds",
    ".build_loocv_folds",
    ".make_stratified_cv_folds",
    ".aggregate_oof_metrics"
  )
  for (fn in internal_helpers) {
    expect_false(fn %in% ns_exports, info = sprintf("Internal helper %s should not be exported", fn))
  }
})

test_that("v0.16.0 Unified API workflows and S3 methods execute cleanly", {
  set.seed(42)
  d <- data.frame(
    y  = c(rep(1L, 12), rep(0L, 12)),
    Q1 = sample(0:2, 24, replace = TRUE),
    Q2 = sample(0:2, 24, replace = TRUE),
    Q3 = sample(0:2, 24, replace = TRUE),
    Q4 = sample(0:2, 24, replace = TRUE)
  )

  # B. cv_sum_roc (Fixed model K-fold)
  res_b <- cv_sum_roc(d, y, c("Q1", "Q2", "Q3"), folds = 3, repeats = 2, seed = 1)
  expect_s3_class(res_b, "cv_sum_roc_result")
  expect_output(print(res_b), "Fixed-Model Cross-Validation")
  expect_output(summary(res_b), "Fixed-Model Cross-Validation")
  expect_equal(res_b$settings$repeats, 2)
  expect_equal(res_b$settings$folds, 3)

  # C. loocv_sum_roc (Fixed model LOOCV)
  res_c <- loocv_sum_roc(d, y, c("Q1", "Q2", "Q3"))
  expect_s3_class(res_c, "cv_sum_roc_result")
  expect_output(print(res_c), "Leave-One-Out \\(LOOCV\\)")
  expect_equal(res_c$cv_method, "loocv")
  expect_equal(res_c$settings$repeats, 1)

  # D. cv_select_sum_roc (Within-size K-fold selection)
  res_d <- cv_select_sum_roc(d, y, Q1:Q4, item_count = 2, folds = 3, repeats = 2, seed = 1)
  expect_s3_class(res_d, "cv_select_sum_roc_result")
  expect_s3_class(res_d, "cross_size_cv_result")
  expect_equal(res_d$settings$selection_scope, "within_size")
  expect_equal(res_d$settings$item_count, 2)
  expect_output(print(res_d), "Within-Size CV Sum-Score Selection")
  expect_output(summary(res_d), "Within-Size CV Sum-Score Selection")

  # E. loocv_select_sum_roc (Within-size LOOCV selection)
  res_e <- loocv_select_sum_roc(d, y, Q1:Q4, item_count = 2)
  expect_s3_class(res_e, "loocv_select_sum_roc_result")
  expect_s3_class(res_e, "cv_select_sum_roc_result")
  expect_s3_class(res_e, "cross_size_cv_result")
  expect_equal(res_e$cv_method, "loocv")
  expect_equal(res_e$settings$selection_scope, "within_size")
  expect_output(print(res_e), "Leave-One-Out \\(LOOCV\\)")

  # F. cross_size_cv (Cross-size K-fold selection)
  res_f <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, folds = 3, repeats = 2, seed = 1)
  expect_s3_class(res_f, "cross_size_cv_result")
  expect_equal(res_f$cv_method, "kfold")
  expect_equal(res_f$model_sizes, 1:3)
  expect_output(print(res_f), "Cross-Size Ordinary Cross-Validation")
  expect_output(summary(res_f), "Cross-Size Ordinary Cross-Validation")

  # G. cross_size_loocv (Cross-size LOOCV selection)
  res_g <- cross_size_loocv(d, y, Q1:Q4, model_sizes = 1:2)
  expect_s3_class(res_g, "cross_size_cv_result")
  expect_equal(res_g$cv_method, "loocv")
  expect_equal(res_g$model_sizes, 1:2)
  expect_output(print(res_g), "Leave-One-Out \\(LOOCV\\)")

  # H. cross_size_nested_cv (Selection-procedure validation)
  res_h <- cross_size_nested_cv(d, y, Q1:Q3, model_sizes = 1:2,
                               outer_folds = 3, inner_folds = 2,
                               outer_repeats = 2, inner_repeats = 1, seed = 1)
  expect_s3_class(res_h, "cross_size_nested_cv_result")
  expect_output(print(res_h), "Cross-Size Nested Cross-Validation")
  expect_output(summary(res_h), "Cross-Size Nested Cross-Validation")

  # I. compare_cv_selection (Ordinary vs Nested optimism)
  res_i <- compare_cv_selection(d, y, Q1:Q3, model_sizes = 1:2,
                               folds = 3, repeats = 2,
                               outer_folds = 3, inner_folds = 2,
                               outer_repeats = 2, inner_repeats = 1, seed = 1)
  expect_s3_class(res_i, "compare_cv_selection_result")
  expect_output(print(res_i), "Selection Performance & Optimism Summary")
  expect_s3_class(summary(res_i), "summary_compare_cv_selection_result")

  # N. LOOCV repeats guard
  expect_error(
    cross_size_cv(d, y, Q1:Q3, cv_method = "loocv", repeats = 2),
    "LOOCV is deterministic and unique; repeats > 1 is not supported."
  )

  # O. K-fold folds = N guard
  expect_error(
    cross_size_cv(d, y, Q1:Q3, cv_method = "kfold", folds = nrow(d)),
    "Under cv_method = 'kfold', folds must satisfy 2 <= folds < n"
  )
})
