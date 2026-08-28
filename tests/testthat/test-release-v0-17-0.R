# test-release-v0-16-0.R — Release sanity and integrity tests for NCVROC

test_that("Package version in DESCRIPTION and NEWS is synchronized", {
  desc_ver <- utils::packageVersion("NCVROC")
  expect_equal(as.character(desc_ver), "0.17.0")

  news_file <- system.file("NEWS.md", package = "NCVROC")
  if (file.exists(news_file)) {
    news_lines <- readLines(news_file, n = 5)
    expect_true(any(grepl("NCVROC 0.17.0", news_lines)))
  }
})

test_that("NAMESPACE exports all intended public API functions", {
  ns_exports <- getNamespaceExports("NCVROC")

  expected_api <- c(
    "cv_sum_roc",
    "loocv_sum_roc",
    "cv_select_sum_roc",
    "loocv_select_sum_roc",
    "cross_size_cv",
    "cross_size_loocv",
    "cross_size_nested_cv",
    "compare_cv_selection",
    "candidate_stability_roc"
  )
  expect_true(all(expected_api %in% ns_exports))

  # Ensure no internal helpers leaked into exports
  internal_patterns <- c("^\\.", "block_stream", "eval_single_combo", "resolve_model_sizes", "evaluate_combos_cv_cpp", "evaluate_candidate_stability")
  for (pat in internal_patterns) {
    leaked <- grep(pat, ns_exports, value = TRUE)
    expect_length(leaked, 0L)
  }
})

test_that("README validation workflow examples run without error (smoke test)", {
  set.seed(42)
  analysis_dat <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Example A: cv_sum_roc & loocv_sum_roc
  cv_fit <- cv_sum_roc(analysis_dat, y, c("Q1", "Q2", "Q3"), folds = 3, repeats = 1, seed = 42)
  expect_s3_class(cv_fit, "cv_sum_roc_result")

  loo_fit <- loocv_sum_roc(analysis_dat, y, c("Q1", "Q2", "Q3"))
  expect_s3_class(loo_fit, "cv_sum_roc_result")

  # Example B: cv_select_sum_roc
  sel_fit <- cv_select_sum_roc(analysis_dat, y, paste0("Q", 1:4), item_count = 2, folds = 3, seed = 42)
  expect_s3_class(sel_fit, "cv_select_sum_roc_result")

  # Example C: cross_size_cv & cross_size_loocv
  ord_fit <- cross_size_cv(analysis_dat, y, paste0("Q", 1:4), model_sizes = 1:2, folds = 3, seed = 42)
  expect_s3_class(ord_fit, "cross_size_cv_result")

  ord_loo <- cross_size_loocv(analysis_dat, y, paste0("Q", 1:4), model_sizes = 1:2)
  expect_s3_class(ord_loo, "cross_size_cv_result")

  # Example D: cross_size_nested_cv
  nest_fit <- cross_size_nested_cv(analysis_dat, y, paste0("Q", 1:4), model_sizes = 1:2, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 42, progress = FALSE)
  expect_s3_class(nest_fit, "cross_size_nested_cv_result")

  # Example E: compare_cv_selection
  comp_fit <- compare_cv_selection(analysis_dat, y, paste0("Q", 1:4), model_sizes = 1:2, folds = 2, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 42, progress = FALSE)
  expect_s3_class(comp_fit, "compare_cv_selection_result")
})

test_that("Input validation: outcome with only one class throws clear error", {
  d_one_class <- data.frame(
    y  = rep(1, 20),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )

  expect_error(cv_sum_roc(d_one_class, y, c("Q1", "Q2")), "must contain both positive and negative cases")
  expect_error(cross_size_cv(d_one_class, y, c("Q1", "Q2"), model_sizes = 1:2), "must contain both positive and negative cases")
  expect_error(cross_size_nested_cv(d_one_class, y, c("Q1", "Q2"), model_sizes = 1:2), "must contain both positive and negative cases")
})
