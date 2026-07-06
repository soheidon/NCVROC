# test-ncvroc.R - Tests for ncvroc() main entry function (v0.4)

# Test data
make_ncvroc_test_data <- function() {
  data.frame(
    id = 1:12,
    y  = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
    Q1 = c(0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0),
    Q2 = c(0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1),
    Q3 = c(1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1),
    Q4 = c(0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1)
  )
}

COMMON_CV <- list(
  outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
  max_items = 2, engine = "R", progress = FALSE, verbose = FALSE,
  seed = 42
)

# ---- Item resolution ----

test_that("ncvroc() resolves bare column range", {
  dat <- make_ncvroc_test_data()
  result <- ncvroc(dat, y, Q1:Q3,
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    max_items = 2, engine = "R", progress = FALSE, verbose = FALSE,
    seed = 42, final_search = FALSE)
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
})

test_that("ncvroc() resolves bare column names with c()", {
  dat <- make_ncvroc_test_data()
  result <- ncvroc(dat, y, c(Q1, Q2, Q3),
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    max_items = 2, engine = "R", progress = FALSE, verbose = FALSE,
    seed = 42, final_search = FALSE)
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
})

test_that("ncvroc() accepts character vector literal", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2", "Q3"),
                                   final_search = FALSE), COMMON_CV))
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
})

test_that("ncvroc() resolves existing variable as items", {
  dat <- make_ncvroc_test_data()
  items <- c("Q1", "Q2", "Q3")
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = items,
                                   final_search = FALSE), COMMON_CV))
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
})

test_that("ncvroc() resolves numeric column positions", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = 3:5,
                                   final_search = FALSE), COMMON_CV))
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
})

# ---- Outcome resolution ----

test_that("ncvroc() resolves bare outcome", {
  dat <- make_ncvroc_test_data()
  result <- ncvroc(dat, y, c("Q1", "Q2"),
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    max_items = 2, engine = "R", progress = FALSE, verbose = FALSE,
    seed = 42, final_search = FALSE)
  expect_equal(result$outcome, "y")
  expect_s3_class(result, "ncvroc_analysis")
})

test_that("ncvroc() resolves character outcome", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_equal(result$outcome, "y")
  expect_s3_class(result, "ncvroc_analysis")
})

# ---- Main analysis ----

test_that("ncvroc() returns ncvroc_analysis object with correct components", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2", "Q3"),
                                   final_search = FALSE), COMMON_CV))

  expect_s3_class(result, "ncvroc_analysis")
  expect_s3_class(result$nested_result, "ncvroc_result")
  expect_s3_class(result$config, "ncvroc_config")
  expect_equal(result$outcome, "y")
  expect_equal(result$items, c("Q1", "Q2", "Q3"))
  expect_false(is.null(result$nested_cv_summary))
  expect_false(is.null(result$selected_model_frequency))
  expect_false(is.null(result$outer_predictions))
})

test_that("ncvroc() with final_search = FALSE returns NULL ranked", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_null(result$final_exhaustive_ranked)
})

test_that("ncvroc() with final_search = TRUE returns data.frame", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = TRUE), COMMON_CV))
  expect_s3_class(result$final_exhaustive_ranked, "data.frame")
  expect_gt(nrow(result$final_exhaustive_ranked), 0)
})

test_that("ncvroc() AUC matches direct nested_sum_roc() call", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2", "Q3"),
                                   final_search = FALSE), COMMON_CV))

  direct <- nested_sum_roc(dat, "y", c("Q1", "Q2", "Q3"),
    max_items = 2, outer_k = 2, inner_k = 2,
    outer_repeats = 1, inner_repeats = 1,
    stratified = TRUE, engine = "R", seed = 42,
    progress = FALSE, verbose = FALSE)

  expect_equal(result$nested_cv_summary$auc, direct$summary$auc)
})

# ---- Print method ----

test_that("print.ncvroc_analysis() works without error", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))

  expect_output(print(result), "NCVROC analysis")
  expect_output(print(result), "Outcome:")
  expect_output(print(result), "Items:")
  expect_output(print(result), "Mode:")
  expect_output(print(result), "Final exhaustive search:")
})

test_that("print.ncvroc_analysis() returns input invisibly", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))

  out <- print(result)
  expect_identical(out, result)
})

# ---- Save results ----

test_that("ncvroc() save_results writes all expected CSV files", {
  dat <- make_ncvroc_test_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_test_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = TRUE, save_results = TRUE,
                                   output_dir = tmp), COMMON_CV))

  expect_true(file.exists(file.path(tmp, "nested_cv_outer_fold_results.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_selected_models.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_outer_predictions.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_summary.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_selected_model_frequency.csv")))
  expect_true(file.exists(file.path(tmp, "final_exhaustive_results_ranked.csv")))

  summary_csv <- read.csv(file.path(tmp, "nested_cv_summary.csv"))
  expect_s3_class(summary_csv, "data.frame")
  expect_equal(nrow(summary_csv), nrow(result$nested_cv_summary))
})

test_that("ncvroc() save_results without final_search omits final CSV", {
  dat <- make_ncvroc_test_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_test_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                         final_search = FALSE, save_results = TRUE,
                         output_dir = tmp), COMMON_CV))

  expect_false(file.exists(file.path(tmp, "final_exhaustive_results_ranked.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_summary.csv")))
})
