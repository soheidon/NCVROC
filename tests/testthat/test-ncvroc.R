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
  expect_true(file.exists(file.path(tmp, "final_candidates.csv")))
  expect_true(file.exists(file.path(tmp, "final_model.csv")))

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
  expect_false(file.exists(file.path(tmp, "final_candidates.csv")))
  expect_false(file.exists(file.path(tmp, "final_model.csv")))
  expect_true(file.exists(file.path(tmp, "nested_cv_summary.csv")))
})

# ---- Final candidate display options ----

COMMON_CV_FINAL <- list(
  outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
  max_items = 2, engine = "R", progress = FALSE, verbose = FALSE,
  seed = 42, final_search = TRUE
)

test_that("final_top_n = 2 stores at most 2 rows", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_top_n = 2), COMMON_CV_FINAL))
  expect_lte(nrow(result$final_candidates), 2)
})

test_that("final_top_n = 1 stores exactly 1 row", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_top_n = 1), COMMON_CV_FINAL))
  expect_equal(nrow(result$final_candidates), 1)
})

test_that("final_top_n = NULL stores all rows", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_top_n = NULL), COMMON_CV_FINAL))
  expect_equal(nrow(result$final_candidates), nrow(result$final_exhaustive_ranked))
})

test_that("final_top_n = 0 stores NULL", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_top_n = 0), COMMON_CV_FINAL))
  expect_null(result$final_candidates)
})

test_that("final_search = FALSE gives NULL for all final fields", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_null(result$final_exhaustive_ranked)
  expect_null(result$final_candidates)
  expect_null(result$final_model)
})

test_that("final_model equals first row of final_exhaustive_ranked", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2")),
                              COMMON_CV_FINAL))
  expect_equal(result$final_model$items, result$final_exhaustive_ranked$items[1])
  expect_equal(result$final_model$auc, result$final_exhaustive_ranked$auc[1])
})

test_that("final_rank_by = 'auc' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_rank_by = "auc"), COMMON_CV_FINAL))
  expect_equal(result$final_rank_by, "auc")
  expect_s3_class(result$final_exhaustive_ranked, "data.frame")
})

test_that("final_rank_by = 'youden' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_rank_by = "youden"), COMMON_CV_FINAL))
  expect_equal(result$final_rank_by, "youden")
  expect_s3_class(result$final_exhaustive_ranked, "data.frame")
})

test_that("final_rank_by = 'sensitivity' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_rank_by = "sensitivity"), COMMON_CV_FINAL))
  expect_equal(result$final_rank_by, "sensitivity")
  expect_s3_class(result$final_exhaustive_ranked, "data.frame")
})

test_that("invalid final_rank_by errors clearly", {
  dat <- make_ncvroc_test_data()
  expect_error(
    do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                           final_rank_by = "invalid"), COMMON_CV_FINAL)),
    "arg.*should be one of"
  )
})

test_that("save_results writes final_candidates.csv and final_model.csv", {
  dat <- make_ncvroc_test_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_test_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                         save_results = TRUE, output_dir = tmp), COMMON_CV_FINAL))

  expect_true(file.exists(file.path(tmp, "final_exhaustive_results_ranked.csv")))
  expect_true(file.exists(file.path(tmp, "final_candidates.csv")))
  expect_true(file.exists(file.path(tmp, "final_model.csv")))
})

# ---- Plot method ----

test_that("plot.ncvroc_analysis() with which='selection' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_silent(plot(result, which = "selection"))
})

test_that("plot.ncvroc_analysis() with which='auc' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_silent(plot(result, which = "auc"))
})

test_that("plot.ncvroc_analysis() with which='all' works", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_silent(plot(result, which = "all"))
})

test_that("plot.ncvroc_analysis() with invalid which errors", {
  dat <- make_ncvroc_test_data()
  result <- do.call(ncvroc, c(list(data = dat, outcome = "y", items = c("Q1", "Q2"),
                                   final_search = FALSE), COMMON_CV))
  expect_error(plot(result, which = "invalid"), "arg.*should be one of")
})

test_that("plot.ncvroc_analysis() with NULL nested_result errors", {
  x <- list(nested_result = NULL)
  class(x) <- "ncvroc_analysis"
  expect_error(plot(x), "nested_result is NULL")
})

# ---- ncvroc_results ----

make_ncvroc_results_x <- function() {
  x <- list(final_exhaustive_ranked = data.frame(
    items       = c("Q1", "Q2", "Q3"),
    n_items     = c(1, 2, 3),
    auc         = c(0.80, 0.85, 0.83),
    cutoff      = c(1, 2, 2),
    sensitivity = c(1.00, 0.90, 0.85),
    specificity = c(0.75, 0.86, 0.90),
    youden      = c(0.75, 0.76, 0.75),
    accuracy    = c(0.78, 0.87, 0.88),
    stringsAsFactors = FALSE
  ))
  class(x) <- "ncvroc_analysis"
  x
}

test_that("sensitivity >= 0.90 filters correctly", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, sensitivity = ">= 0.90")
  expect_equal(nrow(res), 2)
  expect_true(all(res$sensitivity >= 0.90))
})

test_that("specificity >= 0.86 filters correctly", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, specificity = ">= 0.86")
  expect_equal(nrow(res), 2)
  expect_true(all(res$specificity >= 0.86))
})

test_that("two conditions combined with AND", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, sensitivity = ">= 0.90", specificity = ">= 0.86")
  expect_equal(nrow(res), 1)
  expect_true(all(res$sensitivity >= 0.90))
  expect_true(all(res$specificity >= 0.86))
})

test_that("auc >= 0.83 filters", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, auc = ">= 0.83")
  expect_equal(nrow(res), 2)
  expect_true(all(res$auc >= 0.83))
})

test_that("n_items <= 2 filters", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, n_items = "<= 2")
  expect_equal(nrow(res), 2)
  expect_true(all(res$n_items <= 2))
})

test_that("cutoff >= 2 filters", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, cutoff = ">= 2")
  expect_equal(nrow(res), 2)
  expect_true(all(res$cutoff >= 2))
})

test_that("rank_by = 'auc' sorts AUC descending", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, rank_by = "auc")
  expect_true(all(diff(res$auc) <= 0))
})

test_that("rank_by = 'specificity' sorts specificity descending", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, rank_by = "specificity")
  expect_true(all(diff(res$specificity) <= 0))
})

test_that("tiebreakers don't error", {
  x <- make_ncvroc_results_x()
  expect_silent(ncvroc_results(x, rank_by = "youden"))
})

test_that("top_n = 1 returns one row", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, top_n = 1)
  expect_equal(nrow(res), 1)
})

test_that("top_n = NULL returns all matching rows", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, top_n = NULL)
  expect_equal(nrow(res), 3)
})

test_that("top_n = 0 returns 0-row data.frame", {
  x <- make_ncvroc_results_x()
  res <- ncvroc_results(x, top_n = 0)
  expect_equal(nrow(res), 0)
  expect_s3_class(res, "data.frame")
})

test_that("top_n = 1.5 errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, top_n = 1.5), "non-negative integer")
})

test_that("top_n = -1 errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, top_n = -1), "non-negative integer")
})

test_that("top_n = c(1, 2) errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, top_n = c(1, 2)), "non-negative integer")
})

test_that("invalid condition '=> 0.90' errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, sensitivity = "=> 0.90"), "must be a string like")
})

test_that("invalid condition 'greater than 0.90' errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, sensitivity = "greater than 0.90"), "must be a string like")
})

test_that("invalid condition '>= abc' errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, sensitivity = ">= abc"), "must be a string like")
})

test_that("invalid condition '0.90' (no operator) errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, sensitivity = "0.90"), "must be a string like")
})

test_that("missing condition column errors", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, ppv = ">= 0.80"), "Column 'ppv' not found")
})

test_that("missing final_exhaustive_ranked errors clearly", {
  x <- list()
  class(x) <- "ncvroc_analysis"
  expect_error(ncvroc_results(x), "final_exhaustive_ranked is NULL")
})

test_that("invalid rank_by errors via match.arg", {
  x <- make_ncvroc_results_x()
  expect_error(ncvroc_results(x, rank_by = "invalid"), "should be one of")
})

# ---- Factor safety regression ----

test_that("factor outcome column is handled safely by .prepare_ncvroc_data", {
  d <- make_ncvroc_test_data()
  d$y <- factor(d$y)
  result <- ncvroc(d, "y", Q1:Q3, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = FALSE)
  expect_s3_class(result, "ncvroc_analysis")
})

test_that("factor item column is handled safely by .prepare_ncvroc_data", {
  d <- make_ncvroc_test_data()
  d$Q1 <- factor(d$Q1)
  result <- ncvroc(d, "y", Q1:Q3, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = FALSE)
  expect_s3_class(result, "ncvroc_analysis")
})
