# test-rcpp.R — Tests for engine = "Rcpp"
#
# All tests verify that engine = "Rcpp" produces identical results to engine = "R".
# See the exhaustive and nested test files for the shared helper functions.

skip_if_not_installed("Rcpp")

# ---- Basic return structure ----

test_that("engine = 'Rcpp' returns a data.frame", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            engine = "Rcpp", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("engine = 'Rcpp' has all required columns", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            engine = "Rcpp", progress = FALSE)
  expected_cols <- c("rank", "items", "n_items", "auc", "cutoff",
                     "sensitivity", "specificity", "youden", "accuracy",
                     "ppv", "npv", "n_positive", "n_negative")
  expect_true(all(expected_cols %in% colnames(res)))
})

test_that("engine = 'Rcpp' row count matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  expect_equal(nrow(r_rcpp), nrow(r_r))
})

# ---- Exact match with R engine ----

test_that("engine = 'Rcpp' items match R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  # Same items in same order
  expect_equal(r_rcpp$items, r_r$items)
})

test_that("engine = 'Rcpp' n_items matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$n_items, r_r$n_items)
})

test_that("engine = 'Rcpp' AUC matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$auc, r_r$auc, tolerance = 1e-12)
})

test_that("engine = 'Rcpp' numeric metrics match R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$cutoff, r_r$cutoff, tolerance = 1e-12)
  expect_equal(r_rcpp$sensitivity, r_r$sensitivity, tolerance = 1e-12)
  expect_equal(r_rcpp$specificity, r_r$specificity, tolerance = 1e-12)
  expect_equal(r_rcpp$youden, r_r$youden, tolerance = 1e-12)
  expect_equal(r_rcpp$accuracy, r_r$accuracy, tolerance = 1e-12)
})

test_that("engine = 'Rcpp' PPV/NPV match R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  # Compare non-NA values
  ok_rcpp <- !is.na(r_rcpp$ppv)
  ok_r <- !is.na(r_r$ppv)
  expect_equal(ok_rcpp, ok_r)
  expect_equal(r_rcpp$ppv[ok_rcpp], r_r$ppv[ok_r], tolerance = 1e-12)

  ok_rcpp <- !is.na(r_rcpp$npv)
  ok_r <- !is.na(r_r$npv)
  expect_equal(ok_rcpp, ok_r)
  expect_equal(r_rcpp$npv[ok_rcpp], r_r$npv[ok_r], tolerance = 1e-12)
})

# ---- cutoff_method ----

test_that("engine = 'Rcpp' cutoff_method = 'youden' matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                cutoff_method = "youden",
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             cutoff_method = "youden",
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$auc, r_r$auc, tolerance = 1e-12)
  expect_equal(r_rcpp$cutoff, r_r$cutoff, tolerance = 1e-12)
  expect_equal(r_rcpp$youden, r_r$youden, tolerance = 1e-12)
})

test_that("engine = 'Rcpp' cutoff_method = 'closest_topleft' matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                cutoff_method = "closest_topleft",
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             cutoff_method = "closest_topleft",
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$auc, r_r$auc, tolerance = 1e-12)
  expect_equal(r_rcpp$cutoff, r_r$cutoff, tolerance = 1e-12)
})

# ---- rank_by ----

test_that("engine = 'Rcpp' all rank_by values work", {
  d <- make_test_data()
  for (rb in c("auc", "youden", "sensitivity", "specificity", "accuracy")) {
    res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = rb, engine = "Rcpp", progress = FALSE)
    expect_s3_class(res, "data.frame")
    expect_equal(res$rank, seq_len(nrow(res)))
  }
})

# ---- top_n / max_items ----

test_that("engine = 'Rcpp' top_n truncation works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            top_n = 3, engine = "Rcpp", progress = FALSE)
  expect_equal(nrow(res), 3)
})

test_that("engine = 'Rcpp' max_items = 1 works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 1,
                            engine = "Rcpp", progress = FALSE)
  expect_equal(nrow(res), 3)
  expect_true(all(res$n_items == 1))
})

test_that("engine = 'Rcpp' rank column is sequential", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            engine = "Rcpp", progress = FALSE)
  expect_equal(res$rank, seq_len(nrow(res)))
})

# ---- Edge cases ----

test_that("engine = 'Rcpp' single item works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1"), max_items = 1,
                            engine = "Rcpp", progress = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$items, "q1")
})

test_that("engine = 'Rcpp' NA in data errors", {
  d <- make_test_data()
  d$q1[3] <- NA
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                       engine = "Rcpp", progress = FALSE),
    "NA values"
  )
})

test_that("engine = 'Rcpp' constant outcome errors", {
  d <- data.frame(
    y  = c(1, 1, 1),
    q1 = c(0, 1, 2)
  )
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1"), engine = "Rcpp", progress = FALSE),
    "only one unique value"
  )
})

# ---- n_positive / n_negative ----

test_that("engine = 'Rcpp' n_positive matches R engine", {
  d <- make_test_data()
  r_rcpp <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                                engine = "Rcpp", progress = FALSE)
  r_r <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  expect_equal(r_rcpp$n_positive, r_r$n_positive)
  expect_equal(r_rcpp$n_negative, r_r$n_negative)
})

# ---- nested_sum_roc engine = "Rcpp" smoke test ----

test_that("nested_sum_roc(engine = 'Rcpp') works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2,
    engine = "Rcpp", seed = 42, progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  expect_true(all(c("summary", "outer_results", "selected_models",
                    "selected_model_frequency", "outer_predictions",
                    "settings") %in% names(res)))
})
