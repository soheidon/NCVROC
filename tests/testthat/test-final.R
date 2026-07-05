# test-final.R — Tests for fit_final_sum_scale()

make_final_test_data <- function() {
  data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1)
  )
}

# ---- Basic return structure ----

test_that("fit_final_sum_scale returns a data.frame", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("fit_final_sum_scale has all required columns", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              progress = FALSE)
  expected_cols <- c("rank", "items", "n_items", "auc", "cutoff",
                     "sensitivity", "specificity", "youden", "accuracy",
                     "ppv", "npv", "n_positive", "n_negative")
  expect_true(all(expected_cols %in% colnames(res)))
})

# ---- top_n ----

test_that("fit_final_sum_scale top_n truncates correctly", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              top_n = 3, progress = FALSE)
  expect_equal(nrow(res), 3)
})

test_that("fit_final_sum_scale top_n default is 20", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              progress = FALSE)
  # With 3 items and max_items=2, we have C(3,1)+C(3,2)=6 combos, all < 20
  expect_equal(nrow(res), 6)
})

# ---- max_items ----

test_that("fit_final_sum_scale max_items controls combination size", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"),
                              min_items = 2, max_items = 2, progress = FALSE)
  expect_true(all(res$n_items == 2))
  expect_equal(nrow(res), 3)  # C(3,2) = 3
})

test_that("fit_final_sum_scale max_items = 1 works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 1,
                              progress = FALSE)
  expect_equal(nrow(res), 3)
  expect_true(all(res$n_items == 1))
})

# ---- rank_by ----

test_that("fit_final_sum_scale rank_by = 'auc' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = "auc", progress = FALSE)
  expect_equal(res$auc, sort(res$auc, decreasing = TRUE))
})

test_that("fit_final_sum_scale rank_by = 'youden' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = "youden", progress = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(res$youden, sort(res$youden, decreasing = TRUE))
})

test_that("fit_final_sum_scale rank_by = 'sensitivity' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = "sensitivity", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("fit_final_sum_scale rank_by = 'specificity' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = "specificity", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("fit_final_sum_scale rank_by = 'accuracy' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              rank_by = "accuracy", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("fit_final_sum_scale invalid rank_by errors", {
  d <- make_final_test_data()
  expect_error(
    fit_final_sum_scale(d, "y", c("q1"), rank_by = "invalid", progress = FALSE),
    "arg"
  )
})

# ---- cutoff_method ----

test_that("fit_final_sum_scale cutoff_method = 'youden' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2"), max_items = 2,
                              cutoff_method = "youden", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("fit_final_sum_scale cutoff_method = 'closest_topleft' works", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1", "q2"), max_items = 2,
                              cutoff_method = "closest_topleft", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

# ---- Equality with exhaustive_sum_roc ----

test_that("fit_final_sum_scale matches exhaustive_sum_roc with same args", {
  d <- make_final_test_data()
  res1 <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2,
                               top_n = 10, progress = FALSE)
  res2 <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                              top_n = 10, prefer_fewer_items = TRUE,
                              progress = FALSE)
  # Columns should be identical (performance_type attr is only on res1)
  expect_equal(res1$rank, res2$rank)
  expect_equal(res1$items, res2$items)
  expect_equal(res1$auc, res2$auc)
  expect_equal(res1$sensitivity, res2$sensitivity)
  expect_equal(res1$specificity, res2$specificity)
  expect_equal(res1$youden, res2$youden)
})

# ---- performance_type attribute ----

test_that("fit_final_sum_scale has performance_type = 'apparent'", {
  d <- make_final_test_data()
  res <- fit_final_sum_scale(d, "y", c("q1"), max_items = 1, progress = FALSE)
  expect_equal(attr(res, "performance_type"), "apparent")
})

test_that("exhaustive_sum_roc does NOT have performance_type attribute", {
  d <- make_final_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1"), max_items = 1, progress = FALSE)
  expect_null(attr(res, "performance_type"))
})

# ---- Inherited errors ----

test_that("fit_final_sum_scale errors on NA in data", {
  d <- make_final_test_data()
  d$q1[3] <- NA
  expect_error(
    fit_final_sum_scale(d, "y", c("q1", "q2"), max_items = 2, progress = FALSE),
    "NA values"
  )
})

test_that("fit_final_sum_scale errors on invalid outcome", {
  d <- data.frame(
    y  = c(1, 2, 0, 1, 0),
    q1 = c(1, 2, 0, 1, 0)
  )
  expect_error(
    fit_final_sum_scale(d, "y", c("q1"), positive_label = 1,
                         negative_label = 0, progress = FALSE),
    "not matching positive_label"
  )
})

test_that("fit_final_sum_scale errors on constant outcome", {
  d <- data.frame(
    y  = c(1, 1, 1),
    q1 = c(0, 1, 2)
  )
  expect_error(
    fit_final_sum_scale(d, "y", c("q1"), progress = FALSE),
    "only one unique value"
  )
})
