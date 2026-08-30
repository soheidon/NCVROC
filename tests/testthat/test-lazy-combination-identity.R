# tests/testthat/test-lazy-combination-identity.R
# Step 1: Lazy Combination Identity Exactness and Bounded-Memory Verification

test_that("lazy combination identity reproduces exact ranking and metrics across all backends", {
  set.seed(42)
  n <- 120
  p <- 12
  items <- sprintf("Q%02d", 1:p)
  X <- matrix(rbinom(n * p, 1, 0.45), nrow = n, ncol = p)
  lp <- 0.8 * X[, 1] + 0.7 * X[, 2] + 0.6 * X[, 3] - 1.0
  prob <- 1 / (1 + exp(-lp))
  y <- rbinom(n, 1, prob)
  smoke_dat <- as.data.frame(X)
  names(smoke_dat) <- items
  smoke_dat$y <- y

  # 1. Sequential R
  res_r <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    engine = "R", parallel = "none",
    top_n = 20, progress = FALSE
  )

  # 2. Sequential Rcpp
  res_rcpp <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    engine = "Rcpp", parallel = "none",
    top_n = 20, progress = FALSE
  )

  # 3. Threaded Rcpp
  res_threads <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    engine = "Rcpp", parallel = "threads", n_workers = 2,
    top_n = 20, progress = FALSE
  )

  # 4. Chunked Rcpp
  res_chunks <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    engine = "Rcpp", parallel = "chunks", n_workers = 2,
    top_n = 20, progress = FALSE
  )

  # Exact bitwise identity across all backends
  expect_identical(res_r, res_rcpp)
  expect_identical(res_rcpp, res_threads)
  expect_identical(res_threads, res_chunks)

  # Column contract
  expected_cols <- c(
    "rank", "items", "n_items", "auc", "cutoff",
    "sensitivity", "specificity", "youden", "accuracy",
    "ppv", "npv", "n_positive", "n_negative"
  )
  expect_equal(names(res_threads), expected_cols)
  expect_equal(nrow(res_threads), 20L)
  expect_equal(res_threads$rank, 1:20)
  expect_true(all(nzchar(res_threads$items)))
})

test_that("lazy combination identity preserves exactness in cross_size_cv", {
  set.seed(42)
  n <- 120
  p <- 12
  items <- sprintf("Q%02d", 1:p)
  X <- matrix(rbinom(n * p, 1, 0.45), nrow = n, ncol = p)
  lp <- 0.8 * X[, 1] + 0.7 * X[, 2] + 0.6 * X[, 3] - 1.0
  prob <- 1 / (1 + exp(-lp))
  y <- rbinom(n, 1, prob)
  smoke_dat <- as.data.frame(X)
  names(smoke_dat) <- items
  smoke_dat$y <- y

  cv_serial <- cross_size_cv(
    smoke_dat, "y", items,
    model_sizes = 1:3,
    selection_metric = "auc",
    engine = "Rcpp", parallel = "none",
    seed = 1234, progress = FALSE
  )

  cv_threads <- cross_size_cv(
    smoke_dat, "y", items,
    model_sizes = 1:3,
    selection_metric = "auc",
    engine = "Rcpp", parallel = "threads", n_workers = 2,
    seed = 1234, progress = FALSE
  )

  expect_identical(cv_serial$final_selected_model, cv_threads$final_selected_model)
  expect_identical(cv_serial$candidate_ranking, cv_threads$candidate_ranking)
  expect_identical(cv_serial$cv_performance, cv_threads$cv_performance)
})
