# test-thread-parallel.R — Unit tests for C++ multi-threading integration

test_that(".resolve_parallel_mode correctly resolves threads mode and preserves backward compatibility", {
  # Nested context
  expect_equal(NCVROC:::.resolve_parallel_mode(FALSE, context = "nested"), "none")
  expect_equal(NCVROC:::.resolve_parallel_mode(TRUE, context = "nested"), "outer")
  expect_equal(NCVROC:::.resolve_parallel_mode("none", context = "nested"), "none")
  expect_equal(NCVROC:::.resolve_parallel_mode("outer", context = "nested"), "outer")
  expect_equal(NCVROC:::.resolve_parallel_mode("chunks", context = "nested"), "chunks")
  expect_equal(NCVROC:::.resolve_parallel_mode("threads", context = "nested"), "threads")

  # Exhaustive context
  expect_equal(NCVROC:::.resolve_parallel_mode(FALSE, context = "exhaustive"), "none")
  expect_equal(NCVROC:::.resolve_parallel_mode(TRUE, context = "exhaustive"), "chunks")
  expect_equal(NCVROC:::.resolve_parallel_mode("none", context = "exhaustive"), "none")
  expect_equal(NCVROC:::.resolve_parallel_mode("chunks", context = "exhaustive"), "chunks")
  expect_equal(NCVROC:::.resolve_parallel_mode("threads", context = "exhaustive"), "threads")
  expect_error(NCVROC:::.resolve_parallel_mode("outer", context = "exhaustive"), "not supported")

  # Errors
  expect_error(NCVROC:::.resolve_parallel_mode("auto", context = "nested"), "reserved")
  expect_error(NCVROC:::.resolve_parallel_mode("invalid_mode", context = "nested"), "one of")
})

test_that("exhaustive_sum_roc with parallel = 'threads' produces identical results to serial", {
  set.seed(42)
  n <- 80
  p <- 10
  dat <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  colnames(dat) <- sprintf("q%02d", 1:p)
  dat$y <- rep(c(0, 1), each = n / 2)

  ref_serial <- exhaustive_sum_roc(
    data = dat, outcome = "y", items = colnames(dat)[1:p],
    min_items = 1, max_items = 3, cutoff_method = "youden",
    parallel = FALSE, engine = "Rcpp"
  )

  for (th in c(1L, 2L, 4L)) {
    res_th <- exhaustive_sum_roc(
      data = dat, outcome = "y", items = colnames(dat)[1:p],
      min_items = 1, max_items = 3, cutoff_method = "youden",
      parallel = "threads", n_workers = th, engine = "Rcpp"
    )
    expect_identical(ref_serial, res_th)
  }

  # Test closest_topleft cutoff method
  ref_topleft <- exhaustive_sum_roc(
    data = dat, outcome = "y", items = colnames(dat)[1:p],
    min_items = 1, max_items = 3, cutoff_method = "closest_topleft",
    parallel = FALSE, engine = "Rcpp"
  )
  res_topleft_th <- exhaustive_sum_roc(
    data = dat, outcome = "y", items = colnames(dat)[1:p],
    min_items = 1, max_items = 3, cutoff_method = "closest_topleft",
    parallel = "threads", n_workers = 2L, engine = "Rcpp"
  )
  expect_identical(ref_topleft, res_topleft_th)
})

test_that("exhaustive_sum_roc handles massive ties identically under threads and serial", {
  set.seed(123)
  n <- 60
  p <- 8
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  colnames(dat) <- sprintf("item%02d", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)

  ref_serial <- exhaustive_sum_roc(
    data = dat, outcome = "y", items = colnames(dat)[1:p],
    min_items = 1, max_items = 4, cutoff_method = "youden",
    parallel = FALSE, engine = "Rcpp"
  )

  res_threads <- exhaustive_sum_roc(
    data = dat, outcome = "y", items = colnames(dat)[1:p],
    min_items = 1, max_items = 4, cutoff_method = "youden",
    parallel = "threads", n_workers = 4L, engine = "Rcpp"
  )

  expect_identical(ref_serial, res_threads)
})

test_that("roc_bruteforce produces identical best_model and candidates under parallel = 'threads'", {
  set.seed(99)
  n <- 50
  p <- 7
  dat <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  colnames(dat) <- sprintf("Q%d", 1:p)
  dat$y <- rbinom(n, 1, 0.5)

  res_serial <- roc_bruteforce(
    data = dat, outcome = y, items = Q1:Q7,
    max_items = 3, parallel = FALSE, top_n = 10, engine = "Rcpp"
  )

  res_threads <- roc_bruteforce(
    data = dat, outcome = y, items = Q1:Q7,
    max_items = 3, parallel = "threads", n_workers = 2L, top_n = 10, engine = "Rcpp"
  )

  expect_equal(as.data.frame(res_serial$best_model), as.data.frame(res_threads$best_model))
  expect_equal(as.data.frame(res_serial$candidates), as.data.frame(res_threads$candidates))
  expect_equal(as.data.frame(res_serial$results), as.data.frame(res_threads$results))
})

test_that("nested_sum_roc with parallel = 'threads' matches serial exactly", {
  set.seed(777)
  n <- 60
  p <- 6
  dat <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  colnames(dat) <- sprintf("x%d", 1:p)
  dat$outcome <- rep(c(0, 1), each = n / 2)

  res_serial <- nested_sum_roc(
    data = dat, outcome = "outcome", items = colnames(dat)[1:p],
    min_items = 1, max_items = 3, outer_k = 3, inner_k = 2,
    seed = 1234, parallel = FALSE, verbose = FALSE, progress = FALSE,
    engine = "Rcpp"
  )

  res_threads <- nested_sum_roc(
    data = dat, outcome = "outcome", items = colnames(dat)[1:p],
    min_items = 1, max_items = 3, outer_k = 3, inner_k = 2,
    seed = 1234, parallel = "threads", n_workers = 2L,
    verbose = FALSE, progress = FALSE, engine = "Rcpp"
  )

  # Check exact match across all outer CV folds and metrics
  expect_identical(res_serial$summary, res_threads$summary)
  expect_identical(res_serial$selected_models, res_threads$selected_models)
  expect_identical(res_serial$selected_model_frequency, res_threads$selected_model_frequency)
  expect_identical(res_serial$outer_predictions, res_threads$outer_predictions)
})

test_that("ncvroc and ncvroc_config correctly propagate and execute parallel = 'threads'", {
  set.seed(42)
  n <- 50
  p <- 5
  dat <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  colnames(dat) <- sprintf("q%d", 1:p)
  dat$y <- rep(c(0, 1), each = n / 2)

  cfg <- ncvroc_config(
    outcome = "y", items = sprintf("q%d", 1:p),
    max_items = 2, outer_k = 2, inner_k = 2, outer_repeats = 1,
    parallel = "threads", n_workers = 2L
  )

  expect_equal(cfg$parallel, "threads")
  expect_equal(cfg$n_workers, 2L)

  res <- run_ncvroc(data = dat, items = sprintf("q%d", 1:p), config = cfg, seed = 100, progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  expect_equal(nrow(res$summary), 2L)

  # Full ncvroc call with parallel = "threads"
  res_ncv <- ncvroc(
    data = dat, outcome = "y", items = sprintf("q%d", 1:p),
    max_items = 2, outer_k = 2, inner_k = 2, outer_repeats = 1,
    parallel = "threads", n_workers = 2L, seed = 100,
    progress = FALSE, verbose = FALSE
  )
  expect_s3_class(res_ncv, "ncvroc_analysis")
  expect_equal(nrow(res_ncv$nested_cv_summary), 2L)
})

test_that("fit_final_sum_scale supports parallel = 'threads'", {
  set.seed(42)
  n <- 50
  p <- 5
  dat <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  colnames(dat) <- sprintf("q%d", 1:p)
  dat$y <- rep(c(0, 1), each = n / 2)

  ref <- fit_final_sum_scale(dat, "y", sprintf("q%d", 1:p), max_items = 2, engine = "Rcpp", parallel = FALSE, progress = FALSE)
  res <- fit_final_sum_scale(dat, "y", sprintf("q%d", 1:p), max_items = 2, engine = "Rcpp", parallel = "threads", n_workers = 2L, progress = FALSE)

  expect_identical(ref, res)
})

test_that("CRAN limit environment variable is respected", {
  old_val <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit({
    if (is.na(old_val)) {
      Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    } else {
      Sys.setenv("_R_CHECK_LIMIT_CORES_" = old_val)
    }
  }, add = TRUE)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  workers <- NCVROC:::.resolve_n_workers(parallel = TRUE, n_workers = 8L)
  expect_lte(workers, 2L)
})
