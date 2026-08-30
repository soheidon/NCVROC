# tests/testthat/test-streaming-topn.R
# Step 2: In-C++ Streaming Top-N Fast Path Equivalence and Invariant Tests

test_that("streaming C++ top-n is bitwise identical to full evaluation across all metrics and tie-break settings", {
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

  metrics <- c("auc", "youden", "sensitivity", "specificity", "accuracy")
  pfis <- c(TRUE, FALSE)
  top_ns <- c(1L, 5L, 20L)

  for (metric in metrics) {
    for (pfi in pfis) {
      for (tn in top_ns) {
        # Full reference via serial R engine
        ref_full <- exhaustive_sum_roc(
          smoke_dat, "y", items,
          min_items = 1, max_items = 3,
          cutoff_method = "youden",
          rank_by = metric,
          prefer_fewer_items = pfi,
          top_n = NULL,
          engine = "R",
          parallel = "none",
          progress = FALSE
        )
        ref_topn <- utils::head(ref_full, tn)
        rownames(ref_topn) <- NULL

        # Streaming Top-N C++ Serial
        topn_serial <- exhaustive_sum_roc(
          smoke_dat, "y", items,
          min_items = 1, max_items = 3,
          cutoff_method = "youden",
          rank_by = metric,
          prefer_fewer_items = pfi,
          top_n = tn,
          engine = "Rcpp",
          parallel = "none",
          progress = FALSE
        )

        # Streaming Top-N C++ Threads (2 workers)
        topn_threads2 <- exhaustive_sum_roc(
          smoke_dat, "y", items,
          min_items = 1, max_items = 3,
          cutoff_method = "youden",
          rank_by = metric,
          prefer_fewer_items = pfi,
          top_n = tn,
          engine = "Rcpp",
          parallel = "threads",
          n_workers = 2,
          progress = FALSE
        )

        expect_identical(ref_topn, topn_serial)
        expect_identical(ref_topn, topn_threads2)
      }
    }
  }
})

test_that("streaming top-n handles closest_topleft cutoff method identically", {
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

  ref <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    cutoff_method = "closest_topleft",
    rank_by = "auc",
    top_n = NULL,
    engine = "R",
    parallel = "none",
    progress = FALSE
  )
  ref_top10 <- utils::head(ref, 10L)
  rownames(ref_top10) <- NULL

  topn_threads <- exhaustive_sum_roc(
    smoke_dat, "y", items,
    min_items = 1, max_items = 3,
    cutoff_method = "closest_topleft",
    rank_by = "auc",
    top_n = 10L,
    engine = "Rcpp",
    parallel = "threads",
    n_workers = 2,
    progress = FALSE
  )

  expect_identical(ref_top10, topn_threads)
})

test_that("streaming top-n preserves selected model in cross_size_cv", {
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

  cv_res <- cross_size_cv(
    smoke_dat, "y", items,
    model_sizes = 1:3,
    selection_metric = "auc",
    engine = "Rcpp",
    parallel = "threads",
    n_workers = 2,
    seed = 1234,
    progress = FALSE
  )

  expect_s3_class(cv_res, "cross_size_cv_result")
  expect_true(nrow(cv_res$final_selected_model) == 1L)
  expect_true(nrow(cv_res$candidate_ranking) > 0L)
})

test_that("HIGH-3: C++ Top-N parameter validation rejects invalid chunk_size and chunk_start", {
  n <- 20
  p <- 4
  X <- matrix(rbinom(n * p, 1, 0.5), nrow = n, ncol = p)
  y <- as.integer(rbinom(n, 1, 0.5))

  # chunk_size <= 0 must throw an error
  expect_error(
    NCVROC:::evaluate_combos_cpp_chunk_parallel_topn(
      x = X, y = y, min_items = 1L, max_items = 2L,
      cutoff_method = "youden", rank_by = "auc", top_n = 5L,
      prefer_fewer_items = TRUE, chunk_start = 0.0, chunk_size = 0L
    ),
    "chunk_size must be a positive integer"
  )

  # chunk_start < 0 must throw an error
  expect_error(
    NCVROC:::evaluate_combos_cpp_chunk_parallel_topn(
      x = X, y = y, min_items = 1L, max_items = 2L,
      cutoff_method = "youden", rank_by = "auc", top_n = 5L,
      prefer_fewer_items = TRUE, chunk_start = -1.0, chunk_size = 5L
    ),
    "chunk_start must be a non-negative finite number"
  )
})

test_that("HIGH-3: Mandatory batching runs correctly when progress = FALSE on large workloads", {
  set.seed(123)
  n <- 40
  p <- 14
  items <- paste0("Q", 1:p)
  X <- matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p)
  dat <- as.data.frame(X)
  names(dat) <- items
  dat$y <- rbinom(n, 1, 0.5)

  # p=14, sizes=1:4 -> 14 + 91 + 364 + 1001 = 1470 combinations
  res_prog_f <- exhaustive_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = items,
    min_items          = 1,
    max_items          = 4,
    cutoff_method      = "youden",
    rank_by            = "auc",
    top_n              = 10L,
    engine             = "Rcpp",
    parallel           = "threads",
    n_workers          = 2,
    progress           = FALSE
  )

  res_prog_t <- exhaustive_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = items,
    min_items          = 1,
    max_items          = 4,
    cutoff_method      = "youden",
    rank_by            = "auc",
    top_n              = 10L,
    engine             = "Rcpp",
    parallel           = "threads",
    n_workers          = 2,
    progress           = TRUE
  )

  expect_identical(res_prog_f, res_prog_t)
})
