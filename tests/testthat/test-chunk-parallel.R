# test-chunk-parallel.R - Test suite for chunk-level parallelization

test_that(".resolve_parallel_mode correctly resolves context and rejects invalid modes", {
  # Nested context (ncvroc, nested_sum_roc)
  expect_identical(.resolve_parallel_mode(FALSE, "nested"), "none")
  expect_identical(.resolve_parallel_mode(TRUE, "nested"), "outer")
  expect_identical(.resolve_parallel_mode("none", "nested"), "none")
  expect_identical(.resolve_parallel_mode("outer", "nested"), "outer")
  expect_identical(.resolve_parallel_mode("chunks", "nested"), "chunks")

  # Exhaustive context (roc_bruteforce, exhaustive_sum_roc)
  expect_identical(.resolve_parallel_mode(FALSE, "exhaustive"), "none")
  expect_identical(.resolve_parallel_mode(TRUE, "exhaustive"), "chunks")
  expect_identical(.resolve_parallel_mode("none", "exhaustive"), "none")
  expect_identical(.resolve_parallel_mode("chunks", "exhaustive"), "chunks")

  # parallel = "auto" must be rejected in v0.12.0
  expect_error(.resolve_parallel_mode("auto", "nested"), "reserved for a future release")
  expect_error(.resolve_parallel_mode("auto", "exhaustive"), "reserved for a future release")

  # Invalid inputs
  expect_error(.resolve_parallel_mode("invalid", "nested"))
  expect_error(.resolve_parallel_mode(NA, "nested"))
  expect_error(.resolve_parallel_mode(123, "nested"))
})

test_that(".order_and_rank_candidates breaks ties deterministically with .global_combo_index", {
  # Synthetic data with equal AUC and sensitivity
  df <- data.frame(
    items              = c("Q2", "Q1", "Q3"),
    n_items            = c(1L, 1L, 1L),
    auc                = c(0.80, 0.80, 0.80),
    sensitivity        = c(0.75, 0.75, 0.75),
    .global_combo_index = c(2L, 1L, 3L),
    stringsAsFactors   = FALSE
  )

  ordered <- .order_and_rank_candidates(df, rank_by = "auc", prefer_fewer_items = TRUE)
  # Must preserve global combo index order: Q1 (1), Q2 (2), Q3 (3)
  expect_identical(ordered$items, c("Q1", "Q2", "Q3"))
  expect_identical(ordered$.global_combo_index, c(1L, 2L, 3L))
})

test_that("Serial vs. Chunk-Parallel results are identical on exhaustive_sum_roc", {
  set.seed(42)
  n <- 80
  d <- data.frame(
    y  = rep(c(0, 1), each = n / 2),
    q1 = rbinom(n, 2, 0.3),
    q2 = rbinom(n, 2, 0.5),
    q3 = rbinom(n, 2, 0.6),
    q4 = rbinom(n, 2, 0.4),
    q5 = rbinom(n, 2, 0.7)
  )

  # Serial run
  res_serial <- exhaustive_sum_roc(
    data        = d,
    outcome     = "y",
    items       = paste0("q", 1:5),
    min_items   = 1,
    max_items   = 3,
    engine      = "Rcpp",
    parallel    = FALSE,
    progress    = FALSE
  )

  # Chunk parallel run with 2 workers and small chunk size to force multiple chunks
  res_parallel <- exhaustive_sum_roc(
    data        = d,
    outcome     = "y",
    items       = paste0("q", 1:5),
    min_items   = 1,
    max_items   = 3,
    engine      = "Rcpp",
    parallel    = "chunks",
    n_workers   = 2L,
    chunk_size  = 5L,
    progress    = FALSE
  )

  expect_identical(nrow(res_serial), nrow(res_parallel))
  expect_identical(res_serial$items, res_parallel$items)
  expect_equal(res_serial$auc, res_parallel$auc, tolerance = 1e-12)
  expect_equal(res_serial$cutoff, res_parallel$cutoff, tolerance = 1e-12)
  expect_equal(res_serial$sensitivity, res_parallel$sensitivity, tolerance = 1e-12)
  expect_equal(res_serial$specificity, res_parallel$specificity, tolerance = 1e-12)
  expect_equal(res_serial$youden, res_parallel$youden, tolerance = 1e-12)
})

test_that("roc_bruteforce parallel = 'chunks' matches serial execution", {
  set.seed(123)
  n <- 60
  d <- data.frame(
    y  = rep(c(0, 1), each = n / 2),
    x1 = rbinom(n, 2, 0.4),
    x2 = rbinom(n, 2, 0.5),
    x3 = rbinom(n, 2, 0.6),
    x4 = rbinom(n, 2, 0.3)
  )

  res_serial <- roc_bruteforce(
    data      = d,
    outcome   = y,
    items     = x1:x4,
    min_items = 1,
    max_items = 3,
    engine    = "Rcpp",
    parallel  = FALSE,
    progress  = FALSE,
    top_n     = 10
  )

  res_parallel <- roc_bruteforce(
    data       = d,
    outcome    = y,
    items      = x1:x4,
    min_items  = 1,
    max_items  = 3,
    engine     = "Rcpp",
    parallel   = "chunks",
    n_workers  = 2L,
    chunk_size = 4L,
    progress   = FALSE,
    top_n      = 10
  )

  expect_identical(res_serial$best_model$items, res_parallel$best_model$items)
  expect_equal(res_serial$best_model$auc, res_parallel$best_model$auc, tolerance = 1e-12)
  expect_identical(res_serial$candidates$items, res_parallel$candidates$items)
  expect_equal(res_serial$candidates$auc, res_parallel$candidates$auc, tolerance = 1e-12)
})

test_that("Massive tie handling across chunk boundaries produces identical candidate ordering", {
  # Create data where every single item is constant (all combinations have identical AUC = 0.5)
  n <- 40
  d <- data.frame(
    y  = rep(c(0, 1), each = n / 2),
    q1 = rep(1, n),
    q2 = rep(1, n),
    q3 = rep(1, n),
    q4 = rep(1, n),
    q5 = rep(1, n)
  )

  res_serial <- exhaustive_sum_roc(
    data        = d,
    outcome     = "y",
    items       = paste0("q", 1:5),
    min_items   = 1,
    max_items   = 3,
    engine      = "Rcpp",
    parallel    = FALSE,
    progress    = FALSE
  )

  res_parallel <- exhaustive_sum_roc(
    data        = d,
    outcome     = "y",
    items       = paste0("q", 1:5),
    min_items   = 1,
    max_items   = 3,
    engine      = "Rcpp",
    parallel    = "chunks",
    n_workers   = 2L,
    chunk_size  = 3L,  # many small chunks with 100% tie
    progress    = FALSE
  )

  expect_identical(res_serial$items, res_parallel$items)
  expect_identical(res_serial$n_items, res_parallel$n_items)
  expect_identical(res_serial$rank, res_parallel$rank)
})

test_that("Atomic RDS writing ensures incomplete writes are not recognized as valid chunks and handles stale .tmp files", {
  temp_chunk_dir <- file.path(tempdir(), paste0("test_chunk_atomic_", Sys.getpid()))
  dir.create(temp_chunk_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_chunk_dir, recursive = TRUE), add = TRUE)

  dummy_df <- data.frame(items = "q1", n_items = 1L, auc = 0.85)

  path <- .write_chunk_rds(dummy_df, temp_chunk_dir, 0L)
  expect_true(file.exists(path))

  files <- list.files(temp_chunk_dir)
  expect_true("chunk_00000.rds" %in% files)
  # Must have NO temporary files remaining on clean completion
  expect_false(any(grepl("\\.tmp", files)))

  # Simulate a stale/incomplete .tmp file from an interrupted previous process
  stale_tmp <- file.path(temp_chunk_dir, "chunk_00001.rds.tmp-9999-1234")
  writeLines("incomplete corrupted content", stale_tmp)

  # .list_chunk_files must ignore .tmp files and only list valid chunk files
  valid_chunks <- .list_chunk_files(temp_chunk_dir)
  expect_equal(length(valid_chunks), 1L)
  expect_equal(basename(valid_chunks), "chunk_00000.rds")

  # Subsequent write for chunk 1 succeeds cleanly
  dummy_df2 <- data.frame(items = "q2", n_items = 1L, auc = 0.90)
  path2 <- .write_chunk_rds(dummy_df2, temp_chunk_dir, 1L)
  expect_true(file.exists(path2))
  expect_equal(basename(path2), "chunk_00001.rds")

  valid_chunks_after <- .list_chunk_files(temp_chunk_dir)
  expect_equal(length(valid_chunks_after), 2L)
  expect_equal(basename(valid_chunks_after), c("chunk_00000.rds", "chunk_00001.rds"))
})

test_that("nested_sum_roc with parallel = 'chunks' produces identical results to serial", {
  set.seed(999)
  n <- 60
  d <- data.frame(
    y  = rep(c(0, 1), each = n / 2),
    q1 = rbinom(n, 2, 0.4),
    q2 = rbinom(n, 2, 0.5),
    q3 = rbinom(n, 2, 0.6),
    q4 = rbinom(n, 2, 0.3)
  )

  res_serial <- nested_sum_roc(
    data            = d,
    outcome         = "y",
    items           = paste0("q", 1:4),
    min_items       = 1,
    max_items       = 2,
    outer_k         = 3,
    inner_k         = 2,
    engine          = "Rcpp",
    parallel        = FALSE,
    seed            = 12345,
    progress        = FALSE,
    verbose         = FALSE
  )

  res_parallel_chunks <- nested_sum_roc(
    data            = d,
    outcome         = "y",
    items           = paste0("q", 1:4),
    min_items       = 1,
    max_items       = 2,
    outer_k         = 3,
    inner_k         = 2,
    engine          = "Rcpp",
    parallel        = "chunks",
    n_workers       = 2L,
    seed            = 12345,
    progress        = FALSE,
    verbose         = FALSE
  )

  # Check summary and fold equivalence
  expect_equal(res_serial$summary$auc, res_parallel_chunks$summary$auc, tolerance = 1e-12)
  expect_identical(res_serial$summary$selected_items, res_parallel_chunks$summary$selected_items)
  expect_equal(res_serial$cv_predictions$predicted_score,
               res_parallel_chunks$cv_predictions$predicted_score,
               tolerance = 1e-12)
})

test_that("Worker capping and CRAN limit checks work correctly", {
  # Sys.getenv("_R_CHECK_LIMIT_CORES_") capping
  old_env <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  on.exit(if (nzchar(old_env)) Sys.setenv("_R_CHECK_LIMIT_CORES_" = old_env) else Sys.unsetenv("_R_CHECK_LIMIT_CORES_"), add = TRUE)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  expect_lte(.get_max_workers(), 2L)
  expect_lte(.resolve_n_workers(100L), 2L)
})
