# Tests for v0.29.0 fine-grained model_size and candidate_batch checkpointing

.fg_make_test_data <- function(n = 48L, p = 6L, seed = 202611L) {
  set.seed(seed)
  y <- rep(c(0L, 1L), length.out = n)
  x <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  x[y == 1L, 1L] <- x[y == 1L, 1L] + 1.1
  x[y == 1L, 2L] <- x[y == 1L, 2L] + 0.8
  df <- as.data.frame(x)
  names(df) <- paste0("q", seq_len(p))
  df$y <- y
  df[, c("y", paste0("q", seq_len(p)))]
}

.fg_assert_parity <- function(res_a, res_b, tol = 1e-12) {
  expect_equal(res_a$selected_models, res_b$selected_models)
  expect_equal(res_a$summary$outer_fold, res_b$summary$outer_fold)
  expect_equal(res_a$summary$n_items, res_b$summary$n_items)
  expect_equal(res_a$summary$auc, res_b$summary$auc, tolerance = tol)
  expect_equal(res_a$summary$cutoff, res_b$summary$cutoff, tolerance = tol)
  expect_equal(res_a$summary$youden, res_b$summary$youden, tolerance = tol)
  expect_equal(res_a$outer_predictions$row_index, res_b$outer_predictions$row_index)
  expect_equal(res_a$outer_predictions$predicted_class, res_b$outer_predictions$predicted_class)
  expect_equal(res_a$outer_predictions$predicted_score, res_b$outer_predictions$predicted_score, tolerance = tol)
}

test_that("checkpoint_granularity validation and unsupported combinations fail closed", {
  dat <- .fg_make_test_data(n = 36L, p = 4L)
  items <- paste0("q", 1:4)
  cp_dir <- tempfile("ncvroc_fg_val_")
  on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    nested_sum_roc(dat, "y", items, max_items = 2, outer_k = 3, inner_k = 2,
                   checkpoint_granularity = "inner_fold", progress = FALSE, verbose = FALSE),
    "checkpoint_granularity"
  )
  expect_error(
    nested_sum_roc(dat, "y", items, max_items = 2, outer_k = 3, inner_k = 2,
                   checkpoint_granularity = c("outer_fold", "model_size"),
                   progress = FALSE, verbose = FALSE),
    "checkpoint_granularity"
  )
  expect_error(
    nested_sum_roc(dat, "y", items, max_items = 2, outer_k = 3, inner_k = 2,
                   parallel = "outer", checkpoint_dir = cp_dir,
                   checkpoint_granularity = "candidate_batch",
                   progress = FALSE, verbose = FALSE),
    "checkpoint_dir"
  )
  expect_error(
    nested_sum_roc(dat, "y", items, max_items = 2, outer_k = 3, inner_k = 2,
                   parallel = "none", tuning = "auto", checkpoint_dir = cp_dir,
                   checkpoint_granularity = "model_size",
                   progress = FALSE, verbose = FALSE),
    "tuning"
  )
})

test_that("candidate unranking and batch range partitioning match combn exactly", {
  items <- paste0("q", 1:6)
  for (k in 1:4) {
    cm <- utils::combn(length(items), k)
    for (r in seq_len(ncol(cm))) {
      expect_equal(.cp_unrank_combination(length(items), k, r), cm[, r])
    }
  }
  ranges <- .cp_build_batch_ranges(20, 6)
  expect_length(ranges, 4L)
  expect_equal(vapply(ranges, `[[`, numeric(1L), "rank_start"), c(1, 7, 13, 19))
  expect_equal(vapply(ranges, `[[`, numeric(1L), "rank_end"), c(6, 12, 18, 20))
  expect_equal(vapply(ranges, `[[`, numeric(1L), "expected_count"), c(6, 6, 6, 2))
})

test_that("tie-breaking across batch boundaries matches fresh run exactly", {
  set.seed(111)
  n <- 40L
  y <- rep(c(0L, 1L), length.out = n)
  base_col <- ifelse(y == 1L, 1.0, 0.0) + stats::rnorm(n, sd = 0.2)
  # Identical columns force exact metric ties across all combinations of size k
  dat <- data.frame(y = y, q1 = base_col, q2 = base_col, q3 = base_col, q4 = base_col, q5 = base_col)
  items <- paste0("q", 1:5)

  fresh <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    preselect_top_n = 4, outer_k = 3, inner_k = 2, seed = 77,
    engine = "Rcpp", parallel = "none", progress = FALSE, verbose = FALSE
  )

  cp_dir <- tempfile("ncvroc_fg_ties_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 2L

  batched <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    preselect_top_n = 4, outer_k = 3, inner_k = 2, seed = 77,
    engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
    progress = FALSE, verbose = FALSE
  )
  .fg_assert_parity(batched, fresh)
})

test_that("model_size partial resume and outer_fold promotion work in R serial", {
  dat <- .fg_make_test_data(n = 42L, p = 5L)
  items <- paste0("q", 1:5)
  cp_dir <- tempfile("ncvroc_fg_ms_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  fresh <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
    outer_k = 3, inner_k = 2, seed = 42, engine = "Rcpp", parallel = "none",
    progress = FALSE, verbose = FALSE
  )

  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$fail_during_model_size_write <- "r001_f001_k002"
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
      outer_k = 3, inner_k = 2, seed = 42, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "model_size",
      progress = FALSE, verbose = FALSE
    ),
    "Simulated failure during model_size checkpoint write"
  )

  .reset_ncvroc_checkpoint_test_seams()
  resumed <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
    outer_k = 3, inner_k = 2, seed = 42, engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, checkpoint_granularity = "model_size",
    progress = FALSE, verbose = FALSE
  )
  .fg_assert_parity(resumed, fresh)
  expect_equal(resumed$settings$checkpoint_provenance$fine_grained$model_size_reused, 1L)
  expect_equal(resumed$settings$checkpoint_provenance$fine_grained$model_size_computed, 8L)

  # Next run reuses promoted outer_fold checkpoints directly (0 lower units executed)
  .reset_ncvroc_checkpoint_test_seams()
  reused_outer <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
    outer_k = 3, inner_k = 2, seed = 42, engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, checkpoint_granularity = "model_size",
    progress = FALSE, verbose = FALSE
  )
  .fg_assert_parity(reused_outer, fresh)
  expect_equal(reused_outer$settings$checkpoint_provenance$n_reused, 3L)
  expect_equal(.ncvroc_checkpoint_test_seams$model_sizes_executed, 0L)
})

test_that("candidate_batch partial resume and Native parallel thread invariance work", {
  dat <- .fg_make_test_data(n = 48L, p = 6L)
  items <- paste0("q", 1:6)
  cp_dir <- tempfile("ncvroc_fg_cb_nat_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  fresh <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 6,
    outer_k = 3, inner_k = 2, seed = 99, engine = "Rcpp",
    parallel = "native_outer_fold", n_workers = 2L,
    progress = FALSE, verbose = FALSE
  )

  # Interrupt mid-batch during Native execution
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 7L
  .ncvroc_checkpoint_test_seams$fail_during_batch_write <- "r001_f001_k003_b8_14"
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 6,
      outer_k = 3, inner_k = 2, seed = 99, engine = "Rcpp",
      parallel = "native_outer_fold", n_workers = 2L,
      checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
      progress = FALSE, verbose = FALSE
    ),
    "Simulated failure during candidate_batch checkpoint write"
  )

  # Resume with 1 thread and verify saved batches are not recomputed
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 7L
  resumed <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 6,
    outer_k = 3, inner_k = 2, seed = 99, engine = "Rcpp",
    parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
    progress = FALSE, verbose = FALSE
  )
  .fg_assert_parity(resumed, fresh)
  fg <- resumed$settings$checkpoint_provenance$fine_grained
  expect_gt(fg$candidate_batch_reused, 0L)
  expect_gt(fg$model_size_reused, 0L)
  expect_equal(.ncvroc_checkpoint_test_seams$native_batches_executed, fg$candidate_batch_computed)
})

test_that("R <-> Native cross-backend portability works for candidate_batch and model_size", {
  dat <- .fg_make_test_data(n = 44L, p = 5L)
  items <- paste0("q", 1:5)
  cp_dir <- tempfile("ncvroc_fg_xback_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  fresh <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
    outer_k = 3, inner_k = 2, seed = 123, engine = "Rcpp", parallel = "none",
    progress = FALSE, verbose = FALSE
  )

  # Produce partial batches with R serial, fail on batch 2 of k=2
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 5L
  .ncvroc_checkpoint_test_seams$fail_during_batch_write <- "r001_f001_k002_b6_10"
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
      outer_k = 3, inner_k = 2, seed = 123, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
      progress = FALSE, verbose = FALSE
    ),
    "Simulated failure"
  )

  # Resume and complete with Native 2 threads
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 5L
  mixed <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3, preselect_top_n = 5,
    outer_k = 3, inner_k = 2, seed = 123, engine = "Rcpp",
    parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
    progress = FALSE, verbose = FALSE
  )
  .fg_assert_parity(mixed, fresh)
  expect_gt(mixed$settings$checkpoint_provenance$fine_grained$candidate_batch_reused, 0L)
})

test_that("tampering, dependency mismatch, repartitioning, and fault injection fail closed", {
  dat <- .fg_make_test_data(n = 40L, p = 5L)
  items <- paste0("q", 1:5)
  cp_dir <- tempfile("ncvroc_fg_fault_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 5L
  .ncvroc_checkpoint_test_seams$fail_during_batch_write <- "r001_f001_k002_b6_10"
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
      outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
      progress = FALSE, verbose = FALSE
    ),
    "Simulated failure during candidate_batch checkpoint write"
  )

  # Different batch_size on resume must fail closed (refuse silent repartitioning)
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 4L
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
      outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
      progress = FALSE, verbose = FALSE
    ),
    "silent repartitioning"
  )

  # Complete k=2 model_size promotion, but fail before outer_fold promotion
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 5L
  .ncvroc_checkpoint_test_seams$fail_before_outer_promotion <- TRUE
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
      outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
      progress = FALSE, verbose = FALSE
    ),
    "Simulated failure before outer_fold promotion"
  )

  # Clearing child_dependencies on a promoted model_size checkpoint must fail closed
  ms_file <- file.path(cp_dir, "units", "repeat_001_fold_001", "model_size_002.rds")
  expect_true(file.exists(ms_file))
  ms_env <- readRDS(ms_file)
  ms_orig <- ms_env
  ms_env$child_dependencies <- list()
  saveRDS(ms_env, ms_file, compress = FALSE)
  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
      outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "model_size",
      progress = FALSE, verbose = FALSE
    ),
    "dependency_hash mismatch"
  )
  saveRDS(ms_orig, ms_file, compress = FALSE)

  # Tamper child batch payload -> model_size dependency validation must fail closed
  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 5L
  b_file <- file.path(cp_dir, "units", "repeat_001_fold_001", "model_size_002",
                      "batch_000000000001_000000000005.rds")
  expect_true(file.exists(b_file))
  b_env <- readRDS(b_file)
  b_env$payload$top_k_candidates$auc[1] <- 0.123456
  saveRDS(b_env, b_file, compress = FALSE)

  expect_error(
    nested_sum_roc(
      dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
      outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
      checkpoint_dir = cp_dir, checkpoint_granularity = "model_size",
      progress = FALSE, verbose = FALSE
    ),
    "payload integrity hash mismatch"
  )

  # force_recompute = TRUE cleanly replaces tampered units
  recomp <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
    outer_k = 3, inner_k = 2, seed = 55, engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
    force_recompute = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_s3_class(recomp, "ncvroc_result")
})

test_that("seed = NULL RNG state is unaffected by fine-grained checkpointing", {
  dat <- .fg_make_test_data(n = 40L, p = 5L)
  items <- paste0("q", 1:5)
  cp_dir <- tempfile("ncvroc_fg_rng_")
  on.exit({
    .reset_ncvroc_checkpoint_test_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  set.seed(2026)
  fresh <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
    outer_k = 3, inner_k = 2, seed = NULL, engine = "Rcpp", parallel = "none",
    progress = FALSE, verbose = FALSE
  )
  rng_after_fresh <- .Random.seed

  .reset_ncvroc_checkpoint_test_seams()
  .ncvroc_checkpoint_test_seams$batch_size_override <- 4L
  set.seed(2026)
  cp_run <- nested_sum_roc(
    dat, "y", items, min_items = 1, max_items = 2, preselect_top_n = 4,
    outer_k = 3, inner_k = 2, seed = NULL, engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, checkpoint_granularity = "candidate_batch",
    progress = FALSE, verbose = FALSE
  )
  rng_after_cp <- .Random.seed

  .fg_assert_parity(cp_run, fresh)
  expect_identical(rng_after_cp, rng_after_fresh)
})
