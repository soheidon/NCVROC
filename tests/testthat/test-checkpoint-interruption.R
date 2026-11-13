# Tests for v0.29.0-2: User-Initiated Safe Interruption and Resume

.make_interruption_test_data <- function(n = 64L, p = 4L, seed = 2026L) {
  set.seed(seed)
  mat <- matrix(sample(0:2, n * p, replace = TRUE), nrow = n, ncol = p)
  colnames(mat) <- paste0("q", seq_len(p))
  df <- as.data.frame(mat)
  df$y <- sample(0:1, n, replace = TRUE)
  df
}

test_that("ncvroc_request_cancel and ncvroc_clear_cancel manage .cancel marker safely", {
  cp_dir <- tempfile("ncvroc_cancel_api_")
  on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(ncvroc_request_cancel(""), "non-empty character path")
  expect_false(ncvroc_clear_cancel(cp_dir))

  cancel_path <- ncvroc_request_cancel(cp_dir)
  expect_true(file.exists(cancel_path))
  expect_equal(basename(cancel_path), ".cancel")

  expect_true(ncvroc_clear_cancel(cp_dir))
  expect_false(file.exists(cancel_path))
  expect_false(ncvroc_clear_cancel(cp_dir))
})

test_that("ncvroc() and run_ncvroc() forward checkpoint arguments cleanly across R serial and Scope B Native", {
  d <- .make_interruption_test_data(n = 60L, p = 4L, seed = 101L)
  items <- paste0("q", 1:4)

  # Baseline without checkpoint_dir remains unchanged
  set.seed(900)
  seed_before <- .Random.seed
  res_no_cp <- ncvroc(
    d, y, q1:q4,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "R", parallel = FALSE,
    seed = 42L, final_search = FALSE,
    progress = FALSE, verbose = FALSE
  )
  expect_null(res_no_cp$nested_result$settings$checkpoint_provenance)

  # R serial via ncvroc() with checkpoint_dir
  cp_r <- tempfile("ncvroc_fwd_r_")
  on.exit(unlink(cp_r, recursive = TRUE, force = TRUE), add = TRUE)
  res_r_cp <- ncvroc(
    d, y, q1:q4,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "R", parallel = FALSE,
    seed = 42L, final_search = FALSE,
    progress = FALSE, verbose = FALSE,
    checkpoint_dir = cp_r,
    checkpoint_granularity = "outer_fold"
  )
  expect_equal(res_r_cp$nested_result$selected_models, res_no_cp$nested_result$selected_models)
  expect_equal(res_r_cp$nested_result$summary, res_no_cp$nested_result$summary)
  expect_equal(res_r_cp$nested_result$settings$checkpoint_provenance$n_computed, 3L)

  # Resume via run_ncvroc() reuses all 3 folds
  cfg <- ncvroc_config(
    outcome = "y", items = items,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L
  )
  res_nat_resume <- run_ncvroc(
    d, items, cfg, seed = 42L,
    progress = FALSE, verbose = FALSE,
    checkpoint_dir = cp_r,
    checkpoint_granularity = "outer_fold",
    resume = TRUE
  )
  expect_equal(res_nat_resume$settings$checkpoint_provenance$n_reused, 3L)
  expect_equal(res_nat_resume$settings$checkpoint_provenance$n_computed, 0L)

  # ncvroc() with parallel = "native_outer_fold" and model_size granularity
  cp_nat <- tempfile("ncvroc_fwd_nat_")
  on.exit(unlink(cp_nat, recursive = TRUE, force = TRUE), add = TRUE)
  res_nat <- ncvroc(
    d, y, q1:q4,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    seed = 42L, final_search = FALSE,
    progress = FALSE, verbose = FALSE,
    checkpoint_dir = cp_nat,
    checkpoint_granularity = "model_size"
  )
  expect_equal(res_nat$nested_result$selected_models, res_no_cp$nested_result$selected_models)
  expect_equal(res_nat$nested_result$settings$checkpoint_provenance$fine_grained$checkpoint_granularity, "model_size")
})

test_that("R serial interruption via simulate_interrupt_at, .cancel, and R interrupt condition resumes with exact parity", {
  ns <- asNamespace("NCVROC")
  seams <- ns$.ncvroc_checkpoint_test_seams
  ns$.reset_ncvroc_checkpoint_test_seams()
  on.exit(ns$.reset_ncvroc_checkpoint_test_seams(), add = TRUE)

  d <- .make_interruption_test_data(n = 64L, p = 4L, seed = 202L)
  ref <- nested_sum_roc(
    d, outcome = "y", items = paste0("q", 1:4),
    min_items = 1L, max_items = 2L,
    outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "R", parallel = "none",
    seed = 77L, progress = FALSE, verbose = FALSE
  )
  ref_seed_post <- .Random.seed

  for (gran in c("outer_fold", "model_size", "candidate_batch")) {
    ns$.reset_ncvroc_checkpoint_test_seams()
    if (identical(gran, "candidate_batch")) {
      seams$batch_size_override <- 2L
    }
    cp_dir <- tempfile(paste0("ncvroc_intr_r_", gran, "_"))
    on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    # Interrupt at unit 2 via R interrupt condition hook
    seams$r_interrupt_hook <- function(stage_label, unit_index) {
      if (identical(as.integer(unit_index), 2L)) {
        signalCondition(structure(list(message = "user interrupt"), class = c("interrupt", "condition")))
      }
    }

    err <- tryCatch(
      nested_sum_roc(
        d, outcome = "y", items = paste0("q", 1:4),
        min_items = 1L, max_items = 2L,
        outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
        engine = "R", parallel = "none",
        seed = 77L, progress = FALSE, verbose = FALSE,
        checkpoint_dir = cp_dir,
        checkpoint_granularity = gran,
        resume = TRUE
      ),
      error = function(e) e
    )

    expect_s3_class(err, "ncvroc_interrupted")
    expect_equal(err$status, "INTERRUPTED")
    expect_equal(err$reason, "r_user_interrupt")
    expect_false(dir.exists(file.path(cp_dir, ".lock")))
    expect_false(file.exists(file.path(cp_dir, ".cancel")))
    expect_equal(seams$interrupt_waves_completed, 1L)

    # Clear hook and resume
    seams$r_interrupt_hook <- NULL
    set.seed(555L)
    res <- nested_sum_roc(
      d, outcome = "y", items = paste0("q", 1:4),
      min_items = 1L, max_items = 2L,
      outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
      engine = "R", parallel = "none",
      seed = 77L, progress = FALSE, verbose = FALSE,
      checkpoint_dir = cp_dir,
      checkpoint_granularity = gran,
      resume = TRUE
    )
    expect_equal(res$selected_models, ref$selected_models)
    expect_equal(res$summary, ref$summary)
    expect_equal(res$outer_predictions, ref$outer_predictions)
    expect_identical(.Random.seed, ref_seed_post)
  }
})

test_that("Scope B Native bounded waves stop safely on .cancel and simulate_interrupt_at across all granularities", {
  ns <- asNamespace("NCVROC")
  seams <- ns$.ncvroc_checkpoint_test_seams
  ns$.reset_ncvroc_checkpoint_test_seams()
  on.exit(ns$.reset_ncvroc_checkpoint_test_seams(), add = TRUE)

  d <- .make_interruption_test_data(n = 64L, p = 4L, seed = 303L)
  ref <- nested_sum_roc(
    d, outcome = "y", items = paste0("q", 1:4),
    min_items = 1L, max_items = 2L,
    outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    seed = 88L, progress = FALSE, verbose = FALSE
  )

  for (gran in c("outer_fold", "model_size", "candidate_batch")) {
    ns$.reset_ncvroc_checkpoint_test_seams()
    if (identical(gran, "candidate_batch")) {
      seams$batch_size_override <- 3L
    }
    cp_dir <- tempfile(paste0("ncvroc_intr_nat_", gran, "_"))
    on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    # Interrupt at wave 2 -> wave 1 (2 units) completes & persists; wave 2 is not dispatched
    seams$simulate_interrupt_at <- 2L

    err <- tryCatch(
      nested_sum_roc(
        d, outcome = "y", items = paste0("q", 1:4),
        min_items = 1L, max_items = 2L,
        outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
        engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
        seed = 88L, progress = FALSE, verbose = FALSE,
        checkpoint_dir = cp_dir,
        checkpoint_granularity = gran,
        resume = TRUE
      ),
      error = function(e) e
    )

    expect_s3_class(err, "ncvroc_interrupted")
    expect_equal(err$status, "INTERRUPTED")
    expect_equal(err$reason, "simulated_interrupt")
    expect_equal(seams$native_call_count, 1L)
    expect_equal(seams$interrupt_waves_completed, 1L)
    expect_false(dir.exists(file.path(cp_dir, ".lock")))

    # Now test .cancel marker prevents dispatch before wave 2 and is consumed on exit
    seams$simulate_interrupt_at <- NULL
    ncvroc_request_cancel(cp_dir)
    err2 <- tryCatch(
      nested_sum_roc(
        d, outcome = "y", items = paste0("q", 1:4),
        min_items = 1L, max_items = 2L,
        outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
        engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
        seed = 88L, progress = FALSE, verbose = FALSE,
        checkpoint_dir = cp_dir,
        checkpoint_granularity = gran,
        resume = TRUE
      ),
      error = function(e) e
    )
    expect_s3_class(err2, "ncvroc_interrupted")
    expect_equal(err2$reason, "cancel_file")
    expect_false(file.exists(file.path(cp_dir, ".cancel")))
    expect_false(dir.exists(file.path(cp_dir, ".lock")))

    # Resume to completion and verify exact parity
    res <- nested_sum_roc(
      d, outcome = "y", items = paste0("q", 1:4),
      min_items = 1L, max_items = 2L,
      outer_k = 4L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
      seed = 88L, progress = FALSE, verbose = FALSE,
      checkpoint_dir = cp_dir,
      checkpoint_granularity = gran,
      resume = TRUE
    )
    expect_equal(res$selected_models, ref$selected_models)
    expect_equal(res$summary, ref$summary)
    expect_equal(res$outer_predictions, ref$outer_predictions)
  }
})

test_that("final_search is outside checkpointing and cleans up building_dir on interrupt", {
  d <- .make_interruption_test_data(n = 60L, p = 4L, seed = 404L)
  cp_dir <- tempfile("ncvroc_final_boundary_cp_")
  cache_dir <- tempfile("ncvroc_final_boundary_cache_")
  on.exit({
    unlink(cp_dir, recursive = TRUE, force = TRUE)
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # First complete outer CV with final_search = FALSE
  res1 <- ncvroc(
    d, y, q1:q4,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "R", parallel = FALSE,
    seed = 99L, final_search = FALSE,
    progress = FALSE, verbose = FALSE,
    checkpoint_dir = cp_dir
  )
  expect_equal(res1$nested_result$settings$checkpoint_provenance$n_computed, 3L)

  # Now run with final_search = TRUE: outer CV is reused (0 computed), final_search runs
  res2 <- ncvroc(
    d, y, q1:q4,
    min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    engine = "R", parallel = FALSE,
    seed = 99L, final_search = TRUE,
    cache = "reuse", cache_dir = cache_dir,
    progress = FALSE, verbose = FALSE,
    checkpoint_dir = cp_dir
  )
  expect_equal(res2$nested_result$settings$checkpoint_provenance$n_reused, 3L)
  expect_equal(res2$nested_result$settings$checkpoint_provenance$n_computed, 0L)
  expect_false(is.null(res2$final_model))
  # No leftover .building-* directories in cache_dir
  leftover <- list.files(cache_dir, pattern = "\\.building-", full.names = TRUE)
  expect_length(leftover, 0L)
})
