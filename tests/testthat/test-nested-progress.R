.capture_nested_progress <- function(expr) {
  captured <- character()
  value <- withCallingHandlers(
    force(expr),
    message = function(condition) {
      captured <<- c(captured, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = captured)
}

.nested_progress_fixture <- function() {
  set.seed(314L)
  y <- rep(c(0L, 1L), each = 18L)
  x <- matrix(stats::rnorm(36L * 4L), nrow = 36L, ncol = 4L)
  x[y == 1L, 1L] <- x[y == 1L, 1L] + 0.7
  data <- as.data.frame(x)
  names(data) <- paste0("q", seq_len(ncol(x)))
  data$y <- y
  data
}

.run_nested_progress_fixture <- function(data,
                                         parallel,
                                         progress = TRUE,
                                         checkpoint_dir = NULL,
                                         final_search = FALSE,
                                         verbose = FALSE,
                                         max_items = 2L,
                                         cache = "off",
                                         cache_dir = NULL) {
  if (final_search) {
    return(ncvroc(
      data = data,
      outcome = "y",
      items = paste0("q", 1:4),
      min_items = 1L,
      max_items = max_items,
      outer_k = 3L,
      inner_k = 2L,
      outer_repeats = 1L,
      seed = 314L,
      final_search = TRUE,
      ci = FALSE,
      engine = "Rcpp",
      parallel = parallel,
      n_workers = 2L,
      progress = progress,
      verbose = verbose,
      cache = cache,
      cache_dir = cache_dir,
      results_storage = "auto",
      checkpoint_dir = checkpoint_dir,
      checkpoint_granularity = "outer_fold",
      resume = TRUE
    ))
  }

  nested_sum_roc(
    data = data,
    outcome = "y",
    items = paste0("q", 1:4),
    min_items = 1L,
    max_items = max_items,
    preselect_top_n = 8L,
    outer_k = 3L,
    inner_k = 2L,
    outer_repeats = 1L,
    seed = 314L,
    engine = "Rcpp",
    parallel = parallel,
    n_workers = 2L,
    checkpoint_dir = checkpoint_dir,
    checkpoint_granularity = "outer_fold",
    resume = TRUE,
    progress = progress,
    verbose = verbose
  )
}

test_that("Native Outer Fold and Streaming report separate truthful progress stages", {
  data <- .nested_progress_fixture()
  reference <- .run_nested_progress_fixture(
    data, "native_outer_fold", progress = FALSE, max_items = 3L
  )

  native <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "native_outer_fold", progress = TRUE, max_items = 3L)
  )
  native_text <- paste(native$messages, collapse = "\n")
  expect_match(native_text, "Outer folds")
  expect_match(native_text, "3 / 3 complete")
  expect_match(native_text, "All outer folds complete\\.")
  expect_equal(native$value$summary, reference$summary, tolerance = 1e-12)

  streaming <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "native_streaming", progress = TRUE, max_items = 3L)
  )
  streaming_text <- paste(streaming$messages, collapse = "\n")
  expect_match(streaming_text, "Native Streaming candidates")
  expect_match(streaming_text, "42 / 42 evaluated")
  expect_match(streaming_text, "3 / 3 complete")
  expect_equal(streaming$value$summary, reference$summary, tolerance = 1e-12)

  withr::local_options(NCVROC.auto_streaming_threshold = 1L)
  automatic <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "auto", progress = TRUE, max_items = 3L)
  )
  expect_identical(automatic$value$settings$resolved_parallel, "native_streaming")
  expect_match(paste(automatic$messages, collapse = "\n"), "Native Streaming candidates")
  expect_equal(automatic$value$summary, reference$summary, tolerance = 1e-12)

  silent <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "native_streaming", progress = FALSE, max_items = 3L)
  )
  expect_length(silent$messages, 0L)
  expect_equal(silent$value$summary, reference$summary, tolerance = 1e-12)

  verbose_only <- .capture_nested_progress(
    .run_nested_progress_fixture(
      data, "native_streaming", progress = FALSE, verbose = TRUE
    )
  )
  expect_false(any(grepl("All outer folds complete|Native Streaming candidates|complete\\]", verbose_only$messages)))
})

test_that("legacy Nested CV backends report only completed outer folds", {
  data <- .nested_progress_fixture()
  reference <- .run_nested_progress_fixture(data, "none", progress = FALSE)

  for (backend in c("none", "threads", "outer", "chunks", "native_tbb")) {
    captured <- .capture_nested_progress(
      .run_nested_progress_fixture(data, backend, progress = TRUE)
    )
    output <- paste(captured$messages, collapse = "\n")
    expect_match(output, "3 / 3 complete", info = backend)
    expect_match(output, "All outer folds complete\\.", info = backend)
    expect_equal(captured$value$summary, reference$summary, tolerance = 1e-12, info = backend)
  }
})

test_that("checkpoint reuse and resume progress count only completed outer folds", {
  data <- .nested_progress_fixture()
  checkpoint_dir <- tempfile("ncvroc_progress_checkpoint_")
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)

  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  old_interrupt <- seams$simulate_interrupt_at
  on.exit({
    seams$simulate_interrupt_at <- old_interrupt
  }, add = TRUE)

  seams$simulate_interrupt_at <- 3L
  interrupted <- .capture_nested_progress(tryCatch(
    .run_nested_progress_fixture(
      data, "native_streaming", progress = TRUE,
      checkpoint_dir = checkpoint_dir
    ),
    error = function(e) e
  ))
  expect_s3_class(interrupted$value, "ncvroc_interrupted")
  interrupted_text <- paste(interrupted$messages, collapse = "\n")
  expect_match(interrupted_text, "1 / 3 complete")
  expect_false(any(grepl("3 / 3 complete|All outer folds complete|Analysis complete", interrupted$messages)))

  seams$simulate_interrupt_at <- NULL
  resumed <- .capture_nested_progress(
    .run_nested_progress_fixture(
      data, "native_streaming", progress = TRUE,
      checkpoint_dir = checkpoint_dir
    )
  )
  resumed_text <- paste(resumed$messages, collapse = "\n")
  expect_match(resumed_text, "Outer folds reused from checkpoint: 1/3")
  expect_match(resumed_text, "Outer folds computed this run: 2/3; reused: 1/3")
  expect_match(resumed_text, "3 / 3 complete")
  checkpoint_status_pos <- regexpr("Checkpoint:", resumed_text)[[1L]]
  outer_progress_pos <- regexpr("Outer folds", resumed_text)[[1L]]
  expect_gt(checkpoint_status_pos, 0L)
  expect_gt(outer_progress_pos, checkpoint_status_pos)
  expect_equal(resumed$value$settings$checkpoint_provenance$n_reused, 1L)
  expect_equal(resumed$value$settings$checkpoint_provenance$n_computed, 2L)

  fully_reused <- .capture_nested_progress(
    .run_nested_progress_fixture(
      data, "native_streaming", progress = TRUE,
      checkpoint_dir = checkpoint_dir
    )
  )
  fully_reused_text <- paste(fully_reused$messages, collapse = "\n")
  expect_match(fully_reused_text, "Outer folds reused from checkpoint: 3/3")
  expect_match(fully_reused_text, "Outer folds computed this run: 0/3; reused: 3/3")
  expect_match(fully_reused_text, "3 / 3 complete")
  expect_equal(fully_reused$value$settings$checkpoint_provenance$n_reused, 3L)
  expect_equal(fully_reused$value$settings$checkpoint_provenance$n_computed, 0L)

  analysis_checkpoint_dir <- tempfile("ncvroc_progress_analysis_interrupt_")
  on.exit(unlink(analysis_checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
  seams$simulate_interrupt_at <- 3L
  analysis_interrupted <- .capture_nested_progress(tryCatch(
    .run_nested_progress_fixture(
      data,
      "native_streaming",
      progress = TRUE,
      checkpoint_dir = analysis_checkpoint_dir,
      final_search = TRUE
    ),
    error = function(e) e
  ))
  expect_s3_class(analysis_interrupted$value, "ncvroc_interrupted")
  expect_false(any(grepl("Analysis complete\\.", analysis_interrupted$messages)))
})

test_that("checkpoint preflight failure does not render an outer-fold zero bar", {
  data <- .nested_progress_fixture()
  checkpoint_dir <- tempfile("ncvroc_invalid_checkpoint_preflight_")
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- NCVROC:::.cp_resolve_root(checkpoint_dir)
  dir.create(file.path(paths$pending_dir, "desktop.ini"))

  captured <- .capture_nested_progress(tryCatch(
    .run_nested_progress_fixture(
      data, "native_streaming", progress = TRUE,
      checkpoint_dir = checkpoint_dir
    ),
    error = function(e) e
  ))
  expect_s3_class(captured$value, "error")
  expect_match(conditionMessage(captured$value), "missing identity.rds")
  expect_false(any(grepl("Outer folds 0\\.0%|Outer folds .*0 / 3 complete", captured$messages)))
})

test_that("ncvroc reports final search separately and completion only after success", {
  data <- .nested_progress_fixture()
  result <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "native_outer_fold", progress = TRUE, final_search = TRUE)
  )
  result_text <- paste(result$messages, collapse = "\n")
  outer_pos <- regexpr("All outer folds complete\\.", result_text)[[1L]]
  analysis_pos <- regexpr("Analysis complete\\.", result_text)[[1L]]
  expect_gt(outer_pos, 0L)
  expect_gt(analysis_pos, outer_pos)
  expect_match(result_text, "candidates")
  expect_s3_class(result$value, "ncvroc_analysis")

  no_progress <- .capture_nested_progress(
    .run_nested_progress_fixture(data, "native_outer_fold", progress = FALSE, final_search = TRUE)
  )
  expect_length(no_progress$messages, 0L)
  expect_equal(no_progress$value$nested_cv_summary, result$value$nested_cv_summary, tolerance = 1e-12)

  cache_dir <- tempfile("ncvroc_progress_cache_")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  .run_nested_progress_fixture(
    data, "native_outer_fold", progress = FALSE, final_search = TRUE,
    cache = "reuse", cache_dir = cache_dir
  )
  cached <- .capture_nested_progress(
    .run_nested_progress_fixture(
      data, "native_outer_fold", progress = TRUE, final_search = TRUE,
      cache = "reuse", cache_dir = cache_dir
    )
  )
  cached_text <- paste(cached$messages, collapse = "\n")
  expect_true(cached$value$loaded_from_cache)
  expect_match(cached_text, "Analysis complete \\(loaded from cache\\)\\.")
  expect_false(grepl("Outer folds|Final search:", cached_text))

  cached_silent <- .capture_nested_progress(
    .run_nested_progress_fixture(
      data, "native_outer_fold", progress = FALSE, final_search = TRUE,
      cache = "reuse", cache_dir = cache_dir
    )
  )
  expect_length(cached_silent$messages, 0L)
})
