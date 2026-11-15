test_that("native diagnostics leave a durable start boundary and successful return boundary", {
  log_dir <- tempfile("ncvroc-native-diag-")
  on.exit(unlink(log_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ncvroc_enable_native_diagnostics(log_dir)
  on.exit(ncvroc_disable_native_diagnostics(), add = TRUE)
  NCVROC:::.ncvroc_diag_start_run(
    backend = "native_streaming", n_rows = 40L, n_items = 5L,
    n_folds = 2L, candidate_count = 15, num_threads = 2L,
    checkpoint_enabled = FALSE
  )
  tasks <- data.frame(
    repeat_id = 1L, outer_fold_id = 2L, model_size = 2L,
    rank_start_1based = 6, rank_end_1based = 10
  )
  result <- NCVROC:::.ncvroc_diag_batch_call(
    call = function() list(ok = TRUE), tasks = tasks,
    backend = "native_streaming", num_threads = 2L, wave_size = 1L,
    n_rows = 30L, n_items = 5L
  )
  expect_true(result$ok)
  log_file <- list.files(log_dir, pattern = "\\.tsv$", full.names = TRUE)
  expect_length(log_file, 1L)
  log <- read.delim(log_file, check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(log$event, c("run_start", "native_batch_start", "native_batch_returned"))
  expect_equal(log$rank_start_1based[2], 6)
  expect_equal(log$rank_end_1based[2], 10)
  expect_equal(log$batch_size[2], 5)
  expect_equal(log$num_threads[2], 2L)
  expect_false(any(grepl("subject|outcome|candidate_items", names(log), ignore.case = TRUE)))
  NCVROC:::.ncvroc_diag_end_run("COMPLETE")

  NCVROC:::.ncvroc_diag_start_run(
    backend = "native_streaming", n_rows = 40L, n_items = 5L,
    n_folds = 2L, candidate_count = 15, num_threads = 2L,
    checkpoint_enabled = FALSE
  )
  before_return <- function() {
    visible <- readLines(NCVROC:::.ncvroc_native_diag_state$log_path, warn = FALSE)
    expect_match(tail(visible, 1L), "native_batch_start")
    list(ok = TRUE)
  }
  NCVROC:::.ncvroc_diag_batch_call(
    call = before_return, tasks = tasks, backend = "native_streaming",
    num_threads = 2L, wave_size = 1L, n_rows = 30L, n_items = 5L
  )
  NCVROC:::.ncvroc_diag_end_run("COMPLETE")
})

test_that("native diagnostics leave an unfinished batch marker after an R error", {
  log_dir <- tempfile("ncvroc-native-diag-error-")
  on.exit(unlink(log_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ncvroc_enable_native_diagnostics(log_dir)
  on.exit(ncvroc_disable_native_diagnostics(), add = TRUE)
  NCVROC:::.ncvroc_diag_start_run(
    backend = "native_streaming", n_rows = 40L, n_items = 5L,
    n_folds = 2L, candidate_count = 15, num_threads = 1L,
    checkpoint_enabled = FALSE
  )
  tasks <- data.frame(
    repeat_id = 1L, outer_fold_id = 1L, model_size = 3L,
    rank_start_1based = 1, rank_end_1based = 4
  )
  expect_error(
    NCVROC:::.ncvroc_diag_batch_call(
      call = function() stop("injected ordinary R error"), tasks = tasks,
      backend = "native_streaming", num_threads = 1L, wave_size = 1L,
      n_rows = 30L, n_items = 5L
    ),
    "injected ordinary R error"
  )
  log_file <- list.files(log_dir, pattern = "\\.tsv$", full.names = TRUE)
  log <- read.delim(log_file, check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(log$event, c("run_start", "native_batch_start", "native_batch_r_error"))
  expect_equal(log$rank_start_1based[2], 1)
  expect_equal(log$rank_end_1based[2], 4)
  report <- ncvroc_native_diagnostics_report(log_dir)
  expect_equal(nrow(report$pending_batches), 0L)
  expect_equal(nrow(report$failed_batches), 1L)
  NCVROC:::.ncvroc_diag_end_run("ABORTED")
})

test_that("diagnostic report identifies a started batch without a terminal record", {
  log_dir <- tempfile("ncvroc-native-diag-report-")
  on.exit(unlink(log_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ncvroc_enable_native_diagnostics(log_dir)
  on.exit(ncvroc_disable_native_diagnostics(), add = TRUE)
  NCVROC:::.ncvroc_diag_start_run(
    backend = "native_streaming", n_rows = 40L, n_items = 5L,
    n_folds = 1L, candidate_count = 15, num_threads = 2L,
    checkpoint_enabled = TRUE
  )
  NCVROC:::.ncvroc_diag_event(
    "native_batch_start", backend = "native_streaming", repeat_id = 2L,
    outer_fold_id = 4L, model_size = 5L, rank_start_1based = 5001,
    rank_end_1based = 10000, batch_size = 5000, wave_size = 2L,
    num_threads = 2L, n_rows = 86L, n_items = 103L
  )
  log_file <- list.files(log_dir, pattern = "\\.tsv$", full.names = TRUE)
  report <- ncvroc_native_diagnostics_report(log_file)
  expect_s3_class(report, "ncvroc_native_diagnostics_report")
  expect_equal(report$status, "PROCESS_TERMINATED_OR_RUN_INCOMPLETE")
  expect_equal(nrow(report$pending_batches), 1L)
  expect_equal(report$pending_batches$repeat_id, "2")
  expect_equal(report$pending_batches$outer_fold_id, "4")
  expect_equal(report$pending_batches$model_size, "5")
  expect_equal(report$pending_batches$rank_start_1based, "5001")
  expect_equal(report$pending_batches$rank_end_1based, "10000")
  NCVROC:::.ncvroc_diag_end_run("TEST_INCOMPLETE")
})

test_that("diagnostic report matches large batches without quadratic rescanning", {
  log_file <- tempfile("ncvroc-native-diag-large-", fileext = ".tsv")
  on.exit(unlink(log_file), add = TRUE)

  n <- 5000L
  make_rows <- function(event, ids = NA_integer_, detail = "") {
    count <- length(event)
    if (length(ids) == 1L && is.na(ids)) ids <- rep(NA_integer_, count)
    data.frame(
      timestamp_utc = rep("2026-01-01T00:00:00Z", count),
      run_id = rep("large-run", count),
      pid = rep("123", count),
      event = event,
      backend = rep("native_streaming", count),
      repeat_id = rep("1", count),
      outer_fold_id = rep("1", count),
      model_size = rep("5", count),
      rank_start_1based = as.character(ids),
      rank_end_1based = as.character(ids),
      batch_size = rep("1", count),
      wave_size = rep("1", count),
      num_threads = rep("1", count),
      n_rows = rep("100", count),
      n_items = rep("103", count),
      task_count = rep("1", count),
      detail = rep(detail, count),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  events <- rbind(
    make_rows("run_start"),
    make_rows("native_batch_start", seq_len(n + 2L)),
    make_rows("native_batch_returned", seq_len(n)),
    make_rows("native_batch_r_error", n + 1L, "condition_class=error"),
    make_rows("run_end", detail = "status=COMPLETE")
  )
  utils::write.table(events, log_file, sep = "\t", quote = FALSE,
                     row.names = FALSE, na = "")

  report <- ncvroc_native_diagnostics_report(log_file)
  expect_equal(report$event_count, nrow(events))
  expect_equal(report$status, "COMPLETE")
  expect_equal(nrow(report$pending_batches), 1L)
  expect_equal(report$pending_batches$rank_start_1based, as.character(n + 2L))
  expect_equal(nrow(report$failed_batches), 1L)
  expect_equal(report$failed_batches$rank_start_1based, as.character(n + 1L))
})

test_that("diagnostics on and off preserve Native Streaming statistical output", {
  set.seed(2701)
  n <- 48L
  p <- 5L
  x <- matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p)
  dat <- as.data.frame(x)
  names(dat) <- paste0("q", seq_len(p))
  dat$y <- rep(c(0L, 1L), each = n / 2L)
  items <- paste0("q", seq_len(p))
  args <- list(
    data = dat, outcome = "y", items = items, min_items = 1L, max_items = 2L,
    preselect_top_n = 5L, outer_k = 3L, inner_k = 2L,
    outer_repeats = 1L, seed = 4401L, engine = "Rcpp",
    parallel = "native_streaming", n_workers = 2L,
    progress = FALSE, verbose = FALSE
  )
  ref <- do.call(nested_sum_roc, args)
  log_dir <- tempfile("ncvroc-native-diag-integration-")
  on.exit(unlink(log_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ncvroc_enable_native_diagnostics(log_dir)
  on.exit(ncvroc_disable_native_diagnostics(), add = TRUE)
  got <- do.call(nested_sum_roc, args)
  ncvroc_disable_native_diagnostics()
  expect_identical(got$summary, ref$summary)
  expect_identical(got$selected_models, ref$selected_models)
  expect_identical(got$outer_predictions, ref$outer_predictions)
  expect_true(file.exists(got$settings$native_diagnostics$log_path))
  log_file <- list.files(log_dir, pattern = "\\.tsv$", full.names = TRUE)
  expect_length(log_file, 1L)
  log <- read.delim(log_file, check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(sum(log$event == "native_batch_start"), sum(log$event == "native_batch_returned"))
  expect_equal(sum(log$event == "outer_fold_complete"), 3L)
  expect_identical(tail(log$event, 1L), "run_end")
  expect_match(tail(log$detail, 1L), "status=COMPLETE")
  expect_false(any(grepl("q[1-5]", paste(readLines(log_file), collapse = "\n"))))
})

test_that("Windows LocalDumps configuration is scoped to NCVROC-owned keys", {
  skip_if(.Platform$OS.type != "windows")
  registry <- new.env(parent = emptyenv())
  state <- NCVROC:::.ncvroc_native_diag_state
  old_runner <- state$reg_runner
  on.exit({
    state$reg_runner <- old_runner
    ncvroc_disable_native_diagnostics()
  }, add = TRUE)
  state$reg_runner <- function(args) {
    op <- args[[1L]]
    key <- args[[2L]]
    if (identical(op, "query")) {
      if (!exists(key, registry, inherits = FALSE)) return(list(status = 1L, output = "missing"))
      values <- get(key, registry, inherits = FALSE)
      if (length(args) >= 4L && identical(args[[3L]], "/v")) {
        name <- args[[4L]]
        if (is.null(values[[name]])) return(list(status = 1L, output = "missing value"))
        value <- values[[name]][[2L]]
        if (identical(values[[name]][[1L]], "REG_DWORD")) {
          value <- sprintf("0x%08X", as.integer(value))
        }
        return(list(status = 0L, output = paste("    ", name, values[[name]][[1L]], value)))
      }
      return(list(status = 0L, output = "key"))
    }
    if (identical(op, "add")) {
      name <- args[[4L]]
      type <- args[[6L]]
      value <- args[[8L]]
      vals <- if (exists(key, registry, inherits = FALSE)) get(key, registry) else list()
      vals[[name]] <- c(type, value)
      assign(key, vals, registry)
      return(list(status = 0L, output = "added"))
    }
    if (identical(op, "delete")) {
      if (exists(key, registry, inherits = FALSE)) {
        vals <- get(key, registry)
        if (length(args) >= 5L && identical(args[[3L]], "/v")) {
          vals[[args[[4L]]]] <- NULL
          assign(key, vals, registry)
        } else {
          rm(list = key, envir = registry)
        }
      }
      return(list(status = 0L, output = "deleted"))
    }
    list(status = 1L, output = "unknown operation")
  }
  log_dir <- file.path(tempfile("ncvroc-wer-test-"), "任意の保存先")
  on.exit(unlink(dirname(log_dir), recursive = TRUE, force = TRUE), add = TRUE)
  ncvroc_enable_native_diagnostics(log_dir, windows_crash_dumps = TRUE, dump_type = "mini")
  keys <- vapply(NCVROC:::.ncvroc_native_diag_process_names,
                 NCVROC:::.ncvroc_diag_wer_key, character(1L))
  expect_true(all(vapply(keys, exists, logical(1L), envir = registry, inherits = FALSE)))
  expect_true(all(vapply(keys, function(key) {
    identical(get(key, registry)$DumpType[[2L]], "1")
  }, logical(1L))))
  ncvroc_disable_native_diagnostics()
  removed <- ncvroc_disable_windows_crash_dumps()
  expect_setequal(removed, NCVROC:::.ncvroc_native_diag_process_names)
  expect_true(all(vapply(keys, function(key) {
    !exists(key, registry, inherits = FALSE)
  }, logical(1L))))
  ncvroc_enable_native_diagnostics(log_dir, windows_crash_dumps = TRUE, dump_type = "mini")
  expect_setequal(ncvroc_disable_windows_crash_dumps(), NCVROC:::.ncvroc_native_diag_process_names)
  expect_true(all(vapply(keys, function(key) {
    !exists(key, registry, inherits = FALSE)
  }, logical(1L))))
})
