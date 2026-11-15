# Opt-in diagnostics for native candidate-batch execution.

.ncvroc_native_diag_state <- new.env(parent = emptyenv())
.ncvroc_native_diag_state$enabled <- FALSE
.ncvroc_native_diag_state$log_dir <- NULL
.ncvroc_native_diag_state$run_id <- NULL
.ncvroc_native_diag_state$log_path <- NULL
.ncvroc_native_diag_state$run_counter <- 0L
.ncvroc_native_diag_state$reg_runner <- NULL

.ncvroc_native_diag_process_names <- c(
  "Rscript.exe", "Rterm.exe", "Rgui.exe", "rsession.exe"
)
.ncvroc_native_diag_wer_root <-
  "HKCU\\Software\\Microsoft\\Windows\\Windows Error Reporting\\LocalDumps"

.ncvroc_diag_escape <- function(x) {
  x <- if (length(x) == 0L || is.null(x) || is.na(x[[1L]])) "" else as.character(x[[1L]])
  gsub("[\t\r\n]+", " ", x, perl = TRUE)
}

.ncvroc_diag_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC", usetz = FALSE)
}

.ncvroc_diag_event <- function(event, backend = "native_streaming", repeat_id = NA_integer_,
                               outer_fold_id = NA_integer_, model_size = NA_integer_,
                               rank_start_1based = NA_real_, rank_end_1based = NA_real_,
                               batch_size = NA_real_, wave_size = NA_integer_,
                               num_threads = NA_integer_, n_rows = NA_integer_,
                               n_items = NA_integer_, task_count = NA_integer_, detail = "") {
  state <- .ncvroc_native_diag_state
  if (!isTRUE(state$enabled) || is.null(state$log_path) || is.null(state$run_id)) {
    return(invisible(FALSE))
  }
  fields <- c(
    .ncvroc_diag_timestamp(), state$run_id, Sys.getpid(), event, backend,
    repeat_id, outer_fold_id, model_size, rank_start_1based, rank_end_1based,
    batch_size, wave_size, num_threads, n_rows, n_items, task_count, detail
  )
  line <- paste(vapply(fields, .ncvroc_diag_escape, character(1L)), collapse = "\t")
  con <- file(state$log_path, open = "at", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(line, con, useBytes = TRUE)
  flush(con)
  invisible(TRUE)
}

.ncvroc_diag_start_run <- function(backend, n_rows, n_items, n_folds,
                                   candidate_count, num_threads, checkpoint_enabled) {
  state <- .ncvroc_native_diag_state
  if (!isTRUE(state$enabled)) return(invisible(NULL))
  state$run_counter <- as.integer(state$run_counter) + 1L
  stamp <- gsub("[^0-9]", "", .ncvroc_diag_timestamp())
  run_id <- sprintf("%s-p%d-%04d", stamp, Sys.getpid(), state$run_counter)
  log_path <- file.path(state$log_dir, paste0("native-diagnostic-", run_id, ".tsv"))
  con <- file(log_path, open = "wt", encoding = "UTF-8")
  writeLines(paste(c(
    "timestamp_utc", "run_id", "pid", "event", "backend", "repeat_id",
    "outer_fold_id", "model_size", "rank_start_1based", "rank_end_1based",
    "batch_size", "wave_size", "num_threads", "n_rows", "n_items",
    "task_count", "detail"
  ), collapse = "\t"), con, useBytes = TRUE)
  flush(con)
  close(con)
  state$run_id <- run_id
  state$log_path <- log_path
  version <- .cp_resolve_package_version("NCVROC")
  .ncvroc_diag_event(
    "run_start", backend = backend, num_threads = num_threads,
    n_rows = n_rows, n_items = n_items,
    detail = paste0(
      "package_version=", version,
      ";R_version=", getRversion(),
      ";platform=", R.version$platform,
      ";folds=", n_folds,
      ";candidate_count=", format(candidate_count, scientific = FALSE, trim = TRUE),
      ";checkpoint_enabled=", isTRUE(checkpoint_enabled)
    )
  )
  invisible(list(run_id = run_id, log_path = log_path))
}

.ncvroc_diag_end_run <- function(status = "ABORTED", detail = "") {
  state <- .ncvroc_native_diag_state
  on.exit({
    state$run_id <- NULL
    state$log_path <- NULL
  }, add = TRUE)
  if (isTRUE(state$enabled) && !is.null(state$run_id)) {
    .ncvroc_diag_event("run_end", detail = paste0("status=", status, if (nzchar(detail)) paste0(";", detail)))
  }
  invisible(NULL)
}

.ncvroc_diag_batch_call <- function(call, tasks, backend, num_threads, wave_size,
                                   n_rows, n_items, stage = "candidate_batch") {
  if (!is.function(call)) stop("`call` must be a function.", call. = FALSE)
  if (!is.data.frame(tasks) || !all(c("repeat_id", "outer_fold_id", "model_size",
                                      "rank_start_1based", "rank_end_1based") %in% names(tasks))) {
    stop("Invalid native diagnostic task metadata.", call. = FALSE)
  }
  emit <- function(event, row, detail = "") {
    .ncvroc_diag_event(
      event, backend = backend,
      repeat_id = row$repeat_id, outer_fold_id = row$outer_fold_id,
      model_size = row$model_size,
      rank_start_1based = row$rank_start_1based,
      rank_end_1based = row$rank_end_1based,
      batch_size = row$rank_end_1based - row$rank_start_1based + 1,
      wave_size = wave_size, num_threads = num_threads,
      n_rows = n_rows, n_items = n_items, task_count = nrow(tasks),
      detail = paste0("stage=", stage, if (nzchar(detail)) paste0(";", detail))
    )
  }
  for (i in seq_len(nrow(tasks))) emit("native_batch_start", tasks[i, , drop = FALSE])
  result <- tryCatch(call(), error = function(e) {
    error_class <- paste(class(e), collapse = ",")
    for (i in seq_len(nrow(tasks))) {
      emit("native_batch_r_error", tasks[i, , drop = FALSE],
           paste0("condition_class=", error_class))
    }
    stop(e)
  })
  for (i in seq_len(nrow(tasks))) emit("native_batch_returned", tasks[i, , drop = FALSE])
  result
}

.ncvroc_diag_reg_call <- function(args) {
  runner <- .ncvroc_native_diag_state$reg_runner
  if (is.function(runner)) {
    res <- runner(args)
    res$output <- enc2utf8(as.character(res$output))
    return(res)
  }
  out <- suppressWarnings(system2("reg.exe", shQuote(args, type = "cmd"),
                                  stdout = TRUE, stderr = TRUE))
  # Windows pipes can return UTF-8 bytes with Encoding="unknown". Mark them
  # explicitly before regex matching values such as a Japanese log path.
  if (length(out)) Encoding(out) <- "UTF-8"
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = as.character(out))
}

.ncvroc_diag_reg_value <- function(key, value_name) {
  res <- .ncvroc_diag_reg_call(c("query", key, "/v", value_name))
  if (!identical(res$status, 0L)) return(NULL)
  pattern <- paste0("^\\s*", value_name, "\\s+(REG_\\w+)\\s+(.+?)\\s*$")
  hits <- regmatches(res$output, regexec(pattern, res$output, ignore.case = TRUE, perl = TRUE))
  hits <- Filter(function(x) length(x) >= 2L, hits)
  if (!length(hits)) return(NULL)
  value_type <- toupper(hits[[1L]][[2L]])
  value <- trimws(hits[[1L]][[3L]])
  if (identical(value_type, "REG_DWORD") && grepl("^0x[0-9a-f]+$", value, ignore.case = TRUE)) {
    return(as.character(strtoi(substring(value, 3L), base = 16L)))
  }
  value
}

.ncvroc_diag_wer_key <- function(process_name) {
  paste0(.ncvroc_native_diag_wer_root, "\\", process_name)
}

.ncvroc_diag_reg_value_names <- function(key) {
  res <- .ncvroc_diag_reg_call(c("query", key))
  if (!identical(res$status, 0L)) {
    stop("Could not inspect Windows LocalDumps key: ", key,
         " (", paste(res$output, collapse = " "), ")", call. = FALSE)
  }
  pattern <- "^\\s*(.*?)\\s+REG_\\w+\\s+.*$"
  matches <- regmatches(res$output, regexec(pattern, res$output, perl = TRUE))
  names <- vapply(Filter(function(x) length(x) >= 2L, matches), `[[`,
                  character(1L), 2L)
  trimws(names)
}

.ncvroc_diag_configure_wer <- function(log_dir, dump_type, dump_count) {
  if (.Platform$OS.type != "windows") {
    stop("Windows crash dumps can only be configured on Windows.", call. = FALSE)
  }
  keys <- vapply(.ncvroc_native_diag_process_names, .ncvroc_diag_wer_key, character(1L))
  existing <- lapply(keys, function(key) {
    .ncvroc_diag_reg_call(c("query", key))
  })
  for (i in seq_along(keys)) {
    if (identical(existing[[i]]$status, 0L)) {
      managed <- identical(.ncvroc_diag_reg_value(keys[[i]], "NCVROCManaged"), "1")
      current_dir <- .ncvroc_diag_reg_value(keys[[i]], "NCVROCLogDir")
      if (!managed || !identical(tolower(gsub("/", "\\\\", current_dir %||% "")),
                                 tolower(gsub("/", "\\\\", log_dir)))) {
        stop("WER LocalDumps key already exists and is not managed for this diagnostics folder: ",
             keys[[i]], call. = FALSE)
      }
    }
  }
  dump_type_value <- if (identical(dump_type, "full")) "2" else "1"
  created <- character()
  for (i in seq_along(keys)) {
    key <- keys[[i]]
    if (identical(existing[[i]]$status, 0L)) next
    values <- list(
      c("NCVROCManaged", "REG_SZ", "1"),
      c("NCVROCLogDir", "REG_SZ", log_dir),
      c("NCVROCDumpType", "REG_SZ", dump_type_value),
      c("NCVROCDumpCount", "REG_SZ", as.character(dump_count)),
      c("DumpFolder", "REG_EXPAND_SZ", log_dir),
      c("DumpType", "REG_DWORD", dump_type_value),
      c("DumpCount", "REG_DWORD", as.character(dump_count))
    )
    failed <- NULL
    for (value in values) {
      res <- .ncvroc_diag_reg_call(c(
        "add", key, "/v", value[[1L]], "/t", value[[2L]], "/d", value[[3L]], "/f"
      ))
      if (!identical(res$status, 0L)) {
        failed <- paste(res$output, collapse = " ")
        break
      }
    }
    if (!is.null(failed)) {
      for (rollback_key in created) .ncvroc_diag_reg_call(c("delete", rollback_key, "/f"))
      .ncvroc_diag_reg_call(c("delete", key, "/f"))
      stop("Failed to configure Windows LocalDumps; changes made by this call were rolled back. ",
           failed, call. = FALSE)
    }
    created <- c(created, key)
  }
  invisible(keys)
}

#' Enable detailed diagnostics for Native Streaming executions
#'
#' Diagnostics write an append-only, data-free event log around each native
#' candidate batch. The log identifies the last batch entered if the R process
#' terminates inside native code. On Windows, LocalDumps can optionally be
#' configured for common R process executables. Minidumps are the default;
#' full dumps may contain analysis data in process memory.
#'
#' @param log_dir Directory in which per-run diagnostic logs and optional dumps
#'   will be written.
#' @param windows_crash_dumps Configure Windows Error Reporting LocalDumps for
#'   Rscript, Rterm, Rgui, and Positron/RStudio `rsession.exe` processes.
#'   Existing registry keys are never overwritten.
#' @param dump_type `"mini"` (default) or `"full"`. Full dumps can contain
#'   sensitive data held in process memory.
#' @param dump_count Maximum number of dumps Windows retains per executable.
#' @details For symbolized Windows stacks, install from source with
#'   `NCVROC_NATIVE_DEBUG=1` in the environment. The Windows Makevars then enables
#'   `-g -O0` and disables DLL stripping for this opt-in build. Do not pass
#'   `--strip` to `R CMD INSTALL`. Normal builds remain optimized and stripped.
#' @return Invisibly returns the configured log directory.
#' @export
ncvroc_enable_native_diagnostics <- function(log_dir,
                                             windows_crash_dumps = FALSE,
                                             dump_type = c("mini", "full"),
                                             dump_count = 5L) {
  if (!is.character(log_dir) || length(log_dir) != 1L || is.na(log_dir) || !nzchar(trimws(log_dir))) {
    stop("`log_dir` must be a non-empty path.", call. = FALSE)
  }
  if (!is.logical(windows_crash_dumps) || length(windows_crash_dumps) != 1L || is.na(windows_crash_dumps)) {
    stop("`windows_crash_dumps` must be TRUE or FALSE.", call. = FALSE)
  }
  dump_type <- match.arg(dump_type)
  if (!is.numeric(dump_count) || length(dump_count) != 1L || is.na(dump_count) ||
      !is.finite(dump_count) || dump_count < 1 || dump_count != as.integer(dump_count)) {
    stop("`dump_count` must be a positive integer.", call. = FALSE)
  }
  if (identical(dump_type, "full")) {
    warning("Full crash dumps may contain study data and other sensitive process memory.", call. = FALSE)
  }
  if (isTRUE(.ncvroc_native_diag_state$enabled)) {
    stop("Native diagnostics are already enabled in this R process.", call. = FALSE)
  }
  if (!dir.exists(log_dir) && !dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create Native diagnostics directory: ", log_dir, call. = FALSE)
  }
  resolved_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  if (isTRUE(windows_crash_dumps)) {
    .ncvroc_diag_configure_wer(resolved_dir, dump_type, as.integer(dump_count))
  }
  .ncvroc_native_diag_state$log_dir <- resolved_dir
  .ncvroc_native_diag_state$enabled <- TRUE
  invisible(resolved_dir)
}

#' Disable Native Streaming diagnostics in the current R process
#'
#' This disables diagnostic event logs. Windows LocalDumps settings, if
#' explicitly installed, remain active until
#' [ncvroc_disable_windows_crash_dumps()] is called.
#'
#' @return Invisibly returns `NULL`.
#' @export
ncvroc_disable_native_diagnostics <- function() {
  on.exit({
    .ncvroc_native_diag_state$enabled <- FALSE
    .ncvroc_native_diag_state$log_dir <- NULL
  }, add = TRUE)
  if (!is.null(.ncvroc_native_diag_state$run_id)) {
    .ncvroc_diag_end_run("DISABLED_BY_USER")
  }
  invisible(NULL)
}

#' Remove only Windows LocalDumps keys created by NCVROC diagnostics
#'
#' The function checks the NCVROC ownership marker before removing a key. It
#' never removes a pre-existing or unmarked LocalDumps configuration.
#'
#' @return Invisibly returns the names of removed process keys.
#' @export
ncvroc_disable_windows_crash_dumps <- function() {
  if (.Platform$OS.type != "windows") {
    stop("Windows crash dumps can only be changed on Windows.", call. = FALSE)
  }
  removed <- character()
  for (process_name in .ncvroc_native_diag_process_names) {
    key <- .ncvroc_diag_wer_key(process_name)
    owned <- identical(.ncvroc_diag_reg_value(key, "NCVROCManaged"), "1")
    log_dir <- .ncvroc_diag_reg_value(key, "NCVROCLogDir")
    expected_type <- .ncvroc_diag_reg_value(key, "NCVROCDumpType")
    expected_count <- .ncvroc_diag_reg_value(key, "NCVROCDumpCount")
    consistent <- owned && !is.null(log_dir) &&
      identical(.ncvroc_diag_reg_value(key, "DumpFolder"), log_dir) &&
      identical(.ncvroc_diag_reg_value(key, "DumpType"), expected_type) &&
      identical(.ncvroc_diag_reg_value(key, "DumpCount"), expected_count)
    if (!consistent) next
    value_names <- c("DumpFolder", "DumpType", "DumpCount", "NCVROCDumpType",
                     "NCVROCDumpCount", "NCVROCLogDir", "NCVROCManaged")
    for (value_name in value_names) {
      res <- .ncvroc_diag_reg_call(c("delete", key, "/v", value_name, "/f"))
      if (!identical(res$status, 0L)) {
        stop("Could not remove NCVROC-managed LocalDumps value '", value_name,
             "' from: ", key, call. = FALSE)
      }
    }
    remaining <- .ncvroc_diag_reg_value_names(key)
    if (!length(remaining)) {
      res <- .ncvroc_diag_reg_call(c("delete", key, "/f"))
      if (!identical(res$status, 0L)) {
        stop("Could not remove the empty NCVROC-managed LocalDumps key: ", key,
             call. = FALSE)
      }
    }
    removed <- c(removed, process_name)
  }
  invisible(removed)
}

#' Summarize the last recorded Native execution boundary
#'
#' @param log_file Path to a TSV file produced by
#'   [ncvroc_enable_native_diagnostics()], or a directory containing such logs.
#' @return A `ncvroc_native_diagnostics_report` list with run status, last event,
#'   any candidate batches with no terminal event, and batches with reported errors.
#' @export
ncvroc_native_diagnostics_report <- function(log_file) {
  if (!is.character(log_file) || length(log_file) != 1L || is.na(log_file) ||
      !file.exists(log_file)) {
    stop("`log_file` must name an existing Native diagnostics TSV file or diagnostics directory.", call. = FALSE)
  }
  if (dir.exists(log_file)) {
    candidates <- list.files(log_file, pattern = "^native-diagnostic-.*\\.tsv$", full.names = TRUE)
    if (!length(candidates)) stop("No Native diagnostics TSV files were found in: ", log_file, call. = FALSE)
    modified <- file.info(candidates)$mtime
    log_file <- candidates[[which.max(modified)]]
  }
  events <- utils::read.delim(log_file, check.names = FALSE, stringsAsFactors = FALSE,
                              colClasses = "character", fileEncoding = "UTF-8")
  required <- c("run_id", "event", "repeat_id", "outer_fold_id", "model_size",
                "rank_start_1based", "rank_end_1based", "detail")
  if (!all(required %in% names(events)) || nrow(events) < 1L) {
    stop("`log_file` is empty or does not match the NCVROC diagnostics schema.", call. = FALSE)
  }
  starts <- which(events$event == "native_batch_start")
  successful <- which(events$event == "native_batch_returned")
  failures <- which(events$event %in% c("native_batch_r_error", "native_batch_status_failure"))
  batch_key <- function(i) paste(events$run_id[i], events$repeat_id[i],
                                 events$outer_fold_id[i], events$model_size[i],
                                 events$rank_start_1based[i], events$rank_end_1based[i],
                                 sep = "|")
  successful_keys <- batch_key(successful)
  failure_keys <- batch_key(failures)
  terminal_keys <- c(successful_keys, failure_keys)
  pending <- starts[!(batch_key(starts) %in% terminal_keys)]
  # Every row selected above is already a reported failure; avoid rechecking
  # each key against the full failure set (quadratic for large diagnostic logs).
  failed <- failures
  last_idx <- nrow(events)
  end_rows <- which(events$event == "run_end")
  status <- if (length(end_rows)) {
    sub(".*status=([^;]+).*", "\\1", events$detail[[utils::tail(end_rows, 1L)]])
  } else {
    "PROCESS_TERMINATED_OR_RUN_INCOMPLETE"
  }
  report <- list(
    log_file = normalizePath(log_file, winslash = "/", mustWork = TRUE),
    run_id = events$run_id[[last_idx]],
    status = status,
    last_event = events[last_idx, , drop = FALSE],
    pending_batches = events[pending, , drop = FALSE],
    failed_batches = events[failed, , drop = FALSE],
    event_count = nrow(events)
  )
  class(report) <- "ncvroc_native_diagnostics_report"
  report
}

#' @export
print.ncvroc_native_diagnostics_report <- function(x, ...) {
  cat("NCVROC Native diagnostics\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Run: ", x$run_id, "\n", sep = "")
  cat("  Log: ", x$log_file, "\n", sep = "")
  cat("  Events: ", x$event_count, "\n", sep = "")
  if (nrow(x$pending_batches)) {
    cat("  Batch without a return record:\n")
    for (i in seq_len(nrow(x$pending_batches))) {
      b <- x$pending_batches[i, , drop = FALSE]
      cat(sprintf("    repeat %s, outer fold %s, size %s, candidate ranks %s-%s\n",
                  b$repeat_id, b$outer_fold_id, b$model_size,
                  b$rank_start_1based, b$rank_end_1based))
    }
  } else {
    cat("  No unfinished candidate batch is recorded.\n")
  }
  if (nrow(x$failed_batches)) {
    cat("  Candidate batch returned an error status:\n")
    for (i in seq_len(nrow(x$failed_batches))) {
      b <- x$failed_batches[i, , drop = FALSE]
      cat(sprintf("    repeat %s, outer fold %s, size %s, candidate ranks %s-%s (%s)\n",
                  b$repeat_id, b$outer_fold_id, b$model_size,
                  b$rank_start_1based, b$rank_end_1based, b$event))
    }
  }
  invisible(x)
}
