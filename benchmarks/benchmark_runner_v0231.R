# =============================================================================
# NCVROC v0.23.1 Benchmark Runner Adapter with Integrated ETA Reporting
#
# Thin versioned harness orchestrator for future benchmark schedules.
# Integrates cell-calibrated ETA progress reporting without hardcoding
# production workflows.
# =============================================================================

if (!exists("estimate_benchmark_eta", mode = "function")) {
  eta_file <- if (file.exists("benchmarks/benchmark_eta.R")) {
    "benchmarks/benchmark_eta.R"
  } else if (file.exists("../benchmark_eta.R")) {
    "../benchmark_eta.R"
  } else {
    "benchmark_eta.R"
  }
  if (file.exists(eta_file)) {
    source(eta_file, local = FALSE)
  }
}

#' Run a scheduled benchmark workload with integrated ETA observability
#'
#' @param schedule data.frame containing the full benchmark schedule.
#' @param executor_fn Function accepting `(row, call_index)` and returning
#'   a list or data.frame row with explicit `status` and `elapsed_sec` fields.
#' @param clock_fn Function returning a POSIXct timestamp (defaults to `Sys.time`).
#' @param log_fn Function accepting a character message string for progress logging.
#' @param log_every Integer, logging frequency in calls (default 1L, log every call).
#' @return A list containing `completed_records` (data.frame), `final_eta`, and `status`.
#' @export
run_benchmark_schedule_v0231 <- function(schedule,
                                         executor_fn,
                                         clock_fn   = Sys.time,
                                         log_fn     = message,
                                         log_every  = 1L) {
  if (is.null(schedule) || !is.data.frame(schedule) || nrow(schedule) == 0L) {
    stop("Schedule must be a non-empty data.frame.")
  }
  if (!is.function(executor_fn)) {
    stop("executor_fn must be a valid function.")
  }
  if (!is.function(clock_fn)) {
    stop("clock_fn must be a valid function.")
  }

  call_col <- if ("execution_order" %in% names(schedule)) {
    "execution_order"
  } else if ("call_index" %in% names(schedule)) {
    "call_index"
  } else {
    stop("Schedule is missing required call ID column ('execution_order' or 'call_index').")
  }

  total_calls <- nrow(schedule)
  start_time <- clock_fn()
  completed_list <- vector("list", total_calls)

  bind_records <- function(records) {
    if (length(records) == 0L) return(data.frame())
    all_names <- unique(unlist(lapply(records, names)))
    standardized <- lapply(records, function(rec) {
      for (nm in all_names) {
        if (!nm %in% names(rec) || is.null(rec[[nm]])) {
          rec[[nm]] <- NA
        }
      }
      as.data.frame(rec[all_names], stringsAsFactors = FALSE)
    })
    do.call(rbind, standardized)
  }

  for (i in seq_len(total_calls)) {
    sched_row <- schedule[i, , drop = FALSE]
    call_id <- sched_row[[call_col]]

    # Execute the call via executor_fn (fail-closed contract)
    call_res <- tryCatch({
      res <- executor_fn(sched_row, call_id)
      if (!is.list(res)) {
        list(
          status      = "error",
          elapsed_sec = NA_real_,
          error       = "Invalid executor return: must return a list or data frame row."
        )
      } else {
        # Check required fields
        has_status <- "status" %in% names(res) && !is.null(res$status) && !is.na(res$status)
        has_elapsed <- "elapsed_sec" %in% names(res) && !is.null(res$elapsed_sec)

        if (!has_status) {
          # Never synthesize status = 'ok' when executor omits it
          list(
            status      = "error",
            elapsed_sec = NA_real_,
            error       = "Executor result omitted required 'status' field."
          )
        } else {
          # Check call ID if supplied by executor
          if (call_col %in% names(res)) {
            supplied_id <- res[[call_col]]
            is_valid_id <- !is.null(supplied_id) &&
              is.numeric(supplied_id) &&
              length(supplied_id) == 1L &&
              !is.na(supplied_id) &&
              is.finite(supplied_id) &&
              floor(supplied_id) == supplied_id &&
              supplied_id == call_id
            if (!is_valid_id) {
              res$status <- "error"
              res$elapsed_sec <- NA_real_
              disp_val <- if (is.null(supplied_id)) "NULL" else if (length(supplied_id) == 0L) "length 0" else paste(as.character(supplied_id), collapse = ", ")
              res$error <- sprintf("Executor returned invalid or mismatching call ID (%s) for scheduled ID (%s).",
                                   disp_val, as.character(call_id))
            }
          }

          st_lower <- tolower(as.character(res$status))
          is_success <- st_lower %in% c("ok", "success", "completed")

          if (is_success) {
            # Successful result requires finite, strictly positive elapsed_sec
            if (!has_elapsed || is.na(res$elapsed_sec) || !is.finite(res$elapsed_sec) || res$elapsed_sec <= 0) {
              res$status <- "error"
              res$elapsed_sec <- NA_real_
              res$error <- "Executor returned success status with missing, non-finite, or non-positive elapsed_sec."
            }
          } else {
            # Valid failure record may retain elapsed_sec = NA
            if (!"elapsed_sec" %in% names(res) || is.null(res$elapsed_sec)) {
              res$elapsed_sec <- NA_real_
            }
          }
          res
        }
      }
    }, error = function(e) {
      list(status = "error", elapsed_sec = NA_real_, error = e$message)
    })

    # Runner owns attaching the canonical scheduled call ID
    call_res[[call_col]] <- call_id
    completed_list[[i]] <- call_res

    # Estimate progress & ETA
    current_time <- clock_fn()
    comp_so_far <- bind_records(completed_list[seq_len(i)])

    eta <- estimate_benchmark_eta(
      schedule          = schedule,
      completed_records = comp_so_far,
      start_time        = start_time,
      current_time      = current_time
    )

    if (is.function(log_fn)) {
      if (i == 1L || i == total_calls || (i %% log_every == 0L)) {
        log_fn(eta$formatted_message)
      }
    }
  }

  final_df <- bind_records(completed_list)
  final_eta <- estimate_benchmark_eta(
    schedule          = schedule,
    completed_records = final_df,
    start_time        = start_time,
    current_time      = clock_fn()
  )

  all_succeeded <- nrow(final_df) == total_calls &&
    all(tolower(final_df$status) %in% c("ok", "success", "completed")) &&
    all(!is.na(final_df$elapsed_sec) & is.finite(final_df$elapsed_sec) & final_df$elapsed_sec > 0)

  list(
    completed_records = final_df,
    final_eta         = final_eta,
    total_calls       = total_calls,
    status            = if (all_succeeded) "COMPLETED" else "PARTIAL_FAILURE"
  )
}
