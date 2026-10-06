# =============================================================================
# NCVROC Benchmark ETA Estimator & Formatting Helpers
#
# Observability helper for long-running benchmark schedules.
# Estimates remaining time using cell-specific warmup timings calibrated by
# completed measured runs.
#
# This file is for benchmark-harness observability only and is NOT part of the
# core R package runtime or package-level progress API.
# =============================================================================

#' Required cell identity columns in schedule
#' @noRd
REQUIRED_CELL_IDENTITY_COLS <- c(
  "realization_id",
  "backend",
  "requested_resources",
  "effective_resources"
)

#' Validate schedule cell identity columns strictly (fail-closed, no guessed keys)
#'
#' @param schedule data.frame
#' @return NULL on success, throws error on validation failure
#' @noRd
validate_schedule_cell_identity <- function(schedule) {
  missing_cols <- setdiff(REQUIRED_CELL_IDENTITY_COLS, names(schedule))
  if (length(missing_cols) > 0L) {
    stop(sprintf(
      "Schedule missing required cell identity column(s): %s (no default or guessed keys permitted).",
      paste(missing_cols, collapse = ", ")
    ))
  }

  for (col in REQUIRED_CELL_IDENTITY_COLS) {
    vals <- schedule[[col]]
    if (anyNA(vals) || any(!nzchar(trimws(as.character(vals))))) {
      stop(sprintf(
        "Schedule column '%s' contains missing, NA, or blank values.", col
      ))
    }
  }

  NULL
}

#' Extract unique timing cell key from validated schedule data frame
#'
#' @param df data.frame containing required schedule columns.
#' @return Character vector of cell keys.
#' @noRd
extract_schedule_cell_key <- function(df) {
  rid <- as.character(df$realization_id)
  backend <- as.character(df$backend)
  req_res <- as.character(df$requested_resources)
  eff_res <- as.character(df$effective_resources)

  cfg <- if ("base_config_id" %in% names(df)) {
    as.character(df$base_config_id)
  } else {
    rep("", nrow(df))
  }

  paste(rid, cfg, backend, req_res, eff_res, sep = "::")
}

#' Extract calibration stratum key from validated schedule data frame
#'
#' @param df data.frame containing required schedule columns.
#' @return Character vector of stratum keys (`backend_W<effective_resources>`).
#' @noRd
extract_schedule_stratum_key <- function(df) {
  backend <- as.character(df$backend)
  eff_res <- as.character(df$effective_resources)
  paste(backend, eff_res, sep = "_W")
}

#' Check if a schedule row belongs to the warmup phase (vectorized)
#'
#' @param df data.frame
#' @return Logical vector
#' @noRd
is_warmup_phase_row <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(logical(0))
  if ("execution_phase" %in% names(df)) {
    tolower(as.character(df$execution_phase)) %in% c("warmup", "warm_up")
  } else if ("phase_name" %in% names(df)) {
    tolower(as.character(df$phase_name)) %in% c("warmup", "warm_up")
  } else if ("phase_code" %in% names(df) && "rep_code" %in% names(df)) {
    as.integer(df$phase_code) == 1L & as.integer(df$rep_code) == 0L
  } else if ("repetition" %in% names(df)) {
    as.integer(df$repetition) == 0L
  } else {
    rep(FALSE, nrow(df))
  }
}

#' Format seconds into human-readable duration (e.g. 0s, 45s, 3m 12s, 1h 24m 05s, 2d 04h 12m)
#'
#' @param seconds Numeric scalar duration in seconds.
#' @return Character string.
#' @export
format_duration_hms <- function(seconds) {
  if (is.null(seconds) || length(seconds) == 0L || is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("N/A")
  }
  sec_int <- as.integer(round(seconds))
  if (sec_int < 60L) {
    return(sprintf("%ds", sec_int))
  }
  mins <- sec_int %/% 60L
  rem_sec <- sec_int %% 60L
  if (mins < 60L) {
    return(sprintf("%dm %02ds", mins, rem_sec))
  }
  hours <- mins %/% 60L
  rem_min <- mins %% 60L
  if (hours < 24L) {
    return(sprintf("%dh %02dm %02ds", hours, rem_min, rem_sec))
  }
  days <- hours %/% 24L
  rem_hour <- hours %% 24L
  return(sprintf("%dd %02dh %02dm", days, rem_hour, rem_min))
}

#' Estimate remaining time and finish time for a benchmark schedule
#'
#' Uses cell-specific warmup timings as baseline references, calibrated by the
#' median measured/warmup ratio within each `backend x effective_resources` stratum.
#'
#' @param schedule data.frame containing the full pre-registered schedule. Must contain
#'   `realization_id`, `backend`, `requested_resources`, `effective_resources`, and a call ID
#'   column (`execution_order` or `call_index`).
#' @param completed_records data.frame or list of completed call records.
#'   Must contain call identifiers matching `schedule`, `elapsed_sec`, and `status`.
#' @param start_time POSIXct timestamp for schedule start (defaults to `current_time`).
#' @param current_time POSIXct timestamp for current clock time (defaults to `Sys.time()`).
#' @return A list with class `"benchmark_eta"` containing:
#'   \item{available}{Logical, whether a valid ETA estimate could be computed.}
#'   \item{current_phase}{Character, `"warmup"`, `"measured"`, or `"completed"`.}
#'   \item{total_calls}{Integer, total scheduled calls.}
#'   \item{completed_calls}{Integer, successfully completed calls.}
#'   \item{remaining_calls}{Integer, uncompleted scheduled calls.}
#'   \item{percent_complete}{Numeric, 0 to 100.}
#'   \item{elapsed_wallclock_sec}{Numeric, wall-clock seconds from `start_time` to `current_time`.}
#'   \item{remaining_sec}{Numeric, estimated seconds remaining (or `NA_real_`).}
#'   \item{finish_time}{POSIXct, estimated completion time (or `as.POSIXct(NA)`).}
#'   \item{calibration_basis}{Character, `"none"`, `"warmup_baseline"`, `"stratum_median"`, or `"pooled_backend_median"`.}
#'   \item{is_provisional}{Logical, `TRUE` if using warmup baseline or pooled fallback.}
#'   \item{strata_summary}{data.frame of calibration strata and ratios.}
#'   \item{formatted_elapsed}{Character, formatted wall-clock elapsed time.}
#'   \item{formatted_remaining}{Character, formatted remaining duration.}
#'   \item{formatted_finish}{Character, formatted finish timestamp.}
#'   \item{formatted_message}{Character, one-line summary log message.}
#'   \item{error_reason}{Character or `NULL` if no error.}
#' @export
estimate_benchmark_eta <- function(schedule,
                                   completed_records,
                                   start_time   = NULL,
                                   current_time = Sys.time()) {
  res <- tryCatch({
    .compute_benchmark_eta_internal(
      schedule          = schedule,
      completed_records = completed_records,
      start_time        = start_time,
      current_time      = current_time
    )
  }, error = function(e) {
    list(
      available             = FALSE,
      current_phase         = "error",
      total_calls           = if (!is.null(schedule) && is.data.frame(schedule)) nrow(schedule) else 0L,
      completed_calls       = 0L,
      remaining_calls       = if (!is.null(schedule) && is.data.frame(schedule)) nrow(schedule) else 0L,
      percent_complete      = 0.0,
      elapsed_wallclock_sec = 0.0,
      remaining_sec         = NA_real_,
      finish_time           = as.POSIXct(NA),
      calibration_basis     = "none",
      is_provisional        = FALSE,
      strata_summary        = data.frame(),
      formatted_elapsed     = "N/A",
      formatted_remaining   = "N/A",
      formatted_finish      = "N/A",
      formatted_message     = paste("[ETA calculation unavailable:", e$message, "]"),
      error_reason          = e$message
    )
  })

  class(res) <- c("benchmark_eta", "list")
  res
}

#' Internal ETA calculation logic
#' @noRd
.compute_benchmark_eta_internal <- function(schedule,
                                            completed_records,
                                            start_time,
                                            current_time) {
  if (is.null(schedule) || !is.data.frame(schedule) || nrow(schedule) == 0L) {
    stop("Schedule must be a non-empty data.frame.")
  }

  # 1. Validate schedule cell identity strictly (no guessed keys)
  validate_schedule_cell_identity(schedule)

  total_calls <- nrow(schedule)

  # Match call ID column in schedule
  call_col <- if ("execution_order" %in% names(schedule)) {
    "execution_order"
  } else if ("call_index" %in% names(schedule)) {
    "call_index"
  } else {
    stop("Schedule is missing required call ID column ('execution_order' or 'call_index').")
  }

  sched_call_ids <- schedule[[call_col]]
  if (anyNA(sched_call_ids) || any(!is.finite(sched_call_ids)) || any(floor(sched_call_ids) != sched_call_ids)) {
    stop("Schedule call IDs must be finite integer-valued numbers.")
  }
  sched_call_ids <- as.integer(sched_call_ids)

  # Extract cell keys and stratum keys from schedule
  schedule_cell_keys <- extract_schedule_cell_key(schedule)
  schedule_stratum_keys <- extract_schedule_stratum_key(schedule)
  schedule_is_warmup <- is_warmup_phase_row(schedule)

  # Verify exactly one warmup call per cell key (reject duplicate warmup cell keys)
  warmup_indices <- which(schedule_is_warmup)
  warmup_keys <- schedule_cell_keys[warmup_indices]
  if (any(duplicated(warmup_keys))) {
    dup_keys <- unique(warmup_keys[duplicated(warmup_keys)])
    stop(sprintf(
      "Duplicate warmup cell key(s) detected in schedule: %s (each timing cell must have exactly one warmup reference).",
      paste(head(dup_keys, 3), collapse = ", ")
    ))
  }

  # Start time and wall-clock calculation
  if (is.null(start_time)) {
    start_time <- current_time
  }
  elapsed_wallclock_sec <- max(0.0, as.numeric(difftime(current_time, start_time, units = "secs")))
  formatted_elapsed <- format_duration_hms(elapsed_wallclock_sec)

  # Convert completed_records to data.frame if needed
  comp_df <- if (is.data.frame(completed_records)) {
    completed_records
  } else if (is.list(completed_records) && length(completed_records) > 0L) {
    if (is.list(completed_records[[1]])) {
      do.call(rbind, lapply(completed_records, as.data.frame, stringsAsFactors = FALSE))
    } else {
      as.data.frame(completed_records, stringsAsFactors = FALSE)
    }
  } else {
    data.frame()
  }

  # Validate and join completed records to schedule by call ID
  if (nrow(comp_df) > 0L) {
    if (!call_col %in% names(comp_df)) {
      stop(sprintf("Completion records missing required call ID column '%s'.", call_col))
    }
    if (!"elapsed_sec" %in% names(comp_df)) {
      stop("Completion records missing required 'elapsed_sec' column.")
    }
    if (!"status" %in% names(comp_df)) {
      stop("Completion records missing required 'status' column.")
    }

    raw_ids <- comp_df[[call_col]]
    if (anyNA(raw_ids) || any(!is.finite(raw_ids)) || any(floor(raw_ids) != raw_ids)) {
      stop("Completion record call IDs must be finite integer-valued numbers.")
    }
    comp_ids <- as.integer(raw_ids)

    # Check for IDs outside schedule
    if (any(!comp_ids %in% sched_call_ids)) {
      bad_ids <- comp_ids[!comp_ids %in% sched_call_ids]
      stop(sprintf("Completion records contain call ID(s) not in schedule: %s.", paste(head(bad_ids, 5), collapse = ", ")))
    }

    # Check for duplicate completion IDs
    if (any(duplicated(comp_ids))) {
      dup_ids <- comp_ids[duplicated(comp_ids)]
      stop(sprintf("Completion records contain duplicate call ID(s): %s.", paste(head(dup_ids, 5), collapse = ", ")))
    }

    # Filter strictly to successful completed records with positive elapsed time
    is_ok_status <- tolower(as.character(comp_df$status)) %in% c("ok", "success", "completed")
    has_valid_elapsed <- !is.na(comp_df$elapsed_sec) &
      is.finite(comp_df$elapsed_sec) &
      comp_df$elapsed_sec > 0

    valid_mask <- is_ok_status & has_valid_elapsed
    valid_comp_df <- comp_df[valid_mask, , drop = FALSE]
    valid_comp_ids <- comp_ids[valid_mask]
  } else {
    valid_comp_df <- data.frame()
    valid_comp_ids <- integer(0)
  }

  completed_calls <- nrow(valid_comp_df)
  remaining_calls <- max(0L, total_calls - completed_calls)
  percent_complete <- if (total_calls > 0L) (completed_calls / total_calls) * 100 else 0.0

  # Check if all calls are complete
  if (remaining_calls == 0L && completed_calls == total_calls) {
    msg <- sprintf(
      "[Progress 100.0%% | %d/%d calls] Total Elapsed: %s | Status: Completed",
      total_calls, total_calls, formatted_elapsed
    )
    return(list(
      available             = TRUE,
      current_phase         = "completed",
      total_calls           = total_calls,
      completed_calls       = completed_calls,
      remaining_calls       = 0L,
      percent_complete      = 100.0,
      elapsed_wallclock_sec = elapsed_wallclock_sec,
      remaining_sec         = 0.0,
      finish_time           = current_time,
      calibration_basis     = "completed",
      is_provisional        = FALSE,
      strata_summary        = data.frame(),
      formatted_elapsed     = formatted_elapsed,
      formatted_remaining   = "0s",
      formatted_finish      = "N/A",
      formatted_message     = msg,
      error_reason          = NULL
    ))
  }

  # Identify warmup calls and matching completion
  n_warmup_total <- length(warmup_indices)
  warmup_call_ids <- sched_call_ids[warmup_indices]

  completed_warmup_mask <- valid_comp_ids %in% warmup_call_ids
  completed_warmup_df <- valid_comp_df[completed_warmup_mask, , drop = FALSE]
  n_warmup_completed <- nrow(completed_warmup_df)

  # Phase 1: If warmup phase is not fully completed, do NOT display ETA
  if (n_warmup_completed < n_warmup_total) {
    warmup_pct <- if (n_warmup_total > 0L) (n_warmup_completed / n_warmup_total) * 100 else 0.0
    msg <- sprintf(
      "[Warmup %4.1f%% | %d/%d warmup calls, overall %d/%d (%.1f%%)] Elapsed: %s | ETA: unavailable during warmup",
      warmup_pct, n_warmup_completed, n_warmup_total,
      completed_calls, total_calls, percent_complete,
      formatted_elapsed
    )
    return(list(
      available             = FALSE,
      current_phase         = "warmup",
      total_calls           = total_calls,
      completed_calls       = completed_calls,
      remaining_calls       = remaining_calls,
      percent_complete      = percent_complete,
      elapsed_wallclock_sec = elapsed_wallclock_sec,
      remaining_sec         = NA_real_,
      finish_time           = as.POSIXct(NA),
      calibration_basis     = "none",
      is_provisional        = FALSE,
      strata_summary        = data.frame(),
      formatted_elapsed     = formatted_elapsed,
      formatted_remaining   = "N/A",
      formatted_finish      = "N/A",
      formatted_message     = msg,
      error_reason          = "Warmup phase in progress; cell reference timings incomplete."
    ))
  }

  # Phase 2: All warmups complete. Build lookup mapping cell_key -> warmup elapsed time
  warmup_row_order <- match(warmup_call_ids, valid_comp_df[[call_col]])
  warmup_times <- valid_comp_df$elapsed_sec[warmup_row_order]

  if (any(is.na(warmup_times)) || any(!is.finite(warmup_times)) || any(warmup_times <= 0)) {
    msg <- sprintf(
      "[Progress %4.1f%% | %d/%d calls] Elapsed: %s | ETA: unavailable (invalid warmup timings)",
      percent_complete, completed_calls, total_calls, formatted_elapsed
    )
    return(list(
      available             = FALSE,
      current_phase         = "measured",
      total_calls           = total_calls,
      completed_calls       = completed_calls,
      remaining_calls       = remaining_calls,
      percent_complete      = percent_complete,
      elapsed_wallclock_sec = elapsed_wallclock_sec,
      remaining_sec         = NA_real_,
      finish_time           = as.POSIXct(NA),
      calibration_basis     = "none",
      is_provisional        = FALSE,
      strata_summary        = data.frame(),
      formatted_elapsed     = formatted_elapsed,
      formatted_remaining   = "N/A",
      formatted_finish      = "N/A",
      formatted_message     = msg,
      error_reason          = "One or more warmup cells have missing or non-positive elapsed times."
    ))
  }

  cell_warmup_lookup <- setNames(warmup_times, warmup_keys)

  # Check completed measured calls (joined by call ID to schedule)
  completed_measured_mask <- !completed_warmup_mask
  completed_measured_df <- valid_comp_df[completed_measured_mask, , drop = FALSE]
  n_measured_completed <- nrow(completed_measured_df)

  # Join measured completion rows to schedule rows by call ID
  measured_comp_ids <- valid_comp_ids[completed_measured_mask]
  measured_sched_idx <- match(measured_comp_ids, sched_call_ids)

  completed_measured_cell_keys <- schedule_cell_keys[measured_sched_idx]
  completed_measured_stratum_keys <- schedule_stratum_keys[measured_sched_idx]
  completed_measured_backends <- as.character(schedule$backend[measured_sched_idx])

  matched_warmup_for_measured <- cell_warmup_lookup[completed_measured_cell_keys]
  valid_ratio_mask <- !is.na(matched_warmup_for_measured) &
    is.finite(matched_warmup_for_measured) &
    matched_warmup_for_measured > 0

  measured_ratios <- if (any(valid_ratio_mask)) {
    completed_measured_df$elapsed_sec[valid_ratio_mask] / matched_warmup_for_measured[valid_ratio_mask]
  } else {
    numeric(0)
  }

  valid_strata <- completed_measured_stratum_keys[valid_ratio_mask]
  valid_backends <- completed_measured_backends[valid_ratio_mask]

  stratum_medians <- if (length(measured_ratios) > 0L) {
    tapply(measured_ratios, valid_strata, median, na.rm = TRUE)
  } else {
    numeric(0)
  }
  stratum_counts <- if (length(measured_ratios) > 0L) {
    tapply(measured_ratios, valid_strata, length)
  } else {
    integer(0)
  }

  backend_medians <- if (length(measured_ratios) > 0L) {
    tapply(measured_ratios, valid_backends, median, na.rm = TRUE)
  } else {
    numeric(0)
  }
  backend_counts <- if (length(measured_ratios) > 0L) {
    tapply(measured_ratios, valid_backends, length)
  } else {
    integer(0)
  }

  strata_summary <- if (length(stratum_counts) > 0L) {
    data.frame(
      stratum      = names(stratum_counts),
      sample_count = as.integer(stratum_counts),
      median_ratio = as.numeric(stratum_medians[names(stratum_counts)]),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(stratum = character(0), sample_count = integer(0), median_ratio = numeric(0), stringsAsFactors = FALSE)
  }

  # Identify remaining uncompleted schedule rows
  uncompleted_mask <- !(sched_call_ids %in% valid_comp_ids)
  uncompleted_schedule <- schedule[uncompleted_mask, , drop = FALSE]
  uncompleted_cell_keys <- schedule_cell_keys[uncompleted_mask]
  uncompleted_stratum_keys <- schedule_stratum_keys[uncompleted_mask]
  uncompleted_backends <- as.character(uncompleted_schedule$backend)

  rem_warmup_times <- cell_warmup_lookup[uncompleted_cell_keys]

  if (any(is.na(rem_warmup_times)) || any(!is.finite(rem_warmup_times))) {
    missing_cells <- unique(uncompleted_cell_keys[is.na(rem_warmup_times)])
    msg <- sprintf(
      "[Progress %4.1f%% | %d/%d calls] Elapsed: %s | ETA: unavailable (missing warmup for %d cell(s))",
      percent_complete, completed_calls, total_calls,
      formatted_elapsed, length(missing_cells)
    )
    return(list(
      available             = FALSE,
      current_phase         = "measured",
      total_calls           = total_calls,
      completed_calls       = completed_calls,
      remaining_calls       = remaining_calls,
      percent_complete      = percent_complete,
      elapsed_wallclock_sec = elapsed_wallclock_sec,
      remaining_sec         = NA_real_,
      finish_time           = as.POSIXct(NA),
      calibration_basis     = "none",
      is_provisional        = FALSE,
      strata_summary        = strata_summary,
      formatted_elapsed     = formatted_elapsed,
      formatted_remaining   = "N/A",
      formatted_finish      = "N/A",
      formatted_message     = msg,
      error_reason          = sprintf("Missing warmup timing for %d cell(s).", length(missing_cells))
    ))
  }

  # Fixed calibration threshold: exactly 3
  MIN_SAMPLES <- 3L

  rem_ratios <- numeric(nrow(uncompleted_schedule))
  bases_used <- character(nrow(uncompleted_schedule))

  for (i in seq_len(nrow(uncompleted_schedule))) {
    st <- uncompleted_stratum_keys[i]
    be <- uncompleted_backends[i]

    if (!is.na(st) && st %in% names(stratum_counts) && stratum_counts[st] >= MIN_SAMPLES) {
      rem_ratios[i] <- stratum_medians[st]
      bases_used[i] <- "stratum_median"
    } else if (!is.na(be) && be %in% names(backend_counts) && backend_counts[be] >= MIN_SAMPLES) {
      rem_ratios[i] <- backend_medians[be]
      bases_used[i] <- "pooled_backend_median"
    } else {
      rem_ratios[i] <- 1.0
      bases_used[i] <- "warmup_baseline"
    }
  }

  rem_estimates <- rem_warmup_times * rem_ratios
  remaining_sec <- sum(rem_estimates)

  if (is.na(remaining_sec) || !is.finite(remaining_sec) || remaining_sec < 0) {
    remaining_sec <- NA_real_
    finish_time <- as.POSIXct(NA)
    is_avail <- FALSE
    basis_overall <- "none"
    is_prov <- FALSE
  } else {
    finish_time <- current_time + remaining_sec
    is_avail <- TRUE

    # Mark aggregate remaining-time estimate provisional if ANY remaining call uses pooled fallback or ratio 1
    if (all(bases_used == "stratum_median")) {
      basis_overall <- "stratum_median"
      is_prov <- FALSE
    } else if (all(bases_used == "warmup_baseline")) {
      basis_overall <- "warmup_baseline"
      is_prov <- TRUE
    } else if (all(bases_used %in% c("stratum_median", "pooled_backend_median"))) {
      basis_overall <- "pooled_backend_median"
      is_prov <- TRUE
    } else {
      basis_overall <- "partially_calibrated"
      is_prov <- TRUE
    }
  }

  formatted_rem <- format_duration_hms(remaining_sec)
  formatted_fin <- if (!is.na(finish_time)) format(finish_time, "%Y-%m-%d %H:%M:%S") else "N/A"

  basis_label <- if (!is_prov) {
    "stratum-calibrated"
  } else if (basis_overall == "warmup_baseline") {
    "warmup baseline, provisional"
  } else if (basis_overall == "pooled_backend_median") {
    "pooled-backend calibrated, provisional"
  } else {
    "warmup x measured calibration, provisional"
  }

  msg <- sprintf(
    "[Progress %4.1f%% | %d/%d calls] Elapsed: %s | Estimated remaining: %s (%s) | Estimated finish: %s",
    percent_complete, completed_calls, total_calls,
    formatted_elapsed, formatted_rem, basis_label, formatted_fin
  )

  list(
    available             = is_avail,
    current_phase         = "measured",
    total_calls           = total_calls,
    completed_calls       = completed_calls,
    remaining_calls       = remaining_calls,
    percent_complete      = percent_complete,
    elapsed_wallclock_sec = elapsed_wallclock_sec,
    remaining_sec         = remaining_sec,
    finish_time           = finish_time,
    calibration_basis     = basis_overall,
    is_provisional        = is_prov,
    strata_summary        = strata_summary,
    formatted_elapsed     = formatted_elapsed,
    formatted_remaining   = formatted_rem,
    formatted_finish      = formatted_fin,
    formatted_message     = msg,
    error_reason          = NULL
  )
}

#' S3 Print method for benchmark_eta objects
#'
#' @param x An object of class `"benchmark_eta"`.
#' @param ... Additional arguments (ignored).
#' @export
print.benchmark_eta <- function(x, ...) {
  cat(x$formatted_message, "\n")
  invisible(x)
}
