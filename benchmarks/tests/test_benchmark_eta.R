# =============================================================================
# Tests for Benchmark ETA Estimator & Runner Adapter (v0.23.1-R1)
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
})

source(file.path("..", "benchmark_eta.R"))
source(file.path("..", "benchmark_runner_v0231.R"))

test_that("format_duration_hms formats various durations accurately", {
  expect_identical(format_duration_hms(0), "0s")
  expect_identical(format_duration_hms(45), "45s")
  expect_identical(format_duration_hms(59), "59s")
  expect_identical(format_duration_hms(60), "1m 00s")
  expect_identical(format_duration_hms(192), "3m 12s")
  expect_identical(format_duration_hms(3600), "1h 00m 00s")
  expect_identical(format_duration_hms(5045), "1h 24m 05s")
  expect_identical(format_duration_hms(90000), "1d 01h 00m")
  expect_identical(format_duration_hms(NA), "N/A")
  expect_identical(format_duration_hms(Inf), "N/A")
  expect_identical(format_duration_hms(-10), "N/A")
})

test_that("Vectorized warmup classification correctly handles phase_code and rep_code", {
  df_mixed <- data.frame(
    call_index = 1:6,
    phase_code = c(1, 1, 1, 2, 2, 2),
    rep_code   = c(0, 0, 1, 0, 1, 2),
    stringsAsFactors = FALSE
  )
  is_w <- is_warmup_phase_row(df_mixed)
  expect_identical(is_w, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("Schedule cell identity: missing columns or duplicate warmup cell keys make ETA unavailable", {
  valid_sched <- data.frame(
    execution_order     = 1:4,
    execution_phase     = c(rep("warmup", 2), rep("measured", 2)),
    realization_id      = c(1, 2, 1, 2),
    backend             = rep("serial", 4),
    requested_resources = rep(1, 4),
    effective_resources = rep(1, 4),
    stringsAsFactors    = FALSE
  )

  # 1. Missing realization_id
  s_no_rid <- valid_sched[, setdiff(names(valid_sched), "realization_id")]
  eta_no_rid <- estimate_benchmark_eta(s_no_rid, data.frame())
  expect_false(eta_no_rid$available)
  expect_match(eta_no_rid$error_reason, "missing required cell identity column.*realization_id")

  # 2. Missing backend
  s_no_be <- valid_sched[, setdiff(names(valid_sched), "backend")]
  eta_no_be <- estimate_benchmark_eta(s_no_be, data.frame())
  expect_false(eta_no_be$available)
  expect_match(eta_no_be$error_reason, "missing required cell identity column.*backend")

  # 3. Missing requested_resources
  s_no_req <- valid_sched[, setdiff(names(valid_sched), "requested_resources")]
  eta_no_req <- estimate_benchmark_eta(s_no_req, data.frame())
  expect_false(eta_no_req$available)
  expect_match(eta_no_req$error_reason, "missing required cell identity column.*requested_resources")

  # 4. Missing effective_resources
  s_no_eff <- valid_sched[, setdiff(names(valid_sched), "effective_resources")]
  eta_no_eff <- estimate_benchmark_eta(s_no_eff, data.frame())
  expect_false(eta_no_eff$available)
  expect_match(eta_no_eff$error_reason, "missing required cell identity column.*effective_resources")

  # 5. NA in identity column
  s_na_be <- valid_sched
  s_na_be$backend[1] <- NA
  eta_na_be <- estimate_benchmark_eta(s_na_be, data.frame())
  expect_false(eta_na_be$available)
  expect_match(eta_na_be$error_reason, "contains missing, NA, or blank")

  # 6. Duplicate warmup cell keys in schedule
  s_dup_warmup <- data.frame(
    execution_order     = 1:4,
    execution_phase     = c("warmup", "warmup", "measured", "measured"),
    realization_id      = c(1, 1, 1, 1), # Duplicate warmup cell key!
    backend             = rep("serial", 4),
    requested_resources = rep(1, 4),
    effective_resources = rep(1, 4),
    stringsAsFactors    = FALSE
  )
  eta_dup_w <- estimate_benchmark_eta(s_dup_warmup, data.frame())
  expect_false(eta_dup_w$available)
  expect_match(eta_dup_w$error_reason, "Duplicate warmup cell key")
})

test_that("ID/status/elapsed-only completion records join to schedule by call ID and calibrate correctly", {
  sched <- data.frame(
    execution_order     = 1:6,
    execution_phase     = c(rep("warmup", 2), rep("measured", 4)),
    realization_id      = c(1, 2, 1, 2, 1, 2),
    backend             = rep("native_tbb", 6),
    requested_resources = rep(2, 6),
    effective_resources = rep(2, 6),
    stringsAsFactors    = FALSE
  )

  start_clock <- as.POSIXct("2026-10-07 12:00:00", tz = "UTC")
  current_clock <- as.POSIXct("2026-10-07 12:02:00", tz = "UTC")

  # Minimal completion dataframe: ONLY execution_order, elapsed_sec, status (no cell identity columns)
  comp_minimal <- data.frame(
    execution_order = 1:4,
    elapsed_sec     = c(10.0, 10.0, 8.0, 8.0),
    status          = c("ok", "ok", "ok", "ok"),
    stringsAsFactors = FALSE
  )

  eta <- estimate_benchmark_eta(sched, comp_minimal, start_time = start_clock, current_time = current_clock)

  expect_true(eta$available)
  expect_identical(eta$completed_calls, 4L)
  expect_identical(eta$remaining_calls, 2L)
  # Successfully joined to schedule and identified native_tbb_W2 stratum
  expect_identical(eta$strata_summary$stratum, "native_tbb_W2")
  expect_equal(eta$strata_summary$sample_count, 2L)
  expect_equal(eta$strata_summary$median_ratio, 0.8)
})

test_that("Strict completed-call identity validation rejects invalid IDs, duplicates, and out-of-schedule IDs", {
  sched <- data.frame(
    execution_order     = 1:4,
    execution_phase     = c(rep("warmup", 2), rep("measured", 2)),
    realization_id      = c(1, 2, 1, 2),
    backend             = rep("serial", 4),
    requested_resources = rep(1, 4),
    effective_resources = rep(1, 4),
    stringsAsFactors    = FALSE
  )

  # 1. Fractional call ID
  comp_frac <- data.frame(
    execution_order = c(1, 2.5),
    elapsed_sec     = c(1.0, 1.0),
    status          = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  eta_frac <- estimate_benchmark_eta(sched, comp_frac)
  expect_false(eta_frac$available)
  expect_match(eta_frac$error_reason, "finite integer-valued")

  # 2. NA call ID
  comp_na <- data.frame(
    execution_order = c(1, NA),
    elapsed_sec     = c(1.0, 1.0),
    status          = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  eta_na <- estimate_benchmark_eta(sched, comp_na)
  expect_false(eta_na$available)
  expect_match(eta_na$error_reason, "finite integer-valued")

  # 3. Out of schedule call ID
  comp_out <- data.frame(
    execution_order = c(1, 999),
    elapsed_sec     = c(1.0, 1.0),
    status          = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  eta_out <- estimate_benchmark_eta(sched, comp_out)
  expect_false(eta_out$available)
  expect_match(eta_out$error_reason, "not in schedule")

  # 4. Duplicate call ID
  comp_dup <- data.frame(
    execution_order = c(1, 1),
    elapsed_sec     = c(1.0, 1.0),
    status          = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  eta_dup <- estimate_benchmark_eta(sched, comp_dup)
  expect_false(eta_dup$available)
  expect_match(eta_dup$error_reason, "duplicate")

  # 5. Missing status column
  comp_no_status <- data.frame(
    execution_order = 1:2,
    elapsed_sec     = c(1.0, 1.0),
    stringsAsFactors = FALSE
  )
  eta_no_status <- estimate_benchmark_eta(sched, comp_no_status)
  expect_false(eta_no_status$available)
  expect_match(eta_no_status$error_reason, "missing required 'status'")

  # 6. Missing elapsed_sec column
  comp_no_elapsed <- data.frame(
    execution_order = 1:2,
    status          = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  eta_no_elapsed <- estimate_benchmark_eta(sched, comp_no_elapsed)
  expect_false(eta_no_elapsed$available)
  expect_match(eta_no_elapsed$error_reason, "missing required 'elapsed_sec'")
})

test_that("Fail-closed executor validation: missing status, missing elapsed, invalid timings, and ID mismatch", {
  sched <- data.frame(
    execution_order     = 1:2,
    execution_phase     = c("warmup", "measured"),
    realization_id      = c(1, 1),
    backend             = rep("serial", 2),
    requested_resources = rep(1, 2),
    effective_resources = rep(1, 2),
    stringsAsFactors    = FALSE
  )

  # 1. Executor returns elapsed_sec but omits status: converted to error, not COMPLETED
  mock_no_status <- function(row, call_id) {
    list(elapsed_sec = 5.0) # status omitted!
  }
  res1 <- run_benchmark_schedule_v0231(sched, mock_no_status, log_fn = NULL)
  expect_identical(res1$status, "PARTIAL_FAILURE")
  expect_identical(res1$completed_records$status[1], "error")
  expect_true(is.na(res1$completed_records$elapsed_sec[1]))
  expect_match(res1$completed_records$error[1], "omitted required 'status'")

  # 2. Executor returns status ok but omits elapsed_sec: converted to error
  mock_no_elapsed <- function(row, call_id) {
    list(status = "ok") # elapsed omitted!
  }
  res2 <- run_benchmark_schedule_v0231(sched, mock_no_elapsed, log_fn = NULL)
  expect_identical(res2$status, "PARTIAL_FAILURE")
  expect_identical(res2$completed_records$status[1], "error")
  expect_match(res2$completed_records$error[1], "missing, non-finite, or non-positive")

  # 3. Executor returns status ok with NA, Inf, zero, or negative elapsed
  mock_bad_elapsed <- function(row, call_id) {
    list(status = "ok", elapsed_sec = if (call_id == 1) -5.0 else Inf)
  }
  res3 <- run_benchmark_schedule_v0231(sched, mock_bad_elapsed, log_fn = NULL)
  expect_identical(res3$status, "PARTIAL_FAILURE")
  expect_identical(res3$completed_records$status[1], "error")
  expect_identical(res3$completed_records$status[2], "error")

  # 4. Executor returns fractional call ID (1.5) for scheduled ID 1
  mock_fractional_id <- function(row, call_id) {
    if (call_id == 1) {
      list(execution_order = 1.5, status = "ok", elapsed_sec = 1.0)
    } else {
      list(execution_order = 2, status = "ok", elapsed_sec = 2.0)
    }
  }
  res_frac <- run_benchmark_schedule_v0231(sched, mock_fractional_id, log_fn = NULL)
  expect_identical(res_frac$status, "PARTIAL_FAILURE")
  expect_identical(res_frac$completed_records$status[1], "error")
  expect_true(is.na(res_frac$completed_records$elapsed_sec[1]))
  expect_match(res_frac$completed_records$error[1], "invalid or mismatching call ID")
  expect_identical(res_frac$completed_records$execution_order[1], 1L)
  # Directly assert invalid call is excluded from successful ETA completion counts
  expect_identical(res_frac$final_eta$completed_calls, 1L)
  expect_identical(res_frac$completed_records$status[2], "ok")
  expect_equal(res_frac$completed_records$elapsed_sec[2], 2.0)

  # 5. Executor returns named execution_order = NULL
  mock_null_id <- function(row, call_id) {
    if (call_id == 1) {
      list(execution_order = NULL, status = "ok", elapsed_sec = 1.0)
    } else {
      list(execution_order = 2, status = "ok", elapsed_sec = 2.0)
    }
  }
  res_null <- run_benchmark_schedule_v0231(sched, mock_null_id, log_fn = NULL)
  expect_identical(res_null$status, "PARTIAL_FAILURE")
  expect_identical(res_null$completed_records$status[1], "error")
  expect_true(is.na(res_null$completed_records$elapsed_sec[1]))
  expect_match(res_null$completed_records$error[1], "invalid or mismatching call ID")
  expect_identical(res_null$final_eta$completed_calls, 1L)

  # 6. Executor returns named execution_order = numeric(0)
  mock_empty_id <- function(row, call_id) {
    if (call_id == 1) {
      list(execution_order = numeric(0), status = "ok", elapsed_sec = 1.0)
    } else {
      list(execution_order = 2, status = "ok", elapsed_sec = 2.0)
    }
  }
  res_empty <- run_benchmark_schedule_v0231(sched, mock_empty_id, log_fn = NULL)
  expect_identical(res_empty$status, "PARTIAL_FAILURE")
  expect_identical(res_empty$completed_records$status[1], "error")
  expect_true(is.na(res_empty$completed_records$elapsed_sec[1]))
  expect_match(res_empty$completed_records$error[1], "invalid or mismatching call ID")
  expect_identical(res_empty$final_eta$completed_calls, 1L)

  # 7. Executor returns non-finite / malformed call IDs (NA_real_, Inf, length-2 vector)
  mock_na_id <- function(row, call_id) {
    list(execution_order = NA_real_, status = "ok", elapsed_sec = 1.0)
  }
  res_na <- run_benchmark_schedule_v0231(sched, mock_na_id, log_fn = NULL)
  expect_identical(res_na$status, "PARTIAL_FAILURE")
  expect_identical(res_na$completed_records$status[1], "error")
  expect_true(is.na(res_na$completed_records$elapsed_sec[1]))
  expect_match(res_na$completed_records$error[1], "invalid or mismatching call ID")

  mock_inf_id <- function(row, call_id) {
    list(execution_order = Inf, status = "ok", elapsed_sec = 1.0)
  }
  res_inf <- run_benchmark_schedule_v0231(sched, mock_inf_id, log_fn = NULL)
  expect_identical(res_inf$status, "PARTIAL_FAILURE")
  expect_identical(res_inf$completed_records$status[1], "error")
  expect_true(is.na(res_inf$completed_records$elapsed_sec[1]))
  expect_match(res_inf$completed_records$error[1], "invalid or mismatching call ID")

  mock_vec_id <- function(row, call_id) {
    list(execution_order = c(1, 2), status = "ok", elapsed_sec = 1.0)
  }
  res_vec <- run_benchmark_schedule_v0231(sched, mock_vec_id, log_fn = NULL)
  expect_identical(res_vec$status, "PARTIAL_FAILURE")
  expect_identical(res_vec$completed_records$status[1], "error")
  expect_true(is.na(res_vec$completed_records$elapsed_sec[1]))
  expect_match(res_vec$completed_records$error[1], "invalid or mismatching call ID")

  # 8. Executor returns integer mismatched call ID (999 vs 1)
  mock_bad_id <- function(row, call_id) {
    list(execution_order = 999, status = "ok", elapsed_sec = 5.0)
  }
  res4 <- run_benchmark_schedule_v0231(sched, mock_bad_id, log_fn = NULL)
  expect_identical(res4$status, "PARTIAL_FAILURE")
  expect_identical(res4$completed_records$status[1], "error")
  expect_true(is.na(res4$completed_records$elapsed_sec[1]))
  expect_match(res4$completed_records$error[1], "invalid or mismatching call ID")

  # 9. Valid executor failure with elapsed_sec = NA is preserved
  mock_valid_fail <- function(row, call_id) {
    list(status = "error", elapsed_sec = NA_real_, error = "Computation failed.")
  }
  res5 <- run_benchmark_schedule_v0231(sched, mock_valid_fail, log_fn = NULL)
  expect_identical(res5$status, "PARTIAL_FAILURE")
  expect_identical(res5$completed_records$status[1], "error")
  expect_identical(res5$completed_records$error[1], "Computation failed.")

  # 10. Executor omits ID field entirely: accepted under runner-owned ID contract
  mock_omitted_id <- function(row, call_id) {
    list(status = "ok", elapsed_sec = 1.0)
  }
  res_omitted <- run_benchmark_schedule_v0231(sched, mock_omitted_id, log_fn = NULL)
  expect_identical(res_omitted$status, "COMPLETED")
  expect_identical(res_omitted$completed_records$status[1], "ok")
  expect_identical(res_omitted$completed_records$status[2], "ok")
  expect_identical(res_omitted$completed_records$execution_order, 1:2)
  expect_identical(res_omitted$final_eta$completed_calls, 2L)
})

test_that("Calibration hierarchy: stratum median at n >= 3, outlier robustness, pooled fallback", {
  sched <- data.frame(
    execution_order     = 1:6,
    execution_phase     = c("warmup", rep("measured", 5)),
    realization_id      = rep(1, 6),
    backend             = rep("native_tbb", 6),
    requested_resources = rep(4, 6),
    effective_resources = rep(4, 6),
    stringsAsFactors    = FALSE
  )

  start_clock <- as.POSIXct("2026-10-07 12:00:00", tz = "UTC")
  current_clock <- as.POSIXct("2026-10-07 12:05:00", tz = "UTC")

  # Warmup: 10.0s
  # Measured 1: 8.0s (0.8)
  # Measured 2: 8.2s (0.82)
  # Measured 3: 8.1s (0.81)
  # Measured 4 (outlier): 100.0s (10.0)
  comp <- data.frame(
    execution_order     = 1:5,
    elapsed_sec         = c(10.0, 8.0, 8.2, 8.1, 100.0),
    status              = rep("ok", 5),
    stringsAsFactors    = FALSE
  )

  eta <- estimate_benchmark_eta(sched, comp, start_time = start_clock, current_time = current_clock)

  expect_true(eta$available)
  expect_identical(eta$completed_calls, 5L)
  expect_identical(eta$remaining_calls, 1L)
  expect_identical(eta$calibration_basis, "stratum_median")
  expect_false(eta$is_provisional)

  expected_ratio <- median(c(0.8, 0.82, 0.81, 10.0))
  expect_equal(expected_ratio, 0.815)
  expect_equal(eta$remaining_sec, 10.0 * expected_ratio)
  expect_true(eta$remaining_sec < 10.0)
})

test_that("Zero remaining time and clean completion message on finish", {
  sched <- data.frame(
    execution_order     = 1:4,
    execution_phase     = c(rep("warmup", 2), rep("measured", 2)),
    realization_id      = c(1, 2, 1, 2),
    backend             = rep("serial", 4),
    requested_resources = rep(1, 4),
    effective_resources = rep(1, 4),
    stringsAsFactors    = FALSE
  )

  start_clock <- as.POSIXct("2026-10-07 12:00:00", tz = "UTC")
  current_clock <- as.POSIXct("2026-10-07 12:00:10", tz = "UTC")

  comp_all <- data.frame(
    execution_order = 1:4,
    elapsed_sec     = c(1.0, 2.0, 1.1, 2.1),
    status          = rep("ok", 4),
    stringsAsFactors = FALSE
  )

  eta_done <- estimate_benchmark_eta(sched, comp_all, start_time = start_clock, current_time = current_clock)
  expect_true(eta_done$available)
  expect_identical(eta_done$current_phase, "completed")
  expect_identical(eta_done$completed_calls, 4L)
  expect_identical(eta_done$remaining_calls, 0L)
  expect_identical(eta_done$percent_complete, 100.0)
  expect_equal(eta_done$remaining_sec, 0.0)
  expect_identical(eta_done$formatted_remaining, "0s")
  expect_identical(eta_done$formatted_finish, "N/A")
  expect_false(grepl("Estimated finish", eta_done$formatted_message))
  expect_match(eta_done$formatted_message, "Total Elapsed: 10s \\| Status: Completed")
})

test_that("Offline replay of Phase 4A schedule and raw timings produces accurate error table", {
  sched_path <- file.path("..", "results", "v023_phase4a_schedule.csv")
  raw_path   <- file.path("..", "results", "v023_phase4a_raw.csv")

  skip_if_not(file.exists(sched_path) && file.exists(raw_path), "Phase 4A artifacts not found")

  sched_df <- read.csv(sched_path, stringsAsFactors = FALSE)
  raw_df   <- read.csv(raw_path, stringsAsFactors = FALSE)

  expect_identical(nrow(sched_df), 2310L)
  expect_identical(nrow(raw_df), 2310L)

  n_warmup <- 385L
  n_measured_total <- 1925L
  clock_start <- as.POSIXct("2026-10-06 02:30:00", tz = "UTC")

  targets_pct <- c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 1.00)
  target_names <- c("Post-warmup (0%)", "5%", "10%", "25%", "50%", "75%", "100%")

  replay_records <- list()

  for (k in seq_along(targets_pct)) {
    pct <- targets_pct[k]
    m_target <- ceiling(pct * n_measured_total)
    call_idx <- n_warmup + m_target

    comp_subset <- raw_df[seq_len(call_idx), ]

    sim_clock <- clock_start + sum(comp_subset$elapsed_sec)
    eta <- estimate_benchmark_eta(
      schedule          = sched_df,
      completed_records = comp_subset,
      start_time        = clock_start,
      current_time      = sim_clock
    )

    actual_remaining <- if (call_idx < 2310L) {
      sum(raw_df$elapsed_sec[(call_idx + 1L):2310L])
    } else {
      0.0
    }

    est_remaining <- if (!is.na(eta$remaining_sec)) eta$remaining_sec else NA_real_
    signed_err <- if (!is.na(est_remaining)) est_remaining - actual_remaining else NA_real_
    pct_err <- if (!is.na(signed_err) && actual_remaining > 0) (signed_err / actual_remaining) * 100 else 0.0

    replay_records[[k]] <- data.frame(
      target_pct          = target_names[k],
      measured_completed  = m_target,
      total_completed     = call_idx,
      estimated_rem_sec   = round(est_remaining, 2),
      actual_rem_sec      = round(actual_remaining, 2),
      signed_error_sec    = round(signed_err, 2),
      error_pct           = round(pct_err, 2),
      formatted_estimated = eta$formatted_remaining,
      formatted_actual    = format_duration_hms(actual_remaining),
      calibration_basis   = eta$calibration_basis,
      is_provisional      = eta$is_provisional,
      stringsAsFactors    = FALSE
    )
  }

  replay_df <- do.call(rbind, replay_records)

  expect_identical(nrow(replay_df), 7L)
  expect_true(all(is.finite(replay_df$estimated_rem_sec)))
  expect_equal(replay_df$estimated_rem_sec[7], 0.0)
  expect_equal(replay_df$actual_rem_sec[7], 0.0)
  expect_true(all(diff(replay_df$estimated_rem_sec[-1]) <= 0))
  expect_true(all(diff(replay_df$actual_rem_sec) <= 0))

  cat("\n=== Phase 4A Offline ETA Replay Diagnostics Table ===\n")
  print(replay_df[, c("target_pct", "measured_completed", "total_completed",
                      "formatted_estimated", "formatted_actual",
                      "signed_error_sec", "error_pct", "calibration_basis", "is_provisional")])
})
