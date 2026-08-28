# progress.R -- Lightweight progress reporting and approximate ETA helpers

.PROGRESS_ETA_MIN_UNITS        <- 3L
.PROGRESS_ETA_CADENCE_SECS     <- 30.0   # seconds between ETA message lines and minimum initial delay
.PROGRESS_BAR_UPDATE_INTERVAL  <- 0.5    # seconds between txtProgressBar redraws

.progress_batch_size <- function(total_candidates, target_batches = 20L,
                                 minimum = 50000L, maximum = 200000L) {
  if (!is.numeric(total_candidates) || length(total_candidates) != 1L ||
      !is.finite(total_candidates) || total_candidates < 1) return(NA_integer_)
  as.integer(max(minimum, min(maximum, ceiling(total_candidates / target_batches))))
}

.progress_capability <- function(workflow, backend, enabled = TRUE,
                                 exact_candidates = FALSE,
                                 observed_unit = "candidate") {
  if (!isTRUE(enabled)) return(list(progress_mode = "disabled", progress_unit = "none"))
  key <- paste(workflow, backend, sep = "::")
  exact <- c("exhaustive_sum_roc::none", "exhaustive_sum_roc::threads")
  psock <- c("exhaustive_sum_roc::chunks", "cross_size_cv::chunks",
             "nested_sum_roc::outer", "nested_sum_roc::chunks", "nested_sum_roc::hybrid",
             "cross_size_nested_cv::outer", "cross_size_nested_cv::chunks", "cross_size_nested_cv::hybrid")
  if (key %in% exact ||
      (identical(workflow, "cross_size_cv") && isTRUE(exact_candidates) &&
       backend %in% c("none", "threads"))) {
    return(list(progress_mode = "exact", progress_unit = "candidate"))
  }
  if (key %in% psock) return(list(progress_mode = "start_completion", progress_unit = "none"))
  list(progress_mode = "eta", progress_unit = observed_unit)
}

#' Format an approximate remaining-time duration
#'
#' @param remaining_secs Numeric duration in seconds.
#' @return Formatted character string describing the approximate duration.
#' @keywords internal
#' @noRd
.format_eta <- function(remaining_secs) {
  if (is.na(remaining_secs) || !is.finite(remaining_secs) || remaining_secs < 0) {
    return("")
  }
  if (remaining_secs < 10) {
    return("< 10 sec")
  }
  if (remaining_secs < 60) {
    s <- round(remaining_secs / 5) * 5
    if (s == 60) return("~1m")
    return(sprintf("~%d sec", as.integer(s)))
  }
  if (remaining_secs < 3600) {
    m <- floor(remaining_secs / 60)
    s <- round((remaining_secs %% 60) / 5) * 5
    if (s == 60) {
      m <- m + 1
      s <- 0
    }
    return(if (s > 0) sprintf("~%dm %ds", as.integer(m), as.integer(s)) else sprintf("~%dm", as.integer(m)))
  }
  h <- floor(remaining_secs / 3600)
  m <- round((remaining_secs %% 3600) / 60)
  if (m == 60) {
    h <- h + 1
    m <- 0
  }
  if (m > 0) sprintf("~%dh %dm", as.integer(h), as.integer(m)) else sprintf("~%dh", as.integer(h))
}

#' Format an approximate ETA progress message line
#'
#' @param done Number of completed units.
#' @param total Total number of units.
#' @param elapsed_secs Elapsed time in seconds.
#' @return Formatted character message string, or empty string if not applicable.
#' @keywords internal
#' @noRd
.format_eta_message <- function(done, total, elapsed_secs) {
  if (done < .PROGRESS_ETA_MIN_UNITS || total <= 0L || done > total || elapsed_secs <= 0) {
    return("")
  }
  rate <- done / elapsed_secs
  if (rate <= 0 || !is.finite(rate)) {
    return("")
  }
  rem_secs <- (total - done) / rate
  eta_str <- .format_eta(rem_secs)
  if (!nzchar(eta_str)) {
    return("")
  }
  pct <- round((done / total) * 100)
  sprintf("  %s remaining  (%d%% complete, %d/%d units done)", eta_str, pct, done, total)
}

#' Create a progress reporting context with safe cleanup and approximate ETA
#'
#' @param total Integer total number of work units.
#' @param label Optional character label for the progress context.
#' @param enabled Logical indicating whether progress reporting is active.
#' @return A list with functions: `tick(n)`, `eta_message()`, `finish()`, and `close()`.
#' @keywords internal
#' @noRd
.progress_make <- function(total, label = "", enabled = TRUE,
                           progress_mode = c("bar", "exact")) {
  progress_mode <- match.arg(progress_mode)
  if (!isTRUE(enabled) || total <= 0L) {
    return(list(
      mode        = "disabled",
      tick        = function(n = 1L) invisible(NULL),
      eta_message = function() invisible(NULL),
      finish      = function() invisible(NULL),
      close       = function() invisible(NULL)
    ))
  }

  pb <- if (identical(progress_mode, "bar")) utils::txtProgressBar(min = 0, max = total, style = 3) else NULL
  t0 <- proc.time()[["elapsed"]]
  done <- 0L
  last_bar_update_t <- -Inf
  last_eta_t <- t0                                  # first ETA requires >= 30s from t0
  closed <- FALSE                                   # idempotency flag

  list(
    mode = if (identical(progress_mode, "exact")) "exact" else "eta",
    tick = function(n = 1L) {
      done <<- min(done + n, total)                 # clamp to total
      now <- proc.time()[["elapsed"]]
      if (identical(progress_mode, "exact") && now - last_bar_update_t >= .PROGRESS_BAR_UPDATE_INTERVAL) {
        elapsed <- now - t0
        eta <- if (done >= .PROGRESS_ETA_MIN_UNITS && elapsed >= .PROGRESS_ETA_CADENCE_SECS) {
          .format_eta_message(done, total, elapsed)
        } else ""
        message(sprintf("%s%s / %s complete (%.1f%%)%s",
                        if (nzchar(label)) paste0(label, ": ") else "",
                        format(done, big.mark = ","), format(total, big.mark = ","),
                        100 * done / total, if (nzchar(eta)) paste0(" | Approx. remaining: ", sub("^  ", "", eta)) else ""))
        last_bar_update_t <<- now
      } else if (identical(progress_mode, "bar") && now - last_bar_update_t >= .PROGRESS_BAR_UPDATE_INTERVAL) {
        utils::setTxtProgressBar(pb, done)
        last_bar_update_t <<- now
      }
    },

    eta_message = function() {
      now <- proc.time()[["elapsed"]]
      elapsed <- now - t0
      if (done < .PROGRESS_ETA_MIN_UNITS) return(invisible(NULL))
      if (elapsed < .PROGRESS_ETA_CADENCE_SECS) return(invisible(NULL))
      if (now - last_eta_t < .PROGRESS_ETA_CADENCE_SECS) return(invisible(NULL))
      msg <- .format_eta_message(done, total, elapsed)
      if (nzchar(msg) && identical(progress_mode, "bar")) message(msg)
      last_eta_t <<- now
    },

    finish = function() {
      if (isTRUE(closed)) return(invisible(NULL))   # idempotent
      done <<- total
      if (!is.null(pb)) utils::setTxtProgressBar(pb, total)
      closed <<- TRUE
      if (!is.null(pb)) close(pb)
    },

    close = function() {
      if (isTRUE(closed)) return(invisible(NULL))   # idempotent
      closed <<- TRUE
      if (!is.null(pb)) close(pb)
    }
  )
}
