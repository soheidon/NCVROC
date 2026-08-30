# progress.R -- Lightweight progress reporting and approximate ETA helpers

.PROGRESS_ETA_MIN_UNITS              <- 3L
.PROGRESS_ETA_CADENCE_SECS           <- 30.0   # seconds between ETA message lines and minimum initial delay
.PROGRESS_BAR_UPDATE_INTERVAL        <- 0.5    # seconds between txtProgressBar redraws
.PROGRESS_RENDER_MIN_PCT_STEP        <- 1.0    # minimum percentage point increase between non-interactive lines
.PROGRESS_RENDER_MIN_INTERVAL_SECS   <- 5.0    # minimum seconds between non-interactive lines
.PROGRESS_INTERACTIVE_INTERVAL_SECS  <- 0.2    # minimum seconds between interactive in-place refreshes

.progress_batch_size <- function(total_candidates, target_batches = 20L,
                                 minimum = 2500L, maximum = 200000L) {
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
#' @param done Number of completed units (numeric/double).
#' @param total Total number of units (numeric/double).
#' @param elapsed_secs Elapsed time in seconds.
#' @return Formatted character message string, or empty string if not applicable.
#' @keywords internal
#' @noRd
.format_eta_message <- function(done, total, elapsed_secs) {
  done_num <- as.numeric(done)
  total_num <- as.numeric(total)
  if (done_num < .PROGRESS_ETA_MIN_UNITS || total_num <= 0 || done_num > total_num || elapsed_secs <= 0) {
    return("")
  }
  rate <- done_num / elapsed_secs
  if (rate <= 0 || !is.finite(rate)) {
    return("")
  }
  rem_secs <- (total_num - done_num) / rate
  eta_str <- .format_eta(rem_secs)
  if (!nzchar(eta_str)) {
    return("")
  }
  pct <- round((done_num / total_num) * 100)
  sprintf("  %s remaining  (%d%% complete, %s/%s units done)", eta_str, as.integer(pct),
          format(done_num, scientific = FALSE, big.mark = ","),
          format(total_num, scientific = FALSE, big.mark = ","))
}

#' Render visual progress gauge
#' @keywords internal
#' @noRd
.render_gauge <- function(done, total, width = 20L) {
  total_num <- as.numeric(total)
  if (!is.numeric(total_num) || !is.finite(total_num) || total_num <= 0) {
    return(paste0("[", paste0(rep("-", width), collapse = ""), "]"))
  }
  ratio <- as.numeric(done) / total_num
  filled <- max(0L, min(as.integer(width), as.integer(round(ratio * as.numeric(width)))))
  empty <- max(0L, as.integer(width) - filled)
  paste0("[", paste0(rep("#", filled), collapse = ""), paste0(rep("-", empty), collapse = ""), "]")
}

#' Format progress line with optional gauge
#' @keywords internal
#' @noRd
.format_progress_line <- function(done, total, elapsed, label = "", include_gauge = FALSE, unit = "complete") {
  total_num <- as.numeric(total)
  done_num <- as.numeric(done)
  pct <- if (total_num > 0) 100 * done_num / total_num else 0.0
  lbl_prefix <- if (nzchar(label)) paste0(label, " ") else ""
  gauge_str <- if (isTRUE(include_gauge)) paste0(.render_gauge(done_num, total_num), " ") else ""
  pct_str <- sprintf("%.1f%%", pct)
  count_str <- sprintf("(%s / %s %s)", format(done_num, scientific = FALSE, big.mark = ","),
                       format(total_num, scientific = FALSE, big.mark = ","), unit)
  elapsed_val <- if (is.numeric(elapsed) && is.finite(elapsed) && elapsed >= 0) elapsed else 0.0
  elapsed_str <- sprintf("[Elapsed: %.1fs]", elapsed_val)

  sprintf("%s%s%s %s %s", lbl_prefix, gauge_str, pct_str, count_str, elapsed_str)
}

#' Create a progress reporting context with safe cleanup, throttling, and approximate ETA
#'
#' @param total Numeric total number of work units (integer-valued double).
#' @param label Optional character label for the progress context.
#' @param enabled Logical indicating whether progress reporting is active.
#' @param progress_mode Mode of reporting ("exact" or "bar").
#' @param unit Unit label string for the count display (e.g. "complete", "candidates").
#' @param interactive_override Internal test hook to force interactive/non-interactive rendering.
#' @return A list with functions: `tick(n)`, `eta_message()`, `finish()`, and `close()`.
#' @keywords internal
#' @noRd
.progress_make <- function(total, label = "", enabled = TRUE,
                           progress_mode = c("bar", "exact"),
                           unit = "complete",
                           interactive_override = NULL) {
  progress_mode <- match.arg(progress_mode)
  total_num <- as.numeric(total)
  if (!isTRUE(enabled) || !is.numeric(total_num) || !is.finite(total_num) || total_num <= 0) {
    return(list(
      mode        = "disabled",
      tick        = function(n = 1) invisible(NULL),
      eta_message = function() invisible(NULL),
      finish      = function() invisible(NULL),
      close       = function() invisible(NULL)
    ))
  }

  is_interactive <- if (!is.null(interactive_override)) {
    isTRUE(interactive_override)
  } else {
    interactive() && !isTRUE(getOption("knitr.in.progress"))
  }

  # txtProgressBar supports up to .Machine$integer.max; clamp if needed for style 3
  bar_max <- if (total_num <= .Machine$integer.max) as.integer(total_num) else .Machine$integer.max
  pb <- if (identical(progress_mode, "bar") && !is_interactive) utils::txtProgressBar(min = 0, max = bar_max, style = 3) else NULL

  t0 <- proc.time()[["elapsed"]]
  done <- 0.0                                       # double to prevent integer overflow on > 2.147B combos
  last_rendered_t <- -Inf
  last_rendered_pct <- -Inf
  last_eta_t <- t0
  rendered_100 <- FALSE
  closed <- FALSE

  render_update <- function(force_final = FALSE) {
    now <- proc.time()[["elapsed"]]
    elapsed <- max(now - t0, 0)
    pct <- if (total_num > 0) 100 * done / total_num else 100.0

    if (force_final || done >= total_num) {
      if (isTRUE(rendered_100)) return(invisible(NULL))
      line <- .format_progress_line(total_num, total_num, elapsed, label = label, include_gauge = is_interactive, unit = unit)
      if (is_interactive) {
        cat(sprintf("\r%s\n", line), file = stderr())
        utils::flush.console()
      } else {
        message(line)
      }
      rendered_100 <<- TRUE
      last_rendered_t <<- now
      last_rendered_pct <<- 100.0
      return(invisible(NULL))
    }

    if (is_interactive) {
      if (now - last_rendered_t >= .PROGRESS_INTERACTIVE_INTERVAL_SECS || last_rendered_pct < 0) {
        line <- .format_progress_line(done, total_num, elapsed, label = label, include_gauge = TRUE, unit = unit)
        cat(sprintf("\r%s", line), file = stderr())
        utils::flush.console()
        last_rendered_t <<- now
        last_rendered_pct <<- pct
      }
    } else if (identical(progress_mode, "exact")) {
      should_render <- (last_rendered_pct < 0) ||
                       (pct - last_rendered_pct >= .PROGRESS_RENDER_MIN_PCT_STEP) ||
                       (now - last_rendered_t >= .PROGRESS_RENDER_MIN_INTERVAL_SECS)
      if (should_render) {
        line <- .format_progress_line(done, total_num, elapsed, label = label, include_gauge = FALSE, unit = unit)
        message(line)
        last_rendered_t <<- now
        last_rendered_pct <<- pct
      }
    } else if (identical(progress_mode, "bar")) {
      if (now - last_rendered_t >= .PROGRESS_BAR_UPDATE_INTERVAL) {
        if (!is.null(pb)) {
          bar_val <- if (total_num <= .Machine$integer.max) done else (done / total_num) * bar_max
          utils::setTxtProgressBar(pb, bar_val)
        }
        last_rendered_t <<- now
      }
    }
  }

  if (identical(progress_mode, "exact")) {
    render_update(force_final = FALSE)
  }

  list(
    mode = if (identical(progress_mode, "exact")) "exact" else "eta",
    tick = function(n = 1) {
      done <<- min(done + as.numeric(n), total_num)
      render_update(force_final = (done >= total_num))
    },

    eta_message = function() {
      if (is_interactive || identical(progress_mode, "exact")) return(invisible(NULL))
      now <- proc.time()[["elapsed"]]
      elapsed <- now - t0
      if (done < .PROGRESS_ETA_MIN_UNITS || elapsed < .PROGRESS_ETA_CADENCE_SECS || now - last_eta_t < .PROGRESS_ETA_CADENCE_SECS) {
        return(invisible(NULL))
      }
      msg <- .format_eta_message(done, total_num, elapsed)
      if (nzchar(msg) && identical(progress_mode, "bar")) message(msg)
      last_eta_t <<- now
    },

    finish = function() {
      if (isTRUE(closed)) return(invisible(NULL))
      done <<- total_num
      render_update(force_final = TRUE)
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, bar_max)
        close(pb)
      }
      closed <<- TRUE
    },

    close = function() {
      if (isTRUE(closed)) return(invisible(NULL))
      if (!isTRUE(rendered_100) && done >= total_num) {
        render_update(force_final = TRUE)
      }
      if (is_interactive && !isTRUE(rendered_100)) {
        cat("\n", file = stderr())
      }
      if (!is.null(pb)) close(pb)
      closed <<- TRUE
    }
  )
}
