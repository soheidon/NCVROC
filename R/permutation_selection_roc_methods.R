# ==============================================================================
# S3 Methods for permutation_selection_roc_result (v0.33.0)
# ==============================================================================

#' @export
print.permutation_selection_roc_result <- function(x, digits = 4L, ...) {
  cat("NCVROC Permutation Null Benchmark\n")
  cat("---------------------------------\n")
  cat(sprintf("Mode              : %s (%s)\n", x$mode, x$settings$selection_metric))
  cat(sprintf("Sampling          : %s (B = %d, N_all = %s)\n",
              x$sampling, x$n_permutations,
              if (is.finite(x$N_all)) sprintf("%.0f", x$N_all) else sprintf("exp(%.2f)", x$log_N_all)))
  cat(sprintf("Observed stat     : %s (model: %s)\n",
              formatC(x$observed_stat, digits = digits, format = "f"),
              if (nzchar(x$observed_model)) x$observed_model else "<none>"))
  cat(sprintf("Null mean (sd)    : %s (%s), q95 = %s\n",
              formatC(x$null_summary$mean, digits = digits, format = "f"),
              formatC(x$null_summary$sd, digits = digits, format = "f"),
              formatC(x$null_summary$q95, digits = digits, format = "f")))
  if (identical(x$p_value_status, "valid")) {
    cat(sprintf("Permutation p     : %s (p_min = %s, count >= obs: %d / %d)\n",
                formatC(x$p_value, digits = digits, format = "f"),
                formatC(x$p_min, digits = digits, format = "f"),
                x$count_ge, x$n_permutations))
  } else {
    cat(sprintf("Permutation p     : NA [status: %s; bounds = (%s, %s), p_min = %s]\n",
                x$p_value_status,
                formatC(x$p_value_lower, digits = digits, format = "f"),
                formatC(x$p_value_upper, digits = digits, format = "f"),
                formatC(x$p_min, digits = digits, format = "f")))
  }
  if (x$n_failed > 0L) {
    cat(sprintf("Failed iterations : %d / %d (denominator preserved; no silent shrinkage)\n",
                x$n_failed, x$n_permutations))
  }
  cat(sprintf("Pool provenance   : %s\n", x$pool_provenance))
  invisible(x)
}

#' @export
summary.permutation_selection_roc_result <- function(object, ...) {
  structure(
    list(
      mode            = object$mode,
      sampling        = object$sampling,
      observed_stat   = object$observed_stat,
      observed_model  = object$observed_model,
      p_value         = object$p_value,
      p_value_lower   = object$p_value_lower,
      p_value_upper   = object$p_value_upper,
      p_min           = object$p_min,
      p_value_status  = object$p_value_status,
      n_permutations  = object$n_permutations,
      n_valid         = object$n_valid,
      n_failed        = object$n_failed,
      count_ge        = object$count_ge,
      N_all           = object$N_all,
      log_N_all       = object$log_N_all,
      null_summary    = object$null_summary,
      pool_provenance = object$pool_provenance,
      pool_warning    = object$pool_warning,
      settings        = object$settings
    ),
    class = "summary.permutation_selection_roc_result"
  )
}

#' @export
print.summary.permutation_selection_roc_result <- function(x, digits = 4L, ...) {
  print.permutation_selection_roc_result(x, digits = digits, ...)
  if (!is.null(x$pool_warning)) {
    cat(sprintf("Warning           : %s\n", x$pool_warning))
  }
  invisible(x)
}

#' @export
plot.permutation_selection_roc_result <- function(x,
                                                 breaks = "Sturges",
                                                 main = NULL,
                                                 xlab = NULL,
                                                 ...) {
  vals <- x$null_values[!is.na(x$null_values)]
  if (length(vals) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No valid permutation values to plot.")
    return(invisible(x))
  }
  if (is.null(main)) {
    p_txt <- if (!is.na(x$p_value)) sprintf("p = %.4f", x$p_value) else sprintf("p = NA (%s)", x$p_value_status)
    main <- sprintf("Permutation Null (%s, %s, %s)", x$mode, x$sampling, p_txt)
  }
  if (is.null(xlab)) {
    xlab <- sprintf("Null %s", x$settings$selection_metric)
  }
  xlim <- range(c(vals, x$observed_stat), na.rm = TRUE)
  pad <- diff(xlim) * 0.05
  if (pad <= 0) pad <- 0.05
  xlim <- c(xlim[1] - pad, xlim[2] + pad)

  graphics::hist(vals, breaks = breaks, xlim = xlim, main = main, xlab = xlab,
                 col = "grey85", border = "grey50", ...)
  graphics::abline(v = x$observed_stat, col = "firebrick", lwd = 2, lty = 1)
  graphics::legend(
    "topleft",
    legend = c(
      sprintf("Observed = %.4f", x$observed_stat),
      sprintf("Null q95 = %.4f", x$null_summary$q95)
    ),
    col = c("firebrick", NA),
    lty = c(1, NA),
    lwd = c(2, NA),
    bty = "n"
  )
  invisible(x)
}
