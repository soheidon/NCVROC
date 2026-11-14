# ==============================================================================
# S3 Methods for case_influence_roc_result (v0.33.0)
# ==============================================================================

#' @export
print.case_influence_roc_result <- function(x, top_n = 10L, digits = 4L, ...) {
  cat("NCVROC Leave-One-Case-Out Case Influence Analysis\n")
  cat("-------------------------------------------------\n")
  cat(sprintf("Procedure         : %s (%s, %s)\n",
              x$settings$selection_procedure,
              x$settings$selection_metric,
              x$settings$cutoff_method))
  cat(sprintf("Baseline S0       : %s (cutoff = %s, metric = %s)\n",
              x$baseline$selected_label,
              formatC(x$baseline$cutoff, digits = digits, format = "f"),
              formatC(x$baseline$metric_value, digits = digits, format = "f")))
  cat(sprintf("Cases evaluated   : %d (%d valid, %d invalid; subset = %s, anonymized = %s)\n",
              x$summary_stats$n_evaluated,
              x$summary_stats$n_valid,
              x$summary_stats$n_invalid,
              x$settings$case_subset,
              as.character(x$anonymized)))
  cat(sprintf("Selection match   : exact = %s, mean Jaccard = %s, min Jaccard = %s\n",
              formatC(x$summary_stats$exact_match_rate, digits = digits, format = "f"),
              formatC(x$summary_stats$mean_jaccard, digits = digits, format = "f"),
              formatC(x$summary_stats$min_jaccard, digits = digits, format = "f")))
  cat(sprintf("Flip rates        : full = %s (max %s), remaining = %s, excluded (asym) = %s\n",
              formatC(x$summary_stats$mean_flip_rate_full, digits = digits, format = "f"),
              formatC(x$summary_stats$max_flip_rate_full, digits = digits, format = "f"),
              formatC(x$summary_stats$mean_flip_rate_rem, digits = digits, format = "f"),
              formatC(x$summary_stats$excluded_flip_rate, digits = digits, format = "f")))

  df <- x$case_table
  if (nrow(df) > 0L && top_n > 0L) {
    ord <- order(df$jaccard, -abs(df$metric_diff), -df$flip_rate_full, na.last = TRUE)
    show_df <- utils::head(df[ord, c("case_index", "y_obs", "status", "exact_match",
                                     "jaccard", "metric_diff", "flip_rate_full",
                                     "flipped_on_excluded"), drop = FALSE], n = as.integer(top_n))
    cat(sprintf("\nMost influential cases (top %d by lowest Jaccard / largest metric shift):\n", nrow(show_df)))
    print(show_df, row.names = FALSE, digits = digits)
  }
  invisible(x)
}

#' @export
summary.case_influence_roc_result <- function(object, ...) {
  structure(
    list(
      baseline                = object$baseline,
      summary_stats           = object$summary_stats,
      target_comparison_notes = object$target_comparison_notes,
      stability_context       = object$stability_context,
      anonymized              = object$anonymized,
      settings                = object$settings,
      case_table              = object$case_table
    ),
    class = "summary.case_influence_roc_result"
  )
}

#' @export
print.summary.case_influence_roc_result <- function(x, top_n = 10L, digits = 4L, ...) {
  print.case_influence_roc_result(x, top_n = top_n, digits = digits, ...)
  cat("\nTarget Set Separation Notes:\n")
  cat(sprintf("  (a) Full     : %s\n", x$target_comparison_notes$target_full))
  cat(sprintf("  (b) Remaining: %s\n", x$target_comparison_notes$target_remaining))
  cat(sprintf("  (c) Excluded : %s\n", x$target_comparison_notes$target_excluded))
  if (!is.null(x$stability_context)) {
    cat("\nBootstrap Stability Context (separate estimand):\n")
    cat(sprintf("  Bootstrap mean flip rate (B = %d): %s\n",
                x$stability_context$bootstrap_reps,
                formatC(x$stability_context$bootstrap_mean_flip_rate, digits = digits, format = "f")))
  }
  invisible(x)
}

#' @export
plot.case_influence_roc_result <- function(x,
                                           metric = c("jaccard", "metric_diff", "flip_rate_full"),
                                           main = NULL,
                                           xlab = "Anonymized Case Index",
                                           ylab = NULL,
                                           ...) {
  metric <- match.arg(metric)
  df <- x$case_table
  vals <- df[[metric]]
  valid <- !is.na(vals)
  if (!any(valid)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No valid case influence values to plot.")
    return(invisible(x))
  }
  if (is.null(main)) {
    main <- sprintf("Case Influence (%s, procedure = %s)", metric, x$settings$selection_procedure)
  }
  if (is.null(ylab)) {
    ylab <- switch(metric,
      jaccard        = "Jaccard Similarity S(-i) vs S0",
      metric_diff    = sprintf("Metric Diff (%s(-i) - %s0)", x$settings$selection_metric, x$settings$selection_metric),
      flip_rate_full = "Full-Data Classification Flip Rate"
    )
  }
  cols <- ifelse(df$y_obs[valid] == 1L, "firebrick", "steelblue")
  graphics::plot(
    x    = df$case_index[valid],
    y    = vals[valid],
    col  = cols,
    pch  = 16,
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  if (identical(metric, "jaccard")) {
    graphics::abline(h = 1.0, lty = 2, col = "grey50")
  } else if (identical(metric, "metric_diff")) {
    graphics::abline(h = 0.0, lty = 2, col = "grey50")
  } else {
    graphics::abline(h = 0.0, lty = 2, col = "grey50")
  }
  graphics::legend(
    "bottomleft",
    legend = c("Positive (y=1)", "Negative (y=0)"),
    col    = c("firebrick", "steelblue"),
    pch    = 16,
    bty    = "n"
  )
  invisible(x)
}
