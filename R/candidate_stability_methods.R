# candidate_stability_methods.R — S3 print, summary, and plot methods for candidate_stability_result

#' Print Candidate-Level Stability Result
#'
#' @param x An object of class \code{"candidate_stability_result"}.
#' @param top_n Integer, maximum number of top candidates to display (default 10).
#' @param digits Integer, number of decimal places to format numbers (default 3).
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return The input object \code{x}, invisibly.
#' @export
print.candidate_stability_result <- function(x, top_n = 10L, digits = 3L, ...) {
  s <- x$settings
  cat("================================================================================\n")
  cat("Candidate-Level Resampling Stability Audit\n")
  cat("================================================================================\n")
  if (isTRUE(s$conditional_on_screen)) {
    cat(sprintf("Mode                : Combinatorial Screening (Stage-1 Screen -> Stage-2 Resampling Audit)\n"))
    cat(sprintf("Model Sizes         : %s (Total Combinations: %s)\n",
                paste(s$model_sizes, collapse = ", "), format(s$total_candidate_count, big.mark = ",")))
    cat(sprintf("Screening Metric    : Apparent %s (retained top %d from %d candidates)\n",
                s$screening_metric, s$screened_candidate_count, s$total_candidate_count))
  } else {
    cat(sprintf("Mode                : Fixed Candidate Evaluation (%d candidate model(s))\n", s$n_candidates))
  }

  if (s$resampling == "repeated_cv") {
    cat(sprintf("Resampling Scheme   : Repeated K-Fold Cross-Validation (%d-fold CV x %d repeat(s) = %d resamples)\n",
                s$folds, s$repeats, s$total_requested_resamples))
  } else {
    cat(sprintf("Resampling Scheme   : Non-Parametric Bootstrap (B = %d replicates, test evaluation: %s)\n",
                s$bootstrap_reps, s$bootstrap_test))
  }
  cat(sprintf("Candidates Evaluated: %d candidate model(s)\n", s$n_candidates))
  cat(sprintf("Primary Rank Metric : %s (prefer fewer items: %s)\n",
              s$rank_by, if (s$prefer_fewer_items) "TRUE" else "FALSE"))
  cat(sprintf("Cutoff Method       : %s (training-derived only)\n", s$cutoff_method))

  constraint_str <- if (!is.null(s$sensitivity_min) || !is.null(s$specificity_min)) {
    parts <- c()
    if (!is.null(s$sensitivity_min)) parts <- c(parts, sprintf("Sens >= %.*f", digits, s$sensitivity_min))
    if (!is.null(s$specificity_min)) parts <- c(parts, sprintf("Spec >= %.*f", digits, s$specificity_min))
    paste(parts, collapse = ", ")
  } else {
    "None"
  }
  cat(sprintf("Feasibility Targets : %s\n", constraint_str))

  if (isTRUE(s$conditional_on_screen)) {
    cat("\n[!] Note: Stability summaries and selection frequencies are conditional on the\n")
    cat("    Stage-1 screened candidate set and do not account for uncertainty in the screening step.\n")
  }

  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Candidate Performance & Stability (Top %d by Mean Rank):\n",
              min(as.integer(top_n), nrow(x$candidate_summary))))
  cat("--------------------------------------------------------------------------------\n")

  show_n <- min(as.integer(top_n), nrow(x$candidate_summary))
  sub_sum <- x$candidate_summary[seq_len(show_n), , drop = FALSE]

  rank_metric_col <- s$rank_by
  app_col <- paste0("apparent_", rank_metric_col)
  res_mean_col <- paste0("resampled_", rank_metric_col, "_mean")
  res_sd_col <- paste0("resampled_", rank_metric_col, "_sd")

  display_df <- data.frame(
    Candidate = sub_sum$label,
    Items     = ifelse(nchar(sub_sum$items) > 20, paste0(substr(sub_sum$items, 1, 17), "..."), sub_sum$items),
    Size      = sub_sum$n_items,
    Apparent  = sprintf(paste0("%.", digits, "f"), sub_sum[[app_col]]),
    Test_Mean = sprintf(paste0("%.", digits, "f"), sub_sum[[res_mean_col]]),
    Test_SD   = sprintf(paste0("%.", digits, "f"), sub_sum[[res_sd_col]]),
    Mean_Rank = sprintf(paste0("%.", digits, "f"), sub_sum$mean_rank),
    Sel_Freq  = sprintf(paste0("%.", digits, "f"), sub_sum$selection_frequency_all),
    stringsAsFactors = FALSE
  )

  if (s$resampling == "bootstrap" && s$bootstrap_test == "original" && paste0("optimism_", rank_metric_col, "_mean") %in% names(sub_sum)) {
    display_df$Optimism <- sprintf(paste0("%.", digits, "f"), sub_sum[[paste0("optimism_", rank_metric_col, "_mean")]])
    display_df$Opt_Corr <- sprintf(paste0("%.", digits, "f"), sub_sum[[paste0("optimism_corrected_", rank_metric_col)]])
  }

  if (!is.null(s$sensitivity_min) || !is.null(s$specificity_min)) {
    display_df$Feas_Pass <- sprintf(paste0("%.", digits, "f"),
                                    if (!is.null(s$sensitivity_min) && !is.null(s$specificity_min)) sub_sum$both_pass_frequency
                                    else (if (!is.null(s$sensitivity_min)) sub_sum$sens_pass_frequency else sub_sum$spec_pass_frequency))
  }

  colnames(display_df)[4] <- paste0("App_", toupper(rank_metric_col))
  colnames(display_df)[5] <- paste0(if (s$resampling == "repeated_cv") "CV_" else "Test_", toupper(rank_metric_col))
  colnames(display_df)[6] <- "SD"

  print(display_df, row.names = FALSE)

  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Selection Summary Across %d Resamples:\n", s$total_requested_resamples))
  cat(sprintf("  Winner Selected        : %d / %d (%5.1f%%)\n",
              s$n_resamples_with_selected_winner, s$total_requested_resamples,
              100 * (s$n_resamples_with_selected_winner / s$total_requested_resamples)))
  cat(sprintf("  No Feasible Candidate  : %d / %d (%5.1f%%)\n",
              sum(x$replicate_metadata$selection_status == "no_feasible_candidate"),
              s$total_requested_resamples, 100 * s$no_feasible_frequency))
  if (s$invalid_evaluation_frequency > 0) {
    cat(sprintf("  Invalid Evaluation     : %d / %d (%5.1f%%)\n",
                sum(x$replicate_metadata$selection_status == "invalid_evaluation"),
                s$total_requested_resamples, 100 * s$invalid_evaluation_frequency))
  }
  cat("================================================================================\n")
  invisible(x)
}

#' Summary Method for Candidate-Level Stability Result
#'
#' @param object An object of class \code{"candidate_stability_result"}.
#' @param ... Additional arguments passed to \code{summary}.
#'
#' @return An object of class \code{"summary_candidate_stability_result"}.
#' @export
summary.candidate_stability_result <- function(object, ...) {
  structure(
    list(
      candidate_summary    = object$candidate_summary,
      replicate_metadata   = object$replicate_metadata,
      rank_stability       = object$rank_stability,
      selection_frequency  = object$selection_frequency,
      constraint_stability = object$constraint_stability,
      apparent_performance = object$apparent_performance,
      settings             = object$settings
    ),
    class = "summary_candidate_stability_result"
  )
}

#' Print Summary for Candidate-Level Stability Result
#'
#' @param x An object of class \code{"summary_candidate_stability_result"}.
#' @param top_n Integer, maximum number of candidates to display (default 10).
#' @param digits Integer, number of decimal places (default 3).
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return The input object \code{x}, invisibly.
#' @export
print.summary_candidate_stability_result <- function(x, top_n = 10L, digits = 3L, ...) {
  s <- x$settings
  cat("================================================================================\n")
  cat("Summary of Candidate Stability & Optimism Analysis\n")
  cat("================================================================================\n")
  if (isTRUE(s$conditional_on_screen)) {
    cat(sprintf("Mode                : Combinatorial Screening (Stage-1 Screen -> Stage-2 Resampling Audit)\n"))
    cat(sprintf("Model Sizes         : %s (Total Combinations: %s)\n",
                paste(s$model_sizes, collapse = ", "), format(s$total_candidate_count, big.mark = ",")))
    cat(sprintf("Screening Metric    : Apparent %s (retained top %d from %d candidates)\n",
                s$screening_metric, s$screened_candidate_count, s$total_candidate_count))
  } else {
    cat(sprintf("Mode                : Fixed Candidate Evaluation (%d candidate model(s))\n", s$n_candidates))
  }

  if (s$resampling == "repeated_cv") {
    cat(sprintf("Resampling Scheme   : Repeated K-Fold Cross-Validation (%d-fold CV x %d repeat(s) = %d resamples)\n",
                s$folds, s$repeats, s$total_requested_resamples))
  } else {
    cat(sprintf("Resampling Scheme   : Non-Parametric Bootstrap (B = %d replicates, test evaluation: %s)\n",
                s$bootstrap_reps, s$bootstrap_test))
  }
  cat(sprintf("Candidates Evaluated: %d candidate model(s)\n", s$n_candidates))
  cat(sprintf("Primary Rank Metric : %s (prefer fewer items: %s)\n",
              s$rank_by, if (s$prefer_fewer_items) "TRUE" else "FALSE"))
  cat(sprintf("Cutoff Method       : %s (training-derived only)\n", s$cutoff_method))

  constraint_str <- if (!is.null(s$sensitivity_min) || !is.null(s$specificity_min)) {
    parts <- c()
    if (!is.null(s$sensitivity_min)) parts <- c(parts, sprintf("Sens >= %.*f", digits, s$sensitivity_min))
    if (!is.null(s$specificity_min)) parts <- c(parts, sprintf("Spec >= %.*f", digits, s$specificity_min))
    paste(parts, collapse = ", ")
  } else {
    "None"
  }
  cat(sprintf("Feasibility Targets : %s\n", constraint_str))

  if (isTRUE(s$conditional_on_screen)) {
    cat("\n[!] Note: Stability summaries and selection frequencies are conditional on the\n")
    cat("    Stage-1 screened candidate set and do not account for uncertainty in the screening step.\n")
  }

  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Candidate Performance & Stability (Top %d by Mean Rank):\n",
              min(as.integer(top_n), nrow(x$candidate_summary))))
  cat("--------------------------------------------------------------------------------\n")

  show_n <- min(as.integer(top_n), nrow(x$candidate_summary))
  sub_sum <- x$candidate_summary[seq_len(show_n), , drop = FALSE]

  rank_metric_col <- s$rank_by
  app_col <- paste0("apparent_", rank_metric_col)
  res_mean_col <- paste0("resampled_", rank_metric_col, "_mean")
  res_sd_col <- paste0("resampled_", rank_metric_col, "_sd")

  display_df <- data.frame(
    Candidate = sub_sum$label,
    Items     = ifelse(nchar(sub_sum$items) > 20, paste0(substr(sub_sum$items, 1, 17), "..."), sub_sum$items),
    Size      = sub_sum$n_items,
    Apparent  = sprintf(paste0("%.", digits, "f"), sub_sum[[app_col]]),
    Test_Mean = sprintf(paste0("%.", digits, "f"), sub_sum[[res_mean_col]]),
    Test_SD   = sprintf(paste0("%.", digits, "f"), sub_sum[[res_sd_col]]),
    Mean_Rank = sprintf(paste0("%.", digits, "f"), sub_sum$mean_rank),
    Sel_Freq  = sprintf(paste0("%.", digits, "f"), sub_sum$selection_frequency_all),
    stringsAsFactors = FALSE
  )

  if (s$resampling == "bootstrap" && s$bootstrap_test == "original" && paste0("optimism_", rank_metric_col, "_mean") %in% names(sub_sum)) {
    display_df$Optimism <- sprintf(paste0("%.", digits, "f"), sub_sum[[paste0("optimism_", rank_metric_col, "_mean")]])
    display_df$Opt_Corr <- sprintf(paste0("%.", digits, "f"), sub_sum[[paste0("optimism_corrected_", rank_metric_col)]])
  }

  if (!is.null(s$sensitivity_min) || !is.null(s$specificity_min)) {
    display_df$Feas_Pass <- sprintf(paste0("%.", digits, "f"),
                                    if (!is.null(s$sensitivity_min) && !is.null(s$specificity_min)) sub_sum$both_pass_frequency
                                    else (if (!is.null(s$sensitivity_min)) sub_sum$sens_pass_frequency else sub_sum$spec_pass_frequency))
  }

  colnames(display_df)[4] <- paste0("App_", toupper(rank_metric_col))
  colnames(display_df)[5] <- paste0(if (s$resampling == "repeated_cv") "CV_" else "Test_", toupper(rank_metric_col))
  colnames(display_df)[6] <- "SD"

  print(display_df, row.names = FALSE)

  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Selection Summary Across %d Resamples:\n", s$total_requested_resamples))
  cat(sprintf("  Winner Selected        : %d / %d (%5.1f%%)\n",
              s$n_resamples_with_selected_winner, s$total_requested_resamples,
              100 * (s$n_resamples_with_selected_winner / s$total_requested_resamples)))
  cat(sprintf("  No Feasible Candidate  : %d / %d (%5.1f%%)\n",
              sum(x$replicate_metadata$selection_status == "no_feasible_candidate"),
              s$total_requested_resamples, 100 * s$no_feasible_frequency))
  if (s$invalid_evaluation_frequency > 0) {
    cat(sprintf("  Invalid Evaluation     : %d / %d (%5.1f%%)\n",
                sum(x$replicate_metadata$selection_status == "invalid_evaluation"),
                s$total_requested_resamples, 100 * s$invalid_evaluation_frequency))
  }
  cat("================================================================================\n")
  invisible(x)
}

#' Plot Candidate-Level Stability and Optimism Results
#'
#' Visualizes rank distributions, apparent vs resampled performance gaps,
#' selection frequencies, or clinical constraint feasibility for candidate models.
#'
#' @param x An object of class \code{"candidate_stability_result"}.
#' @param type Character string specifying the plot type:
#'   \itemize{
#'     \item \code{"rank_stability"}: Boxplot distribution of candidate ranks across resamples.
#'     \item \code{"performance"}: Paired comparison of apparent performance vs mean resampled test performance (and optimism-corrected metrics if available).
#'     \item \code{"selection_frequency"}: Barplot of unconditional selection frequencies across resamples.
#'     \item \code{"constraint_stability"}: Pass rates for sensitivity, specificity, and joint clinical constraints.
#'   }
#' @param metric Character string specifying the performance metric for \code{type = "performance"}:
#'   \code{"youden"} (default), \code{"sensitivity"}, \code{"specificity"}, \code{"accuracy"},
#'   \code{"ppv"}, \code{"npv"}, or \code{"auc"}.
#'   \strong{Note on AUC}: For fixed unweighted sum scores, out-of-fold AUC in repeated CV
#'   and test AUC in bootstrap original mode are mathematically identical to full-data apparent AUC
#'   (apparent-vs-test gap is 0). Zero apparent-test gap in AUC is a mathematical identity of
#'   fixed unweighted scores rather than empirical evidence of model robustness. In bootstrap OOB mode,
#'   AUC varies across replicates and is undefined (\code{NA}) for single-class OOB samples.
#' @param top_n Integer scalar or \code{Inf}, maximum number of top candidates to display (ordered by mean rank ascending). Default \code{20L}.
#' @param ... Additional graphical parameters passed to plotting functions.
#'
#' @return The input object \code{x}, invisibly.
#' @export
plot.candidate_stability_result <- function(x,
                                            type = c("rank_stability", "performance", "selection_frequency", "constraint_stability"),
                                            metric = NULL,
                                            top_n = 20L,
                                            ...) {
  type <- match.arg(type)

  # Validate top_n
  if (is.null(top_n) || length(top_n) != 1L || is.na(top_n) ||
      (!is.infinite(top_n) && (!is.numeric(top_n) || top_n != as.integer(top_n) || top_n < 1L))) {
    stop("`top_n` must be a positive integer scalar or Inf.", call. = FALSE)
  }
  n_show <- if (is.infinite(top_n)) nrow(x$candidate_summary) else min(as.integer(top_n), nrow(x$candidate_summary))

  show_cands <- x$candidate_summary[seq_len(n_show), , drop = FALSE]
  s <- x$settings

  if (type == "rank_stability") {
    cand_ids <- show_cands$candidate_id
    rank_list <- lapply(rev(cand_ids), function(cid) {
      sub_ranks <- x$resample_results$rank[x$resample_results$candidate_id == cid]
      sub_ranks[!is.na(sub_ranks)]
    })
    names(rank_list) <- rev(show_cands$label)

    if (all(vapply(rank_list, length, integer(1)) == 0L)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No valid rank data across resamples")
      return(invisible(x))
    }

    max_lab_len <- max(nchar(names(rank_list)))
    left_mar <- max(4.5, min(14, max_lab_len * 0.7 + 2))
    old_par <- graphics::par(mar = c(4.5, left_mar, 3.5, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::boxplot(
      rank_list,
      horizontal = TRUE,
      las        = 1,
      xlab       = "Rank across resamples",
      main       = "Candidate Rank Distribution Across Resamples",
      col        = "#4A90E2",
      border     = "#2C3E50",
      ...
    )

    sub_txt <- if (isTRUE(s$conditional_on_screen)) {
      "Conditional on Stage-1 screened candidate set"
    } else {
      sprintf("Evaluated across %d resamples", s$total_requested_resamples)
    }
    graphics::mtext(sub_txt, side = 3, line = 0.5, cex = 0.85, font = 3)

  } else if (type == "performance") {
    if (is.null(metric)) {
      metric <- "youden"
    } else {
      metric <- match.arg(tolower(metric), c("youden", "sensitivity", "specificity", "accuracy", "ppv", "npv", "auc"))
    }

    app_col <- paste0("apparent_", metric)
    res_col <- paste0("resampled_", metric, "_mean")

    app_vals <- show_cands[[app_col]]
    res_vals <- show_cands[[res_col]]

    valid_mask <- !is.na(app_vals) | !is.na(res_vals)
    if (!any(valid_mask)) {
      stop(sprintf("No finite performance values available for metric '%s'.", metric), call. = FALSE)
    }

    has_opt_corr <- (s$resampling == "bootstrap" && s$bootstrap_test == "original" &&
                      paste0("optimism_corrected_", metric) %in% names(show_cands))
    opt_vals <- if (has_opt_corr) show_cands[[paste0("optimism_corrected_", metric)]] else NULL

    all_plotted_vals <- c(app_vals, res_vals, opt_vals)
    all_plotted_vals <- all_plotted_vals[!is.na(all_plotted_vals)]

    xlim_r <- range(all_plotted_vals)
    pad <- max(0.02, (xlim_r[2] - xlim_r[1]) * 0.08)
    xlim <- c(max(0, xlim_r[1] - pad), min(1, xlim_r[2] + pad))

    y_pos <- seq_len(n_show)
    y_labels <- show_cands$label

    max_lab_len <- max(nchar(y_labels))
    left_mar <- max(4.5, min(14, max_lab_len * 0.7 + 2))
    old_par <- graphics::par(mar = c(4.5, left_mar, 3.5, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    main_txt <- if (s$resampling == "repeated_cv") {
      sprintf("Apparent vs Mean Repeated-CV Test %s", toupper(metric))
    } else if (s$bootstrap_test == "original") {
      sprintf("Apparent vs Bootstrap Test %s (Original Test)", toupper(metric))
    } else {
      sprintf("Apparent vs Bootstrap OOB Test %s", toupper(metric))
    }

    graphics::plot(
      x    = NA,
      y    = NA,
      xlim = xlim,
      ylim = c(0.5, n_show + 0.5),
      yaxt = "n",
      xlab = sprintf("Metric Value (%s)", toupper(metric)),
      ylab = "",
      main = main_txt,
      las  = 1,
      ...
    )
    graphics::axis(2, at = rev(y_pos), labels = y_labels, las = 1)
    graphics::grid(nx = NULL, ny = NA, col = "gray90", lty = 1)

    for (i in seq_len(n_show)) {
      y_i <- n_show - i + 1
      a_v <- app_vals[i]
      r_v <- res_vals[i]

      if (!is.na(a_v) && !is.na(r_v)) {
        graphics::segments(x0 = a_v, y0 = y_i, x1 = r_v, y1 = y_i, col = "gray60", lty = 2, lwd = 1.5)
      }
      if (!is.na(a_v)) {
        graphics::points(a_v, y_i, pch = 19, col = "#2C3E50", cex = 1.3)
      }
      if (!is.na(r_v)) {
        graphics::points(r_v, y_i, pch = 17, col = "#E74C3C", cex = 1.3)
      }
      if (has_opt_corr && !is.na(opt_vals[i])) {
        graphics::points(opt_vals[i], y_i, pch = 18, col = "#27AE60", cex = 1.4)
      }
    }

    leg_items <- c("Apparent", if (s$resampling == "repeated_cv") "CV Test Mean" else "Test Mean")
    leg_pchs  <- c(19, 17)
    leg_cols  <- c("#2C3E50", "#E74C3C")

    if (has_opt_corr) {
      leg_items <- c(leg_items, "Optimism-Corrected")
      leg_pchs  <- c(leg_pchs, 18)
      leg_cols  <- c(leg_cols, "#27AE60")
    }

    graphics::legend("bottomright", legend = leg_items, pch = leg_pchs, col = leg_cols,
                     bty = "o", bg = "white", cex = 0.85, pt.cex = 1.1)

    sub_txt <- if (isTRUE(s$conditional_on_screen)) {
      "Conditional on Stage-1 screened candidate set"
    } else {
      sprintf("Evaluated across %d resamples", s$total_requested_resamples)
    }
    graphics::mtext(sub_txt, side = 3, line = 0.5, cex = 0.85, font = 3)

  } else if (type == "selection_frequency") {
    freq_vals <- rev(show_cands$selection_frequency_all)
    lbls <- rev(show_cands$label)

    max_lab_len <- max(nchar(lbls))
    left_mar <- max(4.5, min(14, max_lab_len * 0.7 + 2))
    old_par <- graphics::par(mar = c(5, left_mar, 3.5, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::barplot(
      height    = freq_vals,
      names.arg = lbls,
      horiz     = TRUE,
      las       = 1,
      xlab      = "Selection frequency across all resamples",
      main      = "Candidate Selection Frequency",
      col       = "#2980B9",
      border    = NA,
      xlim      = c(0, 1),
      ...
    )
    graphics::abline(v = seq(0.2, 0.8, by = 0.2), col = "gray90", lty = 3)

    sub_txt <- if (isTRUE(s$conditional_on_screen)) {
      "Selection frequencies are conditional on the Stage-1 screened candidate set"
    } else {
      sprintf("Total resamples: %d", s$total_requested_resamples)
    }
    graphics::mtext(sub_txt, side = 3, line = 0.5, cex = 0.85, font = 3)

    footer_parts <- c()
    if (s$no_feasible_frequency > 0) {
      footer_parts <- c(footer_parts, sprintf("No feasible: %.1f%%", 100 * s$no_feasible_frequency))
    }
    if (s$invalid_evaluation_frequency > 0) {
      footer_parts <- c(footer_parts, sprintf("Invalid eval: %.1f%%", 100 * s$invalid_evaluation_frequency))
    }
    if (length(footer_parts) > 0) {
      graphics::mtext(paste(footer_parts, collapse = "  |  "), side = 1, line = 3.8, cex = 0.8, col = "gray30")
    }

  } else if (type == "constraint_stability") {
    has_sens <- !is.null(s$sensitivity_min)
    has_spec <- !is.null(s$specificity_min)

    if (!has_sens && !has_spec) {
      stop("No sensitivity or specificity constraint was specified in this analysis.", call. = FALSE)
    }

    max_lab_len <- max(nchar(show_cands$label))
    left_mar <- max(4.5, min(14, max_lab_len * 0.7 + 2))

    if (has_sens && !has_spec) {
      old_par <- graphics::par(mar = c(4.5, left_mar, 3.5, 2))
      on.exit(graphics::par(old_par), add = TRUE)

      graphics::barplot(
        height    = rev(show_cands$sens_pass_frequency),
        names.arg = rev(show_cands$label),
        horiz     = TRUE,
        las       = 1,
        xlab      = "Sensitivity constraint pass rate",
        main      = sprintf("Constraint Feasibility (Sensitivity >= %.2f)", s$sensitivity_min),
        col       = "#27AE60",
        border    = NA,
        xlim      = c(0, 1),
        ...
      )
    } else if (!has_sens && has_spec) {
      old_par <- graphics::par(mar = c(4.5, left_mar, 3.5, 2))
      on.exit(graphics::par(old_par), add = TRUE)

      graphics::barplot(
        height    = rev(show_cands$spec_pass_frequency),
        names.arg = rev(show_cands$label),
        horiz     = TRUE,
        las       = 1,
        xlab      = "Specificity constraint pass rate",
        main      = sprintf("Constraint Feasibility (Specificity >= %.2f)", s$specificity_min),
        col       = "#E67E22",
        border    = NA,
        xlim      = c(0, 1),
        ...
      )
    } else {
      # Both constraints specified
      mat_pass <- rbind(
        Both = show_cands$both_pass_frequency,
        Sens = show_cands$sens_pass_frequency,
        Spec = show_cands$spec_pass_frequency
      )
      mat_pass_rev <- mat_pass[, rev(seq_len(n_show)), drop = FALSE]

      old_par <- graphics::par(mar = c(4.5, left_mar, 3.5, 2))
      on.exit(graphics::par(old_par), add = TRUE)

      graphics::barplot(
        height    = mat_pass_rev,
        beside    = TRUE,
        names.arg = rev(show_cands$label),
        horiz     = TRUE,
        las       = 1,
        xlab      = "Constraint pass rate across resamples",
        main      = sprintf("Clinical Constraint Feasibility (Sens >= %.2f, Spec >= %.2f)",
                            s$sensitivity_min, s$specificity_min),
        col       = c("#2980B9", "#27AE60", "#E67E22"),
        border    = NA,
        xlim      = c(0, 1),
        ...
      )
      graphics::legend("bottomright",
                       legend = c(sprintf("Joint Feasible (Sens>=%.2f & Spec>=%.2f)", s$sensitivity_min, s$specificity_min),
                                  sprintf("Sensitivity >= %.2f", s$sensitivity_min),
                                  sprintf("Specificity >= %.2f", s$specificity_min)),
                       fill   = c("#2980B9", "#27AE60", "#E67E22"),
                       border = NA, bty = "o", bg = "white", cex = 0.8)
    }

    sub_txt <- if (isTRUE(s$conditional_on_screen)) {
      "Conditional on Stage-1 screened candidate set"
    } else {
      sprintf("Evaluated across %d resamples", s$total_requested_resamples)
    }
    graphics::mtext(sub_txt, side = 3, line = 0.5, cex = 0.85, font = 3)
  }

  invisible(x)
}
