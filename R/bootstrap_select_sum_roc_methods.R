# bootstrap_select_sum_roc_methods.R — S3 methods for bootstrap_select_sum_roc_result
#
# Provides:
#   - print.bootstrap_select_sum_roc_result()
#   - summary.bootstrap_select_sum_roc_result()
#   - print.summary.bootstrap_select_sum_roc_result()
#   - plot.bootstrap_select_sum_roc_result()

#' Print summary of bootstrap selection and internal validation result
#'
#' @param x An object of class `"bootstrap_select_sum_roc_result"`.
#' @param digits Integer, number of decimal places to print. Default `4L`.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.bootstrap_select_sum_roc_result <- function(x, digits = 4L, ...) {
  cat("\n=======================================================\n")
  cat("  NCVROC Bootstrap Item Selection & Internal Validation\n")
  cat("=======================================================\n\n")

  sett <- x$settings
  cat(sprintf("Model sizes:         %s\n", paste(sett$model_sizes, collapse = ", ")))
  cat(sprintf("Bootstrap reps:      %d (Resampling: %s)\n", sett$bootstrap_reps, sett$resampling))
  cat(sprintf("Selection metric:    %s\n", sett$selection_metric))
  cat(sprintf("Cutoff method:       %s\n", sett$cutoff_method))
  if (sett$cutoff_method == "clinical_constraint") {
    cat(sprintf("Constraints:         sensitivity >= %.2f, specificity >= %.2f\n",
                sett$sensitivity_min, sett$specificity_min))
  }
  cat(sprintf("No feasible rule:    %s\n", sett$no_feasible))

  bs <- x$bootstrap_summary
  cat(sprintf("Replicates:          %d requested, %d valid, %d failed (fallback used: %d)\n",
              bs$n_reps_requested, bs$n_reps_valid, bs$n_reps_failed, bs$n_reps_fallback))
  if (!is.null(bs$n_reps_oob_valid_auc) && !is.null(bs$n_reps_oob_valid_joint) &&
      (isTRUE(x$corrections$b632$requested) || isTRUE(x$corrections$b632plus$requested))) {
    cat(sprintf("OOB valid reps:      AUC = %d / %d; Joint (Se/Sp/Youden) = %d / %d\n",
                bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }

  cat("\n-- Reference Model (Full Data) --\n")
  fm <- x$final_selected_model
  if (is.null(fm) || is.na(fm$items)) {
    cat("  No model selected on full data.\n")
  } else {
    cat(sprintf("  Selected items (%d): %s\n", fm$n_items, fm$items))
    cat(sprintf("  Selected cutoff:    %s\n", format(round(fm$cutoff, digits), nsmall = digits)))
  }

  cat("\n-- Performance & Optimism Summary --\n")
  app <- x$apparent_performance
  cor <- x$corrected_performance
  mean_opt <- bs$mean_optimism

  df_perf <- data.frame(
    Apparent  = c(app$auc, app$sensitivity, app$specificity, app$youden),
    Optimism  = c(mean_opt["auc"], mean_opt["sensitivity"], mean_opt["specificity"], mean_opt["youden"]),
    Harrell   = c(cor$auc, cor$sensitivity, cor$specificity, cor$youden),
    row.names = c("AUC", "Sensitivity", "Specificity", "Youden")
  )
  if (!is.null(x$corrections$b632) && isTRUE(x$corrections$b632$requested)) {
    b632 <- x$corrections$b632
    df_perf[[".632"]] <- c(b632$auc, b632$sensitivity, b632$specificity, b632$youden)
  }
  if (!is.null(x$corrections$b632plus) && isTRUE(x$corrections$b632plus$requested)) {
    b632p <- x$corrections$b632plus
    df_perf[[".632+"]] <- c(b632p$auc, b632p$sensitivity, b632p$specificity, b632p$youden)
  }
  print(round(df_perf, digits = digits))

  # Conditional estimation notes if any method has reduced valid replicates
  cond_notes <- character(0)
  if (isTRUE(cor$is_conditional)) {
    cond_notes <- c(cond_notes, sprintf("Harrell: conditional estimate (%d / %d valid selection resamples).",
                                        bs$n_reps_valid, bs$n_reps_requested))
  }
  if (!is.null(x$corrections$b632) && isTRUE(x$corrections$b632$requested) && isTRUE(x$corrections$b632$is_conditional)) {
    cond_notes <- c(cond_notes, sprintf(".632: conditional estimate (AUC valid reps: %d / %d; Joint Se/Sp/Youden valid reps: %d / %d).",
                                        bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                                        bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }
  if (!is.null(x$corrections$b632plus) && isTRUE(x$corrections$b632plus$requested) && isTRUE(x$corrections$b632plus$is_conditional)) {
    cond_notes <- c(cond_notes, sprintf(".632+: conditional estimate (AUC valid reps: %d / %d; Joint Se/Sp/Youden valid reps: %d / %d).",
                                        bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                                        bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }
  if (length(cond_notes) > 0L) {
    cat("\nConditional Estimation Status:\n")
    for (cn in cond_notes) {
      cat(sprintf("  * %s\n", cn))
    }
  }

  cat("\n-- Selection Stability Summary --\n")
  st <- x$selection_stability
  cat(sprintf("  Exact match rate:   %.1f%% (%d / %d)\n",
              st$exact_match_rate * 100, st$n_exact_match, st$n_reps_denominator))
  if (!is.na(st$jaccard_mean)) {
    cat(sprintf("  Jaccard similarity: mean = %.4f (sd = %.4f, median = %.4f, IQR = %.4f)\n",
                st$jaccard_mean, st$jaccard_sd, st$jaccard_median, st$jaccard_iqr))
  } else {
    cat("  Jaccard similarity: NA (no reference model)\n")
  }

  invisible(x)
}

#' Summarize bootstrap selection and validation result
#'
#' @param object An object of class `"bootstrap_select_sum_roc_result"`.
#' @param digits Integer, number of decimal places to print. Default `4L`.
#' @param ... Additional arguments.
#'
#' @return An object of class `"summary.bootstrap_select_sum_roc_result"`.
#' @export
summary.bootstrap_select_sum_roc_result <- function(object, digits = 4L, ...) {
  app <- object$apparent_performance
  bs  <- object$bootstrap_summary
  cor <- object$corrected_performance
  mean_opt <- bs$mean_optimism
  b632 <- if (!is.null(object$corrections$b632)) object$corrections$b632 else list(auc = NA_real_, sensitivity = NA_real_, specificity = NA_real_, youden = NA_real_, requested = FALSE)
  b632p <- if (!is.null(object$corrections$b632plus)) object$corrections$b632plus else list(auc = NA_real_, sensitivity = NA_real_, specificity = NA_real_, youden = NA_real_, requested = FALSE)

  perf_table <- data.frame(
    apparent  = c(app$auc, app$sensitivity, app$specificity, app$youden),
    harrell   = c(cor$auc, cor$sensitivity, cor$specificity, cor$youden),
    `.632`    = c(b632$auc, b632$sensitivity, b632$specificity, b632$youden),
    `.632+`   = c(b632p$auc, b632p$sensitivity, b632p$specificity, b632p$youden),
    row.names = c("auc", "sensitivity", "specificity", "youden"),
    check.names = FALSE
  )
  # Preserve backward compatibility for access via $corrected or $mean_optimism
  perf_table$mean_optimism <- c(mean_opt["auc"], mean_opt["sensitivity"], mean_opt["specificity"], mean_opt["youden"])
  perf_table$corrected     <- perf_table$harrell

  res <- list(
    final_selected_model    = object$final_selected_model,
    performance_table       = perf_table,
    bootstrap_summary       = object$bootstrap_summary,
    corrections             = object$corrections,
    oob_summary             = object$oob_summary,
    overfitting_diagnostics = object$overfitting_diagnostics,
    selection_stability     = object$selection_stability,
    constraint_stability    = object$constraint_stability,
    top_combinations        = utils::head(object$selection_frequency, 5L),
    item_frequencies        = object$item_selection_frequency,
    settings                = object$settings,
    digits                  = digits
  )
  class(res) <- "summary.bootstrap_select_sum_roc_result"
  res
}

#' Print summary of bootstrap selection result
#'
#' @param x An object of class `"summary.bootstrap_select_sum_roc_result"`.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns `x`.
#' @export
print.summary.bootstrap_select_sum_roc_result <- function(x, ...) {
  digits <- x$digits
  cat("\n=======================================================\n")
  cat("  NCVROC Bootstrap Selection & Validation Summary\n")
  cat("=======================================================\n\n")

  sett <- x$settings
  cat(sprintf("Settings: model_sizes = [%s], reps = %d, resampling = %s, metric = %s\n",
              paste(sett$model_sizes, collapse = ", "),
              sett$bootstrap_reps, sett$resampling, sett$selection_metric))

  bs <- x$bootstrap_summary
  cat(sprintf("Replicates: total = %d, valid = %d, failed = %d, fallback = %d\n",
              bs$n_reps_requested, bs$n_reps_valid, bs$n_reps_failed, bs$n_reps_fallback))
  if (!is.null(bs$n_reps_oob_valid_auc) && !is.null(bs$n_reps_oob_valid_joint) &&
      (isTRUE(x$corrections$b632$requested) || isTRUE(x$corrections$b632plus$requested))) {
    cat(sprintf("OOB valid:  AUC = %d / %d, Joint (Se/Sp/Youden) = %d / %d\n",
                bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }
  cat("\n")

  cat("Final Selected Model (Full Data):\n")
  fm <- x$final_selected_model
  if (is.null(fm) || is.na(fm$items)) {
    cat("  No model selected\n\n")
  } else {
    cat(sprintf("  Items (%d): %s\n", fm$n_items, fm$items))
    cat(sprintf("  Cutoff:    %.4f\n\n", fm$cutoff))
  }

  cat("Diagnostic Performance & Optimism Correction:\n")
  display_cols <- c("apparent", "harrell", ".632", ".632+")
  print(round(x$performance_table[, display_cols, drop = FALSE], digits = digits))
  cat("\n")

  # Notes and explanations
  notes <- character(0)
  sett_methods <- x$settings$correction_methods
  if (!("632" %in% sett_methods)) {
    notes <- c(notes, ".632: Not requested in correction_methods.")
  }
  if (!("632plus" %in% sett_methods)) {
    notes <- c(notes, ".632+: Not requested in correction_methods.")
  }
  if (!is.null(x$overfitting_diagnostics$boundary_notes) && length(x$overfitting_diagnostics$boundary_notes) > 0L) {
    notes <- c(notes, x$overfitting_diagnostics$boundary_notes)
  }

  # Disclose conditional status and denominators
  if (isTRUE(x$corrections$harrell$is_conditional)) {
    notes <- c(notes, sprintf("Harrell: Conditional estimate (%d / %d valid selection resamples).",
                              bs$n_reps_valid, bs$n_reps_requested))
  }
  if (!is.null(x$corrections$b632) && isTRUE(x$corrections$b632$requested) && isTRUE(x$corrections$b632$is_conditional)) {
    notes <- c(notes, sprintf(".632: Conditional estimate (AUC based on %d / %d valid OOB resamples; Sensitivity/Specificity/Youden based on %d / %d valid OOB resamples).",
                              bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                              bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }
  if (!is.null(x$corrections$b632plus) && isTRUE(x$corrections$b632plus$requested) && isTRUE(x$corrections$b632plus$is_conditional)) {
    notes <- c(notes, sprintf(".632+: Conditional estimate (AUC based on %d / %d valid OOB resamples; Sensitivity/Specificity/Youden based on %d / %d valid OOB resamples).",
                              bs$n_reps_oob_valid_auc, bs$n_reps_requested,
                              bs$n_reps_oob_valid_joint, bs$n_reps_requested))
  }
  if (length(notes) > 0L) {
    cat("Correction Notes:\n")
    for (nt in notes) {
      cat(sprintf("  * %s\n", nt))
    }
    cat("\n")
  }
  cat("Note: .632 and .632+ use replicate-mean OOB aggregation on unthresholded sum scores (AUC)\n")
  cat("and fixed selected cutoffs (sensitivity/specificity). .632+ sensitivity and specificity\n")
  cat("use an NCVROC experimental extension with common weight derived from Youden index.\n\n")

  cat("Selection Stability:\n")
  st <- x$selection_stability
  cat(sprintf("  Exact match rate:   %.2f%%\n", st$exact_match_rate * 100))
  cat(sprintf("  Jaccard similarity: mean = %.4f, median = %.4f (IQR = %.4f)\n\n",
              st$jaccard_mean, st$jaccard_median, st$jaccard_iqr))

  if (!is.null(x$constraint_stability)) {
    cs <- x$constraint_stability
    cat("Clinical Constraint Feasibility:\n")
    cat(sprintf("  Bootstrap sample feasibility rate: %.2f%%\n", cs$training_feasible_rate_all * 100))
    cat(sprintf("  Original data feasibility rate:    %.2f%%\n", cs$test_feasible_rate_all * 100))
    cat(sprintf("  Fallback used rate:                %.2f%%\n\n", cs$fallback_rate_all * 100))
  }

  cat("Top Selected Item Combinations:\n")
  if (nrow(x$top_combinations) > 0L) {
    top_print <- x$top_combinations
    top_print$pct_valid <- paste0(round(top_print$proportion_valid * 100, 1), "%")
    top_print$pct_all   <- paste0(round(top_print$proportion_all * 100, 1), "%")
    print(top_print[, c("items", "n_items", "count", "pct_valid", "pct_all")], row.names = FALSE)
  } else {
    cat("  None\n")
  }
  cat("\n")

  invisible(x)
}

#' Plot bootstrap selection and validation diagnostics
#'
#' Base graphics visualization of item selection frequencies, top combinations,
#' Jaccard similarities, cutoffs, and optimism distributions.
#'
#' @param x An object of class `"bootstrap_select_sum_roc_result"`.
#' @param which Character vector of plots to draw: `"items"`, `"combos"`, `"jaccard"`,
#'   `"cutoff"`, `"optimism"`. Defaults to all five.
#' @param ask Logical, if TRUE prompt before each plot when interactive.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return Invisibly returns `x`.
#' @export
plot.bootstrap_select_sum_roc_result <- function(x, which = c("items", "combos", "jaccard", "cutoff", "optimism"),
                                                 ask = interactive() && length(which) > 1L, ...) {
  if (ask) {
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask), add = TRUE)
  }

  which <- match.arg(which, several.ok = TRUE)

  # 1. Item Selection Frequency
  if ("items" %in% which) {
    it_df <- x$item_selection_frequency
    ord <- order(it_df$frequency_valid, decreasing = TRUE)
    it_sub <- it_df[ord, , drop = FALSE]
    graphics::barplot(
      it_sub$frequency_valid * 100,
      names.arg = it_sub$item,
      las = 2,
      ylab = "Selection Frequency (%)",
      main = "Item Selection Frequency (Valid Replicates)",
      ylim = c(0, 100),
      col = "steelblue"
    )
    graphics::abline(h = c(25, 50, 75), lty = 2, col = "gray70")
  }

  # 2. Top Combinations
  if ("combos" %in% which) {
    top_c <- utils::head(x$selection_frequency, 10L)
    if (nrow(top_c) > 0L) {
      rev_top <- top_c[rev(seq_len(nrow(top_c))), , drop = FALSE]
      graphics::barplot(
        rev_top$proportion_valid * 100,
        names.arg = rev_top$items,
        horiz = TRUE,
        las = 1,
        xlab = "Selection Frequency (%)",
        main = "Top Selected Combinations",
        col = "coral"
      )
    }
  }

  # 3. Jaccard Similarity Distribution
  if ("jaccard" %in% which) {
    br <- x$bootstrap_results
    valid_j <- br$jaccard_similarity[br$selection_status %in% c("selected", "fallback_youden")]
    valid_j <- valid_j[!is.na(valid_j)]
    if (length(valid_j) > 0L) {
      graphics::hist(
        valid_j,
        breaks = seq(0, 1, by = 0.05),
        xlim = c(0, 1),
        xlab = "Jaccard Similarity to Full-Data Model",
        main = "Selection Stability: Jaccard Similarity",
        col = "mediumpurple",
        border = "white"
      )
      graphics::abline(v = mean(valid_j), col = "darkblue", lwd = 2, lty = 2)
    }
  }

  # 4. Cutoff Distribution
  if ("cutoff" %in% which) {
    br <- x$bootstrap_results
    valid_cuts <- br$selected_cutoff[br$selection_status %in% c("selected", "fallback_youden")]
    valid_cuts <- valid_cuts[!is.na(valid_cuts)]
    if (length(valid_cuts) > 0L) {
      graphics::hist(
        valid_cuts,
        xlab = "Selected Cutoff",
        main = "Selected Cutoff Distribution",
        col = "olivedrab3",
        border = "white"
      )
      ref_cut <- x$final_selected_model$cutoff
      if (!is.null(ref_cut) && !is.na(ref_cut)) {
        graphics::abline(v = ref_cut, col = "red", lwd = 2, lty = 2)
      }
    }
  }

  # 5. Optimism Distribution
  if ("optimism" %in% which) {
    br <- x$bootstrap_results
    valid_opt <- br$optimism_auc[br$selection_status %in% c("selected", "fallback_youden")]
    valid_opt <- valid_opt[!is.na(valid_opt)]
    if (length(valid_opt) > 0L) {
      graphics::hist(
        valid_opt,
        xlab = "Optimism (Bootstrap AUC - Original AUC)",
        main = "AUC Optimism Distribution",
        col = "skyblue",
        border = "white"
      )
      graphics::abline(v = 0, col = "gray40", lwd = 1, lty = 1)
      graphics::abline(v = mean(valid_opt), col = "red", lwd = 2, lty = 2)
    }
  }

  invisible(x)
}
