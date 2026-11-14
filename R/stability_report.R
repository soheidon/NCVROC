# ==============================================================================
# Common Stability Report & Procedure Comparison (v0.34.0 Phase 4)
# ==============================================================================

#' Build a Multi-Target Model Selection Stability Report
#'
#' Aggregates available stability diagnostics across distinct estimand targets
#' (\code{subsampling_stability}, \code{feature_selection_stability},
#' \code{pairwise_selection_stability}, \code{near_optimal_candidates}, and
#' \code{classification_stability_roc}) under common metadata without compressing
#' distinct estimands into a single composite score. Explicitly marks
#' coefficient stability (RCB/RMSDR) as \code{"not_applicable"} because NCVROC
#' uses unweighted sum-score models without per-item fitted regression weights.
#'
#' @param subsampling Optional \code{"subsampling_stability_result"}.
#' @param nogueira Optional \code{"nogueira_stability_result"} (inferred from
#'   \code{subsampling$nogueira} if omitted).
#' @param pairwise Optional \code{"pairwise_selection_stability_result"}
#'   (inferred from \code{subsampling$pairwise} if omitted).
#' @param near_optimal Optional \code{"near_optimal_candidates_result"}.
#' @param classification_stability Optional \code{"classification_stability_result"}.
#'
#' @return An object of class \code{"model_selection_stability_report"}.
#' @export
model_selection_stability_report <- function(subsampling = NULL,
                                             nogueira = NULL,
                                             pairwise = NULL,
                                             near_optimal = NULL,
                                             classification_stability = NULL) {
  if (!is.null(subsampling) && !inherits(subsampling, "subsampling_stability_result")) {
    stop("`subsampling` must be a `subsampling_stability_result` or NULL.", call. = FALSE)
  }
  if (is.null(nogueira) && !is.null(subsampling)) {
    nogueira <- subsampling$nogueira
  }
  if (is.null(pairwise) && !is.null(subsampling)) {
    pairwise <- subsampling$pairwise
  }
  if (!is.null(nogueira) && !inherits(nogueira, "nogueira_stability_result")) {
    stop("`nogueira` must be a `nogueira_stability_result` or NULL.", call. = FALSE)
  }
  if (!is.null(pairwise) && !inherits(pairwise, "pairwise_selection_stability_result")) {
    stop("`pairwise` must be a `pairwise_selection_stability_result` or NULL.", call. = FALSE)
  }
  if (!is.null(near_optimal) && !inherits(near_optimal, "near_optimal_candidates_result")) {
    stop("`near_optimal` must be a `near_optimal_candidates_result` or NULL.", call. = FALSE)
  }
  if (!is.null(classification_stability) && !inherits(classification_stability, "classification_stability_result")) {
    stop("`classification_stability` must be a `classification_stability_result` or NULL.", call. = FALSE)
  }

  targets <- list(
    item_selection_nogueira = nogueira,
    subsampling_vif_msf     = subsampling,
    pairwise_selection      = pairwise,
    near_optimal_candidates = near_optimal,
    classification_stability = classification_stability,
    coefficient_stability   = list(
      stability_target = "coefficient_stability",
      status           = "not_applicable",
      rcb_status       = "not_applicable",
      rmsd_r_status    = "not_applicable",
      reason           = paste0(
        "NCVROC uses unweighted sum-score models without per-item fitted regression coefficients; ",
        "coefficient distributions, RCB, and RMSDR are not applicable."
      )
    )
  )

  available_targets <- names(targets)[!vapply(targets, is.null, logical(1))]

  structure(
    list(
      schema_version    = "0.34.0",
      available_targets = available_targets,
      targets           = targets,
      separation_note   = paste0(
        "Each stability target measures a distinct estimand and is reported separately. ",
        "No composite stability score is computed, and high stability does not imply high predictive performance or clinical validity."
      )
    ),
    class = "model_selection_stability_report"
  )
}

#' @export
print.model_selection_stability_report <- function(x, digits = 4L, ...) {
  cat("NCVROC Generalized Model Selection Stability Report\n")
  cat("---------------------------------------------------\n")
  cat(sprintf("Schema version    : %s\n", x$schema_version))
  cat(sprintf("Targets present   : %s\n", paste(x$available_targets, collapse = ", ")))

  if (!is.null(x$targets$item_selection_nogueira)) {
    ng <- x$targets$item_selection_nogueira
    cat(sprintf("  [Nogueira]      : %s (d=%d, M=%d)\n",
                if (identical(ng$status, "valid")) formatC(ng$stability_index, digits = digits, format = "f") else sprintf("NA (%s)", ng$undefined_reason),
                ng$d, ng$M))
  }
  if (!is.null(x$targets$subsampling_vif_msf)) {
    sb <- x$targets$subsampling_vif_msf
    cat(sprintf("  [Subsampling]   : B=%d, valid=%d, frac=%.3f, distinct_models=%d\n",
                sb$metadata$requested_iterations, sb$metadata$valid_iterations,
                sb$metadata$sample_fraction, sb$msf_summary$n_distinct_models))
  }
  if (!is.null(x$targets$pairwise_selection)) {
    pw <- x$targets$pairwise_selection
    cat(sprintf("  [Pairwise]      : status=%s, d=%d, M=%d\n", pw$status, pw$d, pw$M))
  }
  if (!is.null(x$targets$near_optimal_candidates)) {
    no <- x$targets$near_optimal_candidates
    cat(sprintf("  [Near-optimal]  : count=%d (delta=%s, best=%s)\n",
                no$exact_near_optimal_count, formatC(no$delta, digits = digits, format = "f"),
                no$best_candidate))
  }
  if (!is.null(x$targets$classification_stability)) {
    cs <- x$targets$classification_stability
    cat(sprintf("  [Classif. CII]  : mean_cii=%s (%s)\n",
                formatC(cs$cii_summary$mean_cii, digits = digits, format = "f"), cs$mode))
  }
  cat(sprintf("  [Coefficients]  : %s (%s)\n",
              x$targets$coefficient_stability$status,
              x$targets$coefficient_stability$reason))
  cat(sprintf("\nNote: %s\n", x$separation_note))
  invisible(x)
}

#' @export
summary.model_selection_stability_report <- function(object, ...) {
  object
}

#' Compare Two Subsampling Stability Results Across Selection Procedures
#'
#' Compares two \code{"subsampling_stability_result"} objects and verifies
#' whether they share identical data, eligible feature universe, sample
#' fraction, stratification, and resampling indices for a paired comparison, or
#' marks the comparison as descriptive only with explicit mismatch reasons.
#'
#' @param result_a First \code{"subsampling_stability_result"}.
#' @param result_b Second \code{"subsampling_stability_result"}.
#'
#' @return An object of class \code{"compare_selection_stability_result"}.
#' @export
compare_selection_stability <- function(result_a, result_b) {
  if (!inherits(result_a, "subsampling_stability_result") ||
      !inherits(result_b, "subsampling_stability_result")) {
    stop("Both `result_a` and `result_b` must be `subsampling_stability_result` objects.", call. = FALSE)
  }

  mismatches <- character(0)
  if (!identical(result_a$data_digest, result_b$data_digest)) {
    mismatches <- c(mismatches, "different_data")
  }
  if (!identical(sort(result_a$metadata$eligible_features), sort(result_b$metadata$eligible_features))) {
    mismatches <- c(mismatches, "different_eligible_features")
  }
  if (!isTRUE(all.equal(result_a$metadata$sample_fraction, result_b$metadata$sample_fraction))) {
    mismatches <- c(mismatches, "different_sample_fraction")
  }
  if (!identical(result_a$metadata$stratified, result_b$metadata$stratified)) {
    mismatches <- c(mismatches, "different_stratification")
  }
  if (!identical(result_a$resampling_indices, result_b$resampling_indices)) {
    mismatches <- c(mismatches, "different_resampling_indices")
  }

  comparability <- if (length(mismatches) == 0L) {
    "paired_identical_resamples"
  } else {
    "descriptive_only_mismatched_design"
  }

  common_feats <- sort(unique(c(result_a$metadata$eligible_features, result_b$metadata$eligible_features)))
  vif_a <- stats::setNames(result_a$vif_table$frequency, result_a$vif_table$feature)[common_feats]
  vif_b <- stats::setNames(result_b$vif_table$frequency, result_b$vif_table$feature)[common_feats]

  vif_comp <- data.frame(
    feature = common_feats,
    vif_a   = as.numeric(vif_a),
    vif_b   = as.numeric(vif_b),
    diff    = as.numeric(vif_b) - as.numeric(vif_a),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      comparability    = comparability,
      mismatch_reasons = mismatches,
      procedure_a      = result_a$settings$selection_procedure,
      procedure_b      = result_b$settings$selection_procedure,
      nogueira_a       = result_a$nogueira$stability_index,
      nogueira_b       = result_b$nogueira$stability_index,
      vif_comparison   = vif_comp
    ),
    class = "compare_selection_stability_result"
  )
}

#' @export
print.compare_selection_stability_result <- function(x, digits = 4L, ...) {
  cat("NCVROC Selection Stability Comparison\n")
  cat("-------------------------------------\n")
  cat(sprintf("Comparability     : %s\n", x$comparability))
  if (length(x$mismatch_reasons) > 0L) {
    cat(sprintf("Mismatch reasons  : %s\n", paste(x$mismatch_reasons, collapse = ", ")))
  }
  cat(sprintf("Procedures        : A = %s vs B = %s\n", x$procedure_a, x$procedure_b))
  cat(sprintf("Nogueira Phi      : A = %s vs B = %s\n",
              formatC(x$nogueira_a, digits = digits, format = "f"),
              formatC(x$nogueira_b, digits = digits, format = "f")))
  print(x$vif_comparison, row.names = FALSE, digits = digits)
  invisible(x)
}
