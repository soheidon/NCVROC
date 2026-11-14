# ==============================================================================
# Nogueira Feature Selection Stability Index (v0.34.0 Phase 4)
# ==============================================================================

#' Compute Nogueira et al. (2018) Feature Selection Stability Index
#'
#' Computes the chance-corrected feature-set selection stability index
#' \eqn{\hat{\Phi}} of Nogueira, Sechidis, and Brown (2018) over a fixed
#' \code{eligible_features} universe. Accepts a list of character item sets, a
#' binary selection matrix \code{Z}, or an existing NCVROC result object
#' (\code{subsampling_stability_result}, \code{classification_stability_result},
#' \code{cross_size_nested_cv_result}, \code{nested_sum_roc_result}, or
#' \code{ncvroc_analysis}).
#'
#' Preserves finite-sample negative estimates down to \eqn{-1/(M - 1)} without
#' clipping. Returns \code{NA_real_} with \code{status = "undefined"} when
#' \eqn{M < 2}, \eqn{d = 0}, \eqn{\bar{k} = 0} (all empty), \eqn{\bar{k} = d}
#' (all full), or the denominator is non-positive.
#'
#' @param x A list of selected feature character vectors, a binary \code{M x d}
#'   selection matrix, or an NCVROC result object.
#' @param eligible_features Character vector of all eligible features in fixed
#'   universe. Required when \code{x} is a list of sets unless inferred from a
#'   structured result object.
#' @param valid_mask Optional logical vector indicating valid iterations.
#' @param ci Logical; compute asymptotic confidence interval (default \code{FALSE}).
#' @param conf_level Confidence level if \code{ci = TRUE} (default \code{0.95}).
#' @param assume_independent Logical; set \code{TRUE} only when selection sets
#'   arise from independent samples rather than overlapping resamples (default \code{FALSE}).
#'
#' @return An object of class \code{"nogueira_stability_result"}.
#' @export
feature_selection_stability <- function(x,
                                        eligible_features = NULL,
                                        valid_mask = NULL,
                                        ci = FALSE,
                                        conf_level = 0.95,
                                        assume_independent = FALSE) {
  if (!is.logical(ci) || length(ci) != 1L || is.na(ci)) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be in (0, 1).", call. = FALSE)
  }

  extracted <- .v034_extract_from_object(x, eligible_features = eligible_features)
  resampling_method <- "external_sets"
  sample_fraction <- NA_real_
  stratified <- NA
  independent_units <- isTRUE(assume_independent)

  if (!is.null(extracted)) {
    raw_sets <- extracted$selected_sets
    elig <- extracted$eligible_features
    if (is.null(valid_mask)) valid_mask <- extracted$valid_mask
    resampling_method <- extracted$resampling_method
    sample_fraction <- extracted$sample_fraction
    stratified <- extracted$stratified
    if (!isTRUE(assume_independent)) {
      independent_units <- isTRUE(extracted$independent_units)
    }
  } else {
    raw_sets <- x
    elig <- eligible_features
  }

  canon <- .v034_canonicalize_selected_sets(
    selected_sets     = raw_sets,
    eligible_features = elig,
    valid_mask        = valid_mask
  )
  n_req <- length(canon$selected_sets)
  z_mat <- .v034_build_selection_matrix(
    canon_sets        = canon$selected_sets,
    eligible_features = canon$eligible_features,
    valid_mask        = canon$valid_mask
  )

  m_valid <- nrow(z_mat)
  d <- ncol(z_mat)
  set_sizes <- if (m_valid > 0L) rowSums(z_mat) else integer(0)
  k_bar <- if (m_valid > 0L) mean(set_sizes) else NA_real_
  p_hat <- if (m_valid > 0L && d > 0L) colMeans(z_mat) else numeric(0)
  if (d > 0L) names(p_hat) <- canon$eligible_features

  status <- "valid"
  undefined_reason <- NA_character_
  phi_hat <- NA_real_
  lower_bound <- if (m_valid >= 2L) -1.0 / (m_valid - 1.0) else NA_real_

  if (d < 1L) {
    status <- "undefined"
    undefined_reason <- "empty_eligible_universe"
  } else if (m_valid < 2L) {
    status <- "undefined"
    undefined_reason <- "insufficient_valid_sets"
  } else if (k_bar <= 0) {
    status <- "undefined"
    undefined_reason <- "all_sets_empty_denominator_zero"
  } else if (k_bar >= d) {
    status <- "undefined"
    undefined_reason <- "all_sets_full_denominator_zero"
  } else {
    s2_f <- (m_valid / (m_valid - 1.0)) * p_hat * (1.0 - p_hat)
    numer <- mean(s2_f)
    denom <- (k_bar / d) * (1.0 - k_bar / d)
    if (!is.finite(denom) || denom <= 0) {
      status <- "undefined"
      undefined_reason <- "denominator_zero_or_nonpositive"
    } else {
      phi_hat <- 1.0 - (numer / denom)
    }
  }

  ci_lower <- NA_real_
  ci_upper <- NA_real_
  ci_status <- "not_requested"
  warnings_vec <- character(0)

  if (isTRUE(ci)) {
    if (!identical(status, "valid")) {
      ci_status <- "undefined_estimate"
    } else if (!isTRUE(independent_units)) {
      ci_status <- "not_applicable_dependent_resamples"
      w_msg <- paste0(
        "Asymptotic Nogueira CI assumes independent selection sets; omitted because ",
        "resampling replicates share overlapping subjects (set `assume_independent = TRUE` only for i.i.d. sets)."
      )
      warnings_vec <- c(warnings_vec, w_msg)
      warning(w_msg, call. = FALSE)
    } else {
      # Nogueira et al. (2018) Theorem 7 asymptotic variance
      denom <- (k_bar / d) * (1.0 - k_bar / d)
      k_i <- rowSums(z_mat)
      phi_i <- vapply(seq_len(m_valid), function(i) {
        num_i <- sum(z_mat[i, ] * (1.0 - p_hat) + (1.0 - z_mat[i, ]) * p_hat) / d
        val_i <- (1.0 / denom) * (
          (1.0 - 2.0 * (k_bar / d)) * (1.0 - phi_hat) * (k_i[i] - k_bar) / d +
            (k_bar / d) * (1.0 - k_bar / d) -
            (sum(z_mat[i, ] * p_hat) / d)
        )
        val_i
      }, numeric(1))
      # Direct influence function from Nogueira (2018) Section 4:
      # phi_i = (1/denom) * ( (1/d) * sum_f (1 - 2*p_hat_f)*z_if - (1 - phi_hat)*(1 - 2*k_bar/d)*(k_i/d) )
      # Note: var(phi_hat) = (4 / M^2) * sum((phi_i - mean(phi_i))^2) in Nogueira (2018) Eq. (11)
      psi_i <- (1.0 / denom) * (
        as.numeric(z_mat %*% (1.0 - 2.0 * p_hat)) / d +
          (1.0 - phi_hat) * (1.0 - 2.0 * k_bar / d) * (k_i / d)
      )
      var_phi <- (4.0 / (m_valid * (m_valid - 1.0))) * sum((psi_i - mean(psi_i))^2)
      se_phi <- sqrt(max(0.0, var_phi))
      z_crit <- stats::qnorm(1.0 - (1.0 - conf_level) / 2.0)
      ci_lower <- phi_hat - z_crit * se_phi
      ci_upper <- phi_hat + z_crit * se_phi
      ci_status <- "valid_asymptotic_independent"
    }
  }

  size_dist <- if (m_valid > 0L) table(factor(set_sizes)) else table(integer(0))
  meta <- .v034_make_metadata(
    stability_target           = "item_selection_nogueira",
    eligible_features          = canon$eligible_features,
    resampling_method          = resampling_method,
    sample_fraction            = sample_fraction,
    stratified                 = stratified,
    requested_iterations       = n_req,
    completed_iterations       = n_req,
    valid_iterations           = m_valid,
    failed_iterations          = n_req - m_valid,
    selected_size_distribution = size_dist,
    backend                    = "R",
    warnings                   = warnings_vec
  )

  structure(
    list(
      stability_index           = phi_hat,
      status                    = status,
      undefined_reason          = undefined_reason,
      finite_sample_lower_bound = lower_bound,
      d                         = as.integer(d),
      M                         = as.integer(m_valid),
      k_bar                     = k_bar,
      feature_frequency         = p_hat,
      ci                        = if (isTRUE(ci)) c(lower = ci_lower, upper = ci_upper) else NULL,
      ci_status                 = ci_status,
      conf_level                = conf_level,
      selection_matrix          = z_mat,
      metadata                  = meta
    ),
    class = "nogueira_stability_result"
  )
}

#' @export
print.nogueira_stability_result <- function(x, digits = 4L, ...) {
  cat("NCVROC Nogueira Feature Selection Stability\n")
  cat("-------------------------------------------\n")
  cat(sprintf("Eligible features (d) : %d\n", x$d))
  cat(sprintf("Valid sets (M)        : %d (requested: %d, failed: %d)\n",
              x$M, x$metadata$requested_iterations, x$metadata$failed_iterations))
  cat(sprintf("Mean selected size    : %s\n",
              if (!is.na(x$k_bar)) formatC(x$k_bar, digits = digits, format = "f") else "NA"))
  if (identical(x$status, "valid")) {
    cat(sprintf("Nogueira index (Phi)  : %s (finite-sample min = %s)\n",
                formatC(x$stability_index, digits = digits, format = "f"),
                formatC(x$finite_sample_lower_bound, digits = digits, format = "f")))
  } else {
    cat(sprintf("Nogueira index (Phi)  : NA [status: %s (%s)]\n", x$status, x$undefined_reason))
  }
  if (!is.null(x$ci)) {
    cat(sprintf("Confidence interval   : [%s, %s] (%s)\n",
                if (!is.na(x$ci[1])) formatC(x$ci[1], digits = digits, format = "f") else "NA",
                if (!is.na(x$ci[2])) formatC(x$ci[2], digits = digits, format = "f") else "NA",
                x$ci_status))
  }
  invisible(x)
}

#' @export
summary.nogueira_stability_result <- function(object, ...) {
  structure(object, class = c("summary.nogueira_stability_result", class(object)))
}

#' @export
print.summary.nogueira_stability_result <- function(x, digits = 4L, top_n = 15L, ...) {
  print.nogueira_stability_result(x, digits = digits, ...)
  if (length(x$feature_frequency) > 0L) {
    ord <- order(-x$feature_frequency, names(x$feature_frequency))
    show_n <- min(length(ord), as.integer(top_n))
    df <- data.frame(
      feature   = names(x$feature_frequency)[ord[seq_len(show_n)]],
      frequency = as.numeric(x$feature_frequency[ord[seq_len(show_n)]]),
      stringsAsFactors = FALSE
    )
    cat(sprintf("\nTop %d feature selection frequencies (conditional on M = %d valid sets):\n",
                show_n, x$M))
    print(df, row.names = FALSE, digits = digits)
  }
  invisible(x)
}
