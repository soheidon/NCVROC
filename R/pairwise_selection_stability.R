# ==============================================================================
# Pairwise Inclusion & Substitution Stability (v0.34.0 Phase 4)
# ==============================================================================

#' Compute Pairwise Inclusion Frequency (PIF) and Substitution Metrics
#'
#' Computes exact pairwise joint selection (\code{both}), mutual exclusion
#' (\code{neither}), asymmetric (\code{i_only}, \code{j_only}), XOR
#' (\code{exactly_one}), and conditional substitution (\code{(i_only + j_only) /
#' (i or j)}) over a common set of \code{M} valid selected models.
#'
#' @param x A list of selected feature character vectors, a binary \code{M x d}
#'   selection matrix, or an NCVROC result object.
#' @param eligible_features Character vector of all eligible features in fixed universe.
#' @param valid_mask Optional logical vector indicating valid iterations.
#'
#' @return An object of class \code{"pairwise_selection_stability_result"}.
#' @export
pairwise_selection_stability <- function(x,
                                         eligible_features = NULL,
                                         valid_mask = NULL) {
  extracted <- .v034_extract_from_object(x, eligible_features = eligible_features)
  resampling_method <- "external_sets"
  sample_fraction <- NA_real_
  stratified <- NA

  if (!is.null(extracted)) {
    raw_sets <- extracted$selected_sets
    elig <- extracted$eligible_features
    if (is.null(valid_mask)) valid_mask <- extracted$valid_mask
    resampling_method <- extracted$resampling_method
    sample_fraction <- extracted$sample_fraction
    stratified <- extracted$stratified
  } else {
    raw_sets <- x
    elig <- eligible_features
  }

  canon <- .v034_canonicalize_selected_sets(
    selected_sets     = raw_sets,
    eligible_features = elig,
    valid_mask        = valid_mask
  )
  z_mat <- .v034_build_selection_matrix(
    canon_sets        = canon$selected_sets,
    eligible_features = canon$eligible_features,
    valid_mask        = canon$valid_mask
  )

  m_valid <- nrow(z_mat)
  d <- ncol(z_mat)
  f_names <- canon$eligible_features
  n_req <- length(canon$selected_sets)

  if (d < 1L) {
    stop("`eligible_features` must contain at least one feature.", call. = FALSE)
  }

  if (m_valid == 0L) {
    na_mat <- matrix(NA_real_, nrow = d, ncol = d, dimnames = list(f_names, f_names))
    zero_int <- matrix(0L, nrow = d, ncol = d, dimnames = list(f_names, f_names))
    meta <- .v034_make_metadata(
      stability_target     = "pairwise_selection",
      eligible_features    = f_names,
      resampling_method    = resampling_method,
      sample_fraction      = sample_fraction,
      stratified           = stratified,
      requested_iterations = n_req,
      completed_iterations = n_req,
      valid_iterations     = 0L,
      failed_iterations    = n_req,
      backend              = "R"
    )
    return(structure(
      list(
        status                          = "undefined_no_valid_sets",
        M                               = 0L,
        d                               = as.integer(d),
        vif                             = stats::setNames(rep(NA_real_, d), f_names),
        both_count                      = zero_int,
        neither_count                   = zero_int,
        i_only_count                    = zero_int,
        j_only_count                    = zero_int,
        pif_matrix                      = na_mat,
        xor_matrix                      = na_mat,
        conditional_substitution_matrix = na_mat,
        expected_independent_matrix     = na_mat,
        independence_note               = "Expected VIF_i * VIF_j is descriptive only; model-size constraints induce structural dependence.",
        resource_estimate               = list(d = as.integer(d), pair_cells = as.numeric(d) * as.numeric(d)),
        metadata                        = meta
      ),
      class = "pairwise_selection_stability_result"
    ))
  }

  # Exact matrix crossproducts over binary Z (M x d)
  both_cnt <- crossprod(z_mat)
  storage.mode(both_cnt) <- "integer"
  dimnames(both_cnt) <- list(f_names, f_names)

  item_cnt <- diag(both_cnt)
  i_only_cnt <- outer(item_cnt, rep(0L, d), "+") - both_cnt
  storage.mode(i_only_cnt) <- "integer"
  dimnames(i_only_cnt) <- list(f_names, f_names)

  j_only_cnt <- t(i_only_cnt)
  neither_cnt <- m_valid - (both_cnt + i_only_cnt + j_only_cnt)
  storage.mode(neither_cnt) <- "integer"
  dimnames(neither_cnt) <- list(f_names, f_names)

  vif <- as.numeric(item_cnt) / m_valid
  names(vif) <- f_names

  pif_mat <- both_cnt / m_valid
  xor_mat <- (i_only_cnt + j_only_cnt) / m_valid
  either_cnt <- both_cnt + i_only_cnt + j_only_cnt
  cond_sub_mat <- ifelse(either_cnt > 0L, (i_only_cnt + j_only_cnt) / either_cnt, NA_real_)
  dimnames(cond_sub_mat) <- list(f_names, f_names)
  exp_indep_mat <- outer(vif, vif, "*")
  dimnames(exp_indep_mat) <- list(f_names, f_names)

  size_dist <- table(factor(rowSums(z_mat)))
  meta <- .v034_make_metadata(
    stability_target           = "pairwise_selection",
    eligible_features          = f_names,
    resampling_method          = resampling_method,
    sample_fraction            = sample_fraction,
    stratified                 = stratified,
    requested_iterations       = n_req,
    completed_iterations       = n_req,
    valid_iterations           = m_valid,
    failed_iterations          = n_req - m_valid,
    selected_size_distribution = size_dist,
    backend                    = "R"
  )

  structure(
    list(
      status                          = "valid",
      M                               = as.integer(m_valid),
      d                               = as.integer(d),
      vif                             = vif,
      both_count                      = both_cnt,
      neither_count                   = neither_cnt,
      i_only_count                    = i_only_cnt,
      j_only_count                    = j_only_cnt,
      pif_matrix                      = pif_mat,
      xor_matrix                      = xor_mat,
      conditional_substitution_matrix = cond_sub_mat,
      expected_independent_matrix     = exp_indep_mat,
      independence_note               = paste0(
        "Expected VIF_i * VIF_j is a descriptive reference under independence; ",
        "fixed or bounded model sizes induce structural dependence and deviations do not imply statistical competition or causality."
      ),
      resource_estimate               = list(
        d               = as.integer(d),
        pair_cells      = as.numeric(d) * as.numeric(d),
        estimated_bytes = as.numeric(d) * as.numeric(d) * 8.0 * 8.0
      ),
      metadata                        = meta
    ),
    class = "pairwise_selection_stability_result"
  )
}

#' @export
print.pairwise_selection_stability_result <- function(x, digits = 4L, top_n = 10L, ...) {
  cat("NCVROC Pairwise Inclusion & Substitution Stability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("Eligible features (d) : %d | Valid sets (M) : %d | Status: %s\n",
              x$d, x$M, x$status))
  if (identical(x$status, "valid") && x$d >= 2L) {
    idx <- which(upper.tri(x$pif_matrix), arr.ind = TRUE)
    f_names <- names(x$vif)
    df <- data.frame(
      item_i   = f_names[idx[, 1L]],
      item_j   = f_names[idx[, 2L]],
      pif      = x$pif_matrix[idx],
      xor_freq = x$xor_matrix[idx],
      cond_sub = x$conditional_substitution_matrix[idx],
      exp_ind  = x$expected_independent_matrix[idx],
      stringsAsFactors = FALSE
    )
    ord <- order(-df$pif, -df$xor_freq, df$item_i, df$item_j)
    show_df <- utils::head(df[ord, , drop = FALSE], n = as.integer(top_n))
    cat(sprintf("\nTop %d pairs by PIF (out of %d pairs; full d x d matrix stored):\n",
                nrow(show_df), nrow(df)))
    print(show_df, row.names = FALSE, digits = digits)
  }
  invisible(x)
}

#' @export
summary.pairwise_selection_stability_result <- function(object, ...) {
  structure(object, class = c("summary.pairwise_selection_stability_result", class(object)))
}

#' @export
print.summary.pairwise_selection_stability_result <- function(x, digits = 4L, top_n = 10L, ...) {
  print.pairwise_selection_stability_result(x, digits = digits, top_n = top_n, ...)
  cat(sprintf("\nNote: %s\n", x$independence_note))
  invisible(x)
}

#' @export
plot.pairwise_selection_stability_result <- function(x,
                                                     type = c("pif", "xor", "vif"),
                                                     display_features = NULL,
                                                     main = NULL,
                                                     ...) {
  type <- match.arg(type)
  if (!identical(x$status, "valid")) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No valid selection sets to plot.")
    return(invisible(x))
  }
  feats <- names(x$vif)
  if (!is.null(display_features)) {
    feats <- intersect(display_features, feats)
    if (length(feats) < 1L) stop("No valid `display_features` found.", call. = FALSE)
  }

  if (identical(type, "vif")) {
    v <- sort(x$vif[feats], decreasing = TRUE)
    if (is.null(main)) main <- sprintf("Variable Inclusion Frequency (displaying %d/%d)", length(v), x$d)
    graphics::barplot(rev(v), horiz = TRUE, las = 1, xlim = c(0, 1), main = main, xlab = "VIF (conditional on valid sets)", ...)
    return(invisible(x))
  }

  mat <- if (identical(type, "pif")) x$pif_matrix[feats, feats, drop = FALSE] else x$xor_matrix[feats, feats, drop = FALSE]
  if (is.null(main)) {
    lbl <- if (identical(type, "pif")) "Pairwise Inclusion Frequency (PIF)" else "Pairwise XOR Substitution Frequency"
    main <- sprintf("%s (displaying %d/%d)", lbl, length(feats), x$d)
  }
  graphics::image(
    x    = seq_along(feats),
    y    = seq_along(feats),
    z    = t(mat[rev(seq_along(feats)), , drop = FALSE]),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    ...
  )
  graphics::axis(1, at = seq_along(feats), labels = feats, las = 2, cex.axis = 0.8)
  graphics::axis(2, at = seq_along(feats), labels = rev(feats), las = 1, cex.axis = 0.8)
  invisible(x)
}
