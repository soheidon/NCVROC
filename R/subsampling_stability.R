# ==============================================================================
# Subsampling Stability (VIF, MSF, Nogueira, Pairwise) (v0.34.0 Phase 4)
# ==============================================================================

#' Compute subsample size m from N and fraction using m_rule
#' @keywords internal
#' @noRd
.v034_apply_m_rule <- function(n, fraction, m_rule = c("floor", "round", "ceiling")) {
  m_rule <- match.arg(m_rule)
  raw <- n * fraction
  m <- switch(m_rule,
    floor   = floor(raw),
    round   = round(raw),
    ceiling = ceiling(raw)
  )
  as.integer(m)
}

#' Generate non-replacement subsampling index schedule
#' @keywords internal
#' @noRd
.v034_generate_subsamples <- function(y_obs,
                                      B,
                                      sample_fraction,
                                      stratified = FALSE,
                                      m_rule = "floor",
                                      seed = NULL) {
  n_obs <- length(y_obs)
  pos_idx <- which(y_obs == 1L)
  neg_idx <- which(y_obs == 0L)

  if (isTRUE(stratified)) {
    m_pos <- .v034_apply_m_rule(length(pos_idx), sample_fraction, m_rule)
    m_neg <- .v034_apply_m_rule(length(neg_idx), sample_fraction, m_rule)
    if (m_pos < 1L || m_neg < 1L) {
      stop(sprintf(
        "Stratified subsampling with fraction %.3f (%s) yields m_pos=%d, m_neg=%d (must be >= 1 each).",
        sample_fraction, m_rule, m_pos, m_neg
      ), call. = FALSE)
    }
    m_total <- m_pos + m_neg
  } else {
    m_total <- .v034_apply_m_rule(n_obs, sample_fraction, m_rule)
    if (m_total < 2L) {
      stop(sprintf("Subsample size m=%d is < 2.", m_total), call. = FALSE)
    }
    m_pos <- NA_integer_
    m_neg <- NA_integer_
  }

  idx_list <- vector("list", B)
  for (b in seq_len(B)) {
    iter_seed <- .v033_derive_iter_seed(seed, b)
    idx_list[[b]] <- .v033_with_seed(iter_seed, {
      if (isTRUE(stratified)) {
        s_pos <- sample(pos_idx, size = m_pos, replace = FALSE)
        s_neg <- sample(neg_idx, size = m_neg, replace = FALSE)
        sort(c(s_pos, s_neg))
      } else {
        sort(sample.int(n_obs, size = m_total, replace = FALSE))
      }
    })
  }

  list(
    indices = idx_list,
    m_total = m_total,
    m_pos   = m_pos,
    m_neg   = m_neg
  )
}

#' Subsampling-Based Model Selection Stability (VIF, MSF, Nogueira, Pairwise)
#'
#' Evaluates model-selection stability by drawing \code{B} subsamples of size
#' \eqn{m} \strong{without replacement} (distinct from bootstrap) and re-running
#' the complete data-dependent selection procedure inside each subsample.
#' Computes Variable Inclusion Frequency (VIF), Model Selection Frequency (MSF),
#' Nogueira's \eqn{\hat{\Phi}} index, and Pairwise Inclusion/Substitution tables
#' over the exact same valid iteration set.
#'
#' @param data A \code{data.frame} containing outcome and item columns.
#' @param outcome Character name of the binary outcome column.
#' @param items Optional character vector of item column names.
#' @param model_sizes Optional integer vector of candidate model sizes.
#' @param min_items Minimum number of items if \code{model_sizes} is NULL.
#' @param max_items Maximum number of items if \code{model_sizes} is NULL.
#' @param item_count Optional formula-like item count string.
#' @param selection_result Optional baseline result object for configuration reconstruction.
#' @param selection_procedure One of \code{"cv"}, \code{"apparent"}, or \code{"nested_cv"}.
#' @param B Number of subsampling iterations (default \code{50L}).
#' @param sample_fraction Subsampling fraction \eqn{r \in (0, 1)} (default \code{0.5};
#'   fraction-specific estimand, not interchangeable across fractions).
#' @param stratified Logical; stratified without-replacement subsampling (default \code{FALSE}).
#' @param m_rule Integer rounding rule for \eqn{m = r \cdot N}: \code{"floor"},
#'   \code{"round"}, or \code{"ceiling"}.
#' @param resampling_indices Optional list of integer row-index vectors (without
#'   replacement) to share identical subsamples across procedures.
#' @param cutoff_method Operating point method.
#' @param sensitivity_min Minimum sensitivity constraint if required.
#' @param specificity_min Minimum specificity constraint if required.
#' @param selection_metric Selection metric (\code{"auc"}, \code{"youden"},
#'   \code{"sensitivity"}, \code{"specificity"}, \code{"accuracy"}).
#' @param prefer_fewer_items Logical; break ties in favor of fewer items (default \code{TRUE}).
#' @param folds Number of CV / outer folds (default \code{5L}).
#' @param inner_folds Number of inner folds for \code{"nested_cv"} (default \code{4L}).
#' @param repeats Number of CV repeats (default \code{1L}).
#' @param outer_repeats Number of outer repeats for \code{"nested_cv"} (default \code{1L}).
#' @param inner_repeats Number of inner repeats for \code{"nested_cv"} (default \code{1L}).
#' @param cv_stratified Logical; use stratified folds inside CV (default \code{TRUE}).
#' @param no_feasible Handling when no candidate meets constraints.
#' @param positive_label Positive outcome value (default \code{1}).
#' @param negative_label Negative outcome value (default \code{0}).
#' @param top_k_models Optional integer cap on stored MSF rows (exact total counts
#'   and truncation flag are always preserved).
#' @param engine \code{"Rcpp"} or \code{"R"}.
#' @param parallel Parallel mode (\code{"none"}, \code{"threads"}, \code{"auto"}).
#' @param n_workers Optional worker/thread count.
#' @param seed Optional integer seed.
#' @param result_file Optional RDS checkpoint path.
#' @param checkpoint_every Checkpoint interval in iterations (default \code{25L}).
#' @param force_recompute Overwrite incompatible checkpoint (default \code{FALSE}).
#' @param progress Show progress messages (default \code{interactive()}).
#'
#' @return An object of class \code{"subsampling_stability_result"}.
#' @export
subsampling_stability <- function(
    data,
    outcome,
    items                 = NULL,
    model_sizes           = NULL,
    min_items             = 1L,
    max_items             = 4L,
    item_count            = NULL,
    selection_result      = NULL,
    selection_procedure   = c("cv", "apparent", "nested_cv"),
    B                     = 50L,
    sample_fraction       = 0.5,
    stratified            = FALSE,
    m_rule                = c("floor", "round", "ceiling"),
    resampling_indices    = NULL,
    cutoff_method         = NULL,
    sensitivity_min       = NULL,
    specificity_min       = NULL,
    selection_metric      = NULL,
    prefer_fewer_items    = TRUE,
    folds                 = 5L,
    inner_folds           = 4L,
    repeats               = 1L,
    outer_repeats         = 1L,
    inner_repeats         = 1L,
    cv_stratified         = TRUE,
    no_feasible           = c("record_failure", "fallback_youden"),
    positive_label        = 1,
    negative_label        = 0,
    top_k_models          = NULL,
    engine                = c("Rcpp", "R"),
    parallel              = c("none", "threads", "auto"),
    n_workers             = NULL,
    seed                  = NULL,
    result_file           = NULL,
    checkpoint_every      = 25L,
    force_recompute       = FALSE,
    progress              = interactive()
) {
  selection_procedure <- match.arg(selection_procedure)
  m_rule              <- match.arg(m_rule)
  no_feasible         <- match.arg(no_feasible)
  engine              <- match.arg(engine)
  parallel            <- match.arg(parallel)

  if (!is.logical(stratified) || length(stratified) != 1L || is.na(stratified)) {
    stop("`stratified` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(sample_fraction) || length(sample_fraction) != 1L ||
      is.na(sample_fraction) || sample_fraction <= 0 || sample_fraction >= 1) {
    stop("`sample_fraction` must be strictly in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1L) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)
  if (!is.numeric(checkpoint_every) || length(checkpoint_every) != 1L || is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be a positive integer.", call. = FALSE)
  }
  checkpoint_every <- as.integer(checkpoint_every)
  if (!is.null(top_k_models)) {
    if (!is.numeric(top_k_models) || length(top_k_models) != 1L || is.na(top_k_models) || top_k_models < 1L) {
      stop("`top_k_models` must be a positive integer or NULL.", call. = FALSE)
    }
    top_k_models <- as.integer(top_k_models)
  }

  spec <- .v033_resolve_selection_spec(
    data               = data,
    items              = items,
    model_sizes        = model_sizes,
    min_items          = min_items,
    max_items          = max_items,
    item_count         = item_count,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    selection_metric   = selection_metric,
    prefer_fewer_items = prefer_fewer_items,
    folds              = folds,
    inner_folds        = inner_folds,
    repeats            = repeats,
    outer_repeats      = outer_repeats,
    inner_repeats      = inner_repeats,
    stratified         = cv_stratified,
    no_feasible        = no_feasible,
    selection_result   = selection_result
  )

  val <- .v033_validate_data(
    data           = data,
    outcome        = outcome,
    items          = spec$items,
    positive_label = positive_label,
    negative_label = negative_label,
    cluster_col    = NULL,
    exchangeable   = TRUE,
    caller_name    = "subsampling_stability"
  )
  y_obs <- val$y
  x_mat <- val$x_mat
  n_obs <- val$n_obs

  if (!is.null(resampling_indices)) {
    if (!is.list(resampling_indices) || length(resampling_indices) < 1L) {
      stop("`resampling_indices` must be a non-empty list of integer index vectors.", call. = FALSE)
    }
    for (b in seq_along(resampling_indices)) {
      idx_b <- as.integer(resampling_indices[[b]])
      if (any(is.na(idx_b)) || any(idx_b < 1L) || any(idx_b > n_obs)) {
        stop(sprintf("`resampling_indices[[%d]]` contains out-of-range or NA indices.", b), call. = FALSE)
      }
      if (anyDuplicated(idx_b)) {
        stop(sprintf("`resampling_indices[[%d]]` contains duplicate indices; subsampling requires without-replacement draws.", b), call. = FALSE)
      }
    }
    B <- length(resampling_indices)
    sub_sched <- list(
      indices = lapply(resampling_indices, function(v) sort(as.integer(v))),
      m_total = length(resampling_indices[[1L]]),
      m_pos   = NA_integer_,
      m_neg   = NA_integer_
    )
    sample_fraction <- sub_sched$m_total / n_obs
  } else {
    sub_sched <- .v034_generate_subsamples(
      y_obs           = y_obs,
      B               = B,
      sample_fraction = sample_fraction,
      stratified      = stratified,
      m_rule          = m_rule,
      seed            = seed
    )
  }

  combo_list <- if (identical(selection_procedure, "apparent")) {
    .v033_build_combos(length(spec$items), spec$model_sizes)
  } else {
    NULL
  }

  data_digest <- .v033_data_digest(x_mat, y_obs)
  chk_key <- list(
    version             = "0.34.0",
    data_digest         = data_digest,
    outcome             = outcome,
    selection_procedure = selection_procedure,
    B                   = B,
    sample_fraction     = sample_fraction,
    stratified          = stratified,
    m_rule              = m_rule,
    indices             = sub_sched$indices,
    items               = spec$items,
    sizes               = spec$model_sizes,
    cutoff              = spec$cutoff_method,
    sens_min            = spec$sensitivity_min,
    spec_min            = spec$specificity_min,
    metric              = spec$selection_metric,
    prefer_fewer_items  = spec$prefer_fewer_items,
    folds               = spec$folds,
    inner_folds         = spec$inner_folds,
    repeats             = spec$repeats,
    outer_repeats       = spec$outer_repeats,
    inner_repeats       = spec$inner_repeats,
    cv_stratified       = spec$stratified,
    no_feasible         = spec$no_feasible,
    engine              = engine,
    seed                = seed
  )

  completed_n <- 0L
  selected_sets <- vector("list", B)
  iter_rows <- vector("list", B)

  if (!is.null(result_file) && file.exists(result_file) && !isTRUE(force_recompute)) {
    prev <- readRDS(result_file)
    if (!is.null(prev$checkpoint_key) && identical(prev$checkpoint_key, chk_key)) {
      completed_n <- as.integer(prev$completed_n)
      if (completed_n > 0L) {
        selected_sets[seq_len(completed_n)] <- prev$selected_sets[seq_len(completed_n)]
        iter_rows[seq_len(completed_n)] <- prev$iter_rows[seq_len(completed_n)]
      }
    } else {
      stop("Existing `result_file` checkpoint does not match current settings/data/indices. Set `force_recompute = TRUE` to overwrite.", call. = FALSE)
    }
  }

  cur_start <- completed_n + 1L
  while (cur_start <= B) {
    if (isTRUE(check_user_interrupt_cpp())) {
      stop("Subsampling stability interrupted by user; completed waves saved if `result_file` was provided.", call. = FALSE)
    }
    cur_end <- min(cur_start + checkpoint_every - 1L, B)
    for (b in seq.int(cur_start, cur_end)) {
      idx_b <- sub_sched$indices[[b]]
      x_sub <- x_mat[idx_b, , drop = FALSE]
      y_sub <- y_obs[idx_b]

      ev_b <- .v033_eval_selection(
        x_mat      = x_sub,
        y_vec      = y_sub,
        spec       = spec,
        procedure  = selection_procedure,
        engine     = engine,
        parallel   = parallel,
        n_workers  = n_workers,
        seed       = .v033_derive_iter_seed(seed, b),
        combo_list = combo_list
      )

      is_ok <- identical(ev_b$status, "valid") || isTRUE(ev_b$fallback_used)
      selected_sets[[b]] <- if (is_ok) sort(ev_b$selected_items) else character(0)
      iter_rows[[b]] <- data.frame(
        iteration      = as.integer(b),
        n_sub          = length(idx_b),
        n_pos_sub      = sum(y_sub == 1L),
        n_neg_sub      = sum(y_sub == 0L),
        status         = ev_b$status,
        failure_reason = if (is.na(ev_b$failure_reason)) NA_character_ else as.character(ev_b$failure_reason),
        selected_model = if (is_ok) ev_b$selected_label else NA_character_,
        n_selected     = if (is_ok) length(ev_b$selected_items) else NA_integer_,
        cutoff         = ev_b$cutoff,
        metric_value   = ev_b$metric_value,
        stringsAsFactors = FALSE
      )
    }
    completed_n <- cur_end
    if (!is.null(result_file)) {
      .save_atomic_rds(
        list(
          checkpoint_key = chk_key,
          completed_n    = completed_n,
          selected_sets  = selected_sets,
          iter_rows      = iter_rows
        ),
        result_file
      )
    }
    if (isTRUE(progress)) {
      message(sprintf("[subsampling_stability] Completed %d / %d subsamples", completed_n, B))
    }
    cur_start <- cur_end + 1L
  }

  iteration_table <- do.call(rbind, iter_rows)
  rownames(iteration_table) <- NULL
  valid_mask <- iteration_table$status == "valid" |
    (identical(no_feasible, "fallback_youden") & !is.na(iteration_table$selected_model))
  n_valid <- sum(valid_mask)
  n_failed <- B - n_valid

  # VIF table (conditional on valid fits; NA if n_valid == 0)
  d <- length(spec$items)
  if (n_valid > 0L) {
    z_mat <- .v034_build_selection_matrix(selected_sets, spec$items, valid_mask)
    vif_cnt <- colSums(z_mat)
    vif_freq <- as.numeric(vif_cnt) / n_valid
  } else {
    vif_cnt <- rep(0L, d)
    vif_freq <- rep(NA_real_, d)
  }
  vif_table <- data.frame(
    feature          = spec$items,
    count            = as.integer(vif_cnt),
    frequency        = vif_freq,
    valid_iterations = as.integer(n_valid),
    stringsAsFactors = FALSE
  )
  vif_table <- vif_table[order(-vif_table$frequency, -vif_table$count, vif_table$feature, na.last = TRUE), , drop = FALSE]
  rownames(vif_table) <- NULL

  # MSF table (canonical sorted signature)
  if (n_valid > 0L) {
    sigs <- iteration_table$selected_model[valid_mask]
    tbl <- sort(table(sigs), decreasing = TRUE)
    msf_full <- data.frame(
      model_signature  = names(tbl),
      count            = as.integer(tbl),
      frequency        = as.numeric(tbl) / n_valid,
      valid_iterations = as.integer(n_valid),
      stringsAsFactors = FALSE
    )
    n_distinct_models <- nrow(msf_full)
    if (!is.null(top_k_models) && n_distinct_models > top_k_models) {
      msf_table <- utils::head(msf_full, n = top_k_models)
      msf_truncated <- TRUE
    } else {
      msf_table <- msf_full
      msf_truncated <- FALSE
    }
  } else {
    msf_table <- data.frame(
      model_signature  = character(0),
      count            = integer(0),
      frequency        = numeric(0),
      valid_iterations = integer(0),
      stringsAsFactors = FALSE
    )
    n_distinct_models <- 0L
    msf_truncated <- FALSE
  }

  # Nogueira & Pairwise over the exact same valid sets
  nogueira_res <- feature_selection_stability(
    x                 = selected_sets,
    eligible_features = spec$items,
    valid_mask        = valid_mask,
    ci                = FALSE
  )
  pairwise_res <- pairwise_selection_stability(
    x                 = selected_sets,
    eligible_features = spec$items,
    valid_mask        = valid_mask
  )

  n_cand <- .count_total_combos_cross_size(d, spec$model_sizes)
  cv_mult <- switch(selection_procedure,
    apparent  = 1,
    cv        = spec$folds * spec$repeats,
    nested_cv = spec$folds * spec$outer_repeats * spec$inner_folds * spec$inner_repeats
  )
  cost_estimate <- list(
    resamples             = B,
    candidate_count       = n_cand,
    cv_multiplier         = cv_mult,
    total_model_evals     = as.numeric(B) * n_cand * cv_mult,
    pair_matrix_cells     = as.numeric(d) * as.numeric(d),
    max_distinct_models   = n_valid
  )

  size_dist <- if (n_valid > 0L) table(factor(iteration_table$n_selected[valid_mask])) else table(integer(0))
  meta <- .v034_make_metadata(
    stability_target           = "subsampling_vif_msf",
    selection_procedure        = selection_procedure,
    eligible_features          = spec$items,
    candidate_scope            = spec$pool_provenance,
    resampling_method          = "subsampling_without_replacement",
    sample_fraction            = sample_fraction,
    stratified                 = stratified,
    class_design               = if (stratified) sprintf("stratified_m=%d_pos=%d_neg=%d", sub_sched$m_total, sub_sched$m_pos, sub_sched$m_neg) else sprintf("unstratified_m=%d", sub_sched$m_total),
    requested_iterations       = B,
    completed_iterations       = B,
    valid_iterations           = n_valid,
    failed_iterations          = n_failed,
    seed_provenance            = list(seed = seed, per_iteration_derived = !is.null(seed)),
    selected_size_distribution = size_dist,
    backend                    = engine
  )

  structure(
    list(
      vif_table         = vif_table,
      msf_table         = msf_table,
      msf_summary       = list(
        total_valid_iterations = as.integer(n_valid),
        n_distinct_models      = as.integer(n_distinct_models),
        stored_model_count     = nrow(msf_table),
        truncated              = msf_truncated
      ),
      nogueira          = nogueira_res,
      pairwise          = pairwise_res,
      selected_sets     = selected_sets,
      resampling_indices = sub_sched$indices,
      iteration_table   = iteration_table,
      data_digest       = data_digest,
      cost_estimate     = cost_estimate,
      metadata          = meta,
      settings          = list(
        outcome             = outcome,
        items               = spec$items,
        model_sizes         = spec$model_sizes,
        selection_procedure = selection_procedure,
        B                   = B,
        sample_fraction     = sample_fraction,
        stratified          = stratified,
        m_rule              = m_rule,
        cutoff_method       = spec$cutoff_method,
        sensitivity_min     = spec$sensitivity_min,
        specificity_min     = spec$specificity_min,
        selection_metric    = spec$selection_metric,
        prefer_fewer_items  = spec$prefer_fewer_items,
        folds               = spec$folds,
        inner_folds         = spec$inner_folds,
        repeats             = spec$repeats,
        outer_repeats       = spec$outer_repeats,
        inner_repeats       = spec$inner_repeats,
        cv_stratified       = spec$stratified,
        no_feasible         = no_feasible,
        n_obs               = n_obs,
        n_pos               = val$n_pos,
        n_neg               = val$n_neg,
        seed                = seed
      )
    ),
    class = "subsampling_stability_result"
  )
}

#' @export
print.subsampling_stability_result <- function(x, top_n = 10L, digits = 4L, ...) {
  cat("NCVROC Subsampling Stability (VIF / MSF / Nogueira / Pairwise)\n")
  cat("--------------------------------------------------------------\n")
  cat(sprintf("Procedure         : %s (%s, %s)\n",
              x$settings$selection_procedure, x$settings$selection_metric, x$settings$cutoff_method))
  cat(sprintf("Subsampling       : B = %d, fraction = %.3f (%s, %s)\n",
              x$metadata$requested_iterations, x$metadata$sample_fraction,
              x$metadata$class_design, x$settings$m_rule))
  cat(sprintf("Valid iterations  : %d / %d (failed: %d)\n",
              x$metadata$valid_iterations, x$metadata$requested_iterations, x$metadata$failed_iterations))
  cat(sprintf("Nogueira index    : %s\n",
              if (identical(x$nogueira$status, "valid")) formatC(x$nogueira$stability_index, digits = digits, format = "f") else sprintf("NA (%s)", x$nogueira$undefined_reason)))

  if (nrow(x$vif_table) > 0L) {
    cat(sprintf("\nTop %d Variable Inclusion Frequencies (VIF, conditional on %d valid fits):\n",
                min(nrow(x$vif_table), as.integer(top_n)), x$metadata$valid_iterations))
    print(utils::head(x$vif_table, n = as.integer(top_n)), row.names = FALSE, digits = digits)
  }
  if (nrow(x$msf_table) > 0L) {
    cat(sprintf("\nTop %d Model Selection Frequencies (MSF, %d distinct models%s):\n",
                min(nrow(x$msf_table), as.integer(top_n)),
                x$msf_summary$n_distinct_models,
                if (x$msf_summary$truncated) ", truncated" else ""))
    print(utils::head(x$msf_table, n = as.integer(top_n)), row.names = FALSE, digits = digits)
  }
  invisible(x)
}

#' @export
summary.subsampling_stability_result <- function(object, ...) {
  structure(object, class = c("summary.subsampling_stability_result", class(object)))
}

#' @export
print.summary.subsampling_stability_result <- function(x, top_n = 10L, digits = 4L, ...) {
  print.subsampling_stability_result(x, top_n = top_n, digits = digits, ...)
  cat("\nNote: VIF and MSF are conditional on valid resampling fits for the specified sample_fraction and do not represent population selection probabilities or causal importance.\n")
  invisible(x)
}

#' @export
plot.subsampling_stability_result <- function(x, type = c("vif", "pif", "xor"), display_features = NULL, ...) {
  type <- match.arg(type)
  plot(x$pairwise, type = type, display_features = display_features, ...)
  invisible(x)
}
