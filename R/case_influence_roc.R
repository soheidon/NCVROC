# ==============================================================================
# Case Influence Analysis via Leave-One-Case-Out Re-Selection (v0.33.0 Part B)
# ==============================================================================

#' Case Influence Analysis for Combinatorial ROC Selection
#'
#' Evaluates the influence of individual observations on combinatorial item-set
#' selection and classification by performing leave-one-case-out (LOO)
#' re-selection \eqn{S(-i)} and comparing each perturbed selection against the
#' baseline selection \eqn{S_0} on full data.
#'
#' Strictly separates three prediction/classification target sets:
#' \itemize{
#'   \item \strong{Target (a) — Full original data}: \eqn{S(-i)} vs \eqn{S_0} on all \eqn{n} cases.
#'   \item \strong{Target (b) — Remaining training data}: \eqn{S(-i)} vs \eqn{S_0} on the \eqn{n - 1} cases excluding \eqn{i}.
#'   \item \strong{Target (c) — Excluded case \eqn{i}}: \eqn{S(-i)} (out-of-sample) vs \eqn{S_0} (in-sample, asymmetric comparison) on case \eqn{i}.
#' }
#'
#' Case identifiers are anonymized to integer row indices by default
#' (\code{anonymize = TRUE}) so no plaintext IDs are stored in outputs or checkpoints.
#'
#' @param data A \code{data.frame} containing outcome and item columns.
#' @param outcome Character name of the binary outcome column.
#' @param selection_procedure One of \code{"cv"}, \code{"apparent"}, or \code{"nested_cv"}.
#' @param case_subset Which cases to evaluate: \code{"all"}, \code{"positive"}, \code{"negative"}, or \code{"selected"}.
#' @param case_indices Integer vector of 1-based row indices when \code{case_subset = "selected"}.
#' @param items Optional character vector of item column names (reconstructed from \code{selection_result} if omitted).
#' @param model_sizes Optional integer vector of candidate model sizes.
#' @param min_items Minimum number of items if \code{model_sizes} is NULL.
#' @param max_items Maximum number of items if \code{model_sizes} is NULL.
#' @param item_count Optional formula-like item count string.
#' @param selection_result Optional baseline result from \code{ncvroc()}, \code{cross_size_cv()}, \code{cross_size_nested_cv()}, or \code{nested_sum_roc()}.
#' @param stability_result Optional v0.32 \code{classification_stability_result} for cross-referencing bootstrap stability without conflating estimands.
#' @param cutoff_method Operating point method (\code{"youden"}, \code{"closest_topleft"}, \code{"sensitivity_target"}, \code{"clinical_constraint"}).
#' @param sensitivity_min Minimum sensitivity constraint if required by \code{cutoff_method}.
#' @param specificity_min Minimum specificity constraint if required by \code{cutoff_method}.
#' @param selection_metric Selection metric (\code{"auc"}, \code{"youden"}, \code{"sensitivity"}, \code{"specificity"}, \code{"accuracy"}).
#' @param prefer_fewer_items Logical; break ties in favor of fewer items (default \code{TRUE}).
#' @param folds Number of CV / outer folds (default \code{5L}).
#' @param inner_folds Number of inner folds for \code{"nested_cv"} (default \code{4L}).
#' @param repeats Number of CV repeats (default \code{1L}).
#' @param outer_repeats Number of outer repeats for \code{"nested_cv"} (default \code{1L}).
#' @param inner_repeats Number of inner repeats for \code{"nested_cv"} (default \code{1L}).
#' @param stratified Logical; use stratified CV folds (default \code{TRUE}).
#' @param no_feasible Handling when no candidate meets constraints (\code{"record_failure"} or \code{"fallback_youden"}).
#' @param positive_label Value of \code{outcome} representing positive cases (default \code{1}).
#' @param negative_label Value of \code{outcome} representing negative cases (default \code{0}).
#' @param anonymize Logical; anonymize case identifiers to integer row indices (default \code{TRUE}).
#' @param engine \code{"Rcpp"} or \code{"R"}.
#' @param parallel Parallel mode (\code{"none"}, \code{"threads"}, \code{"auto"}).
#' @param n_workers Optional worker/thread count.
#' @param seed Optional integer seed for deterministic fold generation.
#' @param result_file Optional path to RDS checkpoint file for resume support.
#' @param checkpoint_every Save checkpoint every \code{checkpoint_every} cases (default \code{25L}).
#' @param force_recompute Logical; overwrite incompatible/existing checkpoint (default \code{FALSE}).
#' @param progress Logical; show progress messages (default \code{interactive()}).
#'
#' @return An object of class \code{"case_influence_roc_result"}.
#' @export
case_influence_roc <- function(
    data,
    outcome,
    selection_procedure = c("cv", "apparent", "nested_cv"),
    case_subset         = c("all", "positive", "negative", "selected"),
    case_indices        = NULL,
    items               = NULL,
    model_sizes         = NULL,
    min_items           = 1L,
    max_items           = 4L,
    item_count          = NULL,
    selection_result    = NULL,
    stability_result    = NULL,
    cutoff_method       = NULL,
    sensitivity_min     = NULL,
    specificity_min     = NULL,
    selection_metric    = NULL,
    prefer_fewer_items  = TRUE,
    folds               = 5L,
    inner_folds         = 4L,
    repeats             = 1L,
    outer_repeats       = 1L,
    inner_repeats       = 1L,
    stratified          = TRUE,
    no_feasible         = c("record_failure", "fallback_youden"),
    positive_label      = 1,
    negative_label      = 0,
    anonymize           = TRUE,
    engine              = c("Rcpp", "R"),
    parallel            = c("none", "threads", "auto"),
    n_workers           = NULL,
    seed                = NULL,
    result_file         = NULL,
    checkpoint_every    = 25L,
    force_recompute     = FALSE,
    progress            = interactive()
) {
  selection_procedure <- match.arg(selection_procedure)
  case_subset         <- match.arg(case_subset)
  no_feasible         <- match.arg(no_feasible)
  engine              <- match.arg(engine)
  parallel            <- match.arg(parallel)

  if (!is.logical(anonymize) || length(anonymize) != 1L || is.na(anonymize)) {
    stop("`anonymize` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(checkpoint_every) || length(checkpoint_every) != 1L || is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be a positive integer.", call. = FALSE)
  }
  checkpoint_every <- as.integer(checkpoint_every)

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
    stratified         = stratified,
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
    caller_name    = "case_influence_roc"
  )
  y_obs <- val$y
  x_mat <- val$x_mat
  n_obs <- val$n_obs

  target_indices <- switch(case_subset,
    all      = seq_len(n_obs),
    positive = which(y_obs == 1L),
    negative = which(y_obs == 0L),
    selected = {
      if (is.null(case_indices) || !is.numeric(case_indices) || length(case_indices) < 1L) {
        stop("`case_indices` must be a non-empty integer vector when `case_subset = \"selected\"`.", call. = FALSE)
      }
      idx_int <- unique(as.integer(case_indices))
      if (any(is.na(idx_int)) || any(idx_int < 1L) || any(idx_int > n_obs)) {
        stop(sprintf("`case_indices` must contain integers between 1 and %d.", n_obs), call. = FALSE)
      }
      sort(idx_int)
    }
  )

  combo_list <- if (identical(selection_procedure, "apparent")) {
    .v033_build_combos(length(spec$items), spec$model_sizes)
  } else {
    NULL
  }

  # Baseline selection S0 on full data
  s0_eval <- .v033_eval_selection(
    x_mat      = x_mat,
    y_vec      = y_obs,
    spec       = spec,
    procedure  = selection_procedure,
    engine     = engine,
    parallel   = parallel,
    n_workers  = n_workers,
    seed       = .v033_derive_iter_seed(seed, 0L),
    combo_list = combo_list
  )
  if (!identical(s0_eval$status, "valid") && !isTRUE(s0_eval$fallback_used)) {
    stop(sprintf("Baseline selection S0 failed (%s: %s).", s0_eval$status, s0_eval$failure_reason), call. = FALSE)
  }

  s0_items <- sort(s0_eval$selected_items)
  s0_item_idx <- match(s0_items, spec$items)
  s0_full_perf <- .v033_eval_candidate_on_data(x_mat, y_obs, s0_item_idx, s0_eval$cutoff)

  n_cases <- length(target_indices)
  chk_key <- list(
    version             = "0.33.0",
    data_digest         = .v033_data_digest(x_mat, y_obs),
    outcome             = outcome,
    selection_procedure = selection_procedure,
    case_subset         = case_subset,
    target_indices      = target_indices,
    n_obs               = n_obs,
    n_pos               = val$n_pos,
    n_neg               = val$n_neg,
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
    stratified          = spec$stratified,
    no_feasible         = spec$no_feasible,
    engine              = engine,
    seed                = seed
  )

  completed_n <- 0L
  row_list <- vector("list", n_cases)

  if (!is.null(result_file) && file.exists(result_file) && !isTRUE(force_recompute)) {
    prev <- readRDS(result_file)
    if (!is.null(prev$checkpoint_key) && identical(prev$checkpoint_key, chk_key)) {
      completed_n <- as.integer(prev$completed_n)
      if (completed_n > 0L) {
        row_list[seq_len(completed_n)] <- prev$row_list[seq_len(completed_n)]
      }
    } else {
      stop("Existing `result_file` checkpoint does not match current settings/data. Set `force_recompute = TRUE` to overwrite.", call. = FALSE)
    }
  }

  cur_start <- completed_n + 1L
  while (cur_start <= n_cases) {
    if (isTRUE(check_user_interrupt_cpp())) {
      stop("Case influence evaluation interrupted by user; completed waves saved if `result_file` was provided.", call. = FALSE)
    }
    cur_end <- min(cur_start + checkpoint_every - 1L, n_cases)
    for (k in seq.int(cur_start, cur_end)) {
      i <- target_indices[k]
      x_loo <- x_mat[-i, , drop = FALSE]
      y_loo <- y_obs[-i]

      ev_loo <- .v033_eval_selection(
        x_mat      = x_loo,
        y_vec      = y_loo,
        spec       = spec,
        procedure  = selection_procedure,
        engine     = engine,
        parallel   = parallel,
        n_workers  = n_workers,
        seed       = .v033_derive_iter_seed(seed, i),
        combo_list = combo_list
      )

      is_ok <- identical(ev_loo$status, "valid") || isTRUE(ev_loo$fallback_used)
      if (is_ok && length(ev_loo$selected_items) > 0L) {
        s_loo <- sort(ev_loo$selected_items)
        exact_m <- identical(s_loo, s0_items)
        n_comm <- length(intersect(s_loo, s0_items))
        jacc <- .v033_compute_jaccard(s_loo, s0_items)
        added_str <- paste(setdiff(s_loo, s0_items), collapse = ", ")
        dropped_str <- paste(setdiff(s0_items, s_loo), collapse = ", ")
        rank_s0 <- if (!is.null(ev_loo$candidate_ranks) && s0_eval$selected_label %in% names(ev_loo$candidate_ranks)) {
          as.integer(ev_loo$candidate_ranks[[s0_eval$selected_label]])
        } else {
          NA_integer_
        }
        cut_diff <- if (exact_m && !is.na(ev_loo$cutoff) && !is.na(s0_eval$cutoff)) {
          ev_loo$cutoff - s0_eval$cutoff
        } else {
          NA_real_
        }
        m_diff <- ev_loo$metric_value - s0_eval$metric_value

        # Target (a): Full original data
        loo_idx <- match(s_loo, spec$items)
        perf_full <- .v033_eval_candidate_on_data(x_mat, y_obs, loo_idx, ev_loo$cutoff)
        flip_full <- mean(perf_full$preds != s0_full_perf$preds)

        # Target (b): Remaining training data (excluding i)
        perf_rem <- .v033_eval_candidate_on_data(x_loo, y_loo, loo_idx, ev_loo$cutoff)
        flip_rem <- mean(perf_rem$preds != s0_full_perf$preds[-i])

        # Target (c): Excluded case i itself (asymmetric: S0 in-sample vs S(-i) out-of-sample)
        pred_s0_excl <- s0_full_perf$preds[i]
        pred_loo_excl <- perf_full$preds[i]
        flip_excl <- !identical(pred_s0_excl, pred_loo_excl)

        row_list[[k]] <- data.frame(
          case_index           = as.integer(i),
          y_obs                = as.integer(y_obs[i]),
          status               = ev_loo$status,
          failure_reason       = if (is.na(ev_loo$failure_reason)) NA_character_ else as.character(ev_loo$failure_reason),
          selected_items       = ev_loo$selected_label,
          exact_match          = exact_m,
          n_common_items       = as.integer(n_comm),
          jaccard              = jacc,
          added_items          = added_str,
          dropped_items        = dropped_str,
          baseline_rank_in_loo = rank_s0,
          cutoff_loo           = ev_loo$cutoff,
          cutoff_diff          = cut_diff,
          metric_loo           = ev_loo$metric_value,
          metric_diff          = m_diff,
          auc_full_loo         = perf_full$auc,
          sens_full_loo        = perf_full$sensitivity,
          spec_full_loo        = perf_full$specificity,
          flip_rate_full       = flip_full,
          auc_rem_loo          = perf_rem$auc,
          sens_rem_loo         = perf_rem$sensitivity,
          spec_rem_loo         = perf_rem$specificity,
          flip_rate_rem        = flip_rem,
          pred_s0_on_excluded  = as.integer(pred_s0_excl),
          pred_loo_on_excluded = as.integer(pred_loo_excl),
          flipped_on_excluded  = as.logical(flip_excl),
          stringsAsFactors     = FALSE
        )
      } else {
        row_list[[k]] <- data.frame(
          case_index           = as.integer(i),
          y_obs                = as.integer(y_obs[i]),
          status               = ev_loo$status,
          failure_reason       = if (is.na(ev_loo$failure_reason)) NA_character_ else as.character(ev_loo$failure_reason),
          selected_items       = NA_character_,
          exact_match          = NA,
          n_common_items       = NA_integer_,
          jaccard              = NA_real_,
          added_items          = NA_character_,
          dropped_items        = NA_character_,
          baseline_rank_in_loo = NA_integer_,
          cutoff_loo           = NA_real_,
          cutoff_diff          = NA_real_,
          metric_loo           = NA_real_,
          metric_diff          = NA_real_,
          auc_full_loo         = NA_real_,
          sens_full_loo        = NA_real_,
          spec_full_loo        = NA_real_,
          flip_rate_full       = NA_real_,
          auc_rem_loo          = NA_real_,
          sens_rem_loo         = NA_real_,
          spec_rem_loo         = NA_real_,
          flip_rate_rem        = NA_real_,
          pred_s0_on_excluded  = as.integer(s0_full_perf$preds[i]),
          pred_loo_on_excluded = NA_integer_,
          flipped_on_excluded  = NA,
          stringsAsFactors     = FALSE
        )
      }
    }
    completed_n <- cur_end
    if (!is.null(result_file)) {
      .save_atomic_rds(
        list(
          checkpoint_key = chk_key,
          completed_n    = completed_n,
          row_list       = row_list
        ),
        result_file
      )
    }
    if (isTRUE(progress)) {
      message(sprintf("[case_influence_roc] Completed %d / %d cases", completed_n, n_cases))
    }
    cur_start <- cur_end + 1L
  }

  case_table <- do.call(rbind, row_list)
  rownames(case_table) <- NULL
  if (!isTRUE(anonymize) && !is.null(rownames(data))) {
    case_table$case_label <- rownames(data)[case_table$case_index]
  }

  valid_rows <- case_table$status == "valid" | (!is.na(case_table$jaccard))
  n_valid <- sum(valid_rows)
  n_invalid <- nrow(case_table) - n_valid

  summary_stats <- list(
    n_evaluated            = nrow(case_table),
    n_valid                = as.integer(n_valid),
    n_invalid              = as.integer(n_invalid),
    exact_match_rate       = if (n_valid > 0L) mean(case_table$exact_match[valid_rows], na.rm = TRUE) else NA_real_,
    mean_jaccard           = if (n_valid > 0L) mean(case_table$jaccard[valid_rows], na.rm = TRUE) else NA_real_,
    min_jaccard            = if (n_valid > 0L) min(case_table$jaccard[valid_rows], na.rm = TRUE) else NA_real_,
    max_abs_metric_diff    = if (n_valid > 0L) max(abs(case_table$metric_diff[valid_rows]), na.rm = TRUE) else NA_real_,
    mean_flip_rate_full    = if (n_valid > 0L) mean(case_table$flip_rate_full[valid_rows], na.rm = TRUE) else NA_real_,
    max_flip_rate_full     = if (n_valid > 0L) max(case_table$flip_rate_full[valid_rows], na.rm = TRUE) else NA_real_,
    mean_flip_rate_rem     = if (n_valid > 0L) mean(case_table$flip_rate_rem[valid_rows], na.rm = TRUE) else NA_real_,
    excluded_flip_rate     = if (n_valid > 0L) mean(case_table$flipped_on_excluded[valid_rows], na.rm = TRUE) else NA_real_
  )

  # Optional cross-reference with v0.32 classification_stability_roc result
  stability_context <- NULL
  if (!is.null(stability_result) && inherits(stability_result, "classification_stability_result")) {
    boot_mean_cii <- if (!is.null(stability_result$cii_summary$mean_cii)) {
      as.numeric(stability_result$cii_summary$mean_cii)
    } else if (!is.null(stability_result$subject_summary$cii)) {
      mean(stability_result$subject_summary$cii, na.rm = TRUE)
    } else {
      NA_real_
    }
    boot_reps <- if (!is.null(stability_result$settings$bootstrap_reps)) {
      as.integer(stability_result$settings$bootstrap_reps)
    } else {
      NA_integer_
    }
    stability_context <- list(
      source_mode                = stability_result$mode,
      bootstrap_reps             = boot_reps,
      bootstrap_mean_flip_rate   = boot_mean_cii,
      estimand_separation_note   = paste0(
        "Bootstrap `classification_stability_roc()` measures sampling variability across B resamples; ",
        "`case_influence_roc()` measures leave-one-case-out perturbation sensitivity. These estimands are reported separately."
      )
    )
  }

  structure(
    list(
      baseline = list(
        selected_items  = s0_items,
        selected_label  = s0_eval$selected_label,
        cutoff          = s0_eval$cutoff,
        metric_value    = s0_eval$metric_value,
        full_auc        = s0_full_perf$auc,
        full_sens       = s0_full_perf$sensitivity,
        full_spec       = s0_full_perf$specificity
      ),
      case_table               = case_table,
      summary_stats            = summary_stats,
      target_comparison_notes  = list(
        target_full      = "S(-i) vs S0 evaluated on all n original cases.",
        target_remaining = "S(-i) vs S0 evaluated on the n - 1 remaining training cases.",
        target_excluded  = "S(-i) (out-of-sample) vs S0 (in-sample, asymmetric) evaluated on excluded case i."
      ),
      stability_context        = stability_context,
      anonymized               = isTRUE(anonymize),
      settings                 = list(
        outcome             = outcome,
        items               = spec$items,
        model_sizes         = spec$model_sizes,
        selection_procedure = selection_procedure,
        case_subset         = case_subset,
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
        stratified          = spec$stratified,
        no_feasible         = spec$no_feasible,
        n_obs               = n_obs,
        n_pos               = val$n_pos,
        n_neg               = val$n_neg,
        seed                = seed
      )
    ),
    class = "case_influence_roc_result"
  )
}
