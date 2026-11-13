# stat_audit.R — Statistical audit and aggregation diagnostics (v0.31.0)
#
# Additive, non-destructive audit functions:
#   - cv_performance_audit()
#   - clinical_constraint_audit()
#   - selection_strategy_audit()
#   - ncvroc_audit()
#   - print.ncvroc_audit()

# ---- Internal helpers ----

#' Normalize an NCVROC result object (unwrapping ncvroc() pipeline wrapper if needed)
#' @keywords internal
.audit_unwrap_result <- function(result) {
  if (inherits(result, "ncvroc_pipeline_result") && !is.null(result$nested_cv)) {
    return(result$nested_cv)
  }
  result
}

#' Compute confusion matrix counts (TP, FP, TN, FN) and class counts from binary truth & predictions
#' @keywords internal
.audit_confusion_counts <- function(y_true, y_pred) {
  valid <- !is.na(y_true) & !is.na(y_pred)
  yt <- as.integer(y_true[valid])
  yp <- as.integer(y_pred[valid])
  n_total <- length(yt)
  n_pos <- sum(yt == 1L)
  n_neg <- sum(yt == 0L)
  tp <- sum(yt == 1L & yp == 1L)
  fp <- sum(yt == 0L & yp == 1L)
  tn <- sum(yt == 0L & yp == 0L)
  fn <- sum(yt == 1L & yp == 0L)
  list(
    n_total = as.integer(n_total),
    n_pos   = as.integer(n_pos),
    n_neg   = as.integer(n_neg),
    tp      = as.integer(tp),
    fp      = as.integer(fp),
    tn      = as.integer(tn),
    fn      = as.integer(fn)
  )
}

#' Compute AUC, sensitivity, specificity safely with explicit NA reason
#' @keywords internal
.audit_metrics_from_preds <- function(y_true, scores, y_pred) {
  cc <- .audit_confusion_counts(y_true, y_pred)
  na_reason <- character(0)
  if (cc$n_total == 0L) {
    na_reason <- c(na_reason, "no_valid_observations")
  } else {
    if (cc$n_pos == 0L) na_reason <- c(na_reason, "zero_positives")
    if (cc$n_neg == 0L) na_reason <- c(na_reason, "zero_negatives")
  }

  sens <- if (cc$n_pos > 0L) cc$tp / cc$n_pos else NA_real_
  spec <- if (cc$n_neg > 0L) cc$tn / cc$n_neg else NA_real_
  auc_val <- if (cc$n_pos > 0L && cc$n_neg > 0L && length(scores) == length(y_true) && !all(is.na(scores))) {
    valid <- !is.na(y_true) & !is.na(scores)
    if (sum(y_true[valid] == 1L) > 0L && sum(y_true[valid] == 0L) > 0L) {
      freq <- compute_score_frequencies(scores[valid], as.integer(y_true[valid]))
      compute_auc_from_table(freq$pos_counts, freq$neg_counts)
    } else {
      NA_real_
    }
  } else {
    NA_real_
  }

  list(
    n_total     = cc$n_total,
    n_pos       = cc$n_pos,
    n_neg       = cc$n_neg,
    tp          = cc$tp,
    fp          = cc$fp,
    tn          = cc$tn,
    fn          = cc$fn,
    auc         = auc_val,
    sensitivity = sens,
    specificity = spec,
    na_reason   = if (length(na_reason) > 0L) paste(na_reason, collapse = "; ") else NA_character_
  )
}

#' Summarize a numeric vector across folds/repeats with NA accounting
#' @keywords internal
.audit_macro_summary <- function(x) {
  n_total <- length(x)
  valid <- x[!is.na(x) & is.finite(x)]
  n_valid <- length(valid)
  list(
    mean       = if (n_valid > 0L) mean(valid) else NA_real_,
    sd         = if (n_valid > 1L) stats::sd(valid) else NA_real_,
    n_total    = as.integer(n_total),
    n_valid    = as.integer(n_valid),
    n_excluded = as.integer(n_total - n_valid)
  )
}

# ---- Scope B: CV Performance & Class-Count Audit ----

#' Audit cross-validation performance estimands and class counts
#'
#' Separates fold-macro summaries, pooled out-of-fold (OOF) metrics, and
#' repeat-level summaries, while reporting per-fold confusion counts (`TP`,
#' `FP`, `TN`, `FN`) and class counts (`n_pos`, `n_neg`). When predictions or
#' per-fold records are absent from a result object, affected components are
#' marked `"not_verifiable"` rather than inferred.
#'
#' @param result An NCVROC result object (`ncvroc_result`,
#'   `ncvroc_pipeline_result`, `cross_size_nested_cv_result`,
#'   `cv_sum_roc_result`, or `cross_size_cv_result`).
#'
#' @return An S3 list of class `"cv_performance_audit"` containing:
#'   `status`, `estimand_definitions`, `fold_metrics`, `fold_macro`,
#'   `repeat_metrics`, `pooled_oof`, and `notes`.
#' @export
cv_performance_audit <- function(result) {
  res <- .audit_unwrap_result(result)
  notes <- character(0)

  estimand_defs <- list(
    fold_macro = paste0(
      "Unweighted arithmetic mean (and SD) across outer/CV folds with finite values; ",
      "folds with zero positives or zero negatives yield NA for affected metrics and are counted in n_excluded."
    ),
    pooled_oof = paste0(
      "Metrics computed by pooling out-of-fold predictions. Note: pooled OOF AUC is not ",
      "generally equal to fold-macro mean AUC, and pooling across multiple repeats counts ",
      "each subject multiple times (prefer per-repeat pooled OOF when repeats > 1)."
    ),
    repeat_level = "Per-repeat fold-macro and per-repeat pooled OOF metrics across unique repeats."
  )

  # Extract prediction data.frame and fold summary depending on class
  preds_df <- NULL
  fold_tbl <- NULL
  if (inherits(res, "ncvroc_result")) {
    preds_df <- res$outer_predictions
    fold_tbl <- res$summary
  } else if (inherits(res, "cross_size_nested_cv_result")) {
    preds_df <- res$outer_predictions
    fold_tbl <- res$outer_fold_results
  } else if (inherits(res, c("cv_sum_roc_result", "cross_size_cv_result"))) {
    preds_df <- res$oof_predictions
    fold_tbl <- res$fold_results
  } else {
    return(structure(
      list(
        status               = "not_verifiable",
        estimand_definitions = estimand_defs,
        fold_metrics         = NULL,
        fold_macro           = NULL,
        repeat_metrics       = NULL,
        pooled_oof           = list(status = "not_verifiable", reason = "unsupported_result_class"),
        notes                = sprintf("Unsupported result class: %s", paste(class(res), collapse = "/"))
      ),
      class = "cv_performance_audit"
    ))
  }

  # 1. Per-fold confusion counts & metrics from predictions when available
  fold_metrics <- NULL
  if (!is.null(preds_df) && is.data.frame(preds_df) && nrow(preds_df) > 0L) {
    # Normalize column names across result classes
    y_col <- if ("true_outcome" %in% names(preds_df)) "true_outcome" else if ("y_true" %in% names(preds_df)) "y_true" else NULL
    s_col <- if ("predicted_score" %in% names(preds_df)) "predicted_score" else if ("score" %in% names(preds_df)) "score" else if ("sum_score" %in% names(preds_df)) "sum_score" else NULL
    p_col <- if ("predicted_class" %in% names(preds_df)) "predicted_class" else if ("predicted" %in% names(preds_df)) "predicted" else if ("pred_class" %in% names(preds_df)) "pred_class" else NULL
    f_col <- if ("outer_fold" %in% names(preds_df)) "outer_fold" else if ("fold_id" %in% names(preds_df)) "fold_id" else NULL
    r_col <- if ("repeat_id" %in% names(preds_df)) "repeat_id" else NULL

    if (!is.null(y_col) && !is.null(p_col) && !is.null(f_col)) {
      # Build composite fold key if repeat_id exists and fold_id is numeric
      fold_keys <- if (!is.null(r_col) && is.numeric(preds_df[[f_col]])) {
        paste0("Rep", preds_df[[r_col]], "_Fold", preds_df[[f_col]])
      } else {
        as.character(preds_df[[f_col]])
      }
      uniq_folds <- unique(fold_keys)
      f_rows <- lapply(uniq_folds, function(fk) {
        idx <- which(fold_keys == fk)
        sc <- if (!is.null(s_col)) preds_df[[s_col]][idx] else rep(NA_real_, length(idx))
        m <- .audit_metrics_from_preds(preds_df[[y_col]][idx], sc, preds_df[[p_col]][idx])
        rep_val <- if (!is.null(r_col)) {
          preds_df[[r_col]][idx[1L]]
        } else if (grepl("^Rep([0-9]+)_", fk)) {
          as.integer(sub("^Rep([0-9]+)_.*$", "\\1", fk))
        } else {
          1L
        }
        data.frame(
          fold        = fk,
          repeat_id   = as.integer(rep_val),
          n_total     = m$n_total,
          n_pos       = m$n_pos,
          n_neg       = m$n_neg,
          tp          = m$tp,
          fp          = m$fp,
          tn          = m$tn,
          fn          = m$fn,
          auc         = m$auc,
          sensitivity = m$sensitivity,
          specificity = m$specificity,
          na_reason   = m$na_reason,
          stringsAsFactors = FALSE
        )
      })
      fold_metrics <- do.call(rbind, f_rows)
      rownames(fold_metrics) <- NULL
    }
  }

  # Fallback to fold_tbl if predictions were absent (e.g. return = "summary")
  if (is.null(fold_metrics) && !is.null(fold_tbl) && is.data.frame(fold_tbl) && nrow(fold_tbl) > 0L) {
    notes <- c(notes, "Per-subject predictions absent; confusion counts (TP/FP/TN/FN) are not verifiable.")
    auc_col  <- if ("auc" %in% names(fold_tbl)) "auc" else if ("outer_auc" %in% names(fold_tbl)) "outer_auc" else NULL
    sens_col <- if ("sensitivity" %in% names(fold_tbl)) "sensitivity" else if ("outer_sensitivity" %in% names(fold_tbl)) "outer_sensitivity" else NULL
    spec_col <- if ("specificity" %in% names(fold_tbl)) "specificity" else if ("outer_specificity" %in% names(fold_tbl)) "outer_specificity" else NULL
    f_id_col <- if ("outer_fold" %in% names(fold_tbl)) "outer_fold" else if ("fold_id" %in% names(fold_tbl)) "fold_id" else NULL
    auc_vals  <- if (!is.null(auc_col)) as.numeric(fold_tbl[[auc_col]]) else rep(NA_real_, nrow(fold_tbl))
    sens_vals <- if (!is.null(sens_col)) as.numeric(fold_tbl[[sens_col]]) else rep(NA_real_, nrow(fold_tbl))
    spec_vals <- if (!is.null(spec_col)) as.numeric(fold_tbl[[spec_col]]) else rep(NA_real_, nrow(fold_tbl))
    na_reasons <- ifelse(
      is.na(auc_vals) | is.na(sens_vals) | is.na(spec_vals),
      "undefined_metric_predictions_absent",
      "predictions_absent_counts_unverifiable"
    )
    fold_metrics <- data.frame(
      fold        = if (!is.null(f_id_col)) as.character(fold_tbl[[f_id_col]]) else as.character(seq_len(nrow(fold_tbl))),
      repeat_id   = if ("repeat_id" %in% names(fold_tbl)) as.integer(fold_tbl$repeat_id) else 1L,
      n_total     = if ("n_test" %in% names(fold_tbl)) as.integer(fold_tbl$n_test) else NA_integer_,
      n_pos       = if ("n_test_pos" %in% names(fold_tbl)) as.integer(fold_tbl$n_test_pos) else NA_integer_,
      n_neg       = if ("n_test_neg" %in% names(fold_tbl)) as.integer(fold_tbl$n_test_neg) else NA_integer_,
      tp          = NA_integer_,
      fp          = NA_integer_,
      tn          = NA_integer_,
      fn          = NA_integer_,
      auc         = auc_vals,
      sensitivity = sens_vals,
      specificity = spec_vals,
      na_reason   = na_reasons,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(fold_metrics)) {
    return(structure(
      list(
        status               = "not_verifiable",
        estimand_definitions = estimand_defs,
        fold_metrics         = NULL,
        fold_macro           = NULL,
        repeat_metrics       = NULL,
        pooled_oof           = list(status = "not_verifiable", reason = "missing_fold_and_prediction_records"),
        notes                = c(notes, "Neither fold-level records nor subject predictions were found.")
      ),
      class = "cv_performance_audit"
    ))
  }

  # 2. Fold-macro summary
  fold_macro <- list(
    auc         = .audit_macro_summary(fold_metrics$auc),
    sensitivity = .audit_macro_summary(fold_metrics$sensitivity),
    specificity = .audit_macro_summary(fold_metrics$specificity),
    count_pooled_sensitivity = if (all(!is.na(fold_metrics$tp)) && sum(fold_metrics$n_pos, na.rm = TRUE) > 0L) {
      sum(fold_metrics$tp) / sum(fold_metrics$n_pos)
    } else NA_real_,
    count_pooled_specificity = if (all(!is.na(fold_metrics$tn)) && sum(fold_metrics$n_neg, na.rm = TRUE) > 0L) {
      sum(fold_metrics$tn) / sum(fold_metrics$n_neg)
    } else NA_real_
  )

  # 3. Repeat-level metrics & Pooled OOF
  repeat_metrics <- NULL
  pooled_oof <- list(status = "not_verifiable", reason = "missing_predictions")

  if (!is.null(preds_df) && is.data.frame(preds_df) && nrow(preds_df) > 0L) {
    y_col <- if ("true_outcome" %in% names(preds_df)) "true_outcome" else if ("y_true" %in% names(preds_df)) "y_true" else NULL
    s_col <- if ("predicted_score" %in% names(preds_df)) "predicted_score" else if ("score" %in% names(preds_df)) "score" else if ("sum_score" %in% names(preds_df)) "sum_score" else NULL
    p_col <- if ("predicted_class" %in% names(preds_df)) "predicted_class" else if ("predicted" %in% names(preds_df)) "predicted" else if ("pred_class" %in% names(preds_df)) "pred_class" else NULL
    f_col <- if ("outer_fold" %in% names(preds_df)) "outer_fold" else if ("fold_id" %in% names(preds_df)) "fold_id" else NULL
    r_col <- if ("repeat_id" %in% names(preds_df)) "repeat_id" else NULL

    rep_ids <- if (!is.null(r_col)) {
      as.integer(preds_df[[r_col]])
    } else if (!is.null(f_col) && any(grepl("^Rep([0-9]+)_", as.character(preds_df[[f_col]])))) {
      ifelse(grepl("^Rep([0-9]+)_", as.character(preds_df[[f_col]])),
             as.integer(sub("^Rep([0-9]+)_.*$", "\\1", as.character(preds_df[[f_col]]))),
             1L)
    } else {
      rep(1L, nrow(preds_df))
    }

    uniq_reps <- sort(unique(rep_ids))
    rep_rows <- lapply(uniq_reps, function(rid) {
      p_idx <- which(rep_ids == rid)
      f_idx <- which(fold_metrics$repeat_id == rid)
      sc <- if (!is.null(s_col)) preds_df[[s_col]][p_idx] else rep(NA_real_, length(p_idx))
      pm <- .audit_metrics_from_preds(preds_df[[y_col]][p_idx], sc, preds_df[[p_col]][p_idx])
      data.frame(
        repeat_id         = rid,
        n_subjects        = pm$n_total,
        n_pos             = pm$n_pos,
        n_neg             = pm$n_neg,
        pooled_auc        = pm$auc,
        pooled_sensitivity = pm$sensitivity,
        pooled_specificity = pm$specificity,
        fold_macro_auc    = mean(fold_metrics$auc[f_idx], na.rm = TRUE),
        fold_macro_sens   = mean(fold_metrics$sensitivity[f_idx], na.rm = TRUE),
        fold_macro_spec   = mean(fold_metrics$specificity[f_idx], na.rm = TRUE),
        stringsAsFactors  = FALSE
      )
    })
    repeat_metrics <- do.call(rbind, rep_rows)
    rownames(repeat_metrics) <- NULL

    n_repeats <- length(uniq_reps)
    if (n_repeats == 1L) {
      sc_all <- if (!is.null(s_col)) preds_df[[s_col]] else rep(NA_real_, nrow(preds_df))
      pm_all <- .audit_metrics_from_preds(preds_df[[y_col]], sc_all, preds_df[[p_col]])
      pooled_oof <- c(list(status = "verified", n_repeats = 1L, repeated_double_counting = FALSE), pm_all)
    } else {
      # When n_repeats > 1, summarize across repeats rather than double-counting subjects
      pooled_oof <- list(
        status                   = "verified",
        n_repeats                = as.integer(n_repeats),
        repeated_double_counting = FALSE,
        mean_repeat_pooled_auc   = mean(repeat_metrics$pooled_auc, na.rm = TRUE),
        sd_repeat_pooled_auc     = stats::sd(repeat_metrics$pooled_auc, na.rm = TRUE),
        mean_repeat_pooled_sens  = mean(repeat_metrics$pooled_sensitivity, na.rm = TRUE),
        mean_repeat_pooled_spec  = mean(repeat_metrics$pooled_specificity, na.rm = TRUE),
        note = "Aggregated per-repeat pooled OOF so repeated observations of the same subjects are not treated as independent."
      )
      notes <- c(notes, sprintf("Multiple repeats (%d) detected; pooled OOF summarized per repeat to prevent subject double-counting.", n_repeats))
    }
  }

  structure(
    list(
      status               = "verified",
      estimand_definitions = estimand_defs,
      fold_metrics         = fold_metrics,
      fold_macro           = fold_macro,
      repeat_metrics       = repeat_metrics,
      pooled_oof           = pooled_oof,
      notes                = notes
    ),
    class = "cv_performance_audit"
  )
}

# ---- Scope C: Clinical Constraint State Audit ----

#' Audit clinical constraint feasibility and failure states
#'
#' Classifies each outer fold (or CV selection / bootstrap replicate) into one
#' of six canonical states: `"feasible"`, `"infeasible"`, `"undefined_metric"`,
#' `"computational_failure"`, `"cancelled"`, or `"not_evaluated"`.
#' Feasibility rate always uses all planned folds as the denominator.
#'
#' @param result An NCVROC result object.
#'
#' @return An S3 list of class `"clinical_constraint_audit"`.
#' @export
clinical_constraint_audit <- function(result) {
  res <- .audit_unwrap_result(result)
  canonical_states <- c(
    "feasible", "infeasible", "undefined_metric",
    "computational_failure", "cancelled", "not_evaluated"
  )

  settings <- res$settings
  cutoff_method <- if (!is.null(settings$cutoff_method)) settings$cutoff_method else NA_character_
  sens_min <- settings$sensitivity_min
  spec_min <- settings$specificity_min
  has_constraint <- identical(cutoff_method, "clinical_constraint") ||
    (!is.null(sens_min) && !is.na(sens_min)) ||
    (!is.null(spec_min) && !is.na(spec_min))

  if (!has_constraint) {
    return(structure(
      list(
        status                = "not_applicable",
        constraint_active     = FALSE,
        sensitivity_min       = sens_min,
        specificity_min       = spec_min,
        total_folds           = if (inherits(res, "cross_size_nested_cv_result") && !is.null(res$outer_fold_results)) {
          as.integer(nrow(res$outer_fold_results))
        } else if (inherits(res, "ncvroc_result") && !is.null(res$summary)) {
          as.integer(nrow(res$summary))
        } else NA_integer_,
        feasible_count        = NA_integer_,
        feasibility_rate      = NA_real_,
        fallback_count        = NA_integer_,
        outer_target_met_rate = NA_real_,
        state_counts          = NULL,
        fold_details          = NULL,
        notes                 = "No clinical constraint (sensitivity_min / specificity_min) was configured."
      ),
      class = "clinical_constraint_audit"
    ))
  }

  if (inherits(res, "cross_size_nested_cv_result") && !is.null(res$outer_fold_results)) {
    ofr <- res$outer_fold_results
    n_total <- nrow(ofr)
    states <- character(n_total)
    fallback_used <- rep(FALSE, n_total)
    target_met <- rep(NA, n_total)

    for (i in seq_len(n_total)) {
      st_raw <- if ("selection_status" %in% names(ofr)) as.character(ofr$selection_status[i]) else "selected"
      if (is.na(st_raw) || st_raw == "not_evaluated") {
        states[i] <- "not_evaluated"
      } else if (st_raw == "cancelled") {
        states[i] <- "cancelled"
      } else if (st_raw %in% c("computational_failure", "error")) {
        states[i] <- "computational_failure"
      } else if (st_raw %in% c("invalid_evaluation", "undefined_metric")) {
        states[i] <- "undefined_metric"
      } else if (st_raw %in% c("no_feasible_candidate", "infeasible")) {
        states[i] <- "infeasible"
      } else if (st_raw == "fallback_youden") {
        states[i] <- "infeasible"
        fallback_used[i] <- TRUE
      } else if (is.na(ofr$outer_auc[i]) || is.na(ofr$outer_sensitivity[i]) || is.na(ofr$outer_specificity[i])) {
        states[i] <- "undefined_metric"
      } else {
        states[i] <- "feasible"
      }

      if (has_constraint && !is.na(ofr$outer_sensitivity[i]) && !is.na(ofr$outer_specificity[i])) {
        s_ok <- is.null(sens_min) || is.na(sens_min) || (ofr$outer_sensitivity[i] >= sens_min)
        p_ok <- is.null(spec_min) || is.na(spec_min) || (ofr$outer_specificity[i] >= spec_min)
        target_met[i] <- isTRUE(s_ok && p_ok)
      }
    }

    state_counts <- stats::setNames(
      vapply(canonical_states, function(s) sum(states == s), integer(1)),
      canonical_states
    )
    fold_details <- data.frame(
      fold_index            = seq_len(n_total),
      state                 = states,
      fallback_used         = fallback_used,
      outer_test_target_met = target_met,
      stringsAsFactors      = FALSE
    )

    return(structure(
      list(
        status                  = "verified",
        constraint_active       = has_constraint,
        sensitivity_min         = sens_min,
        specificity_min         = spec_min,
        total_folds             = as.integer(n_total),
        feasible_count          = as.integer(state_counts[["feasible"]]),
        feasibility_rate        = if (n_total > 0L) state_counts[["feasible"]] / n_total else NA_real_,
        fallback_count          = as.integer(sum(fallback_used)),
        outer_target_met_rate   = if (any(!is.na(target_met))) mean(target_met, na.rm = TRUE) else NA_real_,
        state_counts            = state_counts,
        fold_details            = fold_details
      ),
      class = "clinical_constraint_audit"
    ))
  }

  if (inherits(res, "ncvroc_result") && !is.null(res$summary)) {
    sm <- res$summary
    n_total <- nrow(sm)
    return(structure(
      list(
        status                = "not_verifiable",
        constraint_active     = has_constraint,
        sensitivity_min       = sens_min,
        specificity_min       = spec_min,
        total_folds           = as.integer(n_total),
        feasible_count        = NA_integer_,
        feasibility_rate      = NA_real_,
        fallback_count        = NA_integer_,
        outer_target_met_rate = NA_real_,
        state_counts          = NULL,
        fold_details          = NULL,
        notes                 = "ncvroc_result summary lacks per-fold constraint selection_status; 6-state classification is not verifiable."
      ),
      class = "clinical_constraint_audit"
    ))
  }

  structure(
    list(
      status            = "not_verifiable",
      constraint_active = has_constraint,
      sensitivity_min   = sens_min,
      specificity_min   = spec_min,
      total_folds       = NA_integer_,
      feasible_count    = NA_integer_,
      feasibility_rate  = NA_real_,
      fallback_count    = NA_integer_,
      state_counts      = NULL,
      fold_details      = NULL
    ),
    class = "clinical_constraint_audit"
  )
}

# ---- Scope E: Selection Strategy Compatibility Audit ----

#' Audit comparability across multiple NCVROC selection strategy results
#'
#' Checks whether two or more NCVROC result objects share the same outcome,
#' candidate items, model size bounds, CV split structure, cutoff/selection
#' rules, and seed, returning `"comparable"`, `"incompatible"`, or
#' `"partially_verifiable"` with explicit mismatch reasons.
#'
#' @param results A non-empty list (preferably named) of NCVROC result objects.
#'
#' @return An S3 list of class `"selection_strategy_audit"`.
#' @export
selection_strategy_audit <- function(results) {
  if (!is.list(results) || length(results) < 2L) {
    stop("`results` must be a list of at least 2 NCVROC result objects.", call. = FALSE)
  }
  r_names <- names(results)
  if (is.null(r_names) || any(!nzchar(r_names))) {
    r_names <- paste0("strategy_", seq_along(results))
  }

  unwrapped <- lapply(results, .audit_unwrap_result)

  extract_meta <- function(obj) {
    s <- obj$settings
    if (is.null(s)) s <- list()
    items_v <- if (!is.null(s$items)) s$items else if (!is.null(s$item_names)) s$item_names else NULL
    outcome_v <- if (!is.null(s$outcome)) s$outcome else if (!is.null(s$outcome_name)) s$outcome_name else NULL
    sizes_v <- if (!is.null(s$model_sizes)) {
      as.integer(s$model_sizes)
    } else if (!is.null(s$min_items) && !is.null(s$max_items)) {
      seq.int(as.integer(s$min_items), as.integer(s$max_items))
    } else NULL
    folds_cfg <- if (!is.null(s$outer_folds)) {
      s$outer_folds
    } else if (!is.null(s$outer_k)) {
      list(outer_k = s$outer_k, inner_k = s$inner_k, outer_repeats = s$outer_repeats, stratified = s$stratified)
    } else NULL
    # Extract actual split identity from outer_folds or outer_predictions when available
    split_ids <- if (!is.null(s$outer_folds)) {
      s$outer_folds
    } else if (!is.null(obj$outer_predictions) && is.data.frame(obj$outer_predictions) &&
               "row_index" %in% names(obj$outer_predictions) &&
               any(c("outer_fold", "fold_id") %in% names(obj$outer_predictions))) {
      op <- obj$outer_predictions
      f_col <- if ("outer_fold" %in% names(op)) "outer_fold" else "fold_id"
      split_keys <- if ("repeat_id" %in% names(op) && is.numeric(op[[f_col]])) {
        paste0("Rep", op$repeat_id, "_Fold", op[[f_col]])
      } else {
        as.character(op[[f_col]])
      }
      split(op$row_index, split_keys)
    } else NULL
    preproc_raw <- list(
      preselect_top_n = s$preselect_top_n,
      preselect_by    = s$preselect_by,
      positive_label  = s$positive_label,
      negative_label  = s$negative_label,
      scale           = s$scale,
      center          = s$center,
      impute          = s$impute
    )
    preproc_v <- preproc_raw[!vapply(preproc_raw, is.null, logical(1))]

    list(
      class_name       = class(obj)[1L],
      outcome          = outcome_v,
      items            = items_v,
      model_sizes      = sizes_v,
      folds_signature  = folds_cfg,
      split_identity   = split_ids,
      preprocessing    = preproc_v,
      cutoff_method    = s$cutoff_method,
      sensitivity_min  = s$sensitivity_min,
      specificity_min  = s$specificity_min,
      selection_metric = if (!is.null(s$selection_criterion)) s$selection_criterion else s$selection_metric,
      seed             = s$seed
    )
  }

  metas <- lapply(unwrapped, extract_meta)
  ref <- metas[[1L]]

  mismatches <- character(0)
  unverifiable <- character(0)
  expected_diffs <- character(0)

  for (i in seq_along(metas)[-1L]) {
    cur <- metas[[i]]
    nm <- r_names[i]
    if (!identical(cur$class_name, ref$class_name)) {
      mismatches <- c(mismatches, sprintf("%s: result class differs (%s vs %s)", nm, cur$class_name, ref$class_name))
    }
    if (is.null(cur$outcome) || is.null(ref$outcome)) {
      unverifiable <- c(unverifiable, sprintf("%s: outcome metadata missing", nm))
    } else if (!identical(as.character(cur$outcome), as.character(ref$outcome))) {
      mismatches <- c(mismatches, sprintf("%s: outcome differs (%s vs %s)", nm, cur$outcome, ref$outcome))
    }
    if (is.null(cur$items) || is.null(ref$items)) {
      unverifiable <- c(unverifiable, sprintf("%s: candidate items metadata missing", nm))
    } else if (!identical(as.character(cur$items), as.character(ref$items))) {
      mismatches <- c(mismatches, sprintf("%s: candidate items differ", nm))
    }
    if (is.null(cur$model_sizes) || is.null(ref$model_sizes)) {
      unverifiable <- c(unverifiable, sprintf("%s: model_sizes metadata missing", nm))
    } else if (!identical(cur$model_sizes, ref$model_sizes)) {
      mismatches <- c(mismatches, sprintf("%s: model_sizes differ", nm))
    }
    if (is.null(cur$folds_signature) || is.null(ref$folds_signature)) {
      unverifiable <- c(unverifiable, sprintf("%s: fold split metadata missing", nm))
    } else if (!identical(cur$folds_signature, ref$folds_signature)) {
      mismatches <- c(mismatches, sprintf("%s: CV split configuration differs", nm))
    }
    if (is.null(cur$split_identity) || is.null(ref$split_identity)) {
      unverifiable <- c(unverifiable, sprintf("%s: actual fold split indices missing", nm))
    } else if (!identical(cur$split_identity, ref$split_identity)) {
      mismatches <- c(mismatches, sprintf("%s: actual fold split indices differ", nm))
    }
    if (length(cur$preprocessing) == 0L || length(ref$preprocessing) == 0L) {
      unverifiable <- c(unverifiable, sprintf("%s: preprocessing/preselection metadata missing", nm))
    } else if (!identical(cur$preprocessing, ref$preprocessing)) {
      mismatches <- c(mismatches, sprintf("%s: preprocessing/preselection settings differ", nm))
    }
    if (is.null(cur$seed) || is.null(ref$seed)) {
      unverifiable <- c(unverifiable, sprintf("%s: seed metadata missing", nm))
    } else if (!identical(cur$seed, ref$seed)) {
      mismatches <- c(mismatches, sprintf("%s: seed differs (%s vs %s)", nm, as.character(cur$seed), as.character(ref$seed)))
    }
    if (!identical(cur$cutoff_method, ref$cutoff_method) ||
        !identical(cur$sensitivity_min, ref$sensitivity_min) ||
        !identical(cur$specificity_min, ref$specificity_min) ||
        !identical(cur$selection_metric, ref$selection_metric)) {
      expected_diffs <- c(expected_diffs, sprintf("%s: strategy rule differs (cutoff/constraint/metric)", nm))
    }
  }

  status <- if (length(mismatches) > 0L) {
    "incompatible"
  } else if (length(unverifiable) > 0L) {
    "partially_verifiable"
  } else {
    "comparable"
  }

  structure(
    list(
      status              = status,
      comparable          = identical(status, "comparable"),
      strategy_names      = r_names,
      mismatches          = mismatches,
      unverifiable        = unverifiable,
      strategy_differences = expected_diffs
    ),
    class = "selection_strategy_audit"
  )
}

# ---- Scope F: Unified Audit Object ----

#' Unified statistical audit for NCVROC result objects
#'
#' Integrates data-leakage boundary checks, CV performance estimand auditing,
#' clinical constraint feasibility auditing, candidate-stability estimand
#' documentation, and optional multi-strategy comparability auditing into a
#' single machine-readable object.
#'
#' @param result An NCVROC result object.
#' @param compare_to Optional list of additional NCVROC result objects to audit
#'   comparability against `result` via [selection_strategy_audit()].
#'
#' @return An S3 list of class `"ncvroc_audit"`.
#' @export
ncvroc_audit <- function(result, compare_to = NULL) {
  res <- .audit_unwrap_result(result)
  res_class <- class(res)[1L]

  # Scope A: Leakage boundary audit
  leakage <- list(
    status = "not_verifiable",
    checks = list(),
    notes  = character(0)
  )
  check_repeat_disjoint <- function(fold_idx_list, rep_ids) {
    if (length(fold_idx_list) == 0L || any(vapply(fold_idx_list, function(x) is.null(x) || length(x) == 0L, logical(1)))) {
      return(NA)
    }
    reps <- unique(rep_ids)
    for (r in reps) {
      sub_idx <- unlist(fold_idx_list[rep_ids == r], use.names = FALSE)
      if (anyDuplicated(sub_idx) != 0L) return(FALSE)
    }
    TRUE
  }

  if (inherits(res, "ncvroc_result")) {
    has_outer <- !is.null(res$outer_results) && length(res$outer_results) > 0L
    if (has_outer) {
      fold_test_list <- lapply(res$outer_results, function(f) {
        if (!is.null(f$predictions) && is.data.frame(f$predictions) && "row_index" %in% names(f$predictions)) {
          f$predictions$row_index
        } else {
          NULL
        }
      })
      fold_names <- vapply(seq_along(res$outer_results), function(k) {
        nm <- res$outer_results[[k]]$outer_fold
        if (is.null(nm)) as.character(k) else as.character(nm)
      }, character(1))
      rep_ids <- ifelse(
        grepl("^Rep([0-9]+)_", fold_names),
        as.integer(sub("^Rep([0-9]+)_.*$", "\\1", fold_names)),
        1L
      )
      disjoint_test <- check_repeat_disjoint(fold_test_list, rep_ids)
      # If train_indices are explicitly recorded on any fold, also verify train/test disjointness
      train_checks <- vapply(res$outer_results, function(f) {
        if (!is.null(f$train_indices) && !is.null(f$predictions) && is.data.frame(f$predictions) &&
            "row_index" %in% names(f$predictions)) {
          length(intersect(f$train_indices, f$predictions$row_index)) == 0L
        } else {
          NA
        }
      }, logical(1))
      train_ok <- if (any(!is.na(train_checks))) all(train_checks[!is.na(train_checks)]) else NA

      if (is.na(disjoint_test)) {
        leakage$status <- "not_verifiable"
        leakage$notes <- c(leakage$notes, "Outer fold test row_index records are absent or incomplete; split isolation is not verifiable.")
      } else if (isFALSE(disjoint_test) || isFALSE(train_ok)) {
        leakage$status <- "violation"
      } else {
        leakage$status <- "verified"
      }
      leakage$checks$outer_test_folds_disjoint <- disjoint_test
      leakage$checks$outer_train_test_disjoint <- train_ok
      leakage$checks$preselect_top_n <- res$settings$preselect_top_n
      leakage$checks$selection_criterion <- res$settings$selection_criterion
    } else {
      leakage$notes <- c(leakage$notes, "outer_results missing; split isolation not verifiable from object alone.")
    }
  } else if (inherits(res, "cross_size_nested_cv_result")) {
    op <- res$outer_predictions
    of <- res$settings$outer_folds
    if (!is.null(of) && !is.null(op) && is.data.frame(op) &&
        all(c("row_index", "fold_id") %in% names(op))) {
      rep_vec <- if ("repeat_id" %in% names(op)) op$repeat_id else rep(1L, nrow(op))
      keys <- paste0(rep_vec, "_", op$fold_id)
      uniq_keys <- unique(keys)
      pred_folds <- lapply(uniq_keys, function(k) op$row_index[keys == k])
      pred_reps <- vapply(uniq_keys, function(k) rep_vec[which(keys == k)[1L]], integer(1))
      disjoint_preds <- check_repeat_disjoint(pred_folds, pred_reps)

      # Verify outer_folds structure (either list of index vectors or list of repeats)
      of_disjoint <- NA
      if (is.list(of) && length(of) > 0L) {
        if (is.list(of[[1L]])) {
          of_disjoint <- all(vapply(of, function(rep_folds) {
            isTRUE(check_repeat_disjoint(rep_folds, rep(1L, length(rep_folds))))
          }, logical(1)))
        } else {
          of_disjoint <- check_repeat_disjoint(of, rep(1L, length(of)))
        }
      }

      if (is.na(disjoint_preds) || is.na(of_disjoint)) {
        leakage$status <- "not_verifiable"
        leakage$notes <- c(leakage$notes, "Outer fold index structure could not be verified.")
      } else if (isFALSE(disjoint_preds) || isFALSE(of_disjoint)) {
        leakage$status <- "violation"
      } else {
        leakage$status <- "verified"
      }
      leakage$checks$outer_folds_recorded <- TRUE
      leakage$checks$outer_folds_disjoint <- of_disjoint
      leakage$checks$outer_predictions_disjoint <- disjoint_preds
      leakage$checks$inner_folds_recorded <- !is.null(res$settings$inner_folds)
    } else {
      leakage$status <- "not_verifiable"
      leakage$notes <- c(leakage$notes, "outer_folds or outer_predictions missing; split isolation not verifiable.")
    }
  } else if (inherits(res, c("cv_sum_roc_result", "cross_size_cv_result"))) {
    leakage$status <- "not_applicable"
    leakage$checks$cv_method <- res$cv_method
    leakage$notes <- c(leakage$notes, "Ordinary CV selects on full CV OOF ranking without an outer holdout; nested-CV leakage boundary is not applicable.")
  } else if (inherits(res, "candidate_stability_result")) {
    leakage$status <- "not_applicable"
    leakage$notes <- c(leakage$notes, "Candidate stability evaluates resampling stability rather than nested CV holdout.")
  }

  # Scope B & C
  perf_audit <- if (!inherits(res, "candidate_stability_result")) {
    cv_performance_audit(res)
  } else {
    list(status = "not_applicable")
  }
  const_audit <- if (!inherits(res, "candidate_stability_result")) {
    clinical_constraint_audit(res)
  } else {
    list(status = "not_applicable")
  }

  # Scope D: Candidate stability estimand audit
  stab_audit <- list(status = "not_applicable")
  if (inherits(res, "candidate_stability_result")) {
    s <- res$settings
    cs <- res$candidate_summary
    rmeta <- res$replicate_metadata
    n_resamples <- if (!is.null(s$total_resamples)) {
      s$total_resamples
    } else if (!is.null(rmeta)) {
      nrow(rmeta)
    } else if (!is.null(s$bootstrap_reps)) {
      s$bootstrap_reps
    } else {
      s$repeats
    }
    n_resamples_int <- if (!is.null(n_resamples) && length(n_resamples) == 1L && !is.na(n_resamples)) {
      as.integer(n_resamples)
    } else {
      NA_integer_
    }
    cs_cols <- if (!is.null(cs) && is.data.frame(cs)) names(cs) else character(0)
    estimands <- character(0)
    if (any(c("resampled_auc_mean", "resampled_youden_mean", "mean_auc") %in% cs_cols)) {
      estimands <- c(estimands, "fixed_candidate_performance_stability")
    }
    if (any(c("mean_rank", "median_rank", "rank_sd") %in% cs_cols)) {
      estimands <- c(estimands, "candidate_rank_stability")
    }
    if (any(c("selection_frequency_all", "selection_count", "selection_freq") %in% cs_cols)) {
      estimands <- c(estimands, "candidate_selection_frequency")
    }
    rmeta_match <- !is.null(rmeta) && is.data.frame(rmeta) &&
      !is.na(n_resamples_int) && identical(as.integer(nrow(rmeta)), n_resamples_int)
    n_valid_training <- if (!is.null(rmeta) && "training_valid" %in% names(rmeta)) {
      as.integer(sum(isTRUE(rmeta$training_valid) | (!is.na(rmeta$training_valid) & rmeta$training_valid)))
    } else {
      NA_integer_
    }
    stab_status <- if (!is.null(cs) && is.data.frame(cs) && nrow(cs) > 0L &&
                       !is.na(n_resamples_int) && length(estimands) > 0L && rmeta_match) {
      "verified"
    } else {
      "not_verifiable"
    }
    stab_audit <- list(
      status              = stab_status,
      estimands_included  = estimands,
      candidate_universe  = if (!is.null(s$conditional_on_screen) && isTRUE(s$conditional_on_screen)) {
        "post_stage1_screen_subset"
      } else {
        "user_specified_candidate_sets"
      },
      n_candidates        = if (!is.null(cs) && is.data.frame(cs)) nrow(cs) else NA_integer_,
      resampling          = s$resampling,
      total_resamples     = n_resamples_int,
      denominator_all     = n_resamples_int,
      n_valid_training    = n_valid_training,
      prefer_fewer_items  = s$prefer_fewer_items,
      seed                = s$seed,
      post_selection_note = paste0(
        "Fixed-candidate resampling metrics evaluate the supplied candidate set and do not represent ",
        "full-procedure re-selection stability across an unrestricted screening universe."
      )
    )
  }

  # Scope E: Optional strategy comparison
  strat_audit <- if (!is.null(compare_to)) {
    selection_strategy_audit(c(list(primary = res), compare_to))
  } else {
    list(status = "not_applicable")
  }

  verifiability <- list(
    leakage     = leakage$status,
    performance = perf_audit$status,
    constraints = const_audit$status,
    stability   = stab_audit$status,
    strategy    = strat_audit$status
  )

  structure(
    list(
      result_class          = res_class,
      leakage_audit         = leakage,
      performance_audit     = perf_audit,
      constraint_audit      = const_audit,
      stability_audit       = stab_audit,
      strategy_audit        = strat_audit,
      verifiability_summary = verifiability
    ),
    class = "ncvroc_audit"
  )
}

#' Print method for ncvroc_audit
#'
#' @param x An object of class `"ncvroc_audit"`.
#' @param ... Additional arguments (ignored).
#' @return `x` invisibly.
#' @export
print.ncvroc_audit <- function(x, ...) {
  cat("NCVROC Statistical Audit\n")
  cat("  Result class :", x$result_class, "\n")
  cat("  Leakage      :", x$verifiability_summary$leakage, "\n")
  cat("  Performance  :", x$verifiability_summary$performance, "\n")
  cat("  Constraints  :", x$verifiability_summary$constraints, "\n")
  cat("  Stability    :", x$verifiability_summary$stability, "\n")
  cat("  Strategy cmp :", x$verifiability_summary$strategy, "\n")
  invisible(x)
}
