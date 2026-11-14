# ==============================================================================
# Near-Optimal Candidate Analysis via Two-Pass Streaming (v0.34.0 Phase 4)
# ==============================================================================

#' Evaluate a batch of candidate combinations in apparent or fixed-fold CV mode
#' @keywords internal
#' @noRd
.v034_eval_candidate_batch <- function(x_mat,
                                       y_obs,
                                       batch_combos,
                                       item_names,
                                       evaluation_mode,
                                       metric,
                                       cutoff_method,
                                       sensitivity_min,
                                       specificity_min,
                                       folds_list = NULL,
                                       n_folds = 5L,
                                       repeats = 1L,
                                       engine = "Rcpp") {
  m <- length(batch_combos)
  metric_vals <- rep(NA_real_, m)
  cutoffs <- rep(NA_real_, m)
  feasible <- logical(m)
  labels <- vapply(batch_combos, function(idx) .v033_format_items(item_names[idx]), character(1))
  n_items_vec <- lengths(batch_combos)

  can_cpp <- identical(engine, "Rcpp") &&
    cutoff_method %in% c("youden", "closest_topleft", "clinical_constraint")

  if (identical(evaluation_mode, "apparent")) {
    if (can_cpp) {
      combo_0 <- lapply(batch_combos, function(v) as.integer(v) - 1L)
      s_min <- if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else -1.0
      p_min <- if (!is.null(specificity_min)) as.numeric(specificity_min) else -1.0
      res_cpp <- evaluate_combos_cpp(
        x               = x_mat,
        y               = as.integer(y_obs),
        combo_indices   = combo_0,
        cutoff_method   = cutoff_method,
        sensitivity_min = s_min,
        specificity_min = p_min
      )
      metric_vals <- as.numeric(res_cpp[[metric]])
      cutoffs <- as.numeric(res_cpp$cutoff)
      feasible <- if ("feasible" %in% names(res_cpp)) {
        as.logical(res_cpp$feasible) & !is.na(metric_vals)
      } else {
        !is.na(metric_vals) & !is.na(cutoffs)
      }
    } else {
      for (c_i in seq_len(m)) {
        idx <- batch_combos[[c_i]]
        sc <- rowSums(x_mat[, idx, drop = FALSE])
        freq <- compute_score_frequencies(sc, y_obs)
        roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
        auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
        op <- select_operating_point(
          metrics         = roc_m,
          method          = cutoff_method,
          sensitivity_min = sensitivity_min,
          specificity_min = specificity_min
        )
        if (!is.null(op)) {
          val <- switch(metric,
            auc         = auc_v,
            youden      = op$youden,
            sensitivity = op$sensitivity,
            specificity = op$specificity,
            accuracy    = mean(as.integer(sc >= op$cutoff) == y_obs)
          )
          if (!is.na(val)) {
            feasible[c_i] <- TRUE
            cutoffs[c_i] <- op$cutoff
            metric_vals[c_i] <- val
          }
        }
      }
    }
  } else {
    # evaluation_mode == "cv" with fixed folds_list
    if (can_cpp) {
      combo_0 <- lapply(batch_combos, function(v) as.integer(v) - 1L)
      test_0 <- lapply(folds_list, function(v) as.integer(v) - 1L)
      s_min <- if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else -1.0
      p_min <- if (!is.null(specificity_min)) as.numeric(specificity_min) else -1.0
      res_cpp <- evaluate_combos_cv_cpp(
        x               = x_mat,
        y               = as.integer(y_obs),
        combo_indices   = combo_0,
        test_indices    = test_0,
        n_folds         = as.integer(n_folds),
        repeats         = as.integer(repeats),
        cutoff_method   = cutoff_method,
        sensitivity_min = s_min,
        specificity_min = p_min,
        num_threads     = 1L
      )
      col_name <- paste0("cv_", metric)
      if (!col_name %in% names(res_cpp)) col_name <- metric
      metric_vals <- as.numeric(res_cpp[[col_name]])
      feasible <- if ("valid" %in% names(res_cpp)) {
        as.logical(res_cpp$valid) & !is.na(metric_vals)
      } else {
        !is.na(metric_vals)
      }
    } else {
      n_f <- length(folds_list)
      for (c_i in seq_len(m)) {
        idx <- batch_combos[[c_i]]
        sc_all <- rowSums(x_mat[, idx, drop = FALSE])
        fold_vals <- rep(NA_real_, n_f)
        ok_all <- TRUE
        for (f in seq_len(n_f)) {
          te <- folds_list[[f]]
          tr <- setdiff(seq_along(y_obs), te)
          y_tr <- y_obs[tr]
          y_te <- y_obs[te]
          if (length(unique(y_tr)) < 2L || length(unique(y_te)) < 2L) {
            ok_all <- FALSE
            break
          }
          freq_tr <- compute_score_frequencies(sc_all[tr], y_tr)
          roc_tr <- compute_roc_metrics_from_table(freq_tr$pos_counts, freq_tr$neg_counts)
          op_tr <- select_operating_point(
            metrics         = roc_tr,
            method          = cutoff_method,
            sensitivity_min = sensitivity_min,
            specificity_min = specificity_min
          )
          if (is.null(op_tr)) {
            ok_all <- FALSE
            break
          }
          pred_te <- as.integer(sc_all[te] >= op_tr$cutoff)
          tp <- sum(pred_te == 1L & y_te == 1L)
          tn <- sum(pred_te == 0L & y_te == 0L)
          sens_te <- tp / sum(y_te == 1L)
          spec_te <- tn / sum(y_te == 0L)
          fold_vals[f] <- switch(metric,
            auc = {
              f_te <- compute_score_frequencies(sc_all[te], y_te)
              compute_auc_from_table(f_te$pos_counts, f_te$neg_counts)
            },
            youden      = sens_te + spec_te - 1.0,
            sensitivity = sens_te,
            specificity = spec_te,
            accuracy    = mean(pred_te == y_te)
          )
        }
        if (ok_all && !anyNA(fold_vals)) {
          feasible[c_i] <- TRUE
          metric_vals[c_i] <- mean(fold_vals)
        }
      }
    }
  }

  list(
    labels      = labels,
    n_items     = n_items_vec,
    metric      = metric_vals,
    cutoff      = cutoffs,
    feasible    = feasible
  )
}

#' Generate candidate combinations for a global index slice [start_rank, end_rank]
#' @keywords internal
#' @noRd
.v034_get_combo_batch <- function(n_items, model_sizes, start_idx, end_idx) {
  # Map global 1-based rank across model_sizes using .combination_unrank
  counts <- vapply(model_sizes, function(k) choose(n_items, k), numeric(1))
  cum_counts <- cumsum(counts)
  out <- vector("list", end_idx - start_idx + 1L)
  for (pos in seq_along(out)) {
    g <- start_idx + pos - 1
    size_pos <- which(g <= cum_counts)[1L]
    prev_cum <- if (size_pos == 1L) 0 else cum_counts[size_pos - 1L]
    local_rank_0 <- g - prev_cum - 1
    k <- model_sizes[size_pos]
    out[[pos]] <- .combination_unrank(n_items, k, local_rank_0) + 1L
  }
  out
}

#' Near-Optimal Candidate Analysis via Two-Pass Streaming Aggregation
#'
#' Identifies and summarizes all candidate item sets within \code{delta} of the
#' best achievable performance in the full specified candidate universe using
#' an exact two-pass streaming algorithm:
#' \enumerate{
#'   \item \strong{Pass 1}: Evaluates the complete candidate pool under fixed
#'     data, folds, seed, and constraints to determine the exact best metric
#'     \eqn{T_{\text{best}}} and tie-broken best model.
#'   \item \strong{Pass 2}: Re-evaluates the exact same candidate pool under the
#'     same fixed conditions in bounded-memory batches to stream exact
#'     near-optimal counts, feature frequencies, model-size distribution, and
#'     Jaccard overlap with the best model, while separating stored candidate
#'     details and pairwise candidate overlap caps.
#' }
#'
#' Near-optimal Candidate Analysis is an NCVROC-specific extension; \code{delta}
#' is a user-specified practical threshold and does not represent a statistical
#' equivalence margin or confidence interval.
#'
#' @param data A \code{data.frame} containing outcome and item columns.
#' @param outcome Character name of the binary outcome column.
#' @param items Optional character vector of item column names.
#' @param model_sizes Optional integer vector of candidate model sizes.
#' @param min_items Minimum number of items if \code{model_sizes} is NULL.
#' @param max_items Maximum number of items if \code{model_sizes} is NULL.
#' @param item_count Optional formula-like item count string.
#' @param selection_result Optional baseline result object for configuration reconstruction.
#' @param evaluation_mode One of \code{"apparent"} or \code{"cv"}.
#' @param metric Target scalar metric (\code{"auc"}, \code{"youden"},
#'   \code{"sensitivity"}, \code{"specificity"}, \code{"accuracy"}).
#' @param direction One of \code{"auto"} (maximizes all supported metrics),
#'   \code{"maximize"}, or \code{"minimize"}.
#' @param delta Non-negative practical threshold (default \code{0.01}; \code{0}
#'   yields exact ties).
#' @param cutoff_method Operating point method.
#' @param sensitivity_min Minimum sensitivity constraint if required.
#' @param specificity_min Minimum specificity constraint if required.
#' @param prefer_fewer_items Logical; break ties in favor of fewer items (default \code{TRUE}).
#' @param folds Number of CV folds when \code{evaluation_mode = "cv"} (default \code{5L}).
#' @param repeats Number of CV repeats when \code{evaluation_mode = "cv"} (default \code{1L}).
#' @param stratified Logical; stratified CV folds (default \code{TRUE}).
#' @param seed Optional integer seed for deterministic fold generation.
#' @param batch_size Streaming batch size (default \code{5000L}).
#' @param max_stored_candidates Maximum candidate rows stored in \code{stored_candidates}
#'   (exact counts and frequencies are always computed over ALL near-optimal candidates).
#' @param max_pair_detail_candidates Maximum candidates included in pairwise
#'   \eqn{O(M^2)} candidate-pair overlap calculation (default \code{100L}).
#' @param engine \code{"Rcpp"} or \code{"R"}.
#' @param progress Show progress messages (default \code{interactive()}).
#'
#' @return An object of class \code{"near_optimal_candidates_result"}.
#' @export
near_optimal_candidates <- function(
    data,
    outcome,
    items                      = NULL,
    model_sizes                = NULL,
    min_items                  = 1L,
    max_items                  = 4L,
    item_count                 = NULL,
    selection_result           = NULL,
    evaluation_mode            = c("apparent", "cv"),
    metric                     = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
    direction                  = c("auto", "maximize", "minimize"),
    delta                      = 0.01,
    cutoff_method              = NULL,
    sensitivity_min            = NULL,
    specificity_min            = NULL,
    prefer_fewer_items         = TRUE,
    folds                      = 5L,
    repeats                    = 1L,
    stratified                 = TRUE,
    seed                       = NULL,
    batch_size                 = 5000L,
    max_stored_candidates      = 500L,
    max_pair_detail_candidates = 100L,
    engine                     = c("Rcpp", "R"),
    progress                   = interactive()
) {
  evaluation_mode <- match.arg(evaluation_mode)
  metric          <- match.arg(metric)
  direction       <- match.arg(direction)
  engine          <- match.arg(engine)

  if (identical(direction, "auto")) {
    direction <- "maximize"
  }
  if (!is.numeric(delta) || length(delta) != 1L || is.na(delta) || delta < 0) {
    stop("`delta` must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(batch_size) || length(batch_size) != 1L || is.na(batch_size) || batch_size < 1L) {
    stop("`batch_size` must be a positive integer.", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)
  max_stored_candidates <- as.integer(max_stored_candidates)
  max_pair_detail_candidates <- as.integer(max_pair_detail_candidates)

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
    selection_metric   = metric,
    prefer_fewer_items = prefer_fewer_items,
    folds              = folds,
    repeats            = repeats,
    stratified         = stratified,
    selection_result   = selection_result
  )

  val <- .v033_validate_data(
    data           = data,
    outcome        = outcome,
    items          = spec$items,
    positive_label = 1,
    negative_label = 0,
    cluster_col    = NULL,
    exchangeable   = TRUE,
    caller_name    = "near_optimal_candidates"
  )
  y_obs <- val$y
  x_mat <- val$x_mat
  n_obs <- val$n_obs
  n_items_total <- length(spec$items)
  total_combos <- .count_total_combos_cross_size(n_items_total, spec$model_sizes)

  # Pre-generate fixed CV folds once so Pass 1 and Pass 2 use identical splits
  folds_list <- NULL
  if (identical(evaluation_mode, "cv")) {
    folds_list <- .v033_with_seed(seed, {
      all_f <- list()
      for (r in seq_len(spec$repeats)) {
        if (isTRUE(spec$stratified)) {
          f_r <- make_stratified_folds(y_obs, k = spec$folds)
        } else {
          perm <- sample.int(n_obs)
          f_r <- split(perm, rep(seq_len(spec$folds), length.out = n_obs))
        }
        all_f <- c(all_f, unname(f_r))
      }
      all_f
    })
  }

  # ---- PASS 1: Determine exact T_best and best candidate ----
  t_best <- if (identical(direction, "maximize")) -Inf else Inf
  best_label <- NA_character_
  best_n_items <- Inf
  best_items <- character(0)
  n_feasible <- 0L
  n_infeasible <- 0L
  n_na_metric <- 0L

  start_idx <- 1
  while (start_idx <= total_combos) {
    end_idx <- min(start_idx + batch_size - 1, total_combos)
    batch_combos <- .v034_get_combo_batch(n_items_total, spec$model_sizes, start_idx, end_idx)
    ev <- .v034_eval_candidate_batch(
      x_mat           = x_mat,
      y_obs           = y_obs,
      batch_combos    = batch_combos,
      item_names      = spec$items,
      evaluation_mode = evaluation_mode,
      metric          = metric,
      cutoff_method   = spec$cutoff_method,
      sensitivity_min = spec$sensitivity_min,
      specificity_min = spec$specificity_min,
      folds_list      = folds_list,
      n_folds         = spec$folds,
      repeats         = spec$repeats,
      engine          = engine
    )

    n_na_metric <- n_na_metric + sum(is.na(ev$metric))
    n_feasible <- n_feasible + sum(ev$feasible)
    n_infeasible <- n_infeasible + sum(!ev$feasible)

    feas_idx <- which(ev$feasible)
    if (length(feas_idx) > 0L) {
      ord <- if (identical(direction, "maximize")) {
        if (isTRUE(spec$prefer_fewer_items)) {
          feas_idx[order(-ev$metric[feas_idx], ev$n_items[feas_idx], ev$labels[feas_idx])]
        } else {
          feas_idx[order(-ev$metric[feas_idx], ev$labels[feas_idx])]
        }
      } else {
        if (isTRUE(spec$prefer_fewer_items)) {
          feas_idx[order(ev$metric[feas_idx], ev$n_items[feas_idx], ev$labels[feas_idx])]
        } else {
          feas_idx[order(ev$metric[feas_idx], ev$labels[feas_idx])]
        }
      }
      cand_i <- ord[1L]
      val_i <- ev$metric[cand_i]
      # Track exact directional optimum separately from tie-breaking
      exact_opt <- if (identical(direction, "maximize")) max(ev$metric[feas_idx]) else min(ev$metric[feas_idx])
      is_new_strict <- if (identical(direction, "maximize")) exact_opt > t_best else exact_opt < t_best
      is_exact_tie <- identical(val_i, t_best) || (abs(val_i - t_best) == 0)
      better <- is_new_strict ||
        (is_exact_tie &&
           ((isTRUE(spec$prefer_fewer_items) && ev$n_items[cand_i] < best_n_items) ||
              ((!isTRUE(spec$prefer_fewer_items) || ev$n_items[cand_i] == best_n_items) &&
                 (is.na(best_label) || ev$labels[cand_i] < best_label))))
      if (is_new_strict) {
        t_best <- exact_opt
      }
      if (better) {
        best_label <- ev$labels[cand_i]
        best_n_items <- ev$n_items[cand_i]
        best_items <- sort(spec$items[batch_combos[[cand_i]]])
      }
    }
    start_idx <- end_idx + 1
  }

  if (n_feasible == 0L || !is.finite(t_best)) {
    meta <- .v034_make_metadata(
      stability_target     = "near_optimal_candidates",
      selection_procedure  = evaluation_mode,
      eligible_features    = spec$items,
      candidate_scope      = spec$pool_provenance,
      requested_iterations = 1L,
      completed_iterations = 1L,
      valid_iterations     = 0L,
      failed_iterations    = 1L,
      backend              = engine
    )
    return(structure(
      list(
        status                    = "infeasible_no_valid_candidates",
        best_candidate            = NA_character_,
        best_metric               = NA_real_,
        delta                     = delta,
        direction                 = direction,
        metric                    = metric,
        exact_near_optimal_count  = 0L,
        n_evaluated               = as.integer(total_combos),
        n_feasible                = 0L,
        n_infeasible              = as.integer(n_infeasible),
        n_na_metric               = as.integer(n_na_metric),
        stored_candidates         = data.frame(),
        stored_truncated          = FALSE,
        feature_frequencies       = data.frame(),
        size_distribution         = data.frame(),
        jaccard_to_best_summary   = list(mean = NA_real_, min = NA_real_, max = NA_real_),
        candidate_pair_overlap    = list(status = "not_applicable_zero_candidates"),
        ncvroc_extension_note     = "Near-optimal Candidate Analysis is an NCVROC-specific extension; `delta` is a user-specified practical threshold, not a statistical equivalence margin.",
        cost_estimate             = list(passes = 2L, candidate_count = total_combos, total_candidate_evals = 2 * total_combos),
        metadata                  = meta
      ),
      class = "near_optimal_candidates_result"
    ))
  }

  # ---- PASS 2: Stream exact near-optimal counts & distributions ----
  tol <- 1e-12
  exact_count <- 0L
  feat_counts <- stats::setNames(integer(n_items_total), spec$items)
  size_counts <- stats::setNames(integer(length(spec$model_sizes)), as.character(spec$model_sizes))
  jacc_sum <- 0.0
  jacc_min <- Inf
  jacc_max <- -Inf
  stored_list <- list()
  stored_n <- 0L

  start_idx <- 1
  while (start_idx <= total_combos) {
    end_idx <- min(start_idx + batch_size - 1, total_combos)
    batch_combos <- .v034_get_combo_batch(n_items_total, spec$model_sizes, start_idx, end_idx)
    ev <- .v034_eval_candidate_batch(
      x_mat           = x_mat,
      y_obs           = y_obs,
      batch_combos    = batch_combos,
      item_names      = spec$items,
      evaluation_mode = evaluation_mode,
      metric          = metric,
      cutoff_method   = spec$cutoff_method,
      sensitivity_min = spec$sensitivity_min,
      specificity_min = spec$specificity_min,
      folds_list      = folds_list,
      n_folds         = spec$folds,
      repeats         = spec$repeats,
      engine          = engine
    )

    in_zone <- if (identical(direction, "maximize")) {
      ev$feasible & ((t_best - ev$metric) <= (delta + tol))
    } else {
      ev$feasible & ((ev$metric - t_best) <= (delta + tol))
    }

    zone_idx <- which(in_zone)
    for (ci in zone_idx) {
      exact_count <- exact_count + 1L
      c_items <- sort(spec$items[batch_combos[[ci]]])
      feat_counts[c_items] <- feat_counts[c_items] + 1L
      sz_key <- as.character(length(c_items))
      size_counts[sz_key] <- size_counts[sz_key] + 1L

      j_b <- .v033_compute_jaccard(c_items, best_items)
      jacc_sum <- jacc_sum + j_b
      if (j_b < jacc_min) jacc_min <- j_b
      if (j_b > jacc_max) jacc_max <- j_b

      if (stored_n < max_stored_candidates) {
        stored_n <- stored_n + 1L
        stored_list[[stored_n]] <- data.frame(
          items           = ev$labels[ci],
          n_items         = length(c_items),
          metric_value    = ev$metric[ci],
          gap_from_best   = abs(t_best - ev$metric[ci]),
          jaccard_to_best = j_b,
          stringsAsFactors = FALSE
        )
      }
    }
    start_idx <- end_idx + 1
  }

  stored_df <- if (length(stored_list) > 0L) do.call(rbind, stored_list) else data.frame()
  if (nrow(stored_df) > 0L) {
    ord_st <- if (identical(direction, "maximize")) {
      order(-stored_df$metric_value, stored_df$n_items, stored_df$items)
    } else {
      order(stored_df$metric_value, stored_df$n_items, stored_df$items)
    }
    stored_df <- stored_df[ord_st, , drop = FALSE]
    rownames(stored_df) <- NULL
  }
  stored_truncated <- exact_count > nrow(stored_df)

  feat_freq_df <- data.frame(
    feature   = names(feat_counts),
    count     = as.integer(feat_counts),
    frequency = as.numeric(feat_counts) / exact_count,
    stringsAsFactors = FALSE
  )
  feat_freq_df <- feat_freq_df[order(-feat_freq_df$frequency, feat_freq_df$feature), , drop = FALSE]
  rownames(feat_freq_df) <- NULL

  size_dist_df <- data.frame(
    n_items   = as.integer(names(size_counts)),
    count     = as.integer(size_counts),
    frequency = as.numeric(size_counts) / exact_count,
    stringsAsFactors = FALSE
  )

  # Candidate-pair overlap over bounded stored subset (avoiding O(M^2) blowup)
  pair_overlap <- if (nrow(stored_df) >= 2L) {
    n_pair_c <- min(nrow(stored_df), max_pair_detail_candidates)
    sub_sets <- lapply(seq_len(n_pair_c), function(i) .parse_itemset(stored_df$items[i]))
    n_pairs <- n_pair_c * (n_pair_c - 1L) / 2L
    j_vals <- numeric(n_pairs)
    p_k <- 0L
    for (i in seq_len(n_pair_c - 1L)) {
      for (j in seq.int(i + 1L, n_pair_c)) {
        p_k <- p_k + 1L
        j_vals[p_k] <- .v033_compute_jaccard(sub_sets[[i]], sub_sets[[j]])
      }
    }
    list(
      status              = "computed",
      n_candidates_used   = as.integer(n_pair_c),
      n_pairs_evaluated   = as.integer(n_pairs),
      truncated           = exact_count > n_pair_c,
      mean_pair_jaccard   = mean(j_vals),
      min_pair_jaccard    = min(j_vals),
      max_pair_jaccard    = max(j_vals),
      pair_detail_scope   = sprintf(
        "Computed over %d of %d near-optimal candidates (cap = %d).",
        n_pair_c, exact_count, max_pair_detail_candidates
      )
    )
  } else {
    list(
      status            = "single_candidate_only",
      n_candidates_used = nrow(stored_df),
      n_pairs_evaluated = 0L,
      truncated         = FALSE
    )
  }

  meta <- .v034_make_metadata(
    stability_target           = "near_optimal_candidates",
    selection_procedure        = evaluation_mode,
    eligible_features          = spec$items,
    candidate_scope            = spec$pool_provenance,
    requested_iterations       = 2L,
    completed_iterations       = 2L,
    valid_iterations           = 2L,
    failed_iterations          = 0L,
    selected_size_distribution = stats::setNames(as.integer(size_counts), names(size_counts)),
    backend                    = engine
  )

  structure(
    list(
      status                   = "valid",
      best_candidate           = best_label,
      best_items               = best_items,
      best_metric              = t_best,
      delta                    = delta,
      direction                = direction,
      metric                   = metric,
      exact_near_optimal_count = as.integer(exact_count),
      n_evaluated              = as.integer(total_combos),
      n_feasible               = as.integer(n_feasible),
      n_infeasible             = as.integer(n_infeasible),
      n_na_metric              = as.integer(n_na_metric),
      stored_candidates        = stored_df,
      stored_truncated         = stored_truncated,
      feature_frequencies      = feat_freq_df,
      size_distribution        = size_dist_df,
      jaccard_to_best_summary  = list(
        mean = jacc_sum / exact_count,
        min  = jacc_min,
        max  = jacc_max
      ),
      candidate_pair_overlap   = pair_overlap,
      ncvroc_extension_note    = paste0(
        "Near-optimal Candidate Analysis is an NCVROC-specific extension; `delta` is a user-specified ",
        "practical threshold and does not represent a statistical equivalence margin or confidence interval."
      ),
      cost_estimate            = list(
        passes                = 2L,
        candidate_count       = total_combos,
        total_candidate_evals = 2 * total_combos
      ),
      metadata                 = meta
    ),
    class = "near_optimal_candidates_result"
  )
}

#' @export
print.near_optimal_candidates_result <- function(x, top_n = 10L, digits = 4L, ...) {
  cat("NCVROC Near-Optimal Candidate Analysis (Two-Pass Exact)\n")
  cat("-------------------------------------------------------\n")
  cat(sprintf("Status              : %s | Mode: %s (%s, %s)\n",
              x$status, x$metadata$selection_procedure, x$metric, x$direction))
  if (identical(x$status, "valid")) {
    cat(sprintf("Best candidate      : %s (%s = %s)\n",
                x$best_candidate, x$metric, formatC(x$best_metric, digits = digits, format = "f")))
    cat(sprintf("Near-optimal count  : %d / %d feasible (delta = %s; stored = %d%s)\n",
                x$exact_near_optimal_count, x$n_feasible,
                formatC(x$delta, digits = digits, format = "f"),
                nrow(x$stored_candidates),
                if (x$stored_truncated) ", truncated" else ""))
    cat(sprintf("Jaccard to best     : mean = %s (min = %s, max = %s)\n",
                formatC(x$jaccard_to_best_summary$mean, digits = digits, format = "f"),
                formatC(x$jaccard_to_best_summary$min, digits = digits, format = "f"),
                formatC(x$jaccard_to_best_summary$max, digits = digits, format = "f")))
    if (nrow(x$stored_candidates) > 0L && top_n > 0L) {
      cat(sprintf("\nTop %d stored near-optimal candidates:\n", min(nrow(x$stored_candidates), as.integer(top_n))))
      print(utils::head(x$stored_candidates, n = as.integer(top_n)), row.names = FALSE, digits = digits)
    }
  }
  invisible(x)
}

#' @export
summary.near_optimal_candidates_result <- function(object, ...) {
  structure(object, class = c("summary.near_optimal_candidates_result", class(object)))
}

#' @export
print.summary.near_optimal_candidates_result <- function(x, top_n = 10L, digits = 4L, ...) {
  print.near_optimal_candidates_result(x, top_n = top_n, digits = digits, ...)
  cat(sprintf("\nNote: %s\n", x$ncvroc_extension_note))
  invisible(x)
}

#' @export
plot.near_optimal_candidates_result <- function(x, main = NULL, ...) {
  if (!identical(x$status, "valid") || nrow(x$feature_frequencies) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No near-optimal candidates to plot.")
    return(invisible(x))
  }
  df <- x$feature_frequencies
  if (is.null(main)) {
    main <- sprintf("Feature Frequency in Near-Optimal Pool (n = %d, delta = %.3f)",
                    x$exact_near_optimal_count, x$delta)
  }
  graphics::barplot(
    rev(df$frequency),
    names.arg = rev(df$feature),
    horiz = TRUE,
    las = 1,
    xlim = c(0, 1),
    main = main,
    xlab = "Frequency in Near-Optimal Pool",
    ...
  )
  invisible(x)
}
