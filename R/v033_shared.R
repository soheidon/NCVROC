# ==============================================================================
# Shared Helpers for v0.33.0 Phase 3 Diagnostics
#   - Permutation-based Null Benchmark (permutation_selection_roc)
#   - Case Influence Analysis (case_influence_roc)
# ==============================================================================

#' Unwrap ncvroc_analysis or other result wrappers
#' @keywords internal
#' @noRd
.v033_unwrap_result <- function(selection_result) {
  if (is.null(selection_result)) return(NULL)
  if (inherits(selection_result, "ncvroc_analysis") && !is.null(selection_result$result)) {
    return(selection_result$result)
  }
  selection_result
}

#' Save and restore .Random.seed safely around a seeded expression
#' @keywords internal
#' @noRd
.v033_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(eval.parent(substitute(expr)))
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  eval.parent(substitute(expr))
}

#' Compute deterministic MD5 digest of canonical analysis data (x_mat, y_vec)
#' @keywords internal
#' @noRd
.v033_data_digest <- function(x_mat, y_vec) {
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(list(x = unname(x_mat), y = as.integer(y_vec)), tf, compress = FALSE)
  unname(tools::md5sum(tf)[[1L]])
}

#' Derive a deterministic 31-bit integer seed from base seed and iteration id
#' @keywords internal
#' @noRd
.v033_derive_iter_seed <- function(base_seed, iter_id) {
  if (is.null(base_seed)) return(NULL)
  val <- (as.double(abs(as.integer(base_seed))) * 1000003 + as.double(iter_id) * 977) %% 2147483647
  res <- as.integer(val)
  if (is.na(res) || res < 1L) 1L else res
}

#' Validate data, outcome, labels, and exchangeability/clustering guards
#' @keywords internal
#' @noRd
.v033_validate_data <- function(data,
                                outcome,
                                items,
                                positive_label = 1,
                                negative_label = 0,
                                cluster_col = NULL,
                                exchangeable = TRUE,
                                caller_name = "v033") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L || !(outcome %in% names(data))) {
    stop("`outcome` must name a single column in `data`.", call. = FALSE)
  }
  if (!is.logical(exchangeable) || length(exchangeable) != 1L || is.na(exchangeable)) {
    stop("`exchangeable` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!isTRUE(exchangeable)) {
    stop(
      sprintf(
        "%s() requires exchangeable observations; `exchangeable = FALSE` cannot produce a valid standard permutation p-value.",
        caller_name
      ),
      call. = FALSE
    )
  }
  if (!is.null(cluster_col)) {
    if (!is.character(cluster_col) || length(cluster_col) != 1L || !(cluster_col %in% names(data))) {
      stop("`cluster_col` must be NULL or name a single column in `data`.", call. = FALSE)
    }
    n_clusters <- length(unique(data[[cluster_col]]))
    if (n_clusters < nrow(data)) {
      stop(
        sprintf(
          "%s() detected clustered/repeated-measure structure in `%s` (%d unique clusters for %d rows). Standard row-level permutation is invalid; cluster-level permutation is out of scope.",
          caller_name, cluster_col, n_clusters, nrow(data)
        ),
        call. = FALSE
      )
    }
  }
  if (!is.character(items) || length(items) < 1L) {
    stop("`items` must be a non-empty character vector.", call. = FALSE)
  }
  if (outcome %in% items) {
    stop("`outcome` column must not be included in `items`.", call. = FALSE)
  }
  if (!all(items %in% names(data))) {
    missing_cols <- setdiff(items, names(data))
    stop(sprintf("Item column(s) not found in `data`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  na_y <- is.na(data[[outcome]])
  na_x <- rowSums(is.na(data[, items, drop = FALSE])) > 0L
  if (any(na_y) || any(na_x)) {
    stop(
      sprintf(
        "Missing values detected (outcome NAs: %d, item NAs: %d rows). Address missing data prior to calling `%s()`.",
        sum(na_y), sum(na_x), caller_name
      ),
      call. = FALSE
    )
  }

  y_raw <- data[[outcome]]
  if (!all(unique(y_raw) %in% c(positive_label, negative_label))) {
    stop("`outcome` contains values outside `positive_label` and `negative_label`.", call. = FALSE)
  }
  y <- ifelse(y_raw == positive_label, 1L, 0L)
  if (sum(y == 1L) < 1L || sum(y == 0L) < 1L) {
    stop("Data must contain at least one positive and one negative observation.", call. = FALSE)
  }

  x_mat <- as.matrix(data[, items, drop = FALSE])
  storage.mode(x_mat) <- "double"

  list(
    y     = as.integer(y),
    x_mat = x_mat,
    n_obs = length(y),
    n_pos = sum(y == 1L),
    n_neg = sum(y == 0L)
  )
}

#' Resolve selection specification from explicit arguments or selection_result
#' @keywords internal
#' @noRd
.v033_resolve_selection_spec <- function(data,
                                         items = NULL,
                                         model_sizes = NULL,
                                         min_items = 1L,
                                         max_items = 4L,
                                         item_count = NULL,
                                         cutoff_method = NULL,
                                         sensitivity_min = NULL,
                                         specificity_min = NULL,
                                         selection_metric = NULL,
                                         prefer_fewer_items = TRUE,
                                         folds = 5L,
                                         inner_folds = 4L,
                                         repeats = 1L,
                                         outer_repeats = 1L,
                                         inner_repeats = 1L,
                                         stratified = TRUE,
                                         no_feasible = "record_failure",
                                         selection_result = NULL,
                                         pool_pre_screened = FALSE,
                                         pool_provenance = c("auto", "full_enumerated", "restricted_observed_labels", "unknown")) {
  pool_provenance <- match.arg(pool_provenance)
  sel <- .v033_unwrap_result(selection_result)
  resolved_provenance <- if (!identical(pool_provenance, "auto")) {
    if (identical(pool_provenance, "restricted_observed_labels")) {
      "restricted_pool_from_observed_labels"
    } else {
      pool_provenance
    }
  } else if (!is.null(sel)) {
    "reconstructed_from_selection_result"
  } else {
    "full_enumerated"
  }
  pool_warning <- NULL

  if (!is.null(sel)) {
    s <- sel$settings
    if (is.null(s) || !is.list(s)) {
      stop("`selection_result` is missing `settings` metadata and cannot be reconstructed.", call. = FALSE)
    }
    if (is.null(items)) {
      items <- if (!is.null(s$items)) s$items else s$item_names
    }
    if (is.null(model_sizes) && is.null(item_count)) {
      if (!is.null(s$model_sizes)) {
        model_sizes <- s$model_sizes
      } else if (!is.null(s$min_items) && !is.null(s$max_items)) {
        model_sizes <- seq.int(as.integer(s$min_items), as.integer(s$max_items))
      }
    }
    if (is.null(cutoff_method) && !is.null(s$cutoff_method)) {
      cutoff_method <- s$cutoff_method
    }
    if (is.null(sensitivity_min) && !is.null(s$sensitivity_min)) {
      sensitivity_min <- s$sensitivity_min
    }
    if (is.null(specificity_min) && !is.null(s$specificity_min)) {
      specificity_min <- s$specificity_min
    }
    if (is.null(selection_metric)) {
      if (!is.null(s$selection_metric)) {
        selection_metric <- s$selection_metric
      } else if (!is.null(s$selection_criterion)) {
        selection_metric <- s$selection_criterion
      }
    }
    if (!is.null(s$prefer_fewer_items)) {
      prefer_fewer_items <- isTRUE(s$prefer_fewer_items)
    }
    if (!is.null(s$folds)) folds <- as.integer(s$folds)
    if (!is.null(s$outer_folds)) folds <- as.integer(s$outer_folds)
    if (!is.null(s$outer_k)) folds <- as.integer(s$outer_k)
    if (!is.null(s$inner_folds)) inner_folds <- as.integer(s$inner_folds)
    if (!is.null(s$inner_k)) inner_folds <- as.integer(s$inner_k)
    if (!is.null(s$repeats)) repeats <- as.integer(s$repeats)
    if (!is.null(s$outer_repeats)) outer_repeats <- as.integer(s$outer_repeats)
    if (!is.null(s$inner_repeats)) inner_repeats <- as.integer(s$inner_repeats)
    if (!is.null(s$stratified)) stratified <- isTRUE(s$stratified)

    if (!is.null(s$preselect_top_n) && is.numeric(s$preselect_top_n) && s$preselect_top_n < length(items)) {
      pool_warning <- paste0(
        "Source `selection_result` used `preselect_top_n`; diagnostic reconstructs the full `items` pool ",
        "rather than conditioning on observed-label preselection."
      )
    }
  }

  if (isTRUE(pool_pre_screened) || identical(resolved_provenance, "restricted_pool_from_observed_labels")) {
    resolved_provenance <- "restricted_pool_from_observed_labels"
    pool_warning <- paste0(
      "Candidate pool was screened using observed labels. ",
      "Unconditional permutation p-values on a label-screened pool are invalid (anti-conservative)."
    )
  } else if (identical(resolved_provenance, "unknown")) {
    pool_warning <- paste0(
      "Candidate-pool provenance is unknown; permutation p-value is blocked until ",
      "pool provenance is established (`full_enumerated` or `selection_result`)."
    )
  }

  if (is.null(items) || !is.character(items) || length(items) < 1L) {
    stop("`items` must be a non-empty character vector (or reconstructable from `selection_result`).", call. = FALSE)
  }

  resolved_sizes <- .resolve_model_sizes(
    model_sizes = model_sizes,
    min_items   = min_items,
    max_items   = min(as.integer(max_items), length(items)),
    item_count  = item_count,
    n_available = length(items)
  )

  if (is.null(cutoff_method)) cutoff_method <- "youden"
  if (is.null(selection_metric)) selection_metric <- "auc"

  # Validate clinical constraint requirements
  if (identical(cutoff_method, "sensitivity_target")) {
    if (is.null(sensitivity_min)) {
      stop("`sensitivity_min` is required when `cutoff_method = \"sensitivity_target\"`.", call. = FALSE)
    }
    if (!is.null(specificity_min)) {
      stop("`specificity_min` must be NULL when `cutoff_method = \"sensitivity_target\"`.", call. = FALSE)
    }
  } else if (identical(cutoff_method, "clinical_constraint")) {
    if (is.null(sensitivity_min) || is.null(specificity_min)) {
      stop("Both `sensitivity_min` and `specificity_min` are required when `cutoff_method = \"clinical_constraint\"`.", call. = FALSE)
    }
  }

  list(
    items              = items,
    model_sizes        = resolved_sizes,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    selection_metric   = selection_metric,
    prefer_fewer_items = isTRUE(prefer_fewer_items),
    folds              = as.integer(folds),
    inner_folds        = as.integer(inner_folds),
    repeats            = as.integer(repeats),
    outer_repeats      = as.integer(outer_repeats),
    inner_repeats      = as.integer(inner_repeats),
    stratified         = isTRUE(stratified),
    no_feasible        = no_feasible,
    pool_provenance    = resolved_provenance,
    pool_warning       = pool_warning
  )
}

#' Compute Jaccard similarity between two character item vectors (empty-empty = 1)
#' @keywords internal
#' @noRd
.v033_compute_jaccard <- function(a, b) {
  if (length(a) == 0L && length(b) == 0L) return(1.0)
  if (length(a) == 0L || length(b) == 0L) return(0.0)
  length(intersect(a, b)) / length(union(a, b))
}

#' Build canonical sorted item-set label
#' @keywords internal
#' @noRd
.v033_format_items <- function(items_vec) {
  if (length(items_vec) == 0L) return("")
  paste(sort(items_vec), collapse = " + ")
}

#' Evaluate a fixed candidate item set with a given cutoff on (x_mat, y_vec)
#' @keywords internal
#' @noRd
.v033_eval_candidate_on_data <- function(x_mat, y_vec, item_idx, cutoff) {
  if (length(y_vec) < 1L || length(item_idx) < 1L || is.na(cutoff)) {
    return(list(auc = NA_real_, sensitivity = NA_real_, specificity = NA_real_, preds = rep(NA_integer_, length(y_vec))))
  }
  scores <- rowSums(x_mat[, item_idx, drop = FALSE])
  preds <- as.integer(scores >= cutoff)
  n_pos <- sum(y_vec == 1L)
  n_neg <- sum(y_vec == 0L)
  sens <- if (n_pos > 0L) sum(preds == 1L & y_vec == 1L) / n_pos else NA_real_
  spec <- if (n_neg > 0L) sum(preds == 0L & y_vec == 0L) / n_neg else NA_real_
  auc_v <- if (n_pos > 0L && n_neg > 0L) {
    freq <- compute_score_frequencies(scores, y_vec)
    compute_auc_from_table(freq$pos_counts, freq$neg_counts)
  } else {
    NA_real_
  }
  list(auc = auc_v, sensitivity = sens, specificity = spec, preds = preds)
}

#' Build integer combination index list for given item count and model sizes
#' @keywords internal
#' @noRd
.v033_build_combos <- function(n_items, model_sizes) {
  combo_list <- list()
  for (k in model_sizes) {
    if (k <= n_items) {
      mat <- utils::combn(n_items, k, simplify = TRUE)
      for (j in seq_len(ncol(mat))) {
        combo_list[[length(combo_list) + 1L]] <- as.integer(mat[, j])
      }
    }
  }
  combo_list
}

#' Evaluate selection procedure on a dataset (x_mat, y_vec)
#'
#' Supports modes: "apparent", "cv", "nested_cv".
#' Returns a standardized list with status, selected_items, cutoff, metric_value,
#' and candidate_ranks (when available).
#' @keywords internal
#' @noRd
.v033_eval_selection <- function(x_mat,
                                 y_vec,
                                 spec,
                                 procedure = c("apparent", "cv", "nested_cv"),
                                 engine = "Rcpp",
                                 parallel = "none",
                                 n_workers = NULL,
                                 seed = NULL,
                                 combo_list = NULL) {
  procedure <- match.arg(procedure)
  n_obs <- length(y_vec)
  n_pos <- sum(y_vec == 1L)
  n_neg <- sum(y_vec == 0L)

  if (n_pos < 1L || n_neg < 1L) {
    return(list(
      status          = "invalid_single_class",
      failure_reason  = "single_class_in_sample",
      fallback_used   = FALSE,
      selected_items  = character(0),
      selected_label  = "",
      cutoff          = NA_real_,
      metric_value    = NA_real_,
      candidate_ranks = NULL
    ))
  }

  if (procedure %in% c("cv", "nested_cv")) {
    eff_folds <- spec$folds
    if (n_obs < eff_folds) {
      return(list(
        status          = "invalid_insufficient_folds",
        failure_reason  = sprintf("n_obs (%d) < folds (%d)", n_obs, eff_folds),
        fallback_used   = FALSE,
        selected_items  = character(0),
        selected_label  = "",
        cutoff          = NA_real_,
        metric_value    = NA_real_,
        candidate_ranks = NULL
      ))
    }
    if (isTRUE(spec$stratified) && (n_pos < eff_folds || n_neg < eff_folds)) {
      return(list(
        status          = "invalid_insufficient_folds",
        failure_reason  = sprintf("class count (%d pos, %d neg) < stratified folds (%d)", n_pos, n_neg, eff_folds),
        fallback_used   = FALSE,
        selected_items  = character(0),
        selected_label  = "",
        cutoff          = NA_real_,
        metric_value    = NA_real_,
        candidate_ranks = NULL
      ))
    }
  }

  if (procedure == "apparent") {
    if (is.null(combo_list)) {
      combo_list <- .v033_build_combos(length(spec$items), spec$model_sizes)
    }
    n_combos <- length(combo_list)
    if (n_combos < 1L) {
      return(list(
        status          = "infeasible_selection",
        failure_reason  = "no_candidate_combinations",
        fallback_used   = FALSE,
        selected_items  = character(0),
        selected_label  = "",
        cutoff          = NA_real_,
        metric_value    = NA_real_,
        candidate_ranks = NULL
      ))
    }

    can_use_native <- identical(engine, "Rcpp") &&
      spec$cutoff_method %in% c("youden", "closest_topleft", "clinical_constraint")

    if (can_use_native) {
      combo_0 <- lapply(combo_list, function(v) as.integer(v) - 1L)
      sens_min_c <- if (!is.null(spec$sensitivity_min)) as.numeric(spec$sensitivity_min) else -1.0
      spec_min_c <- if (!is.null(spec$specificity_min)) as.numeric(spec$specificity_min) else -1.0
      # Use evaluate_combos_cpp for pure apparent evaluation? evaluate_bootstrap_replicates_native_cpp
      # returns the exact winning candidate using the same tie-breakers and clinical constraints,
      # OR we evaluate all candidates in R when candidate_ranks are needed.
      # For small/moderate candidate sets, we get full candidate_ranks; for apparent_max_auc
      # in permutation_selection_roc, we only need the winner!
      res_cpp <- evaluate_combos_cpp(
        x = x_mat,
        y = as.integer(y_vec),
        combo_indices = combo_0,
        cutoff_method = spec$cutoff_method
      )
      # Note: evaluate_combos_cpp uses default operating point; if clinical_constraint or sensitivity_target,
      # evaluate_bootstrap_replicates_native_cpp handles clinical_constraint natively.
      if (spec$cutoff_method == "clinical_constraint") {
        boot_df <- evaluate_bootstrap_replicates_native_cpp(
          x                  = x_mat,
          y                  = as.integer(y_vec),
          boot_indices_list  = list(seq.int(0L, n_obs - 1L)),
          combo_0based       = combo_0,
          cutoff_method      = spec$cutoff_method,
          selection_metric   = spec$selection_metric,
          sensitivity_min    = sens_min_c,
          specificity_min    = spec_min_c,
          prefer_fewer_items = spec$prefer_fewer_items,
          fallback_youden    = identical(spec$no_feasible, "fallback_youden"),
          need_oob           = FALSE,
          ref_items_0based   = integer(0),
          num_threads        = 1L
        )
        st_code <- boot_df$status_code[1L]
        if (st_code %in% c(2L, 3L)) {
          return(list(
            status          = if (st_code == 3L) "undefined_metric" else "infeasible_selection",
            failure_reason  = if (st_code == 3L) "undefined_metric" else "no_feasible_candidate",
            fallback_used   = FALSE,
            selected_items  = character(0),
            selected_label  = "",
            cutoff          = NA_real_,
            metric_value    = NA_real_,
            candidate_ranks = NULL
          ))
        }
        win_idx <- as.integer(boot_df$selected_combo_idx[1L])
        sel_items <- spec$items[combo_list[[win_idx]]]
        sel_cut <- as.numeric(boot_df$selected_cutoff[1L])
        eval_w <- .v033_eval_candidate_on_data(x_mat, y_vec, combo_list[[win_idx]], sel_cut)
        mval <- switch(spec$selection_metric,
                       auc = eval_w$auc,
                       sensitivity = eval_w$sensitivity,
                       specificity = eval_w$specificity,
                       youden = eval_w$sensitivity + eval_w$specificity - 1.0,
                       accuracy = mean(eval_w$preds == y_vec))
        return(list(
          status          = if (st_code == 1L) "infeasible_selection" else "valid",
          failure_reason  = if (st_code == 1L) "no_feasible_candidate_fallback_youden" else NA_character_,
          fallback_used   = isTRUE(boot_df$fallback_used[1L]),
          selected_items  = sel_items,
          selected_label  = .v033_format_items(sel_items),
          cutoff          = sel_cut,
          metric_value    = mval,
          candidate_ranks = NULL
        ))
      } else {
        # Compute candidate ranks directly from res_cpp
        n_items_vec <- lengths(combo_list)
        labels_vec <- vapply(combo_list, function(idx) .v033_format_items(spec$items[idx]), character(1))
        metric_vec <- res_cpp[[spec$selection_metric]]
        ord <- if (isTRUE(spec$prefer_fewer_items)) {
          order(-metric_vec, n_items_vec, labels_vec)
        } else {
          order(-metric_vec, labels_vec)
        }
        ranks <- integer(n_combos)
        ranks[ord] <- seq_len(n_combos)
        names(ranks) <- labels_vec
        win_idx <- ord[1L]
        sel_items <- spec$items[combo_list[[win_idx]]]
        return(list(
          status          = "valid",
          failure_reason  = NA_character_,
          fallback_used   = FALSE,
          selected_items  = sel_items,
          selected_label  = labels_vec[win_idx],
          cutoff          = as.numeric(res_cpp$cutoff[win_idx]),
          metric_value    = as.numeric(metric_vec[win_idx]),
          candidate_ranks = ranks
        ))
      }
    }

    # Pure R apparent path (supports all cutoff methods including sensitivity_target)
    metric_vec <- rep(NA_real_, n_combos)
    cut_vec <- rep(NA_real_, n_combos)
    feasible_vec <- logical(n_combos)
    n_items_vec <- lengths(combo_list)
    labels_vec <- character(n_combos)

    for (c_i in seq_len(n_combos)) {
      idx <- combo_list[[c_i]]
      labels_vec[c_i] <- .v033_format_items(spec$items[idx])
      sc <- rowSums(x_mat[, idx, drop = FALSE])
      freq <- compute_score_frequencies(sc, y_vec)
      roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
      auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
      op <- select_operating_point(
        metrics         = roc_m,
        method          = spec$cutoff_method,
        sensitivity_min = spec$sensitivity_min,
        specificity_min = spec$specificity_min
      )
      if (!is.null(op)) {
        feasible_vec[c_i] <- TRUE
        cut_vec[c_i] <- op$cutoff
        metric_vec[c_i] <- switch(spec$selection_metric,
          auc         = auc_v,
          youden      = op$youden,
          sensitivity = op$sensitivity,
          specificity = op$specificity,
          accuracy    = mean(as.integer(sc >= op$cutoff) == y_vec)
        )
      }
    }

    feas_idx <- which(feasible_vec & !is.na(metric_vec))
    fallback_used <- FALSE
    if (length(feas_idx) == 0L) {
      if (identical(spec$no_feasible, "fallback_youden")) {
        fallback_used <- TRUE
        for (c_i in seq_len(n_combos)) {
          idx <- combo_list[[c_i]]
          sc <- rowSums(x_mat[, idx, drop = FALSE])
          freq <- compute_score_frequencies(sc, y_vec)
          roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
          auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
          op_fb <- select_operating_point(roc_m, method = "youden")
          if (!is.null(op_fb)) {
            feasible_vec[c_i] <- TRUE
            cut_vec[c_i] <- op_fb$cutoff
            metric_vec[c_i] <- switch(spec$selection_metric,
              auc         = auc_v,
              youden      = op_fb$youden,
              sensitivity = op_fb$sensitivity,
              specificity = op_fb$specificity,
              accuracy    = mean(as.integer(sc >= op_fb$cutoff) == y_vec)
            )
          }
        }
        feas_idx <- which(feasible_vec & !is.na(metric_vec))
      }
      if (length(feas_idx) == 0L) {
        return(list(
          status          = "infeasible_selection",
          failure_reason  = "no_feasible_candidate",
          fallback_used   = FALSE,
          selected_items  = character(0),
          selected_label  = "",
          cutoff          = NA_real_,
          metric_value    = NA_real_,
          candidate_ranks = NULL
        ))
      }
    }

    ord_feas <- if (isTRUE(spec$prefer_fewer_items)) {
      feas_idx[order(-metric_vec[feas_idx], n_items_vec[feas_idx], labels_vec[feas_idx])]
    } else {
      feas_idx[order(-metric_vec[feas_idx], labels_vec[feas_idx])]
    }
    ranks <- rep(NA_integer_, n_combos)
    ranks[ord_feas] <- seq_along(ord_feas)
    names(ranks) <- labels_vec
    win_idx <- ord_feas[1L]
    sel_items <- spec$items[combo_list[[win_idx]]]

    return(list(
      status          = if (fallback_used) "infeasible_selection" else "valid",
      failure_reason  = if (fallback_used) "no_feasible_candidate_fallback_youden" else NA_character_,
      fallback_used   = fallback_used,
      selected_items  = sel_items,
      selected_label  = labels_vec[win_idx],
      cutoff          = cut_vec[win_idx],
      metric_value    = metric_vec[win_idx],
      candidate_ranks = ranks
    ))
  }

  # Build data.frame for cross_size_cv / cross_size_nested_cv
  df_sub <- as.data.frame(x_mat)
  df_sub[[".__y__"]] <- y_vec

  if (procedure == "cv") {
    cv_res <- tryCatch(
      cross_size_cv(
        data               = df_sub,
        outcome            = ".__y__",
        items              = spec$items,
        model_sizes        = spec$model_sizes,
        folds              = spec$folds,
        repeats            = spec$repeats,
        stratified         = spec$stratified,
        selection_metric   = spec$selection_metric,
        cutoff_method      = spec$cutoff_method,
        sensitivity_min    = spec$sensitivity_min,
        specificity_min    = spec$specificity_min,
        prefer_fewer_items = spec$prefer_fewer_items,
        positive_label     = 1L,
        negative_label     = 0L,
        engine             = engine,
        parallel           = parallel,
        n_workers          = n_workers,
        seed               = seed,
        progress           = FALSE,
        error_on_empty     = FALSE
      ),
      error = function(e) e
    )
    if (inherits(cv_res, "error")) {
      return(list(
        status          = "undefined_metric",
        failure_reason  = conditionMessage(cv_res),
        fallback_used   = FALSE,
        selected_items  = character(0),
        selected_label  = "",
        cutoff          = NA_real_,
        metric_value    = NA_real_,
        candidate_ranks = NULL
      ))
    }
    if (is.null(cv_res$final_selected_model) || identical(cv_res$final_selection_status, "no_feasible_candidate")) {
      return(list(
        status          = "infeasible_selection",
        failure_reason  = "no_feasible_candidate",
        fallback_used   = FALSE,
        selected_items  = character(0),
        selected_label  = "",
        cutoff          = NA_real_,
        metric_value    = NA_real_,
        candidate_ranks = NULL
      ))
    }
    sel_raw <- as.character(cv_res$final_selected_model$items[1L])
    sel_items <- sort(.parse_itemset(sel_raw))
    m_col <- paste0("mean_", spec$selection_metric)
    mval <- if (m_col %in% names(cv_res$final_selected_model)) {
      as.numeric(cv_res$final_selected_model[[m_col]][1L])
    } else if (!is.null(cv_res$cv_performance) && spec$selection_metric %in% cv_res$cv_performance$metric) {
      as.numeric(cv_res$cv_performance$mean[cv_res$cv_performance$metric == spec$selection_metric][1L])
    } else {
      NA_real_
    }

    ranks <- NULL
    if (!is.null(cv_res$candidate_ranking) && nrow(cv_res$candidate_ranking) > 0L) {
      cr_labels <- vapply(
        as.character(cv_res$candidate_ranking$items),
        function(s) .v033_format_items(.parse_itemset(s)),
        character(1)
      )
      ranks <- seq_len(nrow(cv_res$candidate_ranking))
      names(ranks) <- cr_labels
    }

    return(list(
      status          = "valid",
      failure_reason  = NA_character_,
      fallback_used   = FALSE,
      selected_items  = sel_items,
      selected_label  = .v033_format_items(sel_items),
      cutoff          = as.numeric(cv_res$final_full_data_cutoff),
      metric_value    = mval,
      candidate_ranks = ranks
    ))
  }

  # procedure == "nested_cv"
  ncv_res <- tryCatch(
    cross_size_nested_cv(
      data               = df_sub,
      outcome            = ".__y__",
      items              = spec$items,
      model_sizes        = spec$model_sizes,
      outer_folds        = spec$folds,
      inner_folds        = spec$inner_folds,
      outer_repeats      = spec$outer_repeats,
      inner_repeats      = spec$inner_repeats,
      selection_metric   = spec$selection_metric,
      cutoff_method      = spec$cutoff_method,
      sensitivity_min    = spec$sensitivity_min,
      specificity_min    = spec$specificity_min,
      prefer_fewer_items = spec$prefer_fewer_items,
      stratified         = spec$stratified,
      positive_label     = 1L,
      negative_label     = 0L,
      engine             = engine,
      parallel           = parallel,
      n_workers          = n_workers,
      seed               = seed,
      progress           = FALSE,
      verbose            = FALSE
    ),
    error = function(e) e
  )
  if (inherits(ncv_res, "error")) {
    return(list(
      status          = "undefined_metric",
      failure_reason  = conditionMessage(ncv_res),
      fallback_used   = FALSE,
      selected_items  = character(0),
      selected_label  = "",
      cutoff          = NA_real_,
      metric_value    = NA_real_,
      candidate_ranks = NULL
    ))
  }

  mval <- NA_real_
  if (!is.null(ncv_res$summary) && spec$selection_metric %in% ncv_res$summary$metric) {
    mval <- as.numeric(ncv_res$summary$mean[ncv_res$summary$metric == spec$selection_metric][1L])
  }
  sel_items <- character(0)
  sel_cut <- NA_real_
  if (!is.null(ncv_res$item_combination_selection_frequency) &&
      nrow(ncv_res$item_combination_selection_frequency) > 0L) {
    top_str <- as.character(ncv_res$item_combination_selection_frequency$items[1L])
    sel_items <- sort(.parse_itemset(top_str))
    idx <- match(sel_items, spec$items)
    sc <- rowSums(x_mat[, idx, drop = FALSE])
    freq <- compute_score_frequencies(sc, y_vec)
    roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
    op <- select_operating_point(
      metrics         = roc_m,
      method          = spec$cutoff_method,
      sensitivity_min = spec$sensitivity_min,
      specificity_min = spec$specificity_min
    )
    if (!is.null(op)) sel_cut <- op$cutoff
  }

  status_str <- if (is.na(mval)) "undefined_metric" else "valid"
  list(
    status          = status_str,
    failure_reason  = if (is.na(mval)) "nested_cv_metric_na" else NA_character_,
    fallback_used   = FALSE,
    selected_items  = sel_items,
    selected_label  = .v033_format_items(sel_items),
    cutoff          = sel_cut,
    metric_value    = mval,
    candidate_ranks = NULL
  )
}
