# ==============================================================================
# Cutoff Stability & Individual Classification Instability (v0.32.0 Phase 2)
# ==============================================================================

#' @keywords internal
#' @noRd
.cs_unwrap_selection_result <- function(selection_result) {
  if (is.null(selection_result)) return(NULL)
  if (inherits(selection_result, "ncvroc_analysis") && !is.null(selection_result$result)) {
    return(selection_result$result)
  }
  selection_result
}

#' @keywords internal
#' @noRd
.cs_resolve_spec <- function(mode,
                             candidate_set,
                             items,
                             model_sizes,
                             cutoff_method,
                             sensitivity_min,
                             specificity_min,
                             selection_metric,
                             prefer_fewer_items,
                             selection_result,
                             data_names) {
  sel <- .cs_unwrap_selection_result(selection_result)
  sel_audit <- if (!is.null(sel)) tryCatch(ncvroc_audit(sel), error = function(e) NULL) else NULL

  if (!is.null(sel)) {
    s <- sel$settings
    if (is.null(s) || !is.list(s)) {
      stop("`selection_result` is not_reconstructable: missing `settings` metadata.", call. = FALSE)
    }

    if (mode == "fixed_candidate" && is.null(candidate_set)) {
      # Extract winning item set if unambiguous
      if (!is.null(sel$best_model) && !is.null(sel$best_model$items)) {
        candidate_set <- .parse_itemset(sel$best_model$items)
      } else if (!is.null(sel$optimal_model) && !is.null(sel$optimal_model$items)) {
        candidate_set <- .parse_itemset(sel$optimal_model$items)
      } else if (!is.null(sel$selected_items) && is.character(sel$selected_items)) {
        candidate_set <- sel$selected_items
      } else {
        stop("`selection_result` is not_reconstructable for `fixed_candidate`: supply `candidate_set` explicitly.", call. = FALSE)
      }
    }

    if (mode == "full_procedure") {
      if (is.null(items)) {
        items <- if (!is.null(s$items)) s$items else s$item_names
      }
      if (is.null(model_sizes)) {
        if (!is.null(s$model_sizes)) {
          model_sizes <- s$model_sizes
        } else if (!is.null(s$min_items) && !is.null(s$max_items)) {
          model_sizes <- seq.int(as.integer(s$min_items), as.integer(s$max_items))
        }
      }
      if (is.null(items) || is.null(model_sizes)) {
        stop("`selection_result` is not_reconstructable for `full_procedure`: missing `items` or `model_sizes`.", call. = FALSE)
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
        selection_metric <- if (!is.null(s$selection_criterion)) s$selection_criterion else s$selection_metric
      }
      if (!is.null(s$prefer_fewer_items)) {
        prefer_fewer_items <- isTRUE(s$prefer_fewer_items)
      }
    }
  }

  if (mode == "fixed_candidate") {
    if (is.null(candidate_set) || !is.character(candidate_set) || length(candidate_set) < 1L) {
      stop("`candidate_set` must be a non-empty character vector of item names when `mode = \"fixed_candidate\"`.", call. = FALSE)
    }
    if (!all(candidate_set %in% data_names)) {
      stop("All items in `candidate_set` must exist in `data`.", call. = FALSE)
    }
    items <- candidate_set
    model_sizes <- length(candidate_set)
  } else {
    if (is.null(items) || !is.character(items) || length(items) < 1L) {
      stop("`items` must be a non-empty character vector when `mode = \"full_procedure\"`.", call. = FALSE)
    }
    if (!all(items %in% data_names)) {
      stop("All columns in `items` must exist in `data`.", call. = FALSE)
    }
    if (is.null(model_sizes) || !is.numeric(model_sizes) || length(model_sizes) < 1L) {
      stop("`model_sizes` must be a non-empty integer vector when `mode = \"full_procedure\"`.", call. = FALSE)
    }
    model_sizes <- sort(unique(as.integer(model_sizes)))
    if (any(is.na(model_sizes)) || any(model_sizes < 1L) || any(model_sizes > length(items))) {
      stop("`model_sizes` must contain integers between 1 and length(items).", call. = FALSE)
    }
  }

  if (is.null(cutoff_method)) cutoff_method <- "youden"
  if (is.null(selection_metric)) selection_metric <- "auc"

  list(
    candidate_set      = candidate_set,
    items              = items,
    model_sizes        = model_sizes,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    selection_metric   = selection_metric,
    prefer_fewer_items = isTRUE(prefer_fewer_items),
    v031_audit_status  = if (!is.null(sel_audit)) sel_audit$verifiability_summary else NULL
  )
}

#' @keywords internal
#' @noRd
.cs_generate_bootstrap_indices <- function(y, n_obs, bootstrap_reps, resampling, seed) {
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
      }
    }, add = TRUE)
    set.seed(seed)
  }

  boot_list <- vector("list", bootstrap_reps)
  pos_idx <- which(y == 1L)
  neg_idx <- which(y == 0L)
  for (b in seq_len(bootstrap_reps)) {
    if (resampling == "stratified") {
      b_pos <- sample(pos_idx, size = length(pos_idx), replace = TRUE)
      b_neg <- sample(neg_idx, size = length(neg_idx), replace = TRUE)
      boot_list[[b]] <- sample(c(b_pos, b_neg))
    } else {
      boot_list[[b]] <- sample.int(n_obs, size = n_obs, replace = TRUE)
    }
  }
  boot_list
}

#' @keywords internal
#' @noRd
.cs_eval_fixed_on_sample <- function(x_mat, y_vec, candidate_idx,
                                      cutoff_method, sensitivity_min, specificity_min,
                                      no_feasible) {
  if (length(unique(y_vec)) < 2L || sum(y_vec == 1L) < 1L || sum(y_vec == 0L) < 1L) {
    return(list(
      status         = "undefined_score",
      failure_reason = "single_class_in_sample",
      fallback_used  = FALSE,
      cutoff         = NA_real_,
      auc            = NA_real_
    ))
  }
  scores <- rowSums(x_mat[, candidate_idx, drop = FALSE])
  if (anyNA(scores)) {
    return(list(
      status         = "missing_data",
      failure_reason = "na_scores",
      fallback_used  = FALSE,
      cutoff         = NA_real_,
      auc            = NA_real_
    ))
  }
  freq <- compute_score_frequencies(scores, y_vec)
  roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
  auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
  op <- select_operating_point(
    metrics         = roc_m,
    method          = cutoff_method,
    sensitivity_min = sensitivity_min,
    specificity_min = specificity_min
  )
  if (is.null(op)) {
    if (identical(no_feasible, "fallback_youden")) {
      op_fb <- select_operating_point(roc_m, method = "youden")
      return(list(
        status         = "infeasible_selection",
        failure_reason = "no_feasible_cutoff_fallback_youden",
        fallback_used  = TRUE,
        cutoff         = op_fb$cutoff,
        auc            = auc_v
      ))
    }
    return(list(
      status         = "infeasible_selection",
      failure_reason = "no_feasible_cutoff",
      fallback_used  = FALSE,
      cutoff         = NA_real_,
      auc            = auc_v
    ))
  }
  list(
    status         = "valid",
    failure_reason = NA_character_,
    fallback_used  = FALSE,
    cutoff         = op$cutoff,
    auc            = auc_v
  )
}

#' @keywords internal
#' @noRd
.cs_eval_full_on_sample_r <- function(x_mat, y_vec, items, combo_list, combo_sizes, combo_names,
                                       selection_metric, cutoff_method,
                                       sensitivity_min, specificity_min,
                                       prefer_fewer_items, no_feasible) {
  if (length(unique(y_vec)) < 2L || sum(y_vec == 1L) < 1L || sum(y_vec == 0L) < 1L) {
    return(list(
      status             = "undefined_score",
      failure_reason     = "single_class_in_sample",
      fallback_used      = FALSE,
      selected_combo_idx = NA_integer_,
      selected_items     = NA_character_,
      selected_n_items   = NA_integer_,
      cutoff             = NA_real_
    ))
  }

  n_c <- length(combo_list)
  eval_df <- data.frame(
    combo_idx   = seq_len(n_c),
    n_items     = combo_sizes,
    auc         = NA_real_,
    youden      = NA_real_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    accuracy    = NA_real_,
    cutoff      = NA_real_,
    feasible    = FALSE,
    stringsAsFactors = FALSE
  )

  for (j in seq_len(n_c)) {
    sc <- rowSums(x_mat[, combo_list[[j]], drop = FALSE])
    freq <- compute_score_frequencies(sc, y_vec)
    roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
    auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
    op <- select_operating_point(
      metrics         = roc_m,
      method          = cutoff_method,
      sensitivity_min = sensitivity_min,
      specificity_min = specificity_min
    )
    if (!is.null(op)) {
      eval_df$auc[j]         <- auc_v
      eval_df$youden[j]      <- op$youden
      eval_df$sensitivity[j] <- op$sensitivity
      eval_df$specificity[j] <- op$specificity
      eval_df$accuracy[j]    <- op$accuracy
      eval_df$cutoff[j]      <- op$cutoff
      eval_df$feasible[j]    <- TRUE
    }
  }

  feas <- eval_df[eval_df$feasible, , drop = FALSE]
  fallback_used <- FALSE
  if (nrow(feas) == 0L) {
    if (identical(no_feasible, "fallback_youden")) {
      fallback_used <- TRUE
      for (j in seq_len(n_c)) {
        sc <- rowSums(x_mat[, combo_list[[j]], drop = FALSE])
        freq <- compute_score_frequencies(sc, y_vec)
        roc_m <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
        auc_v <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
        op <- select_operating_point(roc_m, method = "youden")
        eval_df$auc[j]         <- auc_v
        eval_df$youden[j]      <- op$youden
        eval_df$sensitivity[j] <- op$sensitivity
        eval_df$specificity[j] <- op$specificity
        eval_df$accuracy[j]    <- op$accuracy
        eval_df$cutoff[j]      <- op$cutoff
        eval_df$feasible[j]    <- TRUE
      }
      feas <- eval_df
    } else {
      return(list(
        status             = "infeasible_selection",
        failure_reason     = "no_feasible_candidate",
        fallback_used      = FALSE,
        selected_combo_idx = NA_integer_,
        selected_items     = NA_character_,
        selected_n_items   = NA_integer_,
        cutoff             = NA_real_
      ))
    }
  }

  tie_n <- if (isTRUE(prefer_fewer_items)) feas$n_items else -feas$n_items
  ord <- order(
    -feas[[selection_metric]],
    tie_n,
    -feas$auc,
    -feas$youden,
    feas$combo_idx
  )
  win <- feas[ord[1L], , drop = FALSE]

  list(
    status             = if (fallback_used) "infeasible_selection" else "valid",
    failure_reason     = if (fallback_used) "no_feasible_candidate_fallback_youden" else NA_character_,
    fallback_used      = fallback_used,
    selected_combo_idx = as.integer(win$combo_idx),
    selected_items     = combo_names[win$combo_idx],
    selected_n_items   = as.integer(win$n_items),
    cutoff             = win$cutoff
  )
}

#' Evaluate cutoff stability and individual classification instability (CII)
#'
#' Evaluates how resampling variation in item selection and/or cutoff selection
#' alters cutoffs and individual-level positive/negative classifications when
#' bootstrap-developed models are applied back to the original subjects.
#'
#' @param data A `data.frame` containing `outcome` and candidate items. No
#'   missing values are permitted in the used columns.
#' @param outcome Character(1) naming the binary outcome column in `data`.
#' @param mode Character(1), either `"fixed_candidate"` (fixed item set,
#'   re-selecting the cutoff in each bootstrap sample) or `"full_procedure"`
#'   (re-running item selection and cutoff selection in each bootstrap sample).
#' @param candidate_set Character vector of item names (required for
#'   `mode = "fixed_candidate"` unless extracted from `selection_result`).
#' @param items Character vector of candidate item names (for `mode = "full_procedure"`).
#' @param model_sizes Integer vector of model sizes (for `mode = "full_procedure"`).
#' @param selection_result Optional existing NCVROC result object from which
#'   the reference specification may be reconstructed.
#' @param resampling Character(1), `"ordinary"` (default) or `"stratified"`.
#' @param bootstrap_reps Integer(1), number of bootstrap replicates (default `200L`).
#' @param cutoff_method Character(1), one of `"youden"`, `"closest_topleft"`,
#'   `"sensitivity_target"`, or `"clinical_constraint"`.
#' @param sensitivity_min Optional numeric scalar in \[0, 1\].
#' @param specificity_min Optional numeric scalar in \[0, 1\].
#' @param selection_metric Character(1), one of `"auc"`, `"youden"`,
#'   `"sensitivity"`, `"specificity"`, `"accuracy"` (for `mode = "full_procedure"`).
#' @param prefer_fewer_items Logical(1), default `TRUE`.
#' @param no_feasible Character(1), `"record_failure"` (default) or `"fallback_youden"`.
#' @param positive_label Positive class value in `data[[outcome]]`, default `1`.
#' @param negative_label Negative class value in `data[[outcome]]`, default `0`.
#' @param bootstrap_indices Optional pre-generated list of 1-based integer
#'   index vectors of length `bootstrap_reps` for exact paired Fixed vs Full runs.
#' @param max_candidates Integer(1), safety guard for total combinations in
#'   `mode = "full_procedure"` (default `50000L`).
#' @param engine Character(1), `"R"` or `"Rcpp"`.
#' @param parallel Character(1), `"none"` or `"threads"`.
#' @param n_workers Optional positive integer worker/thread count.
#' @param seed Optional integer seed. Preserves and restores `.Random.seed`.
#' @param result_file Optional `.rds` path for atomic wave checkpoint/resume.
#' @param checkpoint_every Integer(1), wave size for checkpointing/interruption
#'   polling (default `25L`).
#' @param force_recompute Logical(1), default `FALSE`.
#' @param progress Logical(1), default `interactive()`.
#'
#' @return An S3 object of class `"classification_stability_result"`.
#' @export
classification_stability_roc <- function(
    data,
    outcome,
    mode               = c("fixed_candidate", "full_procedure"),
    candidate_set      = NULL,
    items              = NULL,
    model_sizes        = NULL,
    selection_result   = NULL,
    resampling         = c("ordinary", "stratified"),
    bootstrap_reps     = 200L,
    cutoff_method      = c("youden", "closest_topleft", "sensitivity_target", "clinical_constraint"),
    sensitivity_min    = NULL,
    specificity_min    = NULL,
    selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
    prefer_fewer_items = TRUE,
    no_feasible        = c("record_failure", "fallback_youden"),
    positive_label     = 1,
    negative_label     = 0,
    bootstrap_indices  = NULL,
    max_candidates     = 50000L,
    engine             = c("R", "Rcpp"),
    parallel           = c("none", "threads"),
    n_workers          = NULL,
    seed               = NULL,
    result_file        = NULL,
    checkpoint_every   = 25L,
    force_recompute    = FALSE,
    progress           = interactive()
) {
  mode             <- match.arg(mode)
  resampling       <- match.arg(resampling)
  cutoff_method    <- if (missing(cutoff_method)) NULL else match.arg(cutoff_method)
  selection_metric <- if (missing(selection_metric)) NULL else match.arg(selection_metric)
  no_feasible      <- match.arg(no_feasible)
  engine           <- match.arg(engine)
  parallel         <- match.arg(parallel)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L || !(outcome %in% names(data))) {
    stop("`outcome` must name a single column in `data`.", call. = FALSE)
  }
  if (!is.numeric(bootstrap_reps) || length(bootstrap_reps) != 1L ||
      is.na(bootstrap_reps) || bootstrap_reps < 1L) {
    stop("`bootstrap_reps` must be a positive integer.", call. = FALSE)
  }
  bootstrap_reps <- as.integer(bootstrap_reps)
  if (!is.numeric(checkpoint_every) || length(checkpoint_every) != 1L ||
      is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be a positive integer.", call. = FALSE)
  }
  checkpoint_every <- as.integer(checkpoint_every)

  spec <- .cs_resolve_spec(
    mode               = mode,
    candidate_set      = candidate_set,
    items              = items,
    model_sizes        = model_sizes,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    selection_metric   = selection_metric,
    prefer_fewer_items = prefer_fewer_items,
    selection_result   = selection_result,
    data_names         = names(data)
  )

  used_items <- spec$items
  if (outcome %in% used_items) {
    stop("`outcome` column must not be included in candidate items.", call. = FALSE)
  }

  # Validate no missing values in outcome or used items
  na_y <- is.na(data[[outcome]])
  na_x <- rowSums(is.na(data[, used_items, drop = FALSE])) > 0L
  if (any(na_y) || any(na_x)) {
    stop(sprintf(
      "Missing values detected (outcome NAs: %d, item NAs: %d rows). Address missing data prior to calling `classification_stability_roc()`.",
      sum(na_y), sum(na_x)
    ), call. = FALSE)
  }

  y_raw <- data[[outcome]]
  if (!all(unique(y_raw) %in% c(positive_label, negative_label))) {
    stop("`outcome` contains values outside `positive_label` and `negative_label`.", call. = FALSE)
  }
  y <- ifelse(y_raw == positive_label, 1L, 0L)
  n_obs <- length(y)
  if (sum(y == 1L) < 1L || sum(y == 0L) < 1L) {
    stop("Data must contain at least one positive and one negative observation.", call. = FALSE)
  }

  x_mat <- as.matrix(data[, used_items, drop = FALSE])
  storage.mode(x_mat) <- "double"

  # Build candidate combinations & enforce resource guard for full_procedure
  if (mode == "fixed_candidate") {
    combo_list <- list(seq_along(used_items))
    combo_sizes <- length(used_items)
    combo_names <- paste(used_items, collapse = " + ")
    n_cands_total <- 1L
    cand_scope <- "fixed_candidate"
  } else {
    n_cands_total <- sum(vapply(spec$model_sizes, function(sz) choose(length(used_items), sz), numeric(1)))
    if (n_cands_total > max_candidates) {
      stop(sprintf(
        "Estimated candidate combinations (%s) exceed `max_candidates` (%s). Supply a smaller candidate pool or increase `max_candidates` / use native streaming workflows.",
        format(n_cands_total, big.mark = ","), format(max_candidates, big.mark = ",")
      ), call. = FALSE)
    }
    combo_list <- list()
    for (sz in spec$model_sizes) {
      cm <- utils::combn(seq_along(used_items), sz, simplify = FALSE)
      combo_list <- c(combo_list, cm)
    }
    combo_sizes <- vapply(combo_list, length, integer(1))
    combo_names <- vapply(combo_list, function(idx) paste(used_items[idx], collapse = " + "), character(1))
    cand_scope <- if (!is.null(selection_result)) {
      "restricted_candidate_pool"
    } else {
      "full_eligible_candidate_pool"
    }
  }

  # Fit reference model on original data
  if (mode == "fixed_candidate") {
    ref_eval <- .cs_eval_fixed_on_sample(
      x_mat, y, seq_along(used_items),
      spec$cutoff_method, spec$sensitivity_min, spec$specificity_min, no_feasible
    )
    ref_items_vec <- used_items
    ref_items_str <- combo_names
    ref_scores <- as.numeric(rowSums(x_mat))
  } else {
    ref_eval <- .cs_eval_full_on_sample_r(
      x_mat, y, used_items, combo_list, combo_sizes, combo_names,
      spec$selection_metric, spec$cutoff_method,
      spec$sensitivity_min, spec$specificity_min,
      spec$prefer_fewer_items, no_feasible
    )
    if (!is.na(ref_eval$selected_combo_idx)) {
      ref_items_vec <- used_items[combo_list[[ref_eval$selected_combo_idx]]]
      ref_items_str <- ref_eval$selected_items
      ref_scores <- as.numeric(rowSums(x_mat[, combo_list[[ref_eval$selected_combo_idx]], drop = FALSE]))
    } else {
      ref_items_vec <- character(0)
      ref_items_str <- NA_character_
      ref_scores <- rep(NA_real_, n_obs)
    }
  }

  ref_cutoff <- ref_eval$cutoff
  ref_class <- if (!is.na(ref_cutoff) && !all(is.na(ref_scores))) {
    as.integer(ref_scores >= ref_cutoff)
  } else {
    rep(NA_integer_, n_obs)
  }

  # Generate or validate bootstrap_indices in R (never inside native worker threads)
  if (!is.null(bootstrap_indices)) {
    if (!is.list(bootstrap_indices) || length(bootstrap_indices) != bootstrap_reps) {
      stop("`bootstrap_indices` must be a list of length `bootstrap_reps`.", call. = FALSE)
    }
    for (b in seq_len(bootstrap_reps)) {
      v <- bootstrap_indices[[b]]
      if (!is.numeric(v) || length(v) != n_obs || anyNA(v) || any(v < 1L) || any(v > n_obs)) {
        stop("Each element of `bootstrap_indices` must be an integer vector of length nrow(data) in [1, nrow(data)].", call. = FALSE)
      }
      bootstrap_indices[[b]] <- as.integer(v)
    }
  } else {
    bootstrap_indices <- .cs_generate_bootstrap_indices(y, n_obs, bootstrap_reps, resampling, seed)
  }

  boot_digest <- .cp_hash_object(bootstrap_indices)
  data_digest <- .cp_hash_object(list(x = x_mat, y = y))
  pkg_ver <- as.character(utils::packageVersion("NCVROC"))

  chk_key <- list(
    pkg_version            = pkg_ver,
    data_digest            = data_digest,
    bootstrap_indices_digest = boot_digest,
    settings = list(
      mode               = mode,
      items              = used_items,
      model_sizes        = spec$model_sizes,
      resampling         = resampling,
      bootstrap_reps     = bootstrap_reps,
      cutoff_method      = spec$cutoff_method,
      sensitivity_min    = spec$sensitivity_min,
      specificity_min    = spec$specificity_min,
      selection_metric   = spec$selection_metric,
      prefer_fewer_items = spec$prefer_fewer_items,
      no_feasible        = no_feasible
    )
  )

  # Restore checkpoint if available and compatible
  completed_reps <- 0L
  valid_count <- integer(n_obs)
  pos_count <- integer(n_obs)
  flip_count <- integer(n_obs)
  rep_rows <- vector("list", bootstrap_reps)

  if (!is.null(result_file) && file.exists(result_file) && !isTRUE(force_recompute)) {
    prev <- readRDS(result_file)
    if (!is.null(prev$checkpoint_key) && identical(prev$checkpoint_key, chk_key)) {
      completed_reps <- as.integer(prev$completed_reps)
      if (completed_reps > 0L) {
        valid_count <- prev$valid_count
        pos_count <- prev$pos_count
        flip_count <- prev$flip_count
        rep_rows[seq_len(completed_reps)] <- prev$rep_rows[seq_len(completed_reps)]
      }
    } else {
      stop("Existing `result_file` checkpoint does not match current settings/data/indices. Set `force_recompute = TRUE` to overwrite.", call. = FALSE)
    }
  }

  # Wave-based evaluation with cooperative interruption & atomic checkpointing
  use_native_full <- (mode == "full_procedure" && engine == "Rcpp" &&
                        spec$cutoff_method %in% c("youden", "closest_topleft", "clinical_constraint"))
  eff_threads <- if (parallel == "threads") {
    if (!is.null(n_workers)) as.integer(n_workers) else max(1L, parallel::detectCores(logical = TRUE) - 1L)
  } else {
    1L
  }

  cur_start <- completed_reps + 1L
  while (cur_start <= bootstrap_reps) {
    if (isTRUE(check_user_interrupt_cpp())) {
      stop("Classification stability evaluation interrupted by user; completed waves saved if `result_file` was provided.", call. = FALSE)
    }
    cur_end <- min(cur_start + checkpoint_every - 1L, bootstrap_reps)
    batch_ids <- cur_start:cur_end

    if (use_native_full) {
      combo_0based <- lapply(combo_list, function(v) as.integer(v) - 1L)
      boot_wave_0 <- lapply(bootstrap_indices[batch_ids], function(v) as.integer(v) - 1L)
      sens_min_c <- if (!is.null(spec$sensitivity_min)) as.numeric(spec$sensitivity_min) else -1.0
      spec_min_c <- if (!is.null(spec$specificity_min)) as.numeric(spec$specificity_min) else -1.0
      wave_df <- evaluate_bootstrap_replicates_native_cpp(
        x                  = x_mat,
        y                  = as.integer(y),
        boot_indices_list  = boot_wave_0,
        combo_0based       = combo_0based,
        cutoff_method      = spec$cutoff_method,
        selection_metric   = spec$selection_metric,
        sensitivity_min    = sens_min_c,
        specificity_min    = spec_min_c,
        prefer_fewer_items = spec$prefer_fewer_items,
        fallback_youden    = identical(no_feasible, "fallback_youden"),
        need_oob           = FALSE,
        ref_items_0based   = integer(0),
        num_threads        = eff_threads
      )

      for (k in seq_along(batch_ids)) {
        b <- batch_ids[k]
        st_code <- wave_df$status_code[k]
        c_idx <- wave_df$selected_combo_idx[k]
        cut_b <- wave_df$selected_cutoff[k]
        fb_b <- isTRUE(wave_df$fallback_used[k])
        # status_code: 0=selected, 1=fallback_youden, 2=no_feasible_candidate, 3=invalid_evaluation
        if (st_code == 3L) {
          st_str <- "undefined_score"
          reason_str <- "single_class_or_invalid_sample"
          eval_ok <- FALSE
        } else if (st_code == 2L) {
          st_str <- "infeasible_selection"
          reason_str <- "no_feasible_candidate"
          eval_ok <- FALSE
        } else if (st_code == 1L) {
          st_str <- "infeasible_selection"
          reason_str <- "no_feasible_candidate_fallback_youden"
          eval_ok <- TRUE
        } else {
          st_str <- "valid"
          reason_str <- NA_character_
          eval_ok <- TRUE
        }

        sel_items_b <- if (!is.na(c_idx)) combo_names[c_idx] else NA_character_
        sel_n_b <- if (!is.na(c_idx)) as.integer(combo_sizes[c_idx]) else NA_integer_

        if (eval_ok && !is.na(c_idx) && !is.na(cut_b)) {
          orig_sc_b <- as.numeric(rowSums(x_mat[, combo_list[[c_idx]], drop = FALSE]))
          pred_b <- as.integer(orig_sc_b >= cut_b)
          valid_count <- valid_count + 1L
          pos_count <- pos_count + pred_b
          if (!all(is.na(ref_class))) {
            flip_count <- flip_count + as.integer(pred_b != ref_class)
          }
        }
        rep_rows[[b]] <- data.frame(
          replicate_id   = b,
          status         = st_str,
          failure_reason = reason_str,
          fallback_used  = fb_b,
          cutoff         = if (eval_ok) cut_b else NA_real_,
          selected_items = sel_items_b,
          n_items        = sel_n_b,
          stringsAsFactors = FALSE
        )
      }
    } else {
      for (b in batch_ids) {
        idx_b <- bootstrap_indices[[b]]
        x_b <- x_mat[idx_b, , drop = FALSE]
        y_b <- y[idx_b]

        if (mode == "fixed_candidate") {
          ev <- .cs_eval_fixed_on_sample(
            x_b, y_b, seq_along(used_items),
            spec$cutoff_method, spec$sensitivity_min, spec$specificity_min, no_feasible
          )
          eval_ok <- (ev$status == "valid") || (ev$status == "infeasible_selection" && isTRUE(ev$fallback_used))
          sel_items_b <- combo_names
          sel_n_b <- length(used_items)
          if (eval_ok && !is.na(ev$cutoff)) {
            orig_sc_b <- ref_scores
            pred_b <- as.integer(orig_sc_b >= ev$cutoff)
            valid_count <- valid_count + 1L
            pos_count <- pos_count + pred_b
            if (!all(is.na(ref_class))) {
              flip_count <- flip_count + as.integer(pred_b != ref_class)
            }
          }
        } else {
          ev <- .cs_eval_full_on_sample_r(
            x_b, y_b, used_items, combo_list, combo_sizes, combo_names,
            spec$selection_metric, spec$cutoff_method,
            spec$sensitivity_min, spec$specificity_min,
            spec$prefer_fewer_items, no_feasible
          )
          eval_ok <- (ev$status == "valid") || (ev$status == "infeasible_selection" && isTRUE(ev$fallback_used))
          sel_items_b <- ev$selected_items
          sel_n_b <- ev$selected_n_items
          if (eval_ok && !is.na(ev$selected_combo_idx) && !is.na(ev$cutoff)) {
            orig_sc_b <- as.numeric(rowSums(x_mat[, combo_list[[ev$selected_combo_idx]], drop = FALSE]))
            pred_b <- as.integer(orig_sc_b >= ev$cutoff)
            valid_count <- valid_count + 1L
            pos_count <- pos_count + pred_b
            if (!all(is.na(ref_class))) {
              flip_count <- flip_count + as.integer(pred_b != ref_class)
            }
          }
        }

        rep_rows[[b]] <- data.frame(
          replicate_id   = b,
          status         = ev$status,
          failure_reason = ev$failure_reason,
          fallback_used  = isTRUE(ev$fallback_used),
          cutoff         = if (eval_ok) ev$cutoff else NA_real_,
          selected_items = sel_items_b,
          n_items        = sel_n_b,
          stringsAsFactors = FALSE
        )
      }
    }

    completed_reps <- cur_end
    if (!is.null(result_file)) {
      .save_atomic_rds(
        list(
          checkpoint_key = chk_key,
          completed_reps = completed_reps,
          valid_count    = valid_count,
          pos_count      = pos_count,
          flip_count     = flip_count,
          rep_rows       = rep_rows[seq_len(completed_reps)]
        ),
        result_file
      )
    }
    cur_start <- cur_end + 1L
  }

  replicate_summary <- do.call(rbind, rep_rows)
  rownames(replicate_summary) <- NULL

  # Subject-level summary (anonymous 1..n_obs indices; NA CII when valid_replicates == 0 or ref_class NA)
  pos_freq <- ifelse(valid_count > 0L, pos_count / valid_count, NA_real_)
  cii_vec <- ifelse(valid_count > 0L & !is.na(ref_class), flip_count / valid_count, NA_real_)

  subject_summary <- data.frame(
    subject_index      = seq_len(n_obs),
    ref_score          = ref_scores,
    ref_class          = ref_class,
    valid_replicates   = valid_count,
    positive_count     = pos_count,
    positive_frequency = pos_freq,
    flip_count         = flip_count,
    cii                = cii_vec,
    stringsAsFactors   = FALSE
  )

  # Replicate & cutoff summary
  n_valid_eval <- sum(!is.na(replicate_summary$cutoff))
  n_infeasible <- sum(replicate_summary$status == "infeasible_selection")
  n_undefined <- sum(replicate_summary$status == "undefined_score")
  n_failed <- sum(replicate_summary$status %in% c("computational_failure", "missing_data"))
  n_cancelled <- sum(replicate_summary$status == "cancelled")
  n_fallback <- sum(replicate_summary$fallback_used)
  failure_rate <- (bootstrap_reps - n_valid_eval) / bootstrap_reps

  valid_cuts <- replicate_summary$cutoff[!is.na(replicate_summary$cutoff)]
  same_scale <- (mode == "fixed_candidate") || (length(unique(replicate_summary$n_items[!is.na(replicate_summary$n_items)])) <= 1L)
  cutoff_summary <- list(
    ref_cutoff       = ref_cutoff,
    n_requested      = bootstrap_reps,
    n_valid          = n_valid_eval,
    n_infeasible     = n_infeasible,
    n_undefined      = n_undefined,
    n_failed         = n_failed,
    n_cancelled      = n_cancelled,
    n_fallback       = n_fallback,
    failure_rate     = failure_rate,
    failure_reasons  = table(replicate_summary$failure_reason[!is.na(replicate_summary$failure_reason)]),
    comparable_scale = same_scale,
    mean             = if (same_scale && length(valid_cuts) > 0L) mean(valid_cuts) else NA_real_,
    sd               = if (same_scale && length(valid_cuts) > 1L) stats::sd(valid_cuts) else NA_real_,
    median           = if (same_scale && length(valid_cuts) > 0L) stats::median(valid_cuts) else NA_real_,
    iqr              = if (same_scale && length(valid_cuts) > 0L) stats::IQR(valid_cuts) else NA_real_,
    q025             = if (same_scale && length(valid_cuts) > 0L) unname(stats::quantile(valid_cuts, 0.025)) else NA_real_,
    q975             = if (same_scale && length(valid_cuts) > 0L) unname(stats::quantile(valid_cuts, 0.975)) else NA_real_,
    bias_from_ref    = if (same_scale && length(valid_cuts) > 0L && !is.na(ref_cutoff)) mean(valid_cuts) - ref_cutoff else NA_real_,
    percentile_note  = "Bootstrap percentiles describe the resampling distribution, not independent-sample 95% confidence intervals."
  )

  # Aggregate CII summary
  valid_cii <- cii_vec[!is.na(cii_vec)]
  cii_summary <- list(
    n_subjects           = n_obs,
    n_evaluable_subjects = length(valid_cii),
    mean                 = if (length(valid_cii) > 0L) mean(valid_cii) else NA_real_,
    median               = if (length(valid_cii) > 0L) stats::median(valid_cii) else NA_real_,
    iqr                  = if (length(valid_cii) > 0L) stats::IQR(valid_cii) else NA_real_,
    min                  = if (length(valid_cii) > 0L) min(valid_cii) else NA_real_,
    max                  = if (length(valid_cii) > 0L) max(valid_cii) else NA_real_,
    prop_any_flip        = if (length(valid_cii) > 0L) mean(valid_cii > 0) else NA_real_,
    conditional_note     = if (n_valid_eval < bootstrap_reps) {
      sprintf("CII is conditioned on %d/%d evaluable bootstrap replicates; not an unconditional stability or external misclassification rate.", n_valid_eval, bootstrap_reps)
    } else {
      "CII measures bootstrap classification disagreement with the original-data reference model, not external misclassification rate."
    }
  )

  # Score instability boundary (§3.4)
  score_instability <- if (mode == "fixed_candidate") {
    list(
      status = "applicable_invariant",
      reason = "Unweighted sum score on a fixed item set is invariant across replicates; instability arises solely from cutoff variation."
    )
  } else {
    list(
      status = "not_comparable",
      reason = "Unweighted sum scores across varying item sets/sizes do not share a common calibrated scale; probability MAPE/calibration instability is not applicable."
    )
  }

  out <- structure(
    list(
      mode              = mode,
      reference_model   = list(
        status           = ref_eval$status,
        selected_items   = ref_items_str,
        n_items          = length(ref_items_vec),
        cutoff           = ref_cutoff,
        cutoff_note      = "Selected on full original data (post-selection reference cutoff)."
      ),
      cutoff_summary    = cutoff_summary,
      cii_summary       = cii_summary,
      subject_summary   = subject_summary,
      replicate_summary = replicate_summary,
      score_instability = score_instability,
      candidate_universe = list(
        scope                  = cand_scope,
        items                  = used_items,
        model_sizes            = spec$model_sizes,
        n_candidates_total     = n_cands_total,
        n_candidates_evaluated = n_cands_total
      ),
      settings = list(
        outcome            = outcome,
        resampling         = resampling,
        bootstrap_reps     = bootstrap_reps,
        cutoff_method      = spec$cutoff_method,
        sensitivity_min    = spec$sensitivity_min,
        specificity_min    = spec$specificity_min,
        selection_metric   = spec$selection_metric,
        prefer_fewer_items = spec$prefer_fewer_items,
        no_feasible        = no_feasible,
        engine             = if (use_native_full) "Rcpp" else "R",
        parallel           = if (use_native_full) parallel else "none",
        seed               = seed
      ),
      provenance = list(
        n_obs                    = n_obs,
        bootstrap_indices_digest = boot_digest,
        data_digest              = data_digest,
        full_data_digest         = .cp_hash_object(data),
        outcome_digest           = .cp_hash_object(y),
        rng_preserved            = !is.null(seed),
        v031_audit_status        = spec$v031_audit_status
      )
    ),
    class = "classification_stability_result"
  )

  if (!is.null(result_file)) {
    .save_atomic_rds(
      list(
        checkpoint_key = chk_key,
        completed_reps = bootstrap_reps,
        valid_count    = valid_count,
        pos_count      = pos_count,
        flip_count     = flip_count,
        rep_rows       = rep_rows,
        final_result   = out
      ),
      result_file
    )
  }

  out
}

#' Evaluate fixed-candidate cutoff stability
#'
#' Thin wrapper around [classification_stability_roc()] with
#' `mode = "fixed_candidate"`.
#'
#' @inheritParams classification_stability_roc
#' @param ... Additional arguments passed to [classification_stability_roc()].
#'
#' @return An S3 object of class `"classification_stability_result"`.
#' @export
cutoff_stability_roc <- function(
    data,
    outcome,
    candidate_set = NULL,
    selection_result = NULL,
    ...
) {
  classification_stability_roc(
    data             = data,
    outcome          = outcome,
    mode             = "fixed_candidate",
    candidate_set    = candidate_set,
    selection_result = selection_result,
    ...
  )
}

#' Compare fixed-candidate and full-procedure classification stability
#'
#' Compares two `"classification_stability_result"` objects only when both share
#' the exact same original dataset (`full_data_digest`, `outcome_digest`, `n_obs`)
#' and identical bootstrap resampling indices (`bootstrap_indices_digest`),
#' reporting separate failure rates alongside common-valid-replicate CII differences.
#'
#' @param fixed_res A `"classification_stability_result"` (typically `"fixed_candidate"`).
#' @param full_res A `"classification_stability_result"` (typically `"full_procedure"`).
#' @param data Optional original `data.frame`, required to recompute subject-level
#'   CII on the common valid replicate set when `fixed_res` and `full_res` have
#'   different failed replicates.
#'
#' @return An S3 list of class `"classification_stability_comparison"`.
#' @export
compare_classification_stability <- function(fixed_res, full_res, data = NULL) {
  if (!inherits(fixed_res, "classification_stability_result") ||
      !inherits(full_res, "classification_stability_result")) {
    stop("Both `fixed_res` and `full_res` must be `classification_stability_result` objects.", call. = FALSE)
  }

  is_valid_digest <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[0-9a-f]{64}$", x)
  has_prov <- function(p) {
    !is.null(p) && is.list(p) &&
      !is.null(p$n_obs) && is.numeric(p$n_obs) && length(p$n_obs) == 1L && !is.na(p$n_obs) &&
      is_valid_digest(p$full_data_digest) &&
      is_valid_digest(p$outcome_digest) &&
      is_valid_digest(p$bootstrap_indices_digest)
  }
  if (!has_prov(fixed_res$provenance) || !has_prov(full_res$provenance)) {
    return(structure(
      list(
        status             = "unverifiable_provenance",
        comparable         = FALSE,
        reason             = "Required provenance metadata (`n_obs`, `full_data_digest`, `outcome_digest`, `bootstrap_indices_digest`) is missing or invalid.",
        fixed_failure_rate = fixed_res$cutoff_summary$failure_rate,
        full_failure_rate  = full_res$cutoff_summary$failure_rate
      ),
      class = "classification_stability_comparison"
    ))
  }

  same_n <- identical(fixed_res$provenance$n_obs, full_res$provenance$n_obs)
  same_full_data <- identical(fixed_res$provenance$full_data_digest, full_res$provenance$full_data_digest)
  same_outcome <- identical(fixed_res$provenance$outcome_digest, full_res$provenance$outcome_digest)
  same_boot <- identical(fixed_res$provenance$bootstrap_indices_digest, full_res$provenance$bootstrap_indices_digest)

  if (!same_n || !same_full_data || !same_outcome || !same_boot) {
    reason_msg <- if (!same_n) {
      "Different original subject counts."
    } else if (!same_full_data || !same_outcome) {
      "Original data differ; paired comparison is prohibited."
    } else {
      "Bootstrap resampling indices differ; paired comparison is prohibited."
    }
    status_code <- if (!same_n || !same_full_data || !same_outcome) {
      "incompatible_original_data"
    } else {
      "incompatible_resampling_indices"
    }
    return(structure(
      list(
        status     = status_code,
        comparable = FALSE,
        reason     = reason_msg,
        fixed_failure_rate = fixed_res$cutoff_summary$failure_rate,
        full_failure_rate  = full_res$cutoff_summary$failure_rate
      ),
      class = "classification_stability_comparison"
    ))
  }

  fixed_valid <- !is.na(fixed_res$replicate_summary$cutoff)
  full_valid <- !is.na(full_res$replicate_summary$cutoff)
  common_valid <- fixed_valid & full_valid
  n_common <- sum(common_valid)

  if (n_common == 0L) {
    return(structure(
      list(
        status             = "empty_common_valid_set",
        comparable         = FALSE,
        reason             = "Common valid replicate set is empty; paired CII difference cannot be computed.",
        n_common_valid     = 0L,
        fixed_failure_rate = fixed_res$cutoff_summary$failure_rate,
        full_failure_rate  = full_res$cutoff_summary$failure_rate
      ),
      class = "classification_stability_comparison"
    ))
  }

  recompute_common_cii <- function(res_obj, common_idx, df_orig) {
    n_s <- res_obj$provenance$n_obs
    ref_c <- res_obj$subject_summary$ref_class
    if (all(is.na(ref_c))) return(rep(NA_real_, n_s))
    flips <- integer(n_s)
    for (b in common_idx) {
      it_str <- res_obj$replicate_summary$selected_items[b]
      cut_b <- res_obj$replicate_summary$cutoff[b]
      it_vec <- trimws(strsplit(it_str, "\\+")[[1L]])
      if (!all(it_vec %in% names(df_orig))) {
        stop("`data` is missing candidate columns needed to recompute common-valid-set CII.", call. = FALSE)
      }
      sc_b <- rowSums(as.matrix(df_orig[, it_vec, drop = FALSE]))
      pred_b <- as.integer(sc_b >= cut_b)
      flips <- flips + as.integer(pred_b != ref_c)
    }
    flips / length(common_idx)
  }

  if (all(fixed_valid == full_valid)) {
    common_fixed_cii <- fixed_res$subject_summary$cii
    common_full_cii <- full_res$subject_summary$cii
  } else {
    if (is.null(data)) {
      return(structure(
        list(
          status             = "common_valid_recomputation_requires_data",
          comparable         = FALSE,
          reason             = "Modes have different failed replicates; pass `data` to recompute paired CII on the common valid replicate set.",
          n_common_valid     = as.integer(n_common),
          fixed_failure_rate = fixed_res$cutoff_summary$failure_rate,
          full_failure_rate  = full_res$cutoff_summary$failure_rate
        ),
        class = "classification_stability_comparison"
      ))
    }
    if (!is.data.frame(data) || nrow(data) != fixed_res$provenance$n_obs) {
      stop("`data` must be a data.frame with nrow(data) matching the original subjects.", call. = FALSE)
    }
    common_idx <- which(common_valid)
    common_fixed_cii <- recompute_common_cii(fixed_res, common_idx, data)
    common_full_cii <- recompute_common_cii(full_res, common_idx, data)
  }

  diff_cii <- common_full_cii - common_fixed_cii
  structure(
    list(
      status                 = "comparable",
      comparable             = TRUE,
      n_common_valid         = as.integer(n_common),
      n_requested            = fixed_res$cutoff_summary$n_requested,
      fixed_failure_rate     = fixed_res$cutoff_summary$failure_rate,
      full_failure_rate      = full_res$cutoff_summary$failure_rate,
      fixed_mean_cii         = fixed_res$cii_summary$mean,
      full_mean_cii          = full_res$cii_summary$mean,
      fixed_mean_cii_common  = mean(common_fixed_cii, na.rm = TRUE),
      full_mean_cii_common   = mean(common_full_cii, na.rm = TRUE),
      common_fixed_cii       = common_fixed_cii,
      common_full_cii        = common_full_cii,
      mean_cii_diff          = mean(diff_cii, na.rm = TRUE),
      subject_cii_diff       = diff_cii,
      interpretation_note    = "Differences are computed on the common valid replicate set and reflect combined item-selection, cutoff, and candidate-pool interaction."
    ),
    class = "classification_stability_comparison"
  )
}

#' Print method for classification_stability_result
#'
#' @param x A `"classification_stability_result"` object.
#' @param ... Additional arguments (ignored).
#'
#' @return `x` invisibly.
#' @export
print.classification_stability_result <- function(x, ...) {
  cat(sprintf("NCVROC Classification & Cutoff Stability (%s)\n", x$mode))
  cat(sprintf("  Subjects: %d | Replicates: %d/%d valid (failure rate: %.1f%%)\n",
              x$provenance$n_obs,
              x$cutoff_summary$n_valid,
              x$cutoff_summary$n_requested,
              100 * x$cutoff_summary$failure_rate))
  cat(sprintf("  Candidate scope: %s (%d evaluated)\n",
              x$candidate_universe$scope,
              x$candidate_universe$n_candidates_evaluated))
  cat(sprintf("  Reference cutoff: %s\n",
              if (is.na(x$reference_model$cutoff)) "NA" else format(x$reference_model$cutoff, digits = 4)))
  if (!is.na(x$cii_summary$mean)) {
    cat(sprintf("  CII: mean=%.4f, median=%.4f, max=%.4f (any flip: %.1f%%)\n",
                x$cii_summary$mean, x$cii_summary$median, x$cii_summary$max,
                100 * x$cii_summary$prop_any_flip))
  } else {
    cat("  CII: NA (no evaluable replicates or reference classification)\n")
  }
  invisible(x)
}

#' Summary method for classification_stability_result
#'
#' @param object A `"classification_stability_result"` object.
#' @param ... Additional arguments (ignored).
#'
#' @return A summary list (returned invisibly after printing).
#' @export
summary.classification_stability_result <- function(object, ...) {
  print(object)
  cat("  Note:", object$cii_summary$conditional_note, "\n")
  cat("  Cutoff percentiles:", object$cutoff_summary$percentile_note, "\n")
  invisible(list(
    cutoff_summary     = object$cutoff_summary,
    cii_summary        = object$cii_summary,
    candidate_universe = object$candidate_universe,
    score_instability  = object$score_instability
  ))
}

#' Plot cutoff stability and individual classification instability
#'
#' Produces base-graphics diagnostic plots using anonymous subject indices:
#' `"cutoff"` (bootstrap cutoff distribution), `"instability"` (reference score
#' vs CII), `"frequency"` (positive classification frequency per subject), and
#' `"mode_comparison"` (paired Fixed vs Full CII comparison when bootstrap
#' indices match).
#'
#' @param x A `"classification_stability_result"` object.
#' @param type Character(1), one of `"cutoff"`, `"instability"`, `"frequency"`,
#'   or `"mode_comparison"`.
#' @param compare_to Optional second `"classification_stability_result"` for
#'   `type = "mode_comparison"`.
#' @param data Optional original `data.frame`, passed to
#'   [compare_classification_stability()] when modes have different failed
#'   replicates.
#' @param ... Additional graphical parameters passed to [graphics::plot()] or
#'   [graphics::hist()].
#'
#' @return `x` invisibly.
#' @export
plot.classification_stability_result <- function(
    x,
    type = c("cutoff", "instability", "frequency", "mode_comparison"),
    compare_to = NULL,
    data = NULL,
    ...
) {
  type <- match.arg(type)

  if (type == "cutoff") {
    cuts <- x$replicate_summary$cutoff[!is.na(x$replicate_summary$cutoff)]
    sub_txt <- sprintf("Valid: %d/%d | Method: %s | Scope: %s",
                       x$cutoff_summary$n_valid, x$cutoff_summary$n_requested,
                       x$settings$cutoff_method, x$candidate_universe$scope)
    if (length(cuts) == 0L || !isTRUE(x$cutoff_summary$comparable_scale)) {
      graphics::plot.new()
      graphics::title(main = "Cutoff Stability (Not Plottable)", sub = sub_txt)
      graphics::text(0.5, 0.5, if (length(cuts) == 0L) "No valid bootstrap cutoffs" else "Cutoffs not on a single comparable scale")
      return(invisible(x))
    }
    graphics::hist(cuts, main = "Bootstrap Cutoff Distribution", xlab = "Cutoff (sum score)", sub = sub_txt, col = "grey85", border = "white", ...)
    if (!is.na(x$reference_model$cutoff)) {
      graphics::abline(v = x$reference_model$cutoff, col = "firebrick", lwd = 2, lty = 2)
    }
  } else if (type == "instability") {
    ss <- x$subject_summary
    ok <- !is.na(ss$ref_score) & !is.na(ss$cii)
    sub_txt <- sprintf("Valid reps: %d/%d | Ref cutoff: %s",
                       x$cutoff_summary$n_valid, x$cutoff_summary$n_requested,
                       if (is.na(x$reference_model$cutoff)) "NA" else format(x$reference_model$cutoff, digits = 4))
    if (!any(ok)) {
      graphics::plot.new()
      graphics::title(main = "Classification Instability Index (CII)", sub = sub_txt)
      graphics::text(0.5, 0.5, "No evaluable subjects (CII = NA)")
      return(invisible(x))
    }
    graphics::plot(ss$ref_score[ok], ss$cii[ok],
                   xlab = "Reference sum score (original data)",
                   ylab = "Classification Instability Index (CII)",
                   main = sprintf("Individual Classification Instability (%s)", x$mode),
                   sub = sub_txt, ylim = c(0, 1), pch = 19, col = "steelblue", ...)
    if (!is.na(x$reference_model$cutoff)) {
      graphics::abline(v = x$reference_model$cutoff, col = "firebrick", lwd = 2, lty = 2)
    }
  } else if (type == "frequency") {
    ss <- x$subject_summary
    ok <- !is.na(ss$positive_frequency)
    sub_txt <- sprintf("Anonymous subject index | Valid reps: %d/%d",
                       x$cutoff_summary$n_valid, x$cutoff_summary$n_requested)
    if (!any(ok)) {
      graphics::plot.new()
      graphics::title(main = "Bootstrap Positive Classification Frequency", sub = sub_txt)
      graphics::text(0.5, 0.5, "No evaluable subjects")
      return(invisible(x))
    }
    graphics::plot(ss$subject_index[ok], ss$positive_frequency[ok],
                   xlab = "Anonymous subject index",
                   ylab = "Positive classification frequency",
                   main = sprintf("Classification Frequency (%s)", x$mode),
                   sub = sub_txt, ylim = c(0, 1), pch = 19, col = "darkgreen", ...)
  } else if (type == "mode_comparison") {
    if (is.null(compare_to)) {
      stop("`compare_to` must be supplied for `type = \"mode_comparison\"`.", call. = FALSE)
    }
    cmp <- compare_classification_stability(x, compare_to, data = data)
    if (!isTRUE(cmp$comparable)) {
      stop(sprintf("Mode comparison plot refused: %s", cmp$reason), call. = FALSE)
    }
    ok <- !is.na(cmp$common_fixed_cii) & !is.na(cmp$common_full_cii)
    sub_txt <- sprintf("Common valid reps: %d/%d | Fail rates: %.1f%% vs %.1f%%",
                       cmp$n_common_valid, cmp$n_requested,
                       100 * cmp$fixed_failure_rate, 100 * cmp$full_failure_rate)
    graphics::plot(cmp$common_fixed_cii[ok], cmp$common_full_cii[ok],
                   xlab = sprintf("CII (%s)", x$mode),
                   ylab = sprintf("CII (%s)", compare_to$mode),
                   main = "Mode Comparison Plot (Paired Bootstrap Indices)",
                   sub = sub_txt, xlim = c(0, 1), ylim = c(0, 1), pch = 19, col = "purple4", ...)
    graphics::abline(a = 0, b = 1, lty = 2, col = "grey40")
  }

  invisible(x)
}
