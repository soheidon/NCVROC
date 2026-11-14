# ==============================================================================
# Permutation-Based Null Benchmark (v0.33.0 Phase 3 - Part A)
# ==============================================================================

#' Compute exact collision-free key for a binary label vector
#' @keywords internal
#' @noRd
.v033_y_key <- function(y_vec) {
  paste(which(y_vec == 1L), collapse = ",")
}

#' Generate permutation labelings for exact or uniform unique Monte Carlo
#' @keywords internal
#' @noRd
.v033_generate_permutation_schedule <- function(y_obs,
                                                B,
                                                sampling = c("auto", "exact", "monte_carlo"),
                                                max_exact = 50000L,
                                                seed = NULL) {
  sampling <- match.arg(sampling)
  n_obs <- length(y_obs)
  n_pos <- sum(y_obs == 1L)
  log_N_all <- lchoose(n_obs, n_pos)
  N_all <- if (log_N_all < 53 * log(2)) round(exp(log_N_all)) else Inf

  use_exact <- identical(sampling, "exact") ||
    (identical(sampling, "auto") && is.finite(N_all) && (N_all - 1) <= B)

  if (use_exact) {
    if (!is.finite(N_all) || N_all > max_exact) {
      stop(
        sprintf(
          "Exact permutation enumeration requires %.0f labelings, exceeding `max_exact = %d`. Increase `max_exact` or use `sampling = \"monte_carlo\"`.",
          N_all, max_exact
        ),
        call. = FALSE
      )
    }
    comb_mat <- utils::combn(n_obs, n_pos, simplify = TRUE)
    n_total <- ncol(comb_mat)
    obs_pos <- which(y_obs == 1L)
    pos_list <- vector("list", n_total)
    obs_idx <- NA_integer_
    for (j in seq_len(n_total)) {
      p_j <- as.integer(comb_mat[, j])
      pos_list[[j]] <- p_j
      if (is.na(obs_idx) && identical(p_j, obs_pos)) {
        obs_idx <- j
      }
    }
    return(list(
      sampling_used = "exact",
      N_all         = N_all,
      log_N_all     = log_N_all,
      B_effective   = n_total,
      includes_obs  = TRUE,
      obs_index     = obs_idx,
      pos_list      = pos_list
    ))
  }

  # Monte Carlo: sample B unique labelings EXCLUDING y_obs
  if (is.finite(N_all) && B > (N_all - 1)) {
    stop(
      sprintf(
        "Requested `B = %d` unique non-observed permutations exceeds `N_all - 1 = %.0f`. Use `sampling = \"auto\"` or `\"exact\"`.",
        B, N_all - 1
      ),
      call. = FALSE
    )
  }

  pos_list <- .v033_with_seed(seed, {
    if (is.finite(N_all) && N_all <= 100000 && B >= 0.5 * N_all) {
      comb_mat <- utils::combn(n_obs, n_pos, simplify = TRUE)
      obs_key <- .v033_y_key(y_obs)
      all_pos <- vector("list", ncol(comb_mat))
      keep <- logical(ncol(comb_mat))
      for (j in seq_len(ncol(comb_mat))) {
        p_j <- as.integer(comb_mat[, j])
        all_pos[[j]] <- p_j
        keep[j] <- !identical(paste(p_j, collapse = ","), obs_key)
      }
      non_obs <- all_pos[keep]
      sel_idx <- sample.int(length(non_obs), size = B, replace = FALSE)
      non_obs[sel_idx]
    } else {
      seen <- new.env(hash = TRUE, parent = emptyenv())
      assign(.v033_y_key(y_obs), TRUE, envir = seen)
      out <- vector("list", B)
      for (b in seq_len(B)) {
        repeat {
          perm_pos <- sort(sample.int(n_obs, size = n_pos, replace = FALSE))
          key <- paste(perm_pos, collapse = ",")
          if (!exists(key, envir = seen, inherits = FALSE)) {
            assign(key, TRUE, envir = seen)
            out[[b]] <- perm_pos
            break
          }
        }
      }
      out
    }
  })

  list(
    sampling_used = "monte_carlo",
    N_all         = N_all,
    log_N_all     = log_N_all,
    B_effective   = B,
    includes_obs  = FALSE,
    obs_index     = NA_integer_,
    pos_list      = pos_list
  )
}

#' Permutation-Based Null Benchmark for Combinatorial ROC Selection
#'
#' Evaluates the null distribution of a combinatorial item-set selection
#' statistic by permuting the binary outcome labels \code{y} across rows while
#' preserving class counts and keeping the item matrix \code{X} fixed. Supports
#' three strictly separated null benchmark modes:
#' \itemize{
#'   \item \code{"apparent_max_auc"}: Maximum apparent selection metric across the candidate pool.
#'   \item \code{"cv_selected"}: Cross-validated performance of the selected model (regenerating folds per permutation).
#'   \item \code{"nested_cv"}: Out-of-sample nested-CV performance of the selection procedure (regenerating outer/inner folds per permutation).
#' }
#'
#' Automatically switches to exact enumeration without \code{+1} when
#' \code{B >= N_all - 1} (and \code{N_all <= max_exact}), or performs
#' bounded-memory uniform unique Monte Carlo sampling excluding the observed
#' labeling with Phipson & Smyth (2010) p-value \code{(1 + count) / (B + 1)}.
#'
#' @param data A \code{data.frame} containing outcome and item columns.
#' @param outcome Character name of the binary outcome column.
#' @param mode One of \code{"apparent_max_auc"}, \code{"cv_selected"}, or \code{"nested_cv"}.
#' @param items Optional character vector of item column names. Reconstructed from \code{selection_result} if omitted.
#' @param model_sizes Optional integer vector of candidate model sizes.
#' @param min_items Minimum number of items if \code{model_sizes} is NULL.
#' @param max_items Maximum number of items if \code{model_sizes} is NULL.
#' @param item_count Optional formula-like item count string (e.g. \code{"==4"} or \code{"1:4"}).
#' @param selection_result Optional result object from \code{ncvroc()}, \code{cross_size_cv()}, \code{cross_size_nested_cv()}, or \code{nested_sum_roc()}.
#' @param B Number of Monte Carlo permutations (default \code{199L}).
#' @param sampling One of \code{"auto"}, \code{"exact"}, or \code{"monte_carlo"}.
#' @param max_exact Maximum distinct labelings allowed for exact enumeration (default \code{50000L}).
#' @param cutoff_method Operating point method (\code{"youden"}, \code{"closest_topleft"}, \code{"sensitivity_target"}, \code{"clinical_constraint"}).
#' @param sensitivity_min Minimum sensitivity constraint if required by \code{cutoff_method}.
#' @param specificity_min Minimum specificity constraint if required by \code{cutoff_method}.
#' @param selection_metric Selection/test statistic metric (\code{"auc"}, \code{"youden"}, \code{"sensitivity"}, \code{"specificity"}, \code{"accuracy"}).
#' @param prefer_fewer_items Logical; break ties in favor of fewer items (default \code{TRUE}).
#' @param folds Number of CV / outer folds (default \code{5L}).
#' @param inner_folds Number of inner CV folds for \code{mode = "nested_cv"} (default \code{4L}).
#' @param repeats Number of CV repeats for \code{mode = "cv_selected"} (default \code{1L}).
#' @param outer_repeats Number of outer repeats for \code{mode = "nested_cv"} (default \code{1L}).
#' @param inner_repeats Number of inner repeats for \code{mode = "nested_cv"} (default \code{1L}).
#' @param stratified Logical; use stratified CV folds (default \code{TRUE}).
#' @param no_feasible Handling when no candidate meets clinical constraints (\code{"record_failure"} or \code{"fallback_youden"}).
#' @param positive_label Value of \code{outcome} representing positive cases (default \code{1}).
#' @param negative_label Value of \code{outcome} representing negative cases (default \code{0}).
#' @param exchangeable Logical; confirm observations are exchangeable (default \code{TRUE}).
#' @param cluster_col Optional column name identifying clusters/repeated measures (rejected if non-singleton clusters exist).
#' @param pool_pre_screened Logical; set \code{TRUE} if \code{items} were pre-screened on observed labels (invalidates unconditional p-value).
#' @param pool_provenance Candidate-pool provenance: \code{"auto"}, \code{"full_enumerated"}, \code{"restricted_observed_labels"}, or \code{"unknown"} (blocks p-value).
#' @param allow_restricted_pool Logical; if \code{TRUE} with \code{pool_pre_screened = TRUE}, still computes conditional p-value with a warning.
#' @param engine \code{"Rcpp"} or \code{"R"}.
#' @param parallel Parallel mode passed to underlying evaluators (\code{"none"}, \code{"threads"}, \code{"auto"}).
#' @param n_workers Optional worker/thread count.
#' @param seed Optional integer seed for reproducible permutation schedule and per-permutation RNG.
#' @param result_file Optional path to RDS checkpoint file for resume support.
#' @param checkpoint_every Save checkpoint every \code{checkpoint_every} permutations (default \code{25L}).
#' @param force_recompute Logical; overwrite incompatible/existing checkpoint (default \code{FALSE}).
#' @param progress Logical; show progress messages (default \code{interactive()}).
#'
#' @return An object of class \code{"permutation_selection_roc_result"}.
#' @export
permutation_selection_roc <- function(
    data,
    outcome,
    mode                  = c("apparent_max_auc", "cv_selected", "nested_cv"),
    items                 = NULL,
    model_sizes           = NULL,
    min_items             = 1L,
    max_items             = 4L,
    item_count            = NULL,
    selection_result      = NULL,
    B                     = 199L,
    sampling              = c("auto", "exact", "monte_carlo"),
    max_exact             = 50000L,
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
    stratified            = TRUE,
    no_feasible           = c("record_failure", "fallback_youden"),
    positive_label        = 1,
    negative_label        = 0,
    exchangeable          = TRUE,
    cluster_col           = NULL,
    pool_pre_screened     = FALSE,
    pool_provenance       = c("auto", "full_enumerated", "restricted_observed_labels", "unknown"),
    allow_restricted_pool = FALSE,
    engine                = c("Rcpp", "R"),
    parallel              = c("none", "threads", "auto"),
    n_workers             = NULL,
    seed                  = NULL,
    result_file           = NULL,
    checkpoint_every      = 25L,
    force_recompute       = FALSE,
    progress              = interactive()
) {
  mode            <- match.arg(mode)
  sampling        <- match.arg(sampling)
  no_feasible     <- match.arg(no_feasible)
  pool_provenance <- match.arg(pool_provenance)
  engine          <- match.arg(engine)
  parallel        <- match.arg(parallel)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1L) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)
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
    selection_result   = selection_result,
    pool_pre_screened  = pool_pre_screened,
    pool_provenance    = pool_provenance
  )

  if (!is.null(spec$pool_warning)) {
    warning(spec$pool_warning, call. = FALSE)
  }

  val <- .v033_validate_data(
    data           = data,
    outcome        = outcome,
    items          = spec$items,
    positive_label = positive_label,
    negative_label = negative_label,
    cluster_col    = cluster_col,
    exchangeable   = exchangeable,
    caller_name    = "permutation_selection_roc"
  )
  y_obs <- val$y
  x_mat <- val$x_mat
  n_obs <- val$n_obs

  proc_mode <- switch(mode,
    apparent_max_auc = "apparent",
    cv_selected      = "cv",
    nested_cv        = "nested_cv"
  )
  combo_list <- if (identical(proc_mode, "apparent")) {
    .v033_build_combos(length(spec$items), spec$model_sizes)
  } else {
    NULL
  }

  # Evaluate observed statistic T_obs
  obs_eval <- .v033_eval_selection(
    x_mat      = x_mat,
    y_vec      = y_obs,
    spec       = spec,
    procedure  = proc_mode,
    engine     = engine,
    parallel   = parallel,
    n_workers  = n_workers,
    seed       = .v033_derive_iter_seed(seed, 0L),
    combo_list = combo_list
  )
  if (!identical(obs_eval$status, "valid") && !isTRUE(obs_eval$fallback_used)) {
    stop(sprintf("Observed data selection failed (%s: %s).", obs_eval$status, obs_eval$failure_reason), call. = FALSE)
  }
  t_obs <- obs_eval$metric_value

  # Generate deterministic permutation schedule (exact vs bounded-memory unique MC)
  sched <- .v033_generate_permutation_schedule(
    y_obs     = y_obs,
    B         = B,
    sampling  = sampling,
    max_exact = max_exact,
    seed      = seed
  )
  n_perm <- sched$B_effective

  chk_key <- list(
    version            = "0.33.0",
    data_digest        = .v033_data_digest(x_mat, y_obs),
    outcome            = outcome,
    mode               = mode,
    n_obs              = n_obs,
    n_pos              = val$n_pos,
    n_neg              = val$n_neg,
    items              = spec$items,
    sizes              = spec$model_sizes,
    cutoff             = spec$cutoff_method,
    sens_min           = spec$sensitivity_min,
    spec_min           = spec$specificity_min,
    metric             = spec$selection_metric,
    prefer_fewer_items = spec$prefer_fewer_items,
    folds              = spec$folds,
    inner_folds        = spec$inner_folds,
    repeats            = spec$repeats,
    outer_repeats      = spec$outer_repeats,
    inner_repeats      = spec$inner_repeats,
    stratified         = spec$stratified,
    no_feasible        = spec$no_feasible,
    engine             = engine,
    pool_provenance    = spec$pool_provenance,
    sampling           = sched$sampling_used,
    n_perm             = n_perm,
    seed               = seed
  )

  completed_n <- 0L
  perm_values <- rep(NA_real_, n_perm)
  perm_status <- rep(NA_character_, n_perm)
  perm_items  <- rep(NA_character_, n_perm)

  if (!is.null(result_file) && file.exists(result_file) && !isTRUE(force_recompute)) {
    prev <- readRDS(result_file)
    if (!is.null(prev$checkpoint_key) && identical(prev$checkpoint_key, chk_key)) {
      completed_n <- as.integer(prev$completed_n)
      if (completed_n > 0L) {
        perm_values[seq_len(completed_n)] <- prev$perm_values[seq_len(completed_n)]
        perm_status[seq_len(completed_n)] <- prev$perm_status[seq_len(completed_n)]
        perm_items[seq_len(completed_n)]  <- prev$perm_items[seq_len(completed_n)]
      }
    } else {
      stop("Existing `result_file` checkpoint does not match current settings/data. Set `force_recompute = TRUE` to overwrite.", call. = FALSE)
    }
  }

  cur_start <- completed_n + 1L
  while (cur_start <= n_perm) {
    if (isTRUE(check_user_interrupt_cpp())) {
      stop("Permutation benchmark interrupted by user; completed waves saved if `result_file` was provided.", call. = FALSE)
    }
    cur_end <- min(cur_start + checkpoint_every - 1L, n_perm)
    for (b in seq.int(cur_start, cur_end)) {
      if (isTRUE(sched$includes_obs) && !is.na(sched$obs_index) && b == sched$obs_index) {
        perm_values[b] <- t_obs
        perm_status[b] <- obs_eval$status
        perm_items[b]  <- obs_eval$selected_label
      } else {
        y_perm <- integer(n_obs)
        y_perm[sched$pos_list[[b]]] <- 1L
        ev_b <- .v033_eval_selection(
          x_mat      = x_mat,
          y_vec      = y_perm,
          spec       = spec,
          procedure  = proc_mode,
          engine     = engine,
          parallel   = parallel,
          n_workers  = n_workers,
          seed       = .v033_derive_iter_seed(seed, b),
          combo_list = combo_list
        )
        perm_values[b] <- ev_b$metric_value
        perm_status[b] <- ev_b$status
        perm_items[b]  <- ev_b$selected_label
      }
    }
    completed_n <- cur_end
    if (!is.null(result_file)) {
      .save_atomic_rds(
        list(
          checkpoint_key = chk_key,
          completed_n    = completed_n,
          perm_values    = perm_values,
          perm_status    = perm_status,
          perm_items     = perm_items
        ),
        result_file
      )
    }
    if (isTRUE(progress)) {
      message(sprintf("[permutation_selection_roc] Completed %d / %d permutations", completed_n, n_perm))
    }
    cur_start <- cur_end + 1L
  }

  tol <- 1e-12
  valid_mask <- !is.na(perm_values) & (perm_status == "valid" | isTRUE(spec$no_feasible == "fallback_youden"))
  n_valid <- sum(valid_mask)
  n_failed <- n_perm - n_valid
  count_ge <- sum(perm_values[valid_mask] >= (t_obs - tol))

  if (identical(sched$sampling_used, "exact")) {
    p_min <- 1.0 / n_perm
    p_lower <- count_ge / n_perm
    p_upper <- (count_ge + n_failed) / n_perm
  } else {
    p_min <- 1.0 / (n_perm + 1.0)
    p_lower <- (1.0 + count_ge) / (n_perm + 1.0)
    p_upper <- (1.0 + count_ge + n_failed) / (n_perm + 1.0)
  }

  if (identical(spec$pool_provenance, "unknown")) {
    p_value <- NA_real_
    p_status <- "invalid_unknown_pool_provenance"
  } else if (identical(spec$pool_provenance, "restricted_pool_from_observed_labels") && !isTRUE(allow_restricted_pool)) {
    p_value <- NA_real_
    p_status <- "invalid_restricted_pool"
  } else if (n_failed > 0L) {
    p_value <- NA_real_
    p_status <- "incomplete_due_to_failures"
  } else {
    p_value <- p_lower
    p_status <- "valid"
  }

  valid_vals <- perm_values[valid_mask]
  null_summary <- if (length(valid_vals) > 0L) {
    list(
      mean   = mean(valid_vals),
      sd     = if (length(valid_vals) > 1L) stats::sd(valid_vals) else 0.0,
      median = stats::median(valid_vals),
      q95    = as.numeric(stats::quantile(valid_vals, probs = 0.95, names = FALSE)),
      max    = max(valid_vals)
    )
  } else {
    list(mean = NA_real_, sd = NA_real_, median = NA_real_, q95 = NA_real_, max = NA_real_)
  }

  structure(
    list(
      mode             = mode,
      sampling         = sched$sampling_used,
      observed_stat    = t_obs,
      observed_model   = obs_eval$selected_label,
      observed_items   = obs_eval$selected_items,
      observed_cutoff  = obs_eval$cutoff,
      p_value          = p_value,
      p_value_lower    = p_lower,
      p_value_upper    = p_upper,
      p_min            = p_min,
      p_value_status   = p_status,
      n_permutations   = n_perm,
      n_valid          = as.integer(n_valid),
      n_failed         = as.integer(n_failed),
      count_ge         = as.integer(count_ge),
      N_all            = sched$N_all,
      log_N_all        = sched$log_N_all,
      null_values      = perm_values,
      null_status      = perm_status,
      null_models      = perm_items,
      null_summary     = null_summary,
      pool_provenance  = spec$pool_provenance,
      pool_warning     = spec$pool_warning,
      settings         = list(
        outcome            = outcome,
        items              = spec$items,
        model_sizes        = spec$model_sizes,
        mode               = mode,
        B                  = B,
        sampling_requested = sampling,
        sampling_used      = sched$sampling_used,
        cutoff_method      = spec$cutoff_method,
        sensitivity_min    = spec$sensitivity_min,
        specificity_min    = spec$specificity_min,
        selection_metric   = spec$selection_metric,
        prefer_fewer_items = spec$prefer_fewer_items,
        folds              = spec$folds,
        inner_folds        = spec$inner_folds,
        repeats            = spec$repeats,
        outer_repeats      = spec$outer_repeats,
        inner_repeats      = spec$inner_repeats,
        stratified         = spec$stratified,
        no_feasible        = spec$no_feasible,
        n_obs              = n_obs,
        n_pos              = val$n_pos,
        n_neg              = val$n_neg,
        seed               = seed
      )
    ),
    class = "permutation_selection_roc_result"
  )
}
