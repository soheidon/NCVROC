# cross_size_nested_cv.R — Cross-size nested cross-validation
#
# Provides:
#   - cross_size_nested_cv()
#   - .evaluate_cross_size_outer_fold()
#   - print.cross_size_nested_cv_result()
#   - summary.cross_size_nested_cv_result()

#' Symbols required by worker processes during cross-size outer-fold parallel execution
#' @keywords internal
.CROSS_SIZE_OUTER_EXPORT_SYMBOLS <- c(
  "cross_size_cv",
  ".evaluate_cross_size_outer_fold",
  ".resolve_outcome",
  ".resolve_items",
  ".prepare_ncvroc_data",
  ".resolve_model_sizes",
  ".resolve_parallel_mode",
  ".resolve_n_workers",
  ".build_cv_folds",
  ".build_loocv_folds",
  ".make_stratified_cv_folds",
  ".count_total_combos_cross_size",
  ".select_cross_size_auc_exact",
  ".create_initial_block_streams",
  ".merge_block_streams_hierarchical",
  ".open_block_stream_writer",
  ".open_block_stream_reader",
  ".create_cross_size_block_stream_iterator",
  ".run_fixed_model_cv",
  ".aggregate_oof_metrics",
  ".order_and_rank_candidates",
  ".update_running_top_n",
  ".combination_unrank",
  ".parse_itemset",
  "format_items",
  "exhaustive_sum_roc",
  "compute_score_frequencies",
  "compute_roc_metrics_from_table",
  "find_optimal_cutoff",
  "select_operating_point",
  "compute_auc_from_table",
  "add_performance_cis",
  "cv_sum_roc",
  "loocv_sum_roc",
  "evaluate_combos_cv_cpp"
)

#' Evaluate a single outer fold in cross-size nested cross-validation
#'
#' @param progress_callback Optional callback function receiving progress increment counts.
#' @keywords internal
.evaluate_cross_size_outer_fold <- function(train_data,
                                            test_data,
                                            train_y,
                                            test_y,
                                            outcome_name,
                                            item_names,
                                            sizes,
                                            inner_folds,
                                            inner_repeats,
                                            stratified,
                                            selection_metric,
                                            cutoff_method,
                                            sensitivity_min,
                                            specificity_min,
                                            prefer_fewer_items,
                                            positive_label,
                                            negative_label,
                                            engine,
                                            parallel_mode,
                                            n_workers_res,
                                            threads_per_worker,
                                            seed,
                                            fold_name = "Fold1",
                                            rep_id = 1L,
                                            f_id = 1L,
                                            test_idx = seq_len(nrow(test_data)),
                                            progress_callback = NULL) {
  # Inner parallel setting per outer fold (strict oversubscription prevention)
  inner_parallel <- if (parallel_mode == "threads") {
    "threads"
  } else if (parallel_mode == "chunks") {
    "chunks"
  } else if (parallel_mode == "hybrid") {
    "threads"
  } else {
    "none"
  }

  inner_workers <- if (parallel_mode == "hybrid") {
    threads_per_worker
  } else if (parallel_mode %in% c("threads", "chunks")) {
    n_workers_res
  } else {
    1L
  }

  # Execute inner model selection strictly on outer training set
  inner_fit <- cross_size_cv(
    data               = train_data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = sizes,
    cv_method          = "kfold",
    folds              = inner_folds,
    repeats            = inner_repeats,
    stratified         = stratified,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    top_n              = 1L,
    prefer_fewer_items = prefer_fewer_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = inner_parallel,
    n_workers          = inner_workers,
    tuning             = "off",
    ci                 = FALSE,
    seed               = seed,
    progress           = FALSE,
    progress_callback  = progress_callback,
    error_on_empty     = FALSE
  )

  is_selected <- !is.null(inner_fit$final_selected_model) &&
    nrow(inner_fit$final_selected_model) > 0L &&
    identical(inner_fit$final_selection_status, "selected") &&
    !is.na(inner_fit$final_full_data_cutoff)

  if (is_selected) {
    selected_model_str <- inner_fit$final_selected_model$items
    selected_items_vec <- .parse_itemset(selected_model_str)
    selected_n_items   <- as.integer(inner_fit$final_selected_model$n_items)
    train_cutoff       <- as.numeric(inner_fit$final_full_data_cutoff)

    # Apply selected model and training cutoff to outer test set ONLY
    test_scores <- rowSums(test_data[, selected_items_vec, drop = FALSE])
    pred_class  <- ifelse(test_scores >= train_cutoff, 1L, 0L)

    test_freq <- compute_score_frequencies(test_scores, test_y)
    test_auc  <- compute_auc_from_table(test_freq$pos_counts, test_freq$neg_counts)

    tp <- sum(pred_class == 1L & test_y == 1L)
    tn <- sum(pred_class == 0L & test_y == 0L)
    fp <- sum(pred_class == 1L & test_y == 0L)
    fn <- sum(pred_class == 0L & test_y == 1L)

    sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
    ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    npv  <- if (tn + fn > 0) tn / (tn + fn) else NA_real_
    acc  <- if (tp + tn + fp + fn > 0) (tp + tn) / (tp + tn + fp + fn) else NA_real_
    youd <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1
    fold_status <- "selected"
  } else {
    selected_model_str <- NA_character_
    selected_n_items   <- NA_integer_
    train_cutoff       <- NA_real_

    test_scores <- rep(NA_real_, nrow(test_data))
    pred_class  <- rep(NA_integer_, nrow(test_data))
    test_auc    <- NA_real_
    sens        <- NA_real_
    spec        <- NA_real_
    ppv         <- NA_real_
    npv         <- NA_real_
    acc         <- NA_real_
    youd        <- NA_real_
    fold_status <- if (!is.null(inner_fit$final_selection_status)) inner_fit$final_selection_status else "no_feasible_candidate"
  }

  n_feasible <- if (!is.null(inner_fit$n_feasible_candidates)) {
    as.integer(inner_fit$n_feasible_candidates)
  } else if (!is.null(inner_fit$candidate_ranking)) {
    as.integer(nrow(inner_fit$candidate_ranking))
  } else {
    0L
  }
  n_total_combos <- if (!is.null(inner_fit$total_combinations)) as.numeric(inner_fit$total_combinations) else 0.0

  fold_result_row <- data.frame(
    outer_fold            = if (is.null(fold_name)) paste0("Fold", f_id) else fold_name,
    repeat_id             = as.integer(rep_id),
    fold_id               = as.integer(f_id),
    selected_items        = selected_model_str,
    selected_n_items      = as.integer(selected_n_items),
    selected_cutoff       = train_cutoff,
    outer_auc             = test_auc,
    outer_sensitivity     = sens,
    outer_specificity     = spec,
    outer_youden          = youd,
    outer_accuracy        = acc,
    outer_ppv             = ppv,
    outer_npv             = npv,
    selection_status      = fold_status,
    n_feasible_candidates = n_feasible,
    n_candidates_total    = n_total_combos,
    stringsAsFactors      = FALSE
  )

  preds_df <- data.frame(
    row_index       = test_idx,
    repeat_id       = as.integer(rep_id),
    fold_id         = as.integer(f_id),
    outer_fold      = if (is.null(fold_name)) paste0("Fold", f_id) else fold_name,
    true_outcome    = test_y,
    predicted_score = test_scores,
    predicted_class = pred_class,
    applied_cutoff  = train_cutoff,
    selected_items  = selected_model_str,
    selected_n_items= as.integer(selected_n_items),
    stringsAsFactors = FALSE
  )

  list(
    fold_result = fold_result_row,
    predictions = preds_df
  )
}

#' Cross-size nested cross-validation for item-set model selection
#'
#' Performs nested cross-validation where the full candidate model space across
#' multiple model sizes is explored in each outer training set, the single best
#' model is selected via inner cross-validation, and the selected model is
#' evaluated on the completely independent outer test set.
#'
#' @param data A data.frame containing the outcome and item columns.
#' @param outcome Name of the binary outcome column (bare symbol or character string).
#' @param items Item columns to evaluate (bare range e.g. `Q1:Q10`, bare names in `c()`,
#'   character vector, or numeric positions).
#' @param model_sizes Integer vector of model sizes to evaluate (e.g. `1:5` or `c(1, 3, 5)`).
#'   If `NULL`, resolved from `min_items:max_items` or `item_count`.
#' @param min_items Integer, minimum items per model (default 1, used if `model_sizes` is `NULL`).
#' @param max_items Integer, maximum items per model (default 4, used if `model_sizes` is `NULL`).
#' @param item_count String specification (e.g. `"==4"`, `"<=3"`, `"2:4"`, used if `model_sizes` is `NULL`).
#' @param outer_folds Integer, number of outer CV folds (default 5). Alias: `outer_k`.
#' @param inner_folds Integer, number of inner CV folds (default 4). Alias: `inner_k`.
#' @param outer_repeats Integer, number of outer CV repeats (default 5).
#' @param inner_repeats Integer, number of inner CV repeats (default 1).
#' @param selection_metric Metric for inner model selection:
#'   `"auc"` (default), `"youden"`, `"sensitivity"`, `"specificity"`, or `"accuracy"`.
#' @param cutoff_method Cutoff selection rule: `"youden"` (default), `"closest_topleft"`,
#'   `"sensitivity_target"` (target sensitivity with maximum specificity), or
#'   `"clinical_constraint"` (joint sensitivity and specificity constraints with maximum Youden).
#' @param sensitivity_min Minimum sensitivity threshold (numeric in `[0, 1]`).
#'   Required when `cutoff_method = "sensitivity_target"` or `"clinical_constraint"`.
#'   Optional post-evaluation candidate-level filter when `cutoff_method = "youden"` or `"closest_topleft"`.
#' @param specificity_min Minimum specificity threshold (numeric in `[0, 1]`).
#'   Required when `cutoff_method = "clinical_constraint"`. Prohibited when `cutoff_method = "sensitivity_target"`.
#'   Optional post-evaluation candidate-level filter when `cutoff_method = "youden"` or `"closest_topleft"`.
#' @param prefer_fewer_items Logical, prefer smaller models on ties (default `TRUE`).
#' @param stratified Logical, maintain class balance across folds (default `TRUE`).
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param engine Combinatorial computation engine: `"Rcpp"` (default) or `"R"`.
#' @param parallel Parallel mode: `FALSE` / `"none"` (default), `"outer"`, `"threads"`,
#'   `"chunks"`, or `"hybrid"`.
#'   - `"none"`: sequential execution.
#'   - `"outer"`: outer CV folds evaluated in parallel using socket workers (PSOCK cluster).
#'   - `"threads"`: outer folds sequential; inner exhaustive candidate evaluation uses multi-threaded C++ (RcppParallel).
#'   - `"chunks"`: outer folds sequential; inner exhaustive candidate evaluation uses socket workers.
#'   - `"hybrid"`: outer CV folds use socket workers (`n_workers`), and each worker uses multi-threaded C++ (`threads_per_worker`).
#' @param n_workers Integer, number of worker processes or threads (default `NULL` = auto).
#' @param threads_per_worker Integer, threads per PSOCK worker in `"hybrid"` mode (default 1).
#' @param tuning Automatic execution-planning mode: `"off"` (default; manual
#'   execution configuration is authoritative), `"auto"`, or `"always"`.
#'   Nested runtime probing is candidate-bounded and preserves the complete fold
#'   structure. v0.19.0 does not perform a nested resource sweep when the nested
#'   evaluator cannot accept rank-bounded candidates; it retains the
#'   manual/default configuration and records the fallback in `execution_plan`.
#' @param seed Integer, random seed for reproducible fold generation.
#' @param progress Logical, report observable progress (default
#'   \code{interactive()}). Sequential outer-fold work may show observed progress
#'   and approximate ETA. PSOCK `"outer"`, `"chunks"`, and `"hybrid"` paths report
#'   only truthful start and successful completion, with no percentage or ETA.
#'   `FALSE` is silent.
#' @param verbose Logical, print progress messages (default `FALSE`).
#' @param outer_k Alias for `outer_folds`.
#' @param inner_k Alias for `inner_folds`.
#'
#' @param result_file Optional character string specifying a file path (RDS) to store
#'   or load the completed analysis result. If the file exists and contains a valid,
#'   matching result for the current analysis, it is returned immediately without
#'   recomputation. If the file does not exist, the analysis runs normally and the
#'   completed result is atomically saved upon success. Default \code{NULL} disables
#'   result caching.
#' @param force_recompute Logical, whether to force recomputation even if a valid
#'   cached result file exists at \code{result_file}. The existing file is only replaced
#'   after new computation finishes successfully. Default \code{FALSE}. Specifying
#'   \code{force_recompute = TRUE} when \code{result_file = NULL} raises an error.
#'
#' @return An S3 object of class `"cross_size_nested_cv_result"`, containing:
#' \describe{
#'   \item{summary}{Summary data.frame of pooled outer-fold performance metrics (AUC, sensitivity, specificity, Youden index, accuracy, PPV, NPV).}
#'   \item{outer_fold_results}{A 16-column data.frame containing outer-fold evaluation details:
#'     `outer_fold` (character),
#'     `repeat_id` (integer),
#'     `fold_id` (integer),
#'     `selected_items` (character),
#'     `selected_n_items` (integer),
#'     `selected_cutoff` (numeric),
#'     `outer_auc` (numeric),
#'     `outer_sensitivity` (numeric),
#'     `outer_specificity` (numeric),
#'     `outer_youden` (numeric),
#'     `outer_accuracy` (numeric),
#'     `outer_ppv` (numeric),
#'     `outer_npv` (numeric),
#'     `selection_status` (character, e.g. "success" or "no_feasible_candidate"),
#'     `n_feasible_candidates` (integer),
#'     `n_candidates_total` (numeric).
#'   }
#'   \item{predictions}{A data.frame of outer out-of-fold predictions.}
#'   \item{model_size_distribution}{Frequency distribution of selected model sizes across outer folds.}
#'   \item{selected_models_summary}{Summary table of unique models selected across outer folds.}
#'   \item{item_selection_frequency}{Frequency table of individual item selections.}
#'   \item{settings}{Execution settings and parameter configuration. When
#'     \code{tuning = "auto"} or \code{"always"}, \code{settings$execution_plan}
#'     contains planning metadata detailing the selected execution backend and
#'     approximate runtime estimates. \code{tuning = "off"} attaches no execution
#'     plan metadata.}
#' }
#'
#' @export
cross_size_nested_cv <- function(data,
                                 outcome,
                                 items,
                                 model_sizes        = NULL,
                                 min_items          = 1,
                                 max_items          = 4,
                                 item_count         = NULL,
                                 outer_folds        = 5,
                                 inner_folds        = 4,
                                 outer_repeats      = 5,
                                 inner_repeats      = 1,
                                 selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                                 cutoff_method      = c("youden", "closest_topleft", "sensitivity_target", "clinical_constraint"),
                                 sensitivity_min    = NULL,
                                 specificity_min    = NULL,
                                 prefer_fewer_items = TRUE,
                                 stratified         = TRUE,
                                 positive_label     = 1,
                                 negative_label     = 0,
                                 engine             = c("Rcpp", "R"),
                                 parallel           = FALSE,
                                 n_workers          = NULL,
                                 threads_per_worker = 1L,
                                 tuning             = "off",
                                 seed               = NULL,
                                 progress           = interactive(),
                                 verbose            = FALSE,
                                 outer_k            = NULL,
                                 inner_k            = NULL,
                                 result_file        = NULL,
                                 force_recompute    = FALSE) {
  # ---- Validate result_file and force_recompute ----
  if (!is.logical(force_recompute) || length(force_recompute) != 1L || is.na(force_recompute)) {
    stop("`force_recompute` must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (isTRUE(force_recompute) && is.null(result_file)) {
    stop("`force_recompute = TRUE` was specified, but `result_file` is NULL.", call. = FALSE)
  }
  if (!is.null(result_file)) {
    if (!is.character(result_file) || length(result_file) != 1L || is.na(result_file) || nchar(trimws(result_file)) == 0L) {
      stop("`result_file` must be a single non-empty character string (file path) or NULL.", call. = FALSE)
    }
    if (dir.exists(result_file)) {
      stop(sprintf("`result_file` points to an existing directory, not a file path: '%s'.", result_file), call. = FALSE)
    }
  }

  # Handle aliases
  if (!is.null(outer_k)) outer_folds <- outer_k
  if (!is.null(inner_k)) inner_folds <- inner_k

  # ---- NSE Column Resolution ----
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)

  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  engine           <- match.arg(engine)
  tuning           <- match.arg(tuning, c("off", "auto", "always"))

  # ---- Contract C1: Validate cutoff_method constraints ----
  if (cutoff_method == "sensitivity_target") {
    if (is.null(sensitivity_min)) {
      stop("`sensitivity_min` is required when `cutoff_method = \"sensitivity_target\"`.", call. = FALSE)
    }
    if (!is.null(specificity_min)) {
      stop("`specificity_min` is prohibited and must not be specified when `cutoff_method = \"sensitivity_target\"`.", call. = FALSE)
    }
  } else if (cutoff_method == "clinical_constraint") {
    if (is.null(sensitivity_min)) {
      stop("`sensitivity_min` is required when `cutoff_method = \"clinical_constraint\"`.", call. = FALSE)
    }
    if (is.null(specificity_min)) {
      stop("`specificity_min` is required when `cutoff_method = \"clinical_constraint\"`.", call. = FALSE)
    }
  }

  # ---- Validate constraints ----
  if (!is.null(sensitivity_min)) {
    if (!is.numeric(sensitivity_min) || length(sensitivity_min) != 1 ||
        is.na(sensitivity_min) || sensitivity_min < 0 || sensitivity_min > 1) {
      stop("`sensitivity_min` must be a numeric value between 0 and 1.", call. = FALSE)
    }
  }
  if (!is.null(specificity_min)) {
    if (!is.numeric(specificity_min) || length(specificity_min) != 1 ||
        is.na(specificity_min) || specificity_min < 0 || specificity_min > 1) {
      stop("`specificity_min` must be a numeric value between 0 and 1.", call. = FALSE)
    }
  }

  # ---- Prepare Data ----
  dat_prep <- .prepare_ncvroc_data(data, outcome_name, item_names)
  y <- dat_prep[[outcome_name]]

  if (!all(y %in% c(positive_label, negative_label))) {
    stop("Outcome contains values other than positive_label and negative_label.", call. = FALSE)
  }
  y <- ifelse(y == positive_label, 1L, 0L)
  if (sum(y == 1L) == 0L || sum(y == 0L) == 0L) {
    stop("Outcome must contain both positive and negative cases.", call. = FALSE)
  }
  n_total <- length(y)
  n_items_total <- length(item_names)

  # ---- Resolve model_sizes ----
  sizes <- .resolve_model_sizes(
    model_sizes = model_sizes,
    min_items   = min_items,
    max_items   = max_items,
    item_count  = item_count,
    n_available = n_items_total
  )

  # ---- Parallel Mode Resolution ----
  parallel_mode <- .resolve_parallel_mode(
    parallel,
    context = "nested",
    allowed = c("none", "outer", "chunks", "threads", "hybrid")
  )

  if (!is.null(n_workers)) {
    if (!is.numeric(n_workers) || length(n_workers) != 1 ||
        is.na(n_workers) || n_workers <= 0 || n_workers != as.integer(n_workers)) {
      stop("`n_workers` must be a positive integer or NULL.", call. = FALSE)
    }
    n_workers <- as.integer(n_workers)
  }

  threads_per_worker <- .validate_threads_per_worker(threads_per_worker)
  if (parallel_mode != "hybrid" && threads_per_worker != 1L) {
    stop("`threads_per_worker` can only exceed 1 when `parallel = 'hybrid'`.",
         call. = FALSE)
  }
  if (parallel_mode %in% c("threads", "hybrid") && engine != "Rcpp") {
    stop(sprintf("`parallel = '%s'` requires `engine = 'Rcpp'`.", parallel_mode), call. = FALSE)
  }

  # ---- Result-RDS Cache Check (v0.23.2-R1) ----
  identity_obj <- NULL
  lock_token <- NULL
  canonical_target <- NULL
  if (!is.null(result_file)) {
    canonical_target <- .canonicalize_result_path(result_file)
    identity_obj <- .compute_nested_cv_result_identity(
      data               = dat_prep,
      outcome_name       = outcome_name,
      item_names         = item_names,
      sizes              = sizes,
      outer_folds        = outer_folds,
      inner_folds        = inner_folds,
      outer_repeats      = outer_repeats,
      inner_repeats      = inner_repeats,
      selection_metric   = selection_metric,
      cutoff_method      = cutoff_method,
      sensitivity_min    = sensitivity_min,
      specificity_min    = specificity_min,
      prefer_fewer_items = prefer_fewer_items,
      stratified         = stratified,
      positive_label     = positive_label,
      negative_label     = negative_label,
      engine             = engine,
      parallel_mode      = parallel_mode,
      n_workers          = n_workers,
      threads_per_worker = threads_per_worker,
      tuning             = tuning,
      seed               = seed
    )

    # 1. Read-only cache hit check (if !force_recompute and target exists)
    if (!force_recompute) {
      cached_res <- .check_readonly_nested_cv_hit(
        canonical_target = canonical_target,
        identity_obj     = identity_obj,
        verbose          = verbose
      )
      if (!is.null(cached_res)) {
        return(cached_res)
      }
    }

    # 2. Miss or force_recompute = TRUE: acquire lock, probe writable parent, recover transactions
    parent_dir <- dirname(canonical_target)
    if (parent_dir != "" && !dir.exists(parent_dir)) {
      created <- dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
      if (!created && !dir.exists(parent_dir)) {
        stop(sprintf("Failed to create parent directory for `result_file`: '%s'.", parent_dir), call. = FALSE)
      }
    }

    # Acquire lock before any computation
    lock_token <- .acquire_nested_cv_lock(canonical_target)
    on.exit(.release_nested_cv_lock(lock_token), add = TRUE)

    # Probe parent directory writable
    if (!.probe_parent_writable(parent_dir)) {
      stop(sprintf("Parent directory for `result_file` is not writable: '%s'.", parent_dir), call. = FALSE)
    }

    # Check/recover transaction crash state under lock
    .recover_nested_cv_transactions(canonical_target, lock_token)

    # 3. Post-lock recheck (if another writer completed just before lock acquisition)
    if (!force_recompute && file.exists(canonical_target)) {
      cached_env <- tryCatch(readRDS(canonical_target), error = function(e) e)
      .validate_nested_cv_envelope(cached_env, identity_obj, canonical_target)
      if (isTRUE(verbose)) {
        message(sprintf("Loaded cached cross_size_nested_cv result from: %s", canonical_target))
      }
      return(cached_env$result)
    }
  }

  # Test seam hook: increment computation count
  .ncvroc_test_seams$computation_count <- .ncvroc_test_seams$computation_count + 1L

  # ---- Build Outer CV Folds ----
  outer_fold_indices <- .build_cv_folds(
    y          = y,
    cv_method  = "kfold",
    folds      = outer_folds,
    repeats    = outer_repeats,
    stratified = stratified,
    seed       = seed
  )

  n_outer_evals   <- length(outer_fold_indices)
  effective_folds <- n_outer_evals %/% outer_repeats

  # Deterministically precompute seeds for each outer fold
  fold_seeds <- if (!is.null(seed)) seed + seq_len(n_outer_evals) * 1000L else rep(list(NULL), n_outer_evals)

  # ---- Resolve Worker Budget ----
  execution_metadata <- NULL
  if (!identical(tuning, "off")) {
    planned <- .planner_cross_size_nested_cv_controller(
      dat_prep                   = dat_prep,
      y                          = y,
      outcome_name               = outcome_name,
      item_names                 = item_names,
      sizes                      = sizes,
      outer_fold_indices         = outer_fold_indices,
      inner_folds                = inner_folds,
      inner_repeats              = inner_repeats,
      stratified                 = stratified,
      selection_metric           = selection_metric,
      cutoff_method              = cutoff_method,
      sensitivity_min            = sensitivity_min,
      specificity_min            = specificity_min,
      prefer_fewer_items         = prefer_fewer_items,
      positive_label             = positive_label,
      negative_label             = negative_label,
      engine                     = engine,
      tuning                     = tuning,
      manual_parallel_mode       = parallel_mode,
      manual_n_workers           = n_workers,
      manual_threads_per_worker  = threads_per_worker,
      fold_seeds                 = fold_seeds,
      progress                   = progress
    )
    execution_metadata <- planned$metadata
    parallel_mode <- planned$plan$parallel[[1L]]
    n_workers_res <- if (!is.null(planned$plan$outer_workers)) planned$plan$outer_workers[[1L]] else planned$plan$n_workers[[1L]]
    threads_per_worker_res <- if (!is.null(planned$plan$threads_per_worker)) planned$plan$threads_per_worker[[1L]] else 1L
    if (parallel_mode == "threads") {
      n_workers_res <- 1L
    }
    if (isTRUE(planned$warn)) {
      warning(execution_metadata$fallback_reason, call. = FALSE)
    }
  } else {
    if (parallel_mode == "hybrid") {
      hybrid_budget <- .resolve_hybrid_budget(n_workers, threads_per_worker, n_outer_evals)
      n_workers_res <- hybrid_budget$n_workers
      threads_per_worker_res <- hybrid_budget$threads_per_worker
    } else {
      n_workers_res <- if (parallel_mode != "none") {
        .resolve_n_workers(
          parallel  = parallel_mode,
          n_workers = n_workers,
          n_folds   = if (parallel_mode == "outer") n_outer_evals else NULL
        )
      } else {
        1L
      }
      threads_per_worker_res <- 1L
    }
  }

  # ---- Helper to evaluate a single outer fold ----
  eval_outer_fold <- function(f, progress_callback = NULL) {
    fold_name <- names(outer_fold_indices)[f]
    test_idx  <- outer_fold_indices[[f]]
    train_idx <- setdiff(seq_len(n_total), test_idx)

    rep_id <- 1L
    f_id   <- f
    if (!is.null(fold_name) && grepl("^Rep([0-9]+)_Fold([0-9]+)$", fold_name)) {
      parts <- regmatches(fold_name, regexec("^Rep([0-9]+)_Fold([0-9]+)$", fold_name))[[1]]
      rep_id <- as.integer(parts[2])
      f_id   <- as.integer(parts[3])
    }

    train_data <- dat_prep[train_idx, , drop = FALSE]
    test_data  <- dat_prep[test_idx, , drop = FALSE]
    train_y    <- y[train_idx]
    test_y     <- y[test_idx]

    .evaluate_cross_size_outer_fold(
      train_data         = train_data,
      test_data          = test_data,
      train_y            = train_y,
      test_y             = test_y,
      outcome_name       = outcome_name,
      item_names         = item_names,
      sizes              = sizes,
      inner_folds        = inner_folds,
      inner_repeats      = inner_repeats,
      stratified         = stratified,
      selection_metric   = selection_metric,
      cutoff_method      = cutoff_method,
      sensitivity_min    = sensitivity_min,
      specificity_min    = specificity_min,
      prefer_fewer_items = prefer_fewer_items,
      positive_label     = positive_label,
      negative_label     = negative_label,
      engine             = engine,
      parallel_mode      = parallel_mode,
      n_workers_res      = n_workers_res,
      threads_per_worker = threads_per_worker_res,
      seed               = fold_seeds[[f]],
      fold_name          = fold_name,
      rep_id             = rep_id,
      f_id               = f_id,
      test_idx           = test_idx,
      progress_callback  = progress_callback
    )
  }

  # ---- Outer Fold Execution (Sequential or Parallel) ----
  if (parallel_mode %in% c("outer", "hybrid")) {
    .NCVROC_ROUTING_COUNTERS$outer_psock_count <- .NCVROC_ROUTING_COUNTERS$outer_psock_count + 1L
    cl <- parallel::makePSOCKcluster(n_workers_res)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Sync .libPaths and load NCVROC on workers
    lib_paths <- .libPaths()
    parallel::clusterExport(cl, "lib_paths", envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(lib_paths)
      if (requireNamespace("NCVROC", quietly = TRUE)) {
        try(library(NCVROC), silent = TRUE)
      }
      NULL
    })

    # Export required namespace functions and internal helpers to workers
    ns <- asNamespace("NCVROC")
    available_symbols <- intersect(.CROSS_SIZE_OUTER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
    parallel::clusterExport(cl, varlist = available_symbols, envir = ns)

    parallel::clusterExport(
      cl,
      varlist = c(
        "dat_prep", "y", "outcome_name", "item_names", "sizes", "outer_fold_indices",
        "inner_folds", "inner_repeats", "stratified", "selection_metric", "cutoff_method",
        "sensitivity_min", "specificity_min", "prefer_fewer_items", "positive_label",
        "negative_label", "engine", "parallel_mode", "n_workers_res", "threads_per_worker_res",
        "fold_seeds", "n_total", ".evaluate_cross_size_outer_fold"
      ),
      envir = environment()
    )

    prg <- .progress_make(
      n_outer_evals,
      label = "NCVROC",
      enabled = progress,
      progress_mode = "exact",
      unit = "outer folds complete"
    )
    if (verbose) {
      message("Running ", n_outer_evals, " outer folds in parallel...")
    }
    outer_results_list <- parallel::parLapply(cl, seq_len(n_outer_evals), eval_outer_fold)
    prg$finish()
    if (verbose) {
      message("All outer folds complete.")
    }
  } else {
    total_candidates_all_sizes <- sum(vapply(sizes, function(s) choose(n_items_total, s), numeric(1)))
    outer_results_list <- vector("list", n_outer_evals)
    for (f in seq_len(n_outer_evals)) {
      if (verbose) {
        cat(sprintf("Evaluating outer fold %d/%d (%s)...\n",
                    f, n_outer_evals, names(outer_fold_indices)[f]))
      }
      prg_f <- .progress_make(
        total_candidates_all_sizes,
        label = sprintf("NCVROC [Fold %d/%d]", f, n_outer_evals),
        enabled = progress,
        progress_mode = "exact",
        unit = "candidates"
      )
      cb_f <- if (progress) function(n_done) prg_f$tick(n_done) else NULL
      outer_results_list[[f]] <- eval_outer_fold(f, progress_callback = cb_f)
      prg_f$finish()
    }
  }

  # ---- Aggregate Outer Test Results ----
  outer_fold_results_df <- do.call(rbind, lapply(outer_results_list, function(x) x$fold_result))
  rownames(outer_fold_results_df) <- NULL

  outer_predictions_df <- do.call(rbind, lapply(outer_results_list, function(x) x$predictions))
  ord_preds <- order(outer_predictions_df$repeat_id, outer_predictions_df$row_index)
  outer_predictions_df <- outer_predictions_df[ord_preds, , drop = FALSE]
  rownames(outer_predictions_df) <- NULL

  # Repeat-level and overall performance summary
  metric_keys <- c("auc", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv")
  outer_col_names <- paste0("outer_", metric_keys)

  means <- colMeans(outer_fold_results_df[, outer_col_names, drop = FALSE], na.rm = TRUE)
  sds   <- apply(outer_fold_results_df[, outer_col_names, drop = FALSE], 2, stats::sd, na.rm = TRUE)

  summary_df <- data.frame(
    metric = metric_keys,
    mean   = as.numeric(means),
    sd     = as.numeric(sds),
    stringsAsFactors = FALSE
  )

  # Model size selection frequency
  size_counts <- table(factor(outer_fold_results_df$selected_n_items[!is.na(outer_fold_results_df$selected_n_items)], levels = sizes))
  size_freq_df <- data.frame(
    n_items      = sizes,
    n_selections = as.integer(size_counts),
    frequency    = as.numeric(size_counts) / n_outer_evals,
    stringsAsFactors = FALSE
  )

  # Item combination selection frequency
  valid_items <- outer_fold_results_df$selected_items[!is.na(outer_fold_results_df$selected_items)]
  combo_counts <- if (length(valid_items) > 0L) sort(table(valid_items), decreasing = TRUE) else table(character(0))
  combo_freq_df <- if (length(combo_counts) > 0L) {
    data.frame(
      items        = names(combo_counts),
      n_items      = vapply(names(combo_counts), function(s) length(.parse_itemset(s)), integer(1)),
      n_selections = as.integer(combo_counts),
      frequency    = as.numeric(combo_counts) / n_outer_evals,
      row.names    = NULL,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      items        = character(0),
      n_items      = integer(0),
      n_selections = integer(0),
      frequency    = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Cutoff distribution across outer folds (1 cutoff per outer fold)
  cutoffs_vec <- outer_fold_results_df$selected_cutoff
  valid_cutoffs <- cutoffs_vec[!is.na(cutoffs_vec)]
  cv_cutoff_dist <- if (length(valid_cutoffs) > 0L) {
    list(
      mean            = mean(valid_cutoffs),
      sd              = if (length(valid_cutoffs) > 1) stats::sd(valid_cutoffs) else 0,
      median          = stats::median(valid_cutoffs),
      iqr             = stats::IQR(valid_cutoffs),
      min             = min(valid_cutoffs),
      max             = max(valid_cutoffs),
      per_fold_values = cutoffs_vec
    )
  } else {
    list(
      mean            = NA_real_,
      sd              = NA_real_,
      median          = NA_real_,
      iqr             = NA_real_,
      min             = NA_real_,
      max             = NA_real_,
      per_fold_values = cutoffs_vec
    )
  }

  res <- structure(
    list(
      summary                              = summary_df,
      outer_fold_results                   = outer_fold_results_df,
      selected_models_by_outer_fold        = outer_fold_results_df$selected_items,
      model_size_selection_frequency       = size_freq_df,
      item_combination_selection_frequency = combo_freq_df,
      cutoff_distribution                  = cv_cutoff_dist,
      outer_predictions                    = outer_predictions_df,
      model_sizes                          = sizes,
      settings = c(
        list(
          outcome_name       = outcome_name,
          item_names         = item_names,
          model_sizes        = sizes,
          outer_folds        = outer_folds,
          effective_folds    = effective_folds,
          outer_repeats      = outer_repeats,
          inner_folds        = inner_folds,
          inner_repeats      = inner_repeats,
          selection_metric   = selection_metric,
          cutoff_method      = cutoff_method,
          sensitivity_min    = sensitivity_min,
          specificity_min    = specificity_min,
          prefer_fewer_items = prefer_fewer_items,
          stratified         = stratified,
          engine             = engine,
          parallel           = parallel_mode,
          n_workers          = n_workers_res,
          threads_per_worker = threads_per_worker_res,
          tuning             = tuning,
          seed               = seed
        ),
        if (!identical(tuning, "off")) {
          capability <- .progress_capability("cross_size_nested_cv", parallel_mode, progress,
                                             observed_unit = "outer_fold")
          execution_metadata$progress_mode <- capability$progress_mode
          execution_metadata$progress_unit <- capability$progress_unit
          list(execution_plan = execution_metadata)
        } else list()
      )
    ),
    class = "cross_size_nested_cv_result"
  )

  if (!is.null(canonical_target)) {
    .publish_nested_cv_result_transaction(
      result_obj       = res,
      identity_obj     = identity_obj,
      canonical_target = canonical_target,
      lock_token       = lock_token
    )
  }

  res
}

#' Print method for cross_size_nested_cv_result
#'
#' @param x A `cross_size_nested_cv_result` object.
#' @param ... Additional arguments.
#' @export
print.cross_size_nested_cv_result <- function(x, ...) {
  cat("\n=== Cross-Size Nested Cross-Validation (NCVROC) ===\n")
  cat("Outer CV:          ", x$settings$outer_repeats, "x repeated ", x$settings$effective_folds, "-fold CV (", nrow(x$outer_fold_results), " total outer evaluations)\n", sep = "")
  cat("Inner CV:          ", x$settings$inner_repeats, "x repeated ", x$settings$inner_folds, "-fold CV\n", sep = "")
  cat("Model sizes:       ", paste(x$model_sizes, collapse = ", "), "\n", sep = "")
  cat("Selection metric:  ", x$settings$selection_metric, "\n")
  cat("Cutoff rule:       ", x$settings$cutoff_method, "\n")
  if (!is.null(x$settings$sensitivity_min)) {
    cat(sprintf("Constraint:         OOF sensitivity >= %.2f\n", x$settings$sensitivity_min))
  }
  if (!is.null(x$settings$specificity_min)) {
    cat(sprintf("Constraint:         OOF specificity >= %.2f\n", x$settings$specificity_min))
  }

  cat("\n--- Outer-Test Generalization Performance (Selection Procedure) ---\n")
  s <- x$summary
  for (i in seq_len(nrow(s))) {
    cat(sprintf("  %-12s: %.4f (SD = %.4f)\n", s$metric[i], s$mean[i], s$sd[i]))
  }

  cat("\n--- Model Size Selection Frequency ---\n")
  mf <- x$model_size_selection_frequency
  for (i in seq_len(nrow(mf))) {
    cat(sprintf("  %2d item(s): %3d / %3d (%5.1f%%)\n",
                mf$n_items[i], mf$n_selections[i], nrow(x$outer_fold_results), mf$frequency[i] * 100))
  }

  cat("\n--- Top Selected Item Combinations ---\n")
  cf <- utils::head(x$item_combination_selection_frequency, 5)
  for (i in seq_len(nrow(cf))) {
    cat(sprintf("  %-30s (%d items): %3d (%5.1f%%)\n",
                cf$items[i], cf$n_items[i], cf$n_selections[i], cf$frequency[i] * 100))
  }

  cat("\n--- Outer Cutoff Summary ---\n")
  cd <- x$cutoff_distribution
  cat(sprintf("  Outer training-derived cutoff: mean = %.2f, median = %.2f (range [%.2f, %.2f])\n\n",
              cd$mean, cd$median, cd$min, cd$max))

  invisible(x)
}

#' Summary method for cross_size_nested_cv_result
#'
#' @param object A `cross_size_nested_cv_result` object.
#' @param ... Additional arguments.
#' @export
summary.cross_size_nested_cv_result <- function(object, ...) {
  print(object, ...)
}

#' Plot Cross-Size Nested Cross-Validation Results
#'
#' Renders diagnostic plots of outer-test generalization performance,
#' model size selection frequencies, and top selected item combinations.
#'
#' @param x An object of class `"cross_size_nested_cv_result"`.
#' @param which Plot type to display: `"all"` (3-panel overview),
#'   `"performance"` (outer-fold AUC test distribution),
#'   `"model_size"` (model size selection frequency barplot),
#'   or `"combinations"` (top selected item combinations barplot). Default `"all"`.
#' @param metric Performance metric to plot when `which = "performance"` or in the performance panel of `"all"`. Default `"auc"`.
#' @param ... Additional arguments passed to base graphics functions.
#' @return Invisibly returns `x`.
#' @export
plot.cross_size_nested_cv_result <- function(x, which = c("all", "performance", "model_size", "combinations"), metric = "auc", ...) {
  which <- match.arg(which)

  plot_perf <- function() {
    outer_col <- paste0("outer_", metric)
    if (!outer_col %in% names(x$outer_fold_results)) {
      outer_col <- "outer_auc"
      metric <- "auc"
    }
    vals <- x$outer_fold_results[[outer_col]]
    vals <- vals[is.finite(vals)]

    if (length(vals) == 0L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste0("No valid ", metric, " values available"))
      return()
    }

    metric_label <- toupper(metric)
    graphics::stripchart(
      vals,
      method   = "jitter",
      vertical = TRUE,
      pch      = 21,
      bg       = "steelblue",
      cex      = 1.3,
      xlab     = "",
      ylab     = metric_label,
      main     = paste0("Outer-Fold Test ", metric_label),
      xaxt     = "n",
      ...
    )
    mean_val <- mean(vals, na.rm = TRUE)
    graphics::abline(h = mean_val, col = "red", lty = 2, lwd = 2)
    if (metric == "auc") {
      graphics::abline(h = 0.5, col = "gray50", lty = 3)
      graphics::legend("bottomleft",
        legend = c(sprintf("Mean = %.3f", mean_val), "Chance = 0.5"),
        col    = c("red", "gray50"), lty = c(2, 3), lwd = c(2, 1),
        cex    = 0.85, bg = "white"
      )
    } else {
      graphics::legend("bottomleft",
        legend = sprintf("Mean = %.3f", mean_val),
        col    = "red", lty = 2, lwd = 2,
        cex    = 0.85, bg = "white"
      )
    }
  }

  plot_msize <- function() {
    mf <- x$model_size_selection_frequency
    if (is.null(mf) || nrow(mf) == 0L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No model size frequency data available")
      return()
    }
    graphics::barplot(
      height    = rev(mf$frequency * 100),
      names.arg = rev(paste0(mf$n_items, " items")),
      horiz     = TRUE,
      las       = 1,
      xlab      = "Selection Frequency (%)",
      main      = "Model Size Selection",
      col       = "darkolivegreen3",
      border    = NA,
      xlim      = c(0, max(100, max(mf$frequency * 100, na.rm = TRUE))),
      ...
    )
  }

  plot_combos <- function() {
    cf <- x$item_combination_selection_frequency
    if (is.null(cf) || nrow(cf) == 0L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No combination frequency data available")
      return()
    }
    n_show <- min(8L, nrow(cf))
    cf_sub <- cf[seq_len(n_show), , drop = FALSE]

    graphics::barplot(
      height    = rev(cf_sub$frequency * 100),
      names.arg = rev(cf_sub$items),
      horiz     = TRUE,
      las       = 1,
      xlab      = "Selection Frequency (%)",
      main      = "Top Selected Combinations",
      col       = "coral",
      border    = NA,
      xlim      = c(0, max(100, max(cf_sub$frequency * 100, na.rm = TRUE))),
      ...
    )
  }

  if (which == "all") {
    old_par <- graphics::par(mfrow = c(1, 3), mar = c(4.5, 6, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
    plot_perf()
    plot_msize()
    plot_combos()
  } else if (which == "performance") {
    plot_perf()
  } else if (which == "model_size") {
    old_par <- graphics::par(mar = c(4.5, 6, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
    plot_msize()
  } else if (which == "combinations") {
    old_par <- graphics::par(mar = c(4.5, 10, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
    plot_combos()
  }

  invisible(x)
}
