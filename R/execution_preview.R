# execution_preview.R -- Public execution planning and runtime preview interface

#' Plan and Preview Execution for NCVROC Workflows
#'
#' Evaluates candidate workload, conducts a lightweight deterministic probe, and
#' for flat exhaustive/CV workflows may benchmark legal resource configurations
#' before selecting a measured near-best plan. The primary gate is an empirical
#' 180-second serial estimate, with benchmark overhead capped at 5 percent of
#' that estimate. Suitable two-point pilot timings use an affine runtime model.
#' Nested workflows use only candidate-bounded runtime probing in v0.19.0: their
#' full resource sweep is deferred when a rank-bounded evaluator is unavailable,
#' and the manual/default configuration is retained.
#'
#' @param data A `data.frame` or `matrix` containing the predictor variables and outcome.
#' @param outcome Column name of the binary outcome (as string or unquoted symbol).
#' @param items Optional vector of item/predictor column names (or tidy-selection).
#'   If `NULL`, all columns except `outcome` are used.
#' @param workflow Workflow type to plan. One of `"cross_size_cv"` (default),
#'   `"exhaustive"`, `"nested_sum_roc"`, or `"cross_size_nested_cv"`.
#' @param model_sizes Integer vector of model sizes (number of items per combination)
#'   to evaluate. Default `1:4`.
#' @param min_items Minimum number of items (default 1). Used if `model_sizes` is `NULL`.
#' @param max_items Maximum number of items (default 4). Used if `model_sizes` is `NULL`.
#' @param cv_method Cross-validation method for CV workflows: `"kfold"` (default) or `"loocv"`.
#' @param folds Number of cross-validation folds (default 5).
#' @param repeats Number of CV repeats (default 1).
#' @param outer_folds Number of outer CV folds for nested workflows (default 5).
#' @param inner_folds Number of inner CV folds for nested workflows (default 4).
#' @param outer_repeats Number of outer CV repeats for nested workflows (default 1).
#' @param inner_repeats Number of inner CV repeats for nested workflows (default 1).
#' @param selection_metric Metric for model selection (`"auc"`, `"youden"`,
#'   `"sensitivity"`, `"specificity"`, `"accuracy"`). Default `"auc"`.
#' @param cutoff_method Cutoff optimization method: `"youden"` (default) or `"closest_topleft"`.
#' @param sensitivity_min Optional minimum OOF sensitivity threshold (`[0, 1]`).
#' @param specificity_min Optional minimum OOF specificity threshold (`[0, 1]`).
#' @param engine Computation engine: `"Rcpp"` (default) or `"R"`.
#' @param max_resources Optional upper bound on CPU resources to benchmark.
#'   Default `NULL` (uses detected system cores).
#' @param positive_label Value of `outcome` representing the positive class (default 1).
#' @param negative_label Value of `outcome` representing the negative class (default 0).
#' @param ... Additional arguments passed to internal planning controllers.
#'
#' @return An S3 object of class `"ncvroc_execution_plan"` containing:
#'   \item{workflow}{The targeted workflow name.}
#'   \item{workload}{A list of candidate counts (total and by model size) and fold structure.}
#'   \item{cheap_probe}{Initial serial timing and rough runtime estimate from deterministic probe.}
#'   \item{benchmark_table}{Data frame of all benchmarked resource configurations with speedup, efficiency, and estimated full runtime.}
#'   \item{scaling}{Summary of empirical scaling curves and saturation status.}
#'   \item{selected_plan}{The selected near-best execution plan and resource allocation.}
#'   \item{decision_reason}{Rationale for the execution plan selection.}
#'   \item{environment}{Summary of host CPU cores, memory, and OS architecture.}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' dat <- data.frame(
#'   matrix(rbinom(60 * 8, 1, 0.4), nrow = 60, ncol = 8),
#'   y = rbinom(60, 1, 0.5)
#' )
#' names(dat)[1:8] <- paste0("Q", 1:8)
#'
#' # Preview execution for cross-validation search
#' plan <- plan_ncvroc_execution(dat, outcome = y, workflow = "cross_size_cv",
#'                               model_sizes = 1:3, folds = 3, repeats = 1)
#' print(plan)
#' }
#' @export
plan_ncvroc_execution <- function(data,
                                  outcome,
                                  items = NULL,
                                  workflow = c("cross_size_cv", "exhaustive",
                                               "nested_sum_roc", "cross_size_nested_cv"),
                                  model_sizes = NULL,
                                  min_items = 1,
                                  max_items = 4,
                                  cv_method = c("kfold", "loocv"),
                                  folds = 5,
                                  repeats = 1,
                                  outer_folds = 5,
                                  inner_folds = 4,
                                  outer_repeats = 1,
                                  inner_repeats = 1,
                                  selection_metric = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                                  cutoff_method = c("youden", "closest_topleft"),
                                  sensitivity_min = NULL,
                                  specificity_min = NULL,
                                  engine = c("Rcpp", "R"),
                                  max_resources = NULL,
                                  positive_label = 1,
                                  negative_label = 0,
                                  ...) {
  workflow <- match.arg(workflow)
  cv_method <- match.arg(cv_method)
  selection_metric <- match.arg(selection_metric)
  cutoff_method <- match.arg(cutoff_method)
  engine <- match.arg(engine)

  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names <- if (missing(items) || is.null(items)) {
    setdiff(names(data), outcome_name)
  } else {
    .resolve_items(data, substitute(items), env)
  }

  # Prepare matrix and outcome
  dat_prep <- as.matrix(data[, item_names, drop = FALSE])
  mode(dat_prep) <- "double"
  y_raw <- data[[outcome_name]]
  y_int <- as.integer(y_raw == positive_label)

  # Resolve model sizes
  resolved_sizes <- if (!is.null(model_sizes)) {
    .resolve_model_sizes(as.integer(model_sizes), length(item_names))
  } else {
    as.integer(seq.int(max(1L, as.integer(min_items)), min(as.integer(max_items), length(item_names))))
  }

  available_cap <- if (!is.null(max_resources)) as.integer(max_resources) else NULL

  extra_args <- list(...)
  tuning_arg <- if (!is.null(extra_args$tuning)) extra_args$tuning else "always"
  threshold_arg <- if (!is.null(extra_args$benchmark_threshold)) {
    as.double(extra_args$benchmark_threshold)
  } else if (!is.null(extra_args$threshold)) {
    as.double(extra_args$threshold)
  } else {
    .PLANNER_AUTO_WORKLOAD_THRESHOLD
  }

  # Dispatch to internal planner controller
  if (workflow == "exhaustive") {
    ctrl <- .planner_exhaustive_controller(
      x_mat                = dat_prep,
      y                    = y_int,
      items                = item_names,
      min_items            = min(resolved_sizes),
      max_items            = max(resolved_sizes),
      cutoff_method        = cutoff_method,
      engine               = engine,
      tuning               = tuning_arg,
      manual_parallel_mode = "none",
      manual_n_workers     = available_cap,
      chunk_size           = 200000L,
      dependencies         = list(benchmark_threshold = threshold_arg)
    )
  } else if (workflow == "cross_size_cv") {
    cv_folds_list <- .planner_with_preserved_rng(.build_cv_folds(
      y_int, cv_method = cv_method, folds = as.integer(folds),
      repeats = as.integer(repeats), seed = 42L
    ))
    ctrl <- .planner_cross_size_cv_controller(
      data_matrix          = dat_prep,
      y                    = y_int,
      item_names           = item_names,
      sizes                = resolved_sizes,
      cv_folds             = cv_folds_list,
      folds                = as.integer(folds),
      repeats              = as.integer(repeats),
      stratified           = TRUE,
      cv_method            = cv_method,
      selection_metric     = selection_metric,
      cutoff_method        = cutoff_method,
      sensitivity_min      = sensitivity_min,
      specificity_min      = specificity_min,
      engine               = engine,
      tuning               = tuning_arg,
      manual_parallel_mode = "none",
      manual_n_workers     = available_cap,
      dependencies         = list(benchmark_threshold = threshold_arg)
    )
  } else if (workflow == "nested_sum_roc") {
    outer_folds_list <- .planner_with_preserved_rng(.build_cv_folds(
      y_int, cv_method = "kfold", folds = as.integer(outer_folds),
      repeats = as.integer(outer_repeats), seed = 42L
    ))
    ctrl <- .planner_nested_sum_roc_controller(
      full_data                 = data,
      y                         = y_int,
      items                     = item_names,
      outcome_col               = outcome_name,
      min_items                 = min(resolved_sizes),
      max_items                 = max(resolved_sizes),
      positive_label            = positive_label,
      negative_label            = negative_label,
      cutoff_method             = cutoff_method,
      preselect_top_n           = 20L,
      preselect_by              = selection_metric,
      selection_criterion       = selection_metric,
      outer_k                   = as.integer(outer_folds),
      inner_k                   = as.integer(inner_folds),
      outer_repeats             = as.integer(outer_repeats),
      inner_repeats             = as.integer(inner_repeats),
      stratified                = TRUE,
      seed                      = 42L,
      engine                    = engine,
      tuning                    = tuning_arg,
      manual_parallel_mode      = "none",
      manual_n_workers          = available_cap,
      manual_threads_per_worker = 1L,
      outer_folds               = outer_folds_list,
      threshold                 = threshold_arg
    )
  } else {
    outer_folds_list <- .planner_with_preserved_rng(.build_cv_folds(
      y_int, cv_method = "kfold", folds = as.integer(outer_folds),
      repeats = as.integer(outer_repeats), seed = 42L
    ))
    ctrl <- .planner_cross_size_nested_cv_controller(
      dat_prep                  = dat_prep,
      y                         = y_int,
      outcome_name              = outcome_name,
      item_names                = item_names,
      sizes                     = resolved_sizes,
      outer_fold_indices        = outer_folds_list,
      inner_folds               = as.integer(inner_folds),
      inner_repeats             = as.integer(inner_repeats),
      stratified                = TRUE,
      selection_metric          = selection_metric,
      cutoff_method             = cutoff_method,
      sensitivity_min           = sensitivity_min,
      specificity_min           = specificity_min,
      prefer_fewer_items        = TRUE,
      positive_label            = positive_label,
      negative_label            = negative_label,
      engine                    = engine,
      tuning                    = tuning_arg,
      manual_parallel_mode      = "none",
      manual_n_workers          = available_cap,
      manual_threads_per_worker = 1L,
      fold_seeds                = rep(42L, length(outer_folds_list)),
      threshold                 = threshold_arg
    )
  }

  meta <- ctrl$metadata
  res <- list(
    workflow                    = workflow,
    target_api                  = meta$target_api,
    backend_benchmark_performed = isTRUE(meta$backend_benchmark_performed),
    workload                    = list(
      total_candidates        = meta$total_candidates,
      candidate_count_by_size = meta$candidate_count_by_size,
      model_sizes             = resolved_sizes,
      n_items                 = length(item_names),
      folds                   = if (workflow %in% c("cross_size_cv", "cv")) folds else NULL,
      repeats                 = if (workflow %in% c("cross_size_cv", "cv")) repeats else NULL,
      outer_folds             = if (workflow %in% c("nested_sum_roc", "cross_size_nested_cv")) outer_folds else NULL,
      inner_folds             = if (workflow %in% c("nested_sum_roc", "cross_size_nested_cv")) inner_folds else NULL,
      outer_repeats           = if (workflow %in% c("nested_sum_roc", "cross_size_nested_cv")) outer_repeats else NULL
    ),
    cheap_probe      = list(
      pilot_candidates  = meta$micro_pilot_candidates,
      pilot_elapsed     = meta$micro_pilot_elapsed,
      serial_estimate   = meta$estimated_serial_runtime,
      method            = meta$runtime_estimation_method
    ),
    benchmark_table  = meta$benchmark_table,
    scaling          = meta$scaling_summary,
    saturation       = meta$saturation_summary,
    selected_plan    = list(
      parallel           = meta$selected_parallel,
      n_workers          = meta$selected_n_workers,
      outer_workers      = meta$selected_outer_workers,
      threads_per_worker = meta$selected_threads_per_worker,
      resource_count     = meta$selected_resource_count,
      estimated_runtime  = meta$estimated_runtime
    ),
    decision_reason  = meta$decision_reason,
    environment      = meta$environment_summary
  )
  class(res) <- "ncvroc_execution_plan"
  res
}

#' Format and Print Methods for Execution Plans
#'
#' @param x An object of class `"ncvroc_execution_plan"`.
#' @param ... Additional arguments (currently unused).
#' @return `print` returns `x` invisibly. `format` returns a character string.
#' @export
print.ncvroc_execution_plan <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @rdname print.ncvroc_execution_plan
#' @export
format.ncvroc_execution_plan <- function(x, ...) {
  lines <- character()
  lines <- c(lines, "=== NCVROC Execution Plan ===")
  lines <- c(lines, sprintf("Workflow: %s", x$workflow))
  lines <- c(lines, sprintf("Total Candidates: %s across %d item(s)",
                            format(x$workload$total_candidates, big.mark = ","),
                            x$workload$n_items))

  lines <- c(lines, "")
  lines <- c(lines, "--- Resource Sweep Benchmark ---")
  if (is.data.frame(x$benchmark_table) && nrow(x$benchmark_table) > 0L) {
    tb <- x$benchmark_table
    show_cols <- intersect(
      c("plan_id", "parallel", "n_workers", "threads_per_worker",
        "resource_count", "median_elapsed", "speedup", "parallel_efficiency", "status"),
      names(tb)
    )
    df_show <- tb[, show_cols, drop = FALSE]
    if ("median_elapsed" %in% names(df_show)) {
      df_show$median_elapsed <- sprintf("%.3fs", df_show$median_elapsed)
    }
    if ("speedup" %in% names(df_show)) {
      df_show$speedup <- ifelse(is.finite(df_show$speedup), sprintf("%.2fx", df_show$speedup), "-")
    }
    if ("parallel_efficiency" %in% names(df_show)) {
      df_show$parallel_efficiency <- ifelse(is.finite(df_show$parallel_efficiency),
                                            sprintf("%.1f%%", df_show$parallel_efficiency * 100), "-")
    }
    lines <- c(lines, utils::capture.output(print(df_show, row.names = FALSE)))
  } else {
    lines <- c(lines, "  (No multi-backend benchmark sweep performed)")
  }

  lines <- c(lines, "")
  lines <- c(lines, "--- Suggested Configuration ---")
  plan_desc <- if (x$selected_plan$parallel == "hybrid") {
    sprintf("hybrid (%d outer workers x %d threads)",
            x$selected_plan$outer_workers, x$selected_plan$threads_per_worker)
  } else if (x$selected_plan$parallel == "none") {
    "serial (none, 1 worker)"
  } else {
    sprintf("%s (%d resources)", x$selected_plan$parallel, x$selected_plan$resource_count)
  }
  lines <- c(lines, sprintf("Plan: %s", plan_desc))
  basis_str <- if (!is.null(x$decision_reason) && grepl("benchmark", x$decision_reason, ignore.case = TRUE)) {
    "Benchmark-based suggestion"
  } else if (!is.null(x$decision_reason) && is.character(x$decision_reason) && nzchar(x$decision_reason)) {
    x$decision_reason
  } else {
    "Advisory configuration"
  }
  lines <- c(lines, sprintf("Basis: %s", basis_str))

  if (!is.null(x$saturation) && length(x$saturation) > 0L) {
    lines <- c(lines, "")
    lines <- c(lines, "--- Scaling & Saturation Notes ---")
    for (name in names(x$saturation)) {
      lines <- c(lines, sprintf("  * [%s]: %s", name, x$saturation[[name]]))
    }
  }

  paste(lines, collapse = "\n")
}
