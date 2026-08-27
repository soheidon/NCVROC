# compare_cv_selection.R — Comparison between ordinary and nested cross-size CV
#
# Provides:
#   - compare_cv_selection()
#   - print.compare_cv_selection_result()
#   - summary.compare_cv_selection_result()

#' Compare model selection performance between ordinary and nested cross-validation
#'
#' Compares the apparent / non-nested cross-validation performance of the single model
#' selected by ordinary cross-size cross-validation ([cross_size_cv()]) against the
#' out-of-sample generalization performance of the entire model-selection procedure
#' evaluated via cross-size nested cross-validation ([cross_size_nested_cv()]).
#'
#' The difference `ordinary - nested` is returned as `selection_optimism`.
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
#' @param folds Integer, number of folds for ordinary CV (default 5).
#' @param repeats Integer, number of independent repeats for ordinary CV (default 1).
#' @param outer_folds Integer, number of outer folds for nested CV (default 5). Alias: `outer_k`.
#' @param inner_folds Integer, number of inner folds for nested CV (default 4). Alias: `inner_k`.
#' @param outer_repeats Integer, number of outer repeats for nested CV (default 5).
#' @param inner_repeats Integer, number of inner repeats for nested CV (default 1).
#' @param selection_metric Metric for candidate ranking and model selection:
#'   `"auc"` (default), `"youden"`, `"sensitivity"`, `"specificity"`, or `"accuracy"`.
#' @param cutoff_method Cutoff selection rule: `"youden"` (default) or `"closest_topleft"`.
#' @param sensitivity_min Optional minimum OOF sensitivity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param specificity_min Optional minimum OOF specificity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param top_n Integer, number of top candidates in ordinary ranking (default 20).
#' @param prefer_fewer_items Logical, prefer smaller models on ties (default `TRUE`).
#' @param stratified Logical, maintain class balance across folds (default `TRUE`).
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param engine Combinatorial computation engine: `"Rcpp"` (default) or `"R"`.
#' @param parallel Parallel mode: `FALSE` / `"none"` (default), `"outer"`, `"threads"`,
#'   `"chunks"`, or `"hybrid"`.
#'   - `"none"`: both ordinary and nested CV run sequentially.
#'   - `"outer"`: nested CV uses PSOCK cluster for outer folds; ordinary CV runs sequentially.
#'   - `"threads"`: both ordinary and nested CV use multi-threaded C++ (RcppParallel).
#'   - `"chunks"`: both ordinary and nested CV use PSOCK cluster for candidate chunks.
#'   - `"hybrid"`: nested CV uses outer PSOCK workers with inner threads; ordinary CV uses multi-threaded C++.
#' @param n_workers Integer, number of worker processes or threads (default `NULL` = auto).
#' @param threads_per_worker Integer, threads per PSOCK worker in `"hybrid"` mode (default 1).
#' @param seed Integer, random seed for reproducible fold generation.
#' @param progress Logical, show progress bar (default `interactive()`).
#' @param outer_k Alias for `outer_folds`.
#' @param inner_k Alias for `inner_folds`.
#'
#' @return An S3 object of class `"compare_cv_selection_result"`.
#'
#' @details
#' ### Statistical Semantics of Model Selection Optimism
#' When exploring a large combinatorial candidate model space, selecting the single
#' best-performing model based on non-nested cross-validation leads to selection-induced
#' optimism.
#'
#' Nested cross-validation isolates the entire model-selection process within outer training
#' folds and evaluates generalization performance on completely independent outer test folds.
#'
#' Thus, `selection_optimism = ordinary - nested` provides an empirical estimate of the optimism
#' associated with model selection under the non-nested CV procedure.
#'
#' Importantly, `selection_optimism` reflects optimism of the **selection procedure**,
#' not the bias of any specific fixed final model.
#'
#' For fixed unweighted sum scores, pooled OOF AUC equals full-data apparent AUC;
#' thus, the ordinary AUC represents non-nested selected-model performance, while
#' nested AUC represents the selection procedure's out-of-sample generalization AUC.
#'
#' @export
compare_cv_selection <- function(data,
                                 outcome,
                                 items,
                                 model_sizes        = NULL,
                                 min_items          = 1,
                                 max_items          = 4,
                                 item_count         = NULL,
                                 folds              = 5,
                                 repeats            = 1,
                                 outer_folds        = 5,
                                 inner_folds        = 4,
                                 outer_repeats      = 5,
                                 inner_repeats      = 1,
                                 selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                                 cutoff_method      = c("youden", "closest_topleft"),
                                 sensitivity_min    = NULL,
                                 specificity_min    = NULL,
                                 top_n              = 20,
                                 prefer_fewer_items = TRUE,
                                 stratified         = TRUE,
                                 positive_label     = 1,
                                 negative_label     = 0,
                                 engine             = c("Rcpp", "R"),
                                 parallel           = FALSE,
                                 n_workers          = NULL,
                                 threads_per_worker = 1L,
                                 seed               = NULL,
                                 progress           = interactive(),
                                 outer_k            = NULL,
                                 inner_k            = NULL) {
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

  # ---- Parallel Mode Mapping ----
  parallel_mode <- .resolve_parallel_mode(
    parallel,
    context = "nested",
    allowed = c("none", "outer", "threads", "chunks", "hybrid")
  )

  ordinary_parallel <- switch(
    parallel_mode,
    "none"    = "none",
    "threads" = "threads",
    "chunks"  = "chunks",
    "outer"   = "none",
    "hybrid"  = "threads"
  )

  ordinary_workers <- if (parallel_mode == "hybrid") {
    threads_per_worker
  } else {
    n_workers
  }

  # ---- 1. Run Ordinary Cross-Size CV ----
  if (progress) {
    message("Step 1/2: Running cross-size ordinary CV...")
  }

  ordinary_res <- cross_size_cv(
    data               = data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = model_sizes,
    min_items          = min_items,
    max_items          = max_items,
    item_count         = item_count,
    cv_method          = "kfold",
    folds              = folds,
    repeats            = repeats,
    stratified         = stratified,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    top_n              = top_n,
    prefer_fewer_items = prefer_fewer_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = ordinary_parallel,
    n_workers          = ordinary_workers,
    ci                 = FALSE,
    seed               = seed,
    progress           = progress
  )

  # ---- 2. Run Cross-Size Nested CV ----
  if (progress) {
    message("Step 2/2: Running cross-size nested CV...")
  }

  nested_res <- cross_size_nested_cv(
    data               = data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = model_sizes,
    min_items          = min_items,
    max_items          = max_items,
    item_count         = item_count,
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
    parallel           = parallel_mode,
    n_workers          = n_workers,
    threads_per_worker = threads_per_worker,
    seed               = seed,
    progress           = progress
  )

  # ---- 3. Construct Comparison Table ----
  ord_m <- ordinary_res$final_selected_model
  ord_metrics <- c(
    auc         = ord_m$cv_auc,
    sensitivity = ord_m$cv_sensitivity,
    specificity = ord_m$cv_specificity,
    youden      = ord_m$cv_youden,
    accuracy    = ord_m$cv_accuracy,
    ppv         = ord_m$cv_ppv,
    npv         = ord_m$cv_npv
  )

  nest_s <- nested_res$summary
  nest_metrics <- stats::setNames(nest_s$mean, nest_s$metric)

  metric_names <- c("auc", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv")

  comp_df <- data.frame(
    metric             = metric_names,
    ordinary           = as.numeric(ord_metrics[metric_names]),
    nested             = as.numeric(nest_metrics[metric_names]),
    selection_optimism = as.numeric(ord_metrics[metric_names] - nest_metrics[metric_names]),
    stringsAsFactors   = FALSE
  )

  structure(
    list(
      comparison         = comp_df,
      selected_model     = ord_m,
      ordinary           = ordinary_res,
      nested             = nested_res,
      model_sizes        = ordinary_res$model_sizes,
      settings = list(
        outcome_name       = outcome_name,
        item_names         = item_names,
        model_sizes        = ordinary_res$model_sizes,
        selection_metric   = selection_metric,
        cutoff_method      = cutoff_method,
        sensitivity_min    = sensitivity_min,
        specificity_min    = specificity_min,
        prefer_fewer_items = prefer_fewer_items,
        stratified         = stratified,
        ordinary_folds     = folds,
        ordinary_repeats   = repeats,
        nested_outer_folds = outer_folds,
        nested_inner_folds = inner_folds,
        nested_outer_repeats = outer_repeats,
        nested_inner_repeats = inner_repeats,
        engine             = engine,
        parallel_requested = parallel_mode,
        ordinary_parallel  = ordinary_parallel,
        nested_parallel    = parallel_mode,
        n_workers          = n_workers,
        threads_per_worker = threads_per_worker,
        seed               = seed
      )
    ),
    class = "compare_cv_selection_result"
  )
}

#' Print method for compare_cv_selection_result
#'
#' @param x A `compare_cv_selection_result` object.
#' @param ... Additional arguments.
#' @export
print.compare_cv_selection_result <- function(x, ...) {
  cat("\n=== Cross-Size CV Selection Comparison (NCVROC) ===\n")
  cat("Ordinary Model Selection: ", x$settings$ordinary_repeats, "x repeated ", x$settings$ordinary_folds, "-fold CV\n", sep = "")
  cat("Nested Model Validation:  ", x$settings$nested_outer_repeats, "x repeated ", x$settings$nested_outer_folds, "-fold outer CV, ",
      x$settings$nested_inner_repeats, "x repeated ", x$settings$nested_inner_folds, "-fold inner CV\n", sep = "")
  cat("Model sizes evaluated:    ", paste(x$model_sizes, collapse = ", "), " (Total candidates: ", format(x$ordinary$total_combinations, big.mark = ","), ")\n", sep = "")
  cat("Selection metric:         ", x$settings$selection_metric, " (Cutoff rule: ", x$settings$cutoff_method, ")\n", sep = "")

  cat("\n--- Selected Model (Ordinary Non-Nested Procedure) ---\n")
  m <- x$selected_model
  cat("  Items (", m$n_items, "): ", m$items, "\n", sep = "")
  cat(sprintf("  Final Cutoff:   %.2f (Full-data deployment)\n", m$final_full_data_cutoff))

  cat("\n--- Selection Performance & Optimism Summary ---\n")
  comp <- x$comparison
  formatted_df <- data.frame(
    Metric               = comp$metric,
    Ordinary             = sprintf("%.4f", comp$ordinary),
    Nested               = sprintf("%.4f", comp$nested),
    `Selection Optimism` = sprintf("%+.4f", comp$selection_optimism),
    check.names          = FALSE,
    stringsAsFactors     = FALSE
  )
  print(formatted_df, row.names = FALSE)

  cat("\nNote:\n")
  cat("  Selection optimism = Ordinary - Nested.\n")
  cat("  Nested estimates reflect the out-of-sample generalization performance of\n")
  cat("  the model-selection procedure, not a fixed final model.\n\n")

  invisible(x)
}

#' Summary method for compare_cv_selection_result
#'
#' @param object A `compare_cv_selection_result` object.
#' @param ... Additional arguments.
#' @export
summary.compare_cv_selection_result <- function(object, ...) {
  structure(
    list(
      comparison                           = object$comparison,
      selected_model                       = object$selected_model,
      model_size_selection_frequency       = object$nested$model_size_selection_frequency,
      item_combination_selection_frequency = object$nested$item_combination_selection_frequency,
      settings                             = object$settings
    ),
    class = "summary_compare_cv_selection_result"
  )
}

#' Print method for summary_compare_cv_selection_result
#'
#' @param x A `summary_compare_cv_selection_result` object.
#' @param ... Additional arguments.
#' @export
print.summary_compare_cv_selection_result <- function(x, ...) {
  cat("\n=== Summary of Cross-Size CV Selection Comparison ===\n")
  cat("Selected Model (Ordinary Non-Nested): ", x$selected_model$items, " (", x$selected_model$n_items, " items)\n\n", sep = "")

  cat("--- Performance & Selection Optimism ---\n")
  comp <- x$comparison
  formatted_df <- data.frame(
    Metric               = comp$metric,
    Ordinary             = sprintf("%.4f", comp$ordinary),
    Nested               = sprintf("%.4f", comp$nested),
    `Selection Optimism` = sprintf("%+.4f", comp$selection_optimism),
    check.names          = FALSE,
    stringsAsFactors     = FALSE
  )
  print(formatted_df, row.names = FALSE)

  cat("\n--- Nested CV Model Size Selection Frequency ---\n")
  mf <- x$model_size_selection_frequency
  for (i in seq_len(nrow(mf))) {
    cat(sprintf("  %2d item(s): %3d selections (%5.1f%%)\n",
                mf$n_items[i], mf$n_selections[i], mf$frequency[i] * 100))
  }

  cat("\n--- Top Selected Item Combinations in Nested CV ---\n")
  cf <- utils::head(x$item_combination_selection_frequency, 5)
  for (i in seq_len(nrow(cf))) {
    cat(sprintf("  %-30s (%d items): %3d selections (%5.1f%%)\n",
                cf$items[i], cf$n_items[i], cf$n_selections[i], cf$frequency[i] * 100))
  }
  cat("\n")

  invisible(x)
}
