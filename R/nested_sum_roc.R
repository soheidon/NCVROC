# nested_sum_roc.R — Nested cross-validation for item-set score selection
#
# Internal helpers (all @keywords internal):
#   .parse_itemset()
#   .evaluate_fixed_itemset()
#   .select_top_candidates()
#   .evaluate_candidates_inner_cv()
#   .apply_model_to_test()
#
# Exported:
#   nested_sum_roc()
#
# S3 methods:
#   print.ncvroc_result()
#   summary.ncvroc_result()
#   plot.ncvroc_result()

# ---- Internal helpers ----

#' Parse a comma-separated items string back to a character vector
#'
#' @param items_str A string like "q1, q2".
#' @return Character vector c("q1", "q2").
#' @keywords internal
.parse_itemset <- function(items_str) {
  if (length(items_str) != 1 || !is.character(items_str)) {
    return(items_str)
  }
  if (!grepl(",", items_str, fixed = TRUE)) {
    return(items_str)
  }
  trimws(strsplit(items_str, ",", fixed = TRUE)[[1]])
}

#' Evaluate a fixed item set: sum scores, ROC metrics, optimal cutoff
#'
#' Accepts either a character vector or a comma-separated string.
#'
#' @param itemset Character vector or comma-separated string of item names.
#' @param data A data.frame containing the item columns.
#' @param y Binary outcome vector (0/1).
#' @param cutoff_method One of "youden" or "closest_topleft".
#'
#' @return A named list: items (string), n_items, auc, cutoff,
#'   sensitivity, specificity, youden, accuracy, ppv, npv.
#' @keywords internal
.evaluate_fixed_itemset <- function(itemset, data, y, cutoff_method) {
  items_vec <- .parse_itemset(itemset)
  k <- length(items_vec)

  scores <- rowSums(data[, items_vec, drop = FALSE])
  freq <- compute_score_frequencies(scores, y)
  auc_val <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
  metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
  best <- find_optimal_cutoff(metrics, method = cutoff_method)

  list(
    items       = format_items(items_vec),
    n_items     = k,
    auc         = auc_val,
    cutoff      = best$cutoff,
    sensitivity = best$sensitivity,
    specificity = best$specificity,
    youden      = best$youden,
    accuracy    = best$accuracy,
    ppv         = best$ppv,
    npv         = best$npv
  )
}

#' Select top N candidates from exhaustive search results
#'
#' @param results data.frame from exhaustive_sum_roc().
#' @param n Integer, number of top candidates to keep.
#' @param by Character, metric column name to sort by.
#'
#' @return data.frame of top N rows, sorted by `by` descending.
#' @keywords internal
.select_top_candidates <- function(results, n, by) {
  n <- min(n, nrow(results))

  # Fixed tie-breaking: by desc, then youden desc, sens desc, spec desc, n_items asc
  ord <- order(
    -results[[by]],
    -results$youden,
    -results$sensitivity,
    -results$specificity,
    results$n_items
  )
  sorted <- results[ord, , drop = FALSE]
  sorted[seq_len(n), , drop = FALSE]
}

#' Evaluate candidate item sets via inner cross-validation
#'
#' For each candidate, creates inner stratified folds and evaluates
#' performance. Cutoffs are always determined on inner training data only
#' to prevent information leakage.
#'
#' @param candidates_df data.frame with at minimum an `items` column (strings).
#' @param data data.frame (outer training subset).
#' @param y Binary outcome vector (0/1) for outer training data.
#' @param inner_k Integer, number of inner CV folds.
#' @param inner_repeats Integer, number of inner repeats (always 1 in v0.1).
#' @param cutoff_method Character, cutoff method.
#' @param seed_offset Integer or NULL, base seed for inner fold creation.
#' @param progress Logical, show progress bar?
#'
#' @return data.frame with columns: items, n_items, mean_auc,
#'   mean_sensitivity, mean_specificity, mean_youden.
#' @keywords internal
.evaluate_candidates_inner_cv <- function(candidates_df,
                                          data,
                                          y,
                                          inner_k,
                                          inner_repeats,
                                          cutoff_method,
                                          seed_offset,
                                          progress) {
  n_candidates <- nrow(candidates_df)

  if (progress && n_candidates > 1) {
    pb <- utils::txtProgressBar(min = 0, max = n_candidates, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  inner_results <- vector("list", n_candidates)

  for (i in seq_len(n_candidates)) {
    items_str <- candidates_df$items[i]
    items_vec <- .parse_itemset(items_str)
    k <- length(items_vec)

    # Create inner folds on outer training data
    inner_seed <- if (!is.null(seed_offset)) seed_offset + i else NULL
    inner_folds <- make_stratified_folds(
      y, k = inner_k, repeats = inner_repeats, seed = inner_seed
    )

    n_inner_folds <- length(inner_folds)
    aucs <- numeric(n_inner_folds)
    sensitivities <- numeric(n_inner_folds)
    specificities <- numeric(n_inner_folds)
    youdens <- numeric(n_inner_folds)

    for (f in seq_len(n_inner_folds)) {
      inner_test_idx <- inner_folds[[f]]
      inner_train_idx <- setdiff(seq_along(y), inner_test_idx)

      # Determine cutoff on inner train ONLY
      train_eval <- .evaluate_fixed_itemset(
        itemset       = items_vec,
        data          = data[inner_train_idx, , drop = FALSE],
        y             = y[inner_train_idx],
        cutoff_method = cutoff_method
      )
      cutoff_val <- train_eval$cutoff

      # Evaluate on inner test
      test_scores <- rowSums(data[inner_test_idx, items_vec, drop = FALSE])
      test_y      <- y[inner_test_idx]
      pred_class  <- ifelse(test_scores >= cutoff_val, 1L, 0L)

      # AUC on inner test
      test_freq <- compute_score_frequencies(test_scores, test_y)
      test_auc  <- compute_auc_from_table(test_freq$pos_counts, test_freq$neg_counts)

      # Sensitivity / specificity at the chosen cutoff
      tp <- sum(pred_class == 1L & test_y == 1L)
      tn <- sum(pred_class == 0L & test_y == 0L)
      fp <- sum(pred_class == 1L & test_y == 0L)
      fn <- sum(pred_class == 0L & test_y == 1L)

      sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
      spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_

      aucs[f]          <- if (is.na(test_auc)) 0.5 else test_auc
      sensitivities[f] <- sens
      specificities[f] <- spec
      youdens[f]       <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1
    }

    inner_results[[i]] <- data.frame(
      items             = items_str,
      n_items           = k,
      mean_auc          = mean(aucs, na.rm = TRUE),
      mean_sensitivity  = mean(sensitivities, na.rm = TRUE),
      mean_specificity  = mean(specificities, na.rm = TRUE),
      mean_youden       = mean(youdens, na.rm = TRUE),
      stringsAsFactors  = FALSE
    )

    if (progress && n_candidates > 1) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  do.call(rbind, inner_results)
}

#' Apply a selected model to test data
#'
#' Fits (determines cutoff) on training data, then predicts on test data.
#'
#' @param itemset Character vector or comma-separated string of item names.
#' @param data_train data.frame, training data.
#' @param y_train Binary outcome (0/1) for training data.
#' @param data_test data.frame, test data.
#' @param y_test Binary outcome (0/1) for test data.
#' @param cutoff_method Character, cutoff method.
#'
#' @return A list: items, n_items, cutoff, auc, sensitivity, specificity,
#'   youden, accuracy, ppv, npv, and predictions (data.frame with
#'   row_index, true_outcome, predicted_score, predicted_class).
#' @keywords internal
.apply_model_to_test <- function(itemset,
                                 data_train, y_train,
                                 data_test,  y_test,
                                 cutoff_method) {
  items_vec <- .parse_itemset(itemset)

  # Fit on train
  train_eval <- .evaluate_fixed_itemset(
    itemset       = items_vec,
    data          = data_train,
    y             = y_train,
    cutoff_method = cutoff_method
  )
  cutoff_val <- train_eval$cutoff

  # Predict on test
  test_scores <- rowSums(data_test[, items_vec, drop = FALSE])
  pred_class  <- ifelse(test_scores >= cutoff_val, 1L, 0L)

  # Test AUC
  test_freq <- compute_score_frequencies(test_scores, y_test)
  test_auc  <- compute_auc_from_table(test_freq$pos_counts, test_freq$neg_counts)

  # Test metrics at cutoff
  tp <- sum(pred_class == 1L & y_test == 1L)
  tn <- sum(pred_class == 0L & y_test == 0L)
  fp <- sum(pred_class == 1L & y_test == 0L)
  fn <- sum(pred_class == 0L & y_test == 1L)

  sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
  ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  npv  <- if (tn + fn > 0) tn / (tn + fn) else NA_real_
  acc  <- (tp + tn) / (tp + tn + fp + fn)
  youd <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1

  predictions <- data.frame(
    row_index       = seq_along(y_test),
    true_outcome    = y_test,
    predicted_score = test_scores,
    predicted_class = pred_class,
    stringsAsFactors = FALSE
  )

  list(
    items       = train_eval$items,
    n_items     = train_eval$n_items,
    cutoff      = cutoff_val,
    auc         = if (is.na(test_auc)) NA_real_ else test_auc,
    sensitivity = sens,
    specificity = spec,
    youden      = youd,
    accuracy    = acc,
    ppv         = ppv,
    npv         = npv,
    predictions = predictions
  )
}

#' Streaming top-N exhaustive search
#'
#' Evaluates all combinations chunk-by-chunk and keeps a running top-N buffer.
#' Never builds the full candidate table in memory. Used by nested_sum_roc()
#' for per-fold preselection when total combos exceed AUTO_MEMORY_LIMIT.
#'
#' @param data A data.frame.
#' @param outcome Character, outcome column name.
#' @param items Character vector of item names.
#' @param min_items Integer.
#' @param max_items Integer.
#' @param positive_label Scalar.
#' @param negative_label Scalar.
#' @param cutoff_method Character.
#' @param rank_by Character, metric to sort by.
#' @param top_n Integer, number of top candidates to keep.
#' @param engine Character, "R" or "Rcpp".
#'
#' @param parallel Parallel mode for inner exhaustive search (e.g. "threads" or "none").
#' @param n_workers Number of workers/threads for inner evaluation.
#' @return A data.frame of at most `top_n` rows in the standard exhaustive_sum_roc format.
#' @keywords internal
.streaming_top_n_exhaustive <- function(data,
                                         outcome,
                                         items,
                                         min_items,
                                         max_items,
                                         positive_label,
                                         negative_label,
                                         cutoff_method,
                                         rank_by,
                                         top_n,
                                         engine,
                                         parallel = "none",
                                         n_workers = NULL) {
  validated <- validate_inputs(data, outcome, items, positive_label, negative_label)
  x     <- validated$data
  y     <- validated$y
  items <- validated$items
  n_items <- length(items)

  # Use the non-chunked version if total combos fits in one chunk
  total <- .count_total_combos(n_items, min_items, max_items)
  chunk_size <- DEFAULT_CHUNK_SIZE

  best_so_far <- NULL
  chunk_start <- 0.0
  chunk_index <- 0L

  while (chunk_start < total) {
    chunk <- exhaustive_sum_roc(
      data              = data,
      outcome           = outcome,
      items             = items,
      min_items         = min_items,
      max_items         = max_items,
      positive_label    = positive_label,
      negative_label    = negative_label,
      cutoff_method     = cutoff_method,
      rank_by           = rank_by,
      top_n             = NULL,
      prefer_fewer_items = TRUE,
      engine            = engine,
      parallel          = parallel,
      n_workers         = n_workers,
      progress          = FALSE,
      chunk_start       = chunk_start,
      chunk_size        = chunk_size
    )

    # Keep running top-N
    combined <- if (is.null(best_so_far)) {
      chunk
    } else {
      rbind(best_so_far, chunk)
    }

    ord <- order(
      -combined[[rank_by]],
      -combined$youden,
      -combined$sensitivity,
      -combined$specificity,
      combined$n_items
    )
    combined <- combined[ord, , drop = FALSE]
    best_so_far <- utils::head(combined, top_n)

    chunk_start <- chunk_start + chunk_size
    chunk_index <- chunk_index + 1L
  }

  if (is.null(best_so_far)) {
    stop("No combinations evaluated in streaming search.", call. = FALSE)
  }

  best_so_far
}

#' Evaluate a single outer fold
#'
#' @noRd
.evaluate_single_outer_fold <- function(i,
                                        outer_folds,
                                        full_data,
                                        y,
                                        n_total,
                                        items,
                                        outcome_col,
                                        min_items,
                                        max_items,
                                        positive_label,
                                        negative_label,
                                        cutoff_method,
                                        preselect_top_n,
                                        preselect_by,
                                        selection_criterion,
                                        inner_k,
                                        inner_repeats,
                                        use_streaming_ncv,
                                        engine,
                                        seed,
                                        progress,
                                        verbose,
                                        cl_chunk = NULL,
                                        parallel_inner = "none",
                                        n_workers_inner = NULL) {
  test_idx  <- outer_folds[[i]]
  train_idx <- setdiff(seq_len(n_total), test_idx)
  fold_name <- names(outer_folds)[i]

  if (verbose) {
    message("Outer fold ", i, "/", length(outer_folds), " (", fold_name, "): ",
            length(train_idx), " train, ", length(test_idx), " test")
  }

  # Step 1: exhaustive search on outer train ONLY
  if (!is.null(cl_chunk)) {
    candidates <- .parallel_chunk_exhaustive(
      x                  = full_data[train_idx, items, drop = FALSE],
      y                  = y[train_idx],
      items              = items,
      min_items          = min_items,
      max_items          = max_items,
      cutoff_method      = cutoff_method,
      rank_by            = preselect_by,
      top_n              = preselect_top_n,
      prefer_fewer_items = TRUE,
      engine             = engine,
      save_rds           = FALSE,
      chunk_dir          = NULL,
      cl                 = cl_chunk
    )
  } else if (use_streaming_ncv) {
    candidates <- .streaming_top_n_exhaustive(
      data             = full_data[train_idx, , drop = FALSE],
      outcome          = outcome_col,
      items            = items,
      min_items        = min_items,
      max_items        = max_items,
      positive_label   = positive_label,
      negative_label   = negative_label,
      cutoff_method    = cutoff_method,
      rank_by          = preselect_by,
      top_n            = preselect_top_n,
      engine           = engine,
      parallel         = parallel_inner,
      n_workers        = n_workers_inner
    )
  } else {
    candidates <- exhaustive_sum_roc(
      data             = full_data[train_idx, , drop = FALSE],
      outcome          = outcome_col,
      items            = items,
      min_items        = min_items,
      max_items        = max_items,
      positive_label   = positive_label,
      negative_label   = negative_label,
      cutoff_method    = cutoff_method,
      rank_by          = preselect_by,
      top_n            = NULL,
      prefer_fewer_items = TRUE,
      engine           = engine,
      parallel         = parallel_inner,
      n_workers        = n_workers_inner,
      progress         = FALSE
    )
  }

  # Step 2: pre-select top candidates
  top_candidates <- .select_top_candidates(candidates, preselect_top_n, preselect_by)

  if (verbose) {
    message("  Pre-selected ", nrow(top_candidates), " candidate(s) for inner CV")
  }

  # Step 3: inner CV for each candidate
  inner_seed <- if (!is.null(seed)) seed + i else NULL
  inner_results <- .evaluate_candidates_inner_cv(
    candidates_df  = top_candidates,
    data           = full_data[train_idx, , drop = FALSE],
    y              = y[train_idx],
    inner_k        = inner_k,
    inner_repeats  = as.integer(inner_repeats),
    cutoff_method  = cutoff_method,
    seed_offset    = inner_seed,
    progress       = progress && verbose
  )

  # Step 4: select best model by inner CV criterion
  criterion_col <- paste0("mean_", selection_criterion)
  best_idx <- which.max(inner_results[[criterion_col]])
  # Tie-break: highest mean_youden, then fewest items
  if (length(best_idx) > 1) {
    tie_scores <- inner_results$mean_youden[best_idx] -
      inner_results$n_items[best_idx] * 0.001
    best_idx <- best_idx[which.max(tie_scores)]
  }
  best_row <- inner_results[best_idx, ]

  # Step 5: apply best model to outer test
  test_result <- .apply_model_to_test(
    itemset       = best_row$items,
    data_train    = full_data[train_idx, , drop = FALSE],
    y_train       = y[train_idx],
    data_test     = full_data[test_idx, , drop = FALSE],
    y_test        = y[test_idx],
    cutoff_method = cutoff_method
  )

  # Map predictions row_index back to original row numbers
  test_result$predictions$row_index <- test_idx

  list(
    outer_fold         = fold_name,
    selected_items     = best_row$items,
    n_items            = best_row$n_items,
    inner_mean_auc     = best_row$mean_auc,
    inner_mean_youden  = best_row$mean_youden,
    auc                = test_result$auc,
    sensitivity        = test_result$sensitivity,
    specificity        = test_result$specificity,
    youden             = test_result$youden,
    accuracy           = test_result$accuracy,
    ppv                = test_result$ppv,
    npv                = test_result$npv,
    cutoff             = test_result$cutoff,
    predictions        = test_result$predictions
  )
}

#' Symbols required by worker processes during outer-fold parallel execution
#' @noRd
.OUTER_WORKER_EXPORT_SYMBOLS <- c(
  ".evaluate_single_outer_fold",
  ".streaming_top_n_exhaustive",
  ".select_top_candidates",
  ".evaluate_candidates_inner_cv",
  ".apply_model_to_test",
  ".count_total_combos",
  ".parse_itemset_string",
  "exhaustive_sum_roc",
  "validate_inputs",
  "make_stratified_folds",
  "roc_calc",
  "compute_delong_auc_ci",
  "compute_clopper_pearson_ci",
  "fit_final_sum_scale",
  "AUTO_MEMORY_LIMIT",
  "DEFAULT_CHUNK_SIZE"
)

#' Get maximum allowable workers considering CRAN limits
#'
#' @noRd
.get_max_workers <- function() {
  max_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(max_cores) || max_cores < 1L) {
    max_cores <- parallel::detectCores()
    if (is.na(max_cores) || max_cores < 1L) max_cores <- 1L
  }

  cran_limit_raw <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(cran_limit_raw)) {
    cran_limit_lower <- tolower(trimws(cran_limit_raw))
    if (cran_limit_lower %in% c("true", "t", "warn")) {
      max_cores <- min(max_cores, 2L)
    } else {
      parsed_num <- suppressWarnings(as.integer(cran_limit_lower))
      if (!is.na(parsed_num) && parsed_num >= 1L) {
        max_cores <- min(max_cores, parsed_num)
      }
    }
  }

  as.integer(max_cores)
}

#' Resolve worker count for parallelization
#'
#' @noRd
.resolve_n_workers <- function(parallel = TRUE, n_workers = NULL, n_folds = NULL, max_tasks = NULL) {
  if (isFALSE(parallel) || identical(parallel, "none")) {
    return(1L)
  }

  tasks <- if (!is.null(n_folds)) n_folds else max_tasks
  max_cores <- .get_max_workers()

  if (is.null(n_workers)) {
    limit <- if (!is.null(tasks)) min(as.integer(max_cores) - 1L, as.integer(tasks)) else as.integer(max_cores) - 1L
    workers <- max(1L, limit)
  } else {
    limit <- if (!is.null(tasks)) min(as.integer(n_workers), as.integer(tasks), as.integer(max_cores)) else min(as.integer(n_workers), as.integer(max_cores))
    workers <- max(1L, limit)
  }

  max(1L, as.integer(workers))
}

#' Validate threads per outer worker
#'
#' @noRd
.validate_threads_per_worker <- function(threads_per_worker) {
  if (!is.numeric(threads_per_worker) || length(threads_per_worker) != 1L ||
      is.na(threads_per_worker) || threads_per_worker <= 0 ||
      threads_per_worker != as.integer(threads_per_worker)) {
    stop("`threads_per_worker` must be a positive integer.", call. = FALSE)
  }
  as.integer(threads_per_worker)
}

#' Resolve a safe hybrid outer-process and intra-process thread budget
#'
#' @noRd
.resolve_hybrid_budget <- function(n_workers = NULL,
                                   threads_per_worker = 1L,
                                   n_folds = NULL,
                                   warn = TRUE) {
  requested_threads <- .validate_threads_per_worker(threads_per_worker)
  max_cores <- .get_max_workers()
  outer_workers <- .resolve_n_workers(
    parallel = TRUE,
    n_workers = n_workers,
    n_folds = n_folds
  )
  safe_threads <- max(1L, min(requested_threads, max_cores %/% outer_workers))

  requested_outer <- if (is.null(n_workers)) outer_workers else as.integer(n_workers)
  requested_total <- requested_outer * requested_threads
  effective_total <- outer_workers * safe_threads
  if (isTRUE(warn) &&
      (requested_outer != outer_workers || requested_threads != safe_threads)) {
    warning(
      "Hybrid parallelism was capped from ", requested_outer, " x ",
      requested_threads, " = ", requested_total, " to ", outer_workers,
      " x ", safe_threads, " = ", effective_total,
      " to respect outer-fold, available CPU, and CRAN core limits.",
      call. = FALSE
    )
  }

  list(
    n_workers = as.integer(outer_workers),
    threads_per_worker = as.integer(safe_threads),
    total_parallelism = as.integer(effective_total),
    max_cores = as.integer(max_cores)
  )
}

# ---- Main exported function ----

#' Nested cross-validation for item-set score selection
#'
#' Performs nested cross-validation to evaluate and select item combinations
#' for short screening scales. Outer cross-validation evaluates predictive
#' performance, while inner cross-validation selects the best item set and
#' cutoff within each outer training fold, reducing optimistic bias.
#'
#' **Core assumptions:**
#' - Higher sum scores indicate higher probability of a positive outcome.
#' - Users must reverse-code items beforehand if needed.
#' - The cutoff rule is `predicted_positive = score >= cutoff`.
#'
#' @param data A data.frame containing item columns and a binary outcome column.
#' @param outcome Character, name of the binary outcome column.
#' @param items Character vector of item column names.
#' @param min_items Integer, minimum items per combination (default 1).
#' @param max_items Integer, maximum items per combination (default 4).
#' @param positive_label Value for a positive case (default 1).
#' @param negative_label Value for a negative case (default 0).
#' @param cutoff_method Method for the optimal cutoff: `"youden"` or
#'   `"closest_topleft"`. Default `"youden"`.
#' @param preselect_top_n Integer, top N models from outer-train exhaustive
#'   search to evaluate via inner CV (default 20).
#' @param preselect_by Metric for pre-selecting top candidates. One of
#'   `"auc"`, `"youden"`, `"sensitivity"`, or `"specificity"`. Default `"auc"`.
#' @param selection_criterion Metric for selecting the best model via inner CV.
#'   One of `"auc"`, `"youden"`, `"sensitivity"`, or `"specificity"`.
#'   Default `"auc"`.
#' @param outer_k Integer, number of outer CV folds (default 5).
#' @param inner_k Integer, number of inner CV folds (default 4).
#' @param outer_repeats Integer, number of outer CV repeats (default 1).
#' @param inner_repeats Integer, number of inner CV repeats. Must be 1 in
#'   v0.1; values > 1 warn and are reset to 1.
#' @param stratified Logical, use stratified folds? Must be `TRUE` in v0.1.
#' @param seed Integer, seed for reproducibility (default `NULL`).
#' @param engine Character, computation engine. `"R"` (default) or `"Rcpp"`.
#' @param parallel Logical or character. If `TRUE` or `"outer"`, outer cross-validation
#'   folds are evaluated in parallel using socket workers (\code{\link[parallel]{makePSOCKcluster}}).
#'   If `"chunks"`, inner exhaustive searches are evaluated across persistent chunk socket
#'   workers. If `"threads"`, outer folds are evaluated serially while inner exhaustive
#'   searches use C++ multi-threading within the main R process via RcppParallel.
#'   If `"hybrid"`, outer folds use socket workers and each worker uses C++
#'   multi-threading for its exhaustive candidate evaluation.
#'   Default `FALSE`.
#' @param n_workers Integer, number of worker processes (for `"outer"` or `"chunks"`),
#'   threads (for `"threads"`), or outer PSOCK workers (for `"hybrid"`), or
#'   `NULL` (default) for automatic detection. In hybrid mode, the outer worker
#'   count is resolved first; `threads_per_worker` is then capped to the remaining
#'   CPU budget.
#'   Ignored when `parallel = FALSE` or `"none"`.
#'   When `parallel = TRUE` and `n_workers = NULL`, worker count defaults to
#'   \code{max(1L, parallel::detectCores(logical = FALSE) - 1L)}. The effective
#'   worker count is automatically capped by available CPU cores and CRAN core limits
#'   (\code{_R_CHECK_LIMIT_CORES_}). For `"outer"`, it is additionally capped by the
#'   number of outer folds.
#' @param threads_per_worker Positive integer, requested number of C++ threads
#'   used inside each outer socket worker when `parallel = "hybrid"` (default 1).
#'   The effective value may be reduced according to the CPU budget, resolved
#'   outer worker count, and `_R_CHECK_LIMIT_CORES_`. For other modes this must
#'   remain 1.
#' @param progress Logical, show progress bars? Default `TRUE`.
#' @param verbose Logical, print progress messages? Default `TRUE`.
#' @param return Character, `"full"` (all details) or `"summary"` (summary
#'   only). Only `"full"` is implemented in v0.1.
#' @param output_dir Character, directory for CSV output. Deferred to v0.2;
#'   warns if non-NULL.
#' @param file_prefix Character, prefix for CSV filenames (default `"NCVROC"`).
#'
#' @return An object of class `"ncvroc_result"`, a list with elements:
#'   \item{summary}{data.frame of per-fold test performance.}
#'   \item{outer_results}{list of per-fold details including predictions.}
#'   \item{selected_models}{character vector of selected item sets per fold.}
#'   \item{selected_model_frequency}{data.frame of item-set selection counts.}
#'   \item{outer_predictions}{data.frame of all out-of-fold predictions.}
#'   \item{settings}{list of all argument values.}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   y  = sample(0:1, 100, replace = TRUE),
#'   q1 = sample(0:2, 100, replace = TRUE),
#'   q2 = sample(0:2, 100, replace = TRUE),
#'   q3 = sample(0:2, 100, replace = TRUE)
#' )
#' result <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
#'   max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
#'   progress = FALSE, verbose = FALSE)
#' summary(result)
#'
#' @export
nested_sum_roc <- function(data,
                           outcome,
                           items,
                           min_items = 1,
                           max_items = 4,
                           positive_label = 1,
                           negative_label = 0,
                           cutoff_method = c("youden", "closest_topleft"),
                           preselect_top_n = 20,
                           preselect_by = "auc",
                           selection_criterion = "auc",
                           outer_k = 5,
                           inner_k = 4,
                           outer_repeats = 1,
                           inner_repeats = 1,
                           stratified = TRUE,
                           seed = NULL,
                           engine = "R",
                           parallel = FALSE,
                           n_workers = NULL,
                           threads_per_worker = 1L,
                           progress = TRUE,
                           verbose = TRUE,
                           return = c("full", "summary"),
                           output_dir = NULL,
                           file_prefix = "NCVROC") {

  # ---- Capture settings ----
  settings <- as.list(environment())

  # ---- Argument validation ----
  cutoff_method      <- match.arg(cutoff_method)
  return             <- match.arg(return)

  valid_metrics <- c("auc", "youden", "sensitivity", "specificity")
  if (!preselect_by %in% valid_metrics) {
    stop("`preselect_by` must be one of: ", paste(valid_metrics, collapse = ", "), ".", call. = FALSE)
  }
  if (!selection_criterion %in% valid_metrics) {
    stop("`selection_criterion` must be one of: ", paste(valid_metrics, collapse = ", "), ".", call. = FALSE)
  }

  if (!is.numeric(preselect_top_n) || length(preselect_top_n) != 1 ||
      preselect_top_n <= 0 || preselect_top_n != as.integer(preselect_top_n)) {
    stop("`preselect_top_n` must be a positive integer.", call. = FALSE)
  }
  preselect_top_n <- as.integer(preselect_top_n)

  if (!is.numeric(outer_k) || length(outer_k) != 1 || outer_k < 2 ||
      outer_k != as.integer(outer_k)) {
    stop("`outer_k` must be an integer >= 2.", call. = FALSE)
  }
  outer_k <- as.integer(outer_k)

  if (!is.numeric(inner_k) || length(inner_k) != 1 || inner_k < 2 ||
      inner_k != as.integer(inner_k)) {
    stop("`inner_k` must be an integer >= 2.", call. = FALSE)
  }
  inner_k <- as.integer(inner_k)

  if (!isTRUE(stratified)) {
    stop("Only `stratified = TRUE` is supported in v0.1.", call. = FALSE)
  }

  if (inner_repeats != 1) {
    warning("`inner_repeats > 1` is not implemented in v0.1; using `inner_repeats = 1`.",
            call. = FALSE)
    inner_repeats <- 1L
  }

  if (!is.null(output_dir)) {
    warning("CSV output is deferred to v0.2; ignoring `output_dir`.",
            call. = FALSE)
  }

  if (return == "summary") {
    warning("`return = 'summary'` is not yet implemented; returning 'full'.",
            call. = FALSE)
  }

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
  if (parallel_mode == "hybrid" && engine != "Rcpp") {
    stop("`parallel = 'hybrid'` requires `engine = 'Rcpp'`.", call. = FALSE)
  }

  # ---- Validate inputs ----
  validated    <- validate_inputs(data, outcome, items, positive_label, negative_label)
  full_data    <- validated$data
  y            <- validated$y
  items        <- validated$items
  outcome_col  <- validated$outcome_col

  n_total <- length(y)

  # ---- Create outer folds ----
  outer_folds <- make_stratified_folds(
    y, k = outer_k, repeats = outer_repeats, seed = seed
  )
  n_folds <- length(outer_folds)

  # ---- Resolve parallel execution ----
  hybrid_budget <- if (parallel_mode == "hybrid") {
    .resolve_hybrid_budget(n_workers, threads_per_worker, n_folds)
  } else {
    NULL
  }
  actual_workers <- if (parallel_mode == "outer") {
    .resolve_n_workers(parallel = TRUE, n_workers = n_workers, n_folds = n_folds)
  } else if (parallel_mode == "hybrid") {
    hybrid_budget$n_workers
  } else if (parallel_mode %in% c("chunks", "threads")) {
    .resolve_n_workers(parallel = TRUE, n_workers = n_workers)
  } else {
    1L
  }
  actual_threads_per_worker <- if (parallel_mode == "hybrid") {
    hybrid_budget$threads_per_worker
  } else {
    1L
  }
  if (parallel_mode == "hybrid") {
    settings$requested_outer_workers <- if (is.null(n_workers)) NA_integer_ else n_workers
    settings$requested_threads_per_worker <- threads_per_worker
    settings$effective_outer_workers <- actual_workers
    settings$effective_threads_per_worker <- actual_threads_per_worker
    settings$effective_total_parallelism <- hybrid_budget$total_parallelism
    settings$effective_max_cores <- hybrid_budget$max_cores
  }

  # ---- Determine if nested CV needs streaming ----
  total_ncv_combos <- .count_total_combos(length(items), min_items, max_items)
  use_streaming_ncv <- total_ncv_combos > AUTO_MEMORY_LIMIT

  if (verbose && use_streaming_ncv) {
    message(
      "Nested CV: ", total_ncv_combos, " combinations, using streaming top-",
      preselect_top_n, " preselection"
    )
  }

  if (verbose) {
    msg <- paste0(
      "Nested CV: ", outer_k, "-fold outer CV x ", outer_repeats, " repeat(s), ",
      inner_k, "-fold inner CV"
    )
    if (parallel_mode == "hybrid") {
      msg <- paste0(msg, " (parallel: hybrid, ", actual_workers,
                    " outer worker", if (actual_workers == 1L) "" else "s",
                    " x ", actual_threads_per_worker, " threads = ",
                    hybrid_budget$total_parallelism, " total)")
    } else if (parallel_mode != "none" && actual_workers > 1L) {
      msg <- paste0(msg, " (parallel: ", parallel_mode, ", ", actual_workers, " workers)")
    }
    message(msg)
  }

  # ---- Main loop over outer folds (Serial, Outer-Parallel, Chunks-Parallel, or Threads-Parallel) ----
  if (parallel_mode %in% c("outer", "hybrid") && actual_workers > 1L) {
    cl <- parallel::makePSOCKcluster(actual_workers)
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
    available_symbols <- intersect(.OUTER_WORKER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
    parallel::clusterExport(cl, varlist = available_symbols, envir = ns)

    fold_worker <- function(i) {
      .evaluate_single_outer_fold(
        i                   = i,
        outer_folds         = outer_folds,
        full_data           = full_data,
        y                   = y,
        n_total             = n_total,
        items               = items,
        outcome_col         = outcome_col,
        min_items           = min_items,
        max_items           = max_items,
        positive_label      = positive_label,
        negative_label      = negative_label,
        cutoff_method       = cutoff_method,
        preselect_top_n     = preselect_top_n,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        inner_k             = inner_k,
        inner_repeats       = inner_repeats,
        use_streaming_ncv   = use_streaming_ncv,
        engine              = engine,
        seed                = seed,
        progress            = FALSE,
        verbose             = FALSE,
        cl_chunk            = NULL,
        parallel_inner      = if (parallel_mode == "hybrid") "threads" else "none",
        n_workers_inner     = if (parallel_mode == "hybrid") actual_threads_per_worker else 1L
      )
    }

    per_fold <- parallel::parLapply(cl, seq_len(n_folds), fold_worker)

  } else if (parallel_mode == "chunks" && actual_workers > 1L) {
    # Persistent chunk cluster reused across all outer folds
    cl_chunk <- parallel::makePSOCKcluster(actual_workers)
    on.exit(parallel::stopCluster(cl_chunk), add = TRUE)

    lib_paths <- .libPaths()
    parallel::clusterExport(cl_chunk, "lib_paths", envir = environment())
    parallel::clusterEvalQ(cl_chunk, {
      .libPaths(lib_paths)
      if (requireNamespace("NCVROC", quietly = TRUE)) {
        try(library(NCVROC), silent = TRUE)
      }
      NULL
    })

    ns <- asNamespace("NCVROC")
    available_chunk_syms <- intersect(.CHUNK_WORKER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
    if (length(available_chunk_syms) > 0) {
      parallel::clusterExport(cl_chunk, varlist = available_chunk_syms, envir = ns)
    }

    per_fold <- vector("list", n_folds)
    for (i in seq_len(n_folds)) {
      per_fold[[i]] <- .evaluate_single_outer_fold(
        i                   = i,
        outer_folds         = outer_folds,
        full_data           = full_data,
        y                   = y,
        n_total             = n_total,
        items               = items,
        outcome_col         = outcome_col,
        min_items           = min_items,
        max_items           = max_items,
        positive_label      = positive_label,
        negative_label      = negative_label,
        cutoff_method       = cutoff_method,
        preselect_top_n     = preselect_top_n,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        inner_k             = inner_k,
        inner_repeats       = inner_repeats,
        use_streaming_ncv   = use_streaming_ncv,
        engine              = engine,
        seed                = seed,
        progress            = progress && verbose,
        verbose             = verbose,
        cl_chunk            = cl_chunk,
        parallel_inner      = "none",
        n_workers_inner     = 1L
      )
    }

  } else if (parallel_mode %in% c("threads", "hybrid")) {
    # Sequential outer folds with multi-threaded C++ inner exhaustive evaluation
    per_fold <- vector("list", n_folds)
    for (i in seq_len(n_folds)) {
      per_fold[[i]] <- .evaluate_single_outer_fold(
        i                   = i,
        outer_folds         = outer_folds,
        full_data           = full_data,
        y                   = y,
        n_total             = n_total,
        items               = items,
        outcome_col         = outcome_col,
        min_items           = min_items,
        max_items           = max_items,
        positive_label      = positive_label,
        negative_label      = negative_label,
        cutoff_method       = cutoff_method,
        preselect_top_n     = preselect_top_n,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        inner_k             = inner_k,
        inner_repeats       = inner_repeats,
        use_streaming_ncv   = use_streaming_ncv,
        engine              = engine,
        seed                = seed,
        progress            = progress && verbose,
        verbose             = verbose,
        cl_chunk            = NULL,
        parallel_inner      = "threads",
        n_workers_inner     = if (parallel_mode == "hybrid") actual_threads_per_worker else actual_workers
      )
    }

  } else {
    per_fold <- vector("list", n_folds)
    for (i in seq_len(n_folds)) {
      per_fold[[i]] <- .evaluate_single_outer_fold(
        i                   = i,
        outer_folds         = outer_folds,
        full_data           = full_data,
        y                   = y,
        n_total             = n_total,
        items               = items,
        outcome_col         = outcome_col,
        min_items           = min_items,
        max_items           = max_items,
        positive_label      = positive_label,
        negative_label      = negative_label,
        cutoff_method       = cutoff_method,
        preselect_top_n     = preselect_top_n,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        inner_k             = inner_k,
        inner_repeats       = inner_repeats,
        use_streaming_ncv   = use_streaming_ncv,
        engine              = engine,
        seed                = seed,
        progress            = progress && verbose,
        verbose             = verbose,
        cl_chunk            = NULL,
        parallel_inner      = "none",
        n_workers_inner     = 1L
      )
    }
  }

  # ---- Aggregate results ----

  # Summary table
  summary_df <- do.call(rbind, lapply(per_fold, function(x) {
    data.frame(
      outer_fold     = x$outer_fold,
      selected_items = x$selected_items,
      n_items        = x$n_items,
      auc            = x$auc,
      cutoff         = x$cutoff,
      sensitivity    = x$sensitivity,
      specificity    = x$specificity,
      youden         = x$youden,
      accuracy       = x$accuracy,
      ppv            = x$ppv,
      npv            = x$npv,
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_df) <- NULL

  # Selected models
  selected_models <- vapply(per_fold, `[[`, character(1), "selected_items",
                            USE.NAMES = FALSE)

  # Model frequency
  freq_table <- table(selected_models)
  freq_df <- data.frame(
    items        = names(freq_table),
    n_selections = as.integer(freq_table),
    frequency    = as.numeric(freq_table / n_folds),
    stringsAsFactors = FALSE
  )
  freq_df <- freq_df[order(-freq_df$frequency), ]
  rownames(freq_df) <- NULL

  # Outer predictions
  all_predictions <- do.call(rbind, lapply(per_fold, function(x) {
    df <- x$predictions
    df$outer_fold <- x$outer_fold
    df
  }))
  all_predictions <- all_predictions[order(all_predictions$row_index), ]
  rownames(all_predictions) <- NULL

  # Build result
  result <- list(
    summary                  = summary_df,
    outer_results            = per_fold,
    selected_models          = selected_models,
    selected_model_frequency = freq_df,
    outer_predictions        = all_predictions,
    settings                 = settings
  )
  class(result) <- "ncvroc_result"

  result
}

# ---- S3 methods ----

#' Print NCVROC nested cross-validation result
#'
#' @param x An object of class `"ncvroc_result"`.
#' @param ... Additional arguments (ignored).
#' @return The original object of class `"ncvroc_result"`, returned
#'   invisibly. The object contains the nested cross-validation results.
#'   This method is called primarily for its side effect of printing
#'   formatted nested cross-validation results.
#' @keywords internal
#' @export
print.ncvroc_result <- function(x, ...) {
  cat("NCVROC nested cross-validation result\n\n")
  cols <- intersect(
    c("outer_fold", "selected_items", "n_items", "auc", "sensitivity", "specificity"),
    colnames(x$summary)
  )
  print(x$summary[, cols, drop = FALSE])

  cat("\nMost frequently selected items:\n")
  n_show <- min(3, nrow(x$selected_model_frequency))
  if (n_show > 0) {
    print(x$selected_model_frequency[seq_len(n_show),
          c("items", "n_selections", "frequency"), drop = FALSE])
  }

  invisible(x)
}

#' Summarize NCVROC nested cross-validation result
#'
#' @param object An object of class `"ncvroc_result"`.
#' @param ... Additional arguments (ignored).
#' @return The original object of class `"ncvroc_result"`, returned
#'   invisibly. The object contains the nested cross-validation results.
#'   This method is called primarily for its side effect of printing
#'   a detailed summary of those results.
#' @keywords internal
#' @export
summary.ncvroc_result <- function(object, ...) {
  settings <- object$settings
  smry <- object$summary

  n_items <- length(settings$items)
  n_y     <- length(settings$items)  # not accessible directly; estimate from outer_predictions
  n_pos   <- sum(object$outer_predictions$true_outcome == 1L)
  n_neg   <- sum(object$outer_predictions$true_outcome == 0L)

  cat("NCVROC nested cross-validation summary\n")
  cat(rep("-", 50), "\n", sep = "")

  cat(sprintf("Observations : %d (positive: %d, negative: %d)\n",
              nrow(object$outer_predictions), n_pos, n_neg))
  cat(sprintf("Candidate items : %d\n", n_items))
  cat(sprintf("Max items per scale : %d\n", settings$max_items))
  cat(sprintf("Outer CV : %d-fold x %d repeat(s)\n",
              settings$outer_k, settings$outer_repeats))
  cat(sprintf("Inner CV : %d-fold\n", settings$inner_k))
  cat(sprintf("Pre-select : top %d by %s\n",
              settings$preselect_top_n, settings$preselect_by))
  cat(sprintf("Selection criterion : %s\n", settings$selection_criterion))
  cat(rep("-", 50), "\n", sep = "")

  cat(sprintf("Mean AUC          : %.4f (SD = %.4f)\n",
              mean(smry$auc, na.rm = TRUE), sd(smry$auc, na.rm = TRUE)))
  cat(sprintf("Mean sensitivity  : %.4f (SD = %.4f)\n",
              mean(smry$sensitivity, na.rm = TRUE),
              sd(smry$sensitivity, na.rm = TRUE)))
  cat(sprintf("Mean specificity  : %.4f (SD = %.4f)\n",
              mean(smry$specificity, na.rm = TRUE),
              sd(smry$specificity, na.rm = TRUE)))
  cat(sprintf("Mean Youden index : %.4f (SD = %.4f)\n",
              mean(smry$youden, na.rm = TRUE), sd(smry$youden, na.rm = TRUE)))
  cat(rep("-", 50), "\n", sep = "")

  n_unique <- nrow(object$selected_model_frequency)
  cat(sprintf("Unique item sets selected : %d / %d folds\n", n_unique, nrow(smry)))
  cat("Most frequent:\n")
  n_show <- min(3, n_unique)
  if (n_show > 0) {
    for (i in seq_len(n_show)) {
      row <- object$selected_model_frequency[i, ]
      cat(sprintf("  %s (%d times, %.0f%%)\n",
                  row$items, row$n_selections, row$frequency * 100))
    }
  }

  invisible(object)
}

#' Plot NCVROC nested cross-validation result
#'
#' @param x An object of class `"ncvroc_result"`.
#' @param which Character, which plot: `"selection"` (barplot of model
#'   frequencies) or `"auc"` (dotplot of per-fold AUC). Default `"selection"`.
#' @param ... Additional arguments (ignored).
#' @return The original object of class `"ncvroc_result"`, returned
#'   invisibly. The object contains the nested cross-validation results.
#'   This method is called primarily for its side effect of producing
#'   a base-graphics plot of those results.
#' @keywords internal
#' @export
plot.ncvroc_result <- function(x, which = c("selection", "auc"), ...) {
  which <- match.arg(which)

  if (which == "selection") {
    freq <- x$selected_model_frequency
    n_show <- min(10, nrow(freq))
    if (n_show == 0) {
      plot.new()
      text(0.5, 0.5, "No models selected")
      return(invisible(x))
    }
    freq_sub <- freq[seq_len(n_show), , drop = FALSE]

    old_par <- graphics::par(mar = c(4, 12, 2, 2))
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::barplot(
      height = rev(freq_sub$frequency),
      names.arg = rev(freq_sub$items),
      horiz = TRUE,
      las = 1,
      xlab = "Selection frequency",
      main = "Item set selection frequency",
      col = "steelblue",
      border = NA
    )
  } else if (which == "auc") {
    auc_vals <- x$summary$auc
    auc_vals <- auc_vals[!is.na(auc_vals)]

    if (length(auc_vals) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid AUC values")
      return(invisible(x))
    }

    graphics::stripchart(
      auc_vals,
      method = "jitter",
      vertical = TRUE,
      pch = 21,
      bg = "steelblue",
      xlab = "",
      ylab = "AUC",
      main = "Outer fold test AUC",
      xaxt = "n"
    )
    mean_auc <- mean(auc_vals, na.rm = TRUE)
    graphics::abline(h = mean_auc, col = "red", lty = 2, lwd = 2)
    graphics::abline(h = 0.5, col = "gray50", lty = 3)
    graphics::legend("bottomleft",
      legend = c(sprintf("Mean = %.3f", mean_auc), "AUC = 0.5"),
      col = c("red", "gray50"), lty = c(2, 3), lwd = c(2, 1),
      cex = 0.8, bty = "n"
    )
  }

  invisible(x)
}
