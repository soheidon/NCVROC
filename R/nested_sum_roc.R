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
#' @param engine Character, computation engine. Only `"R"` in v0.1.
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
#' \dontrun{
#' d <- data.frame(
#'   y  = sample(0:1, 100, replace = TRUE),
#'   q1 = sample(0:2, 100, replace = TRUE),
#'   q2 = sample(0:2, 100, replace = TRUE),
#'   q3 = sample(0:2, 100, replace = TRUE)
#' )
#' result <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
#'   max_items = 2, outer_k = 3, inner_k = 2, seed = 42)
#' summary(result)
#' }
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

  if (verbose) {
    message(
      "Nested CV: ", outer_k, "-fold outer CV x ", outer_repeats, " repeat(s), ",
      inner_k, "-fold inner CV"
    )
  }

  # ---- Main loop over outer folds ----
  per_fold <- vector("list", n_folds)

  for (i in seq_len(n_folds)) {
    test_idx  <- outer_folds[[i]]
    train_idx <- setdiff(seq_len(n_total), test_idx)
    fold_name <- names(outer_folds)[i]

    if (verbose) {
      message("Outer fold ", i, "/", n_folds, " (", fold_name, "): ",
              length(train_idx), " train, ", length(test_idx), " test")
    }

    # Step 1: exhaustive search on outer train ONLY
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
      engine           = "R",
      progress         = FALSE
    )

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

    # Record
    per_fold[[i]] <- list(
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
