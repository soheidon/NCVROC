# cv_engine.R — Unified cross-validation engine for NCVROC
#
# Provides core cross-validation machinery for:
#   - Dedicated LOOCV fold generation (.build_loocv_folds)
#   - Unified fold builder (.build_cv_folds)
#   - Single-model OOF prediction evaluator (.run_fixed_model_cv)
#   - Repeated OOF metrics aggregator (.aggregate_oof_metrics)
#   - Public functions: cv_sum_roc(), loocv_sum_roc()
#   - S3 methods: print.cv_sum_roc_result(), summary.cv_sum_roc_result()

#' Build LOOCV folds
#'
#' Generates a dedicated list of leave-one-out test indices for `n` observations.
#' Each element is a single integer index `1:n`.
#'
#' @param n Integer, total number of observations.
#' @return A named list of length `n`, each containing a single integer test index.
#' @keywords internal
.build_loocv_folds <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || n < 2 || n != as.integer(n)) {
    stop("`n` must be an integer >= 2 for LOOCV.", call. = FALSE)
  }
  n <- as.integer(n)
  folds <- as.list(seq_len(n))
  names(folds) <- paste0("Rep1_Fold", seq_len(n))
  folds
}

#' Build cross-validation fold indices
#'
#' Unified builder for k-fold and LOOCV partitions.
#'
#' @param y Binary outcome vector (0/1).
#' @param cv_method Character, "kfold" or "loocv".
#' @param folds Integer, number of folds for k-fold CV. Ignored for LOOCV.
#' @param repeats Integer, number of independent repeats for k-fold CV. Must be 1 for LOOCV.
#' @param stratified Logical, maintain class balance across folds (k-fold only).
#' @param seed Integer or NULL, random seed.
#'
#' @return A named list of integer test-index vectors.
#' @keywords internal
.build_cv_folds <- function(y,
                            cv_method = c("kfold", "loocv"),
                            folds = 5,
                            repeats = 1,
                            stratified = TRUE,
                            seed = NULL) {
  cv_method <- match.arg(cv_method)

  if (cv_method == "loocv") {
    if (!is.null(repeats) && repeats > 1) {
      stop("LOOCV does not support `repeats > 1` because the leave-one-out partition is deterministic and unique.",
           call. = FALSE)
    }
    return(.build_loocv_folds(length(y)))
  }

  # k-fold CV
  if (!is.numeric(folds) || length(folds) != 1 || folds < 2 || folds != as.integer(folds)) {
    stop("`folds` must be an integer >= 2.", call. = FALSE)
  }
  folds <- as.integer(folds)

  if (!is.numeric(repeats) || length(repeats) != 1 || repeats < 1 || repeats != as.integer(repeats)) {
    stop("`repeats` must be a positive integer.", call. = FALSE)
  }
  repeats <- as.integer(repeats)

  if (stratified) {
    make_stratified_folds(y = y, k = folds, repeats = repeats, seed = seed)
  } else {
    # Non-stratified k-fold splits
    n <- length(y)
    if (folds > n) {
      warning("`folds` (", folds, ") exceeds sample size (", n, "). Reducing `folds` to ", n, ".", call. = FALSE)
      folds <- n
    }
    all_folds <- vector("list", folds * repeats)
    idx <- 1L

    split_n_into_k <- function(shuffled_idx, n_total, k_folds) {
      sizes <- rep(floor(n_total / k_folds), k_folds)
      extra <- n_total %% k_folds
      if (extra > 0) {
        sizes[1:extra] <- sizes[1:extra] + 1L
      }
      end_pts <- cumsum(sizes)
      start_pts <- c(1L, end_pts[-k_folds] + 1L)
      lapply(seq_len(k_folds), function(f) shuffled_idx[start_pts[f]:end_pts[f]])
    }

    for (r in seq_len(repeats)) {
      if (!is.null(seed)) {
        set.seed(seed + r - 1L)
      }
      shuffled <- sample.int(n)
      f_list <- split_n_into_k(shuffled, n, folds)
      for (f in seq_len(folds)) {
        name <- paste0("Rep", r, "_Fold", f)
        all_folds[[idx]] <- f_list[[f]]
        names(all_folds)[idx] <- name
        idx <- idx + 1L
      }
    }
    all_folds
  }
}

#' Run fixed-model cross-validation
#'
#' Fits cutoff on each training fold ONLY and applies it to test observations.
#'
#' @param itemset Character vector of item column names.
#' @param data data.frame with item columns.
#' @param y Binary outcome vector (0/1).
#' @param cv_folds Named list of test index vectors (from .build_cv_folds).
#' @param cutoff_method Character, "youden" or "closest_topleft".
#'
#' @return A list with `oof_predictions` (data.frame) and `fold_results` (data.frame).
#' @keywords internal
.run_fixed_model_cv <- function(itemset,
                                data,
                                y,
                                cv_folds,
                                cutoff_method = c("youden", "closest_topleft")) {
  cutoff_method <- match.arg(cutoff_method)
  items_vec <- if (is.character(itemset) && length(itemset) == 1 && grepl(",", itemset)) {
    .parse_itemset(itemset)
  } else {
    as.character(itemset)
  }

  n_folds <- length(cv_folds)
  oof_list <- vector("list", n_folds)
  fold_res_list <- vector("list", n_folds)

  for (f in seq_len(n_folds)) {
    fold_name <- names(cv_folds)[f]
    test_idx <- cv_folds[[f]]
    train_idx <- setdiff(seq_along(y), test_idx)

    # Parse repeat_id and fold_id from name (e.g. "Rep1_Fold3")
    rep_id <- 1L
    f_id <- f
    if (!is.null(fold_name) && grepl("^Rep([0-9]+)_Fold([0-9]+)$", fold_name)) {
      parts <- regmatches(fold_name, regexec("^Rep([0-9]+)_Fold([0-9]+)$", fold_name))[[1]]
      rep_id <- as.integer(parts[2])
      f_id <- as.integer(parts[3])
    }

    # Step 1: Fit cutoff on training data ONLY
    train_scores <- rowSums(data[train_idx, items_vec, drop = FALSE])
    train_y <- y[train_idx]
    train_freq <- compute_score_frequencies(train_scores, train_y)
    train_metrics <- compute_roc_metrics_from_table(train_freq$pos_counts, train_freq$neg_counts)
    best_row <- find_optimal_cutoff(train_metrics, method = cutoff_method)
    train_cutoff <- best_row$cutoff

    # Step 2: Apply training cutoff to test observations
    test_scores <- rowSums(data[test_idx, items_vec, drop = FALSE])
    test_y <- y[test_idx]
    pred_class <- ifelse(test_scores >= train_cutoff, 1L, 0L)

    oof_list[[f]] <- data.frame(
      row_index       = test_idx,
      repeat_id       = rep_id,
      fold_id         = f_id,
      true_outcome    = test_y,
      predicted_score = test_scores,
      predicted_class = pred_class,
      applied_cutoff  = train_cutoff,
      stringsAsFactors = FALSE
    )

    fold_res_list[[f]] <- data.frame(
      fold_name    = if (is.null(fold_name)) paste0("Fold", f) else fold_name,
      repeat_id    = rep_id,
      fold_id      = f_id,
      n_train      = length(train_idx),
      n_test       = length(test_idx),
      train_cutoff = train_cutoff,
      test_n_pos   = sum(test_y == 1L),
      test_n_neg   = sum(test_y == 0L),
      stringsAsFactors = FALSE
    )
  }

  oof_df <- do.call(rbind, oof_list)
  # Sort oof_df by repeat_id, then row_index
  ord <- order(oof_df$repeat_id, oof_df$row_index)
  oof_df <- oof_df[ord, , drop = FALSE]
  rownames(oof_df) <- NULL

  fold_results_df <- do.call(rbind, fold_res_list)
  rownames(fold_results_df) <- NULL

  list(
    oof_predictions = oof_df,
    fold_results     = fold_results_df
  )
}

#' Aggregate OOF predictions across repeats and folds
#'
#' Aggregates out-of-fold predictions per repeat, computes classification
#' metrics and pooled AUC per repeat, and reports mean and SD across repeats.
#' Summarizes training-derived cutoffs as an unweighted fold-level distribution (1 cutoff per fold).
#'
#' @param oof_df data.frame with columns row_index, repeat_id, fold_id,
#'   true_outcome, predicted_score, predicted_class, applied_cutoff.
#' @param repeats Integer, number of repeats.
#' @param fold_results data.frame with fold-level metadata containing `train_cutoff`.
#'
#' @return A list with `repeat_metrics` (data.frame), `summary` (data.frame),
#'   `overall_pooled` (named vector), and `cv_cutoff_distribution` (list).
#' @keywords internal
.aggregate_oof_metrics <- function(oof_df, repeats = 1, fold_results = NULL) {
  unique_repeats <- sort(unique(oof_df$repeat_id))
  n_reps <- length(unique_repeats)

  calc_class_metrics <- function(pred_class, true_y) {
    tp <- sum(pred_class == 1L & true_y == 1L)
    tn <- sum(pred_class == 0L & true_y == 0L)
    fp <- sum(pred_class == 1L & true_y == 0L)
    fn <- sum(pred_class == 0L & true_y == 1L)

    sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    spec <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
    ppv  <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    npv  <- if (tn + fn > 0) tn / (tn + fn) else NA_real_
    acc  <- if (tp + tn + fp + fn > 0) (tp + tn) / (tp + tn + fp + fn) else NA_real_
    youd <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1

    c(sensitivity = sens, specificity = spec, youden = youd,
      accuracy = acc, ppv = ppv, npv = npv)
  }

  rep_list <- vector("list", n_reps)

  for (i in seq_along(unique_repeats)) {
    r <- unique_repeats[i]
    sub <- oof_df[oof_df$repeat_id == r, , drop = FALSE]

    # AUC on OOF scores of this repeat
    freq_r <- compute_score_frequencies(sub$predicted_score, sub$true_outcome)
    auc_r  <- compute_auc_from_table(freq_r$pos_counts, freq_r$neg_counts)

    cm <- calc_class_metrics(sub$predicted_class, sub$true_outcome)

    rep_list[[i]] <- data.frame(
      repeat_id   = r,
      auc         = auc_r,
      sensitivity = cm["sensitivity"],
      specificity = cm["specificity"],
      youden      = cm["youden"],
      accuracy    = cm["accuracy"],
      ppv         = cm["ppv"],
      npv         = cm["npv"],
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
  }

  repeat_metrics <- do.call(rbind, rep_list)

  metric_cols <- c("auc", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv")

  if (n_reps == 1) {
    summary_df <- data.frame(
      metric = metric_cols,
      mean   = as.numeric(repeat_metrics[1, metric_cols]),
      sd     = rep(NA_real_, length(metric_cols)),
      stringsAsFactors = FALSE
    )
  } else {
    means <- colMeans(repeat_metrics[, metric_cols, drop = FALSE], na.rm = TRUE)
    sds   <- apply(repeat_metrics[, metric_cols, drop = FALSE], 2, stats::sd, na.rm = TRUE)
    summary_df <- data.frame(
      metric = metric_cols,
      mean   = as.numeric(means),
      sd     = as.numeric(sds),
      stringsAsFactors = FALSE
    )
  }

  # Overall pooled calculation across all rows
  all_freq <- compute_score_frequencies(oof_df$predicted_score, oof_df$true_outcome)
  all_auc  <- compute_auc_from_table(all_freq$pos_counts, all_freq$neg_counts)
  all_cm   <- calc_class_metrics(oof_df$predicted_class, oof_df$true_outcome)
  overall_pooled <- c(auc = all_auc, all_cm)

  # Cutoff distribution: exactly 1 cutoff per fold (unweighted fold-level distribution)
  fold_cutoffs <- if (!is.null(fold_results) && "train_cutoff" %in% names(fold_results)) {
    fold_results$train_cutoff
  } else {
    dedup <- unique(oof_df[, c("repeat_id", "fold_id", "applied_cutoff")])
    dedup$applied_cutoff
  }

  cv_cutoff_dist <- list(
    mean            = mean(fold_cutoffs),
    sd              = if (length(fold_cutoffs) > 1) stats::sd(fold_cutoffs) else 0,
    median          = stats::median(fold_cutoffs),
    iqr             = stats::IQR(fold_cutoffs),
    min             = min(fold_cutoffs),
    max             = max(fold_cutoffs),
    per_fold_values = fold_cutoffs
  )

  list(
    repeat_metrics         = repeat_metrics,
    summary                = summary_df,
    overall_pooled         = overall_pooled,
    cv_cutoff_distribution = cv_cutoff_dist
  )
}

#' Fixed-model ordinary k-fold cross-validation
#'
#' Performs ordinary k-fold cross-validation or leave-one-out cross-validation (LOOCV)
#' for a user-specified, fixed item-set sum score model.
#'
#' @param data A data.frame containing the outcome and item columns.
#' @param outcome Name of the binary outcome column (bare symbol or character string).
#' @param items Item columns to include in the fixed sum score (bare range e.g. `Q1:Q5`,
#'   bare names in `c()`, character vector, or numeric positions).
#' @param cv_method Character, `"kfold"` (default) or `"loocv"`.
#' @param folds Integer, number of folds for k-fold CV (default 5). Ignored when `cv_method = "loocv"`.
#' @param repeats Integer, number of independent repeats for k-fold CV (default 1).
#'   Must be 1 for LOOCV.
#' @param stratified Logical, maintain class balance across folds (default `TRUE`).
#' @param cutoff_method Cutoff selection rule: `"youden"` (default) or `"closest_topleft"`.
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param ci Logical, compute confidence intervals for full-data apparent metrics (default `FALSE`).
#' @param conf_level Numeric, confidence level (default 0.95).
#' @param seed Integer, random seed for reproducible fold generation.
#'
#' @return An S3 object of class `"cv_sum_roc_result"`.
#'
#' @export
cv_sum_roc <- function(data,
                       outcome,
                       items,
                       cv_method      = c("kfold", "loocv"),
                       folds          = 5,
                       repeats        = 1,
                       stratified     = TRUE,
                       cutoff_method  = c("youden", "closest_topleft"),
                       positive_label = 1,
                       negative_label = 0,
                       ci             = FALSE,
                       conf_level     = 0.95,
                       seed           = NULL) {
  # ---- NSE Column Resolution ----
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)

  cv_method     <- match.arg(cv_method)
  cutoff_method <- match.arg(cutoff_method)

  if (cv_method == "loocv") {
    if (!is.null(repeats) && repeats > 1) {
      stop("LOOCV does not support `repeats > 1` because the leave-one-out partition is deterministic and unique.",
           call. = FALSE)
    }
    repeats <- 1L
  }

  # ---- Prepare Data ----
  dat_prep <- .prepare_ncvroc_data(data, outcome_name, item_names)
  y <- dat_prep[[outcome_name]]

  # Validate binary labels
  if (!all(y %in% c(positive_label, negative_label))) {
    stop("Outcome contains values other than positive_label and negative_label.", call. = FALSE)
  }
  y <- ifelse(y == positive_label, 1L, 0L)
  if (sum(y == 1L) == 0L || sum(y == 0L) == 0L) {
    stop("Outcome must contain both positive and negative cases.", call. = FALSE)
  }
  n_total <- length(y)

  # ---- Build Folds ----
  cv_folds <- .build_cv_folds(
    y          = y,
    cv_method  = cv_method,
    folds      = folds,
    repeats    = repeats,
    stratified = stratified,
    seed       = seed
  )

  # ---- Run OOF CV ----
  cv_exec <- .run_fixed_model_cv(
    itemset       = item_names,
    data          = dat_prep,
    y             = y,
    cv_folds      = cv_folds,
    cutoff_method = cutoff_method
  )

  # ---- Aggregate Metrics ----
  agg <- .aggregate_oof_metrics(
    oof_df       = cv_exec$oof_predictions,
    repeats      = repeats,
    fold_results = cv_exec$fold_results
  )

  # ---- Full Data Deployment Cutoff and Metrics ----
  full_scores  <- rowSums(dat_prep[, item_names, drop = FALSE])
  full_freq    <- compute_score_frequencies(full_scores, y)
  full_metrics <- compute_roc_metrics_from_table(full_freq$pos_counts, full_freq$neg_counts)
  best_full    <- find_optimal_cutoff(full_metrics, method = cutoff_method)
  full_auc     <- compute_auc_from_table(full_freq$pos_counts, full_freq$neg_counts)

  final_full_df <- data.frame(
    items       = format_items(item_names),
    n_items     = length(item_names),
    auc         = full_auc,
    cutoff      = best_full$cutoff,
    sensitivity = best_full$sensitivity,
    specificity = best_full$specificity,
    youden      = best_full$youden,
    accuracy    = best_full$accuracy,
    ppv         = best_full$ppv,
    npv         = best_full$npv,
    n_positive  = sum(y == 1L),
    n_negative  = sum(y == 0L),
    stringsAsFactors = FALSE
  )

  if (ci) {
    final_full_df <- add_performance_cis(
      results    = final_full_df,
      data       = dat_prep,
      outcome    = outcome_name,
      conf_level = conf_level
    )
  }

  structure(
    list(
      summary                = agg$summary,
      repeat_metrics         = agg$repeat_metrics,
      overall_pooled         = agg$overall_pooled,
      cv_cutoff_distribution = agg$cv_cutoff_distribution,
      final_full_data_cutoff = best_full$cutoff,
      final_full_data_metrics= final_full_df,
      oof_predictions        = cv_exec$oof_predictions,
      fold_results           = cv_exec$fold_results,
      items                  = item_names,
      n_items                = length(item_names),
      cv_method              = cv_method,
      settings = list(
        outcome_name   = outcome_name,
        item_names     = item_names,
        cv_method      = cv_method,
        folds          = if (cv_method == "loocv") length(y) else folds,
        repeats        = repeats,
        stratified     = stratified,
        cutoff_method  = cutoff_method,
        positive_label = positive_label,
        negative_label = negative_label,
        ci             = ci,
        conf_level     = conf_level,
        seed           = seed
      )
    ),
    class = "cv_sum_roc_result"
  )
}

#' Fixed-model leave-one-out cross-validation (LOOCV)
#'
#' Dedicated wrapper for leave-one-out cross-validation of a fixed item-set sum score model.
#'
#' @param data A data.frame containing the outcome and item columns.
#' @param outcome Name of the binary outcome column (bare symbol or character string).
#' @param items Item columns to include in the fixed sum score.
#' @param cutoff_method Cutoff selection rule: `"youden"` (default) or `"closest_topleft"`.
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param ci Logical, compute confidence intervals for full-data apparent metrics (default `FALSE`).
#' @param conf_level Numeric, confidence level (default 0.95).
#'
#' @return An S3 object of class `"cv_sum_roc_result"`.
#'
#' @export
loocv_sum_roc <- function(data,
                          outcome,
                          items,
                          cutoff_method  = c("youden", "closest_topleft"),
                          positive_label = 1,
                          negative_label = 0,
                          ci             = FALSE,
                          conf_level     = 0.95) {
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)
  cutoff_method <- match.arg(cutoff_method)

  cv_sum_roc(
    data           = data,
    outcome        = outcome_name,
    items          = item_names,
    cv_method      = "loocv",
    folds          = nrow(data),
    repeats        = 1,
    stratified     = FALSE,
    cutoff_method  = cutoff_method,
    positive_label = positive_label,
    negative_label = negative_label,
    ci             = ci,
    conf_level     = conf_level,
    seed           = NULL
  )
}

#' Print method for cv_sum_roc_result
#'
#' @param x A `cv_sum_roc_result` object.
#' @param ... Additional arguments.
#' @export
print.cv_sum_roc_result <- function(x, ...) {
  cat("\n=== Fixed-Model Cross-Validation (NCVROC) ===\n")
  cat("Method:     ", if (x$cv_method == "loocv") "Leave-One-Out (LOOCV)" else paste0(x$settings$repeats, "x repeated ", x$settings$folds, "-fold CV"), "\n")
  cat("Items (", x$n_items, "): ", paste(x$items, collapse = ", "), "\n", sep = "")
  cat("Cutoff rule:", x$settings$cutoff_method, "\n\n")

  cat("--- Out-of-Fold (OOF) Performance ---\n")
  s <- x$summary
  for (i in seq_len(nrow(s))) {
    m <- s$metric[i]
    val <- s$mean[i]
    sd_val <- s$sd[i]
    if (is.na(sd_val)) {
      cat(sprintf("  %-12s: %.4f\n", m, val))
    } else {
      cat(sprintf("  %-12s: %.4f (SD = %.4f)\n", m, val, sd_val))
    }
  }

  cat("\n--- Cutoff Summary ---\n")
  cd <- x$cv_cutoff_distribution
  cat(sprintf("  CV training-derived cutoff (n_folds = %d): mean = %.2f, median = %.2f (range [%.2f, %.2f])\n",
              length(cd$per_fold_values), cd$mean, cd$median, cd$min, cd$max))
  cat(sprintf("  Final full-data cutoff                   : %.2f\n\n", x$final_full_data_cutoff))

  invisible(x)
}

#' Summary method for cv_sum_roc_result
#'
#' @param object A `cv_sum_roc_result` object.
#' @param ... Additional arguments.
#' @export
summary.cv_sum_roc_result <- function(object, ...) {
  print(object, ...)
}
