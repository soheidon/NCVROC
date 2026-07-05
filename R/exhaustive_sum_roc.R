# exhaustive_sum_roc.R — Exhaustive ROC evaluation of all item subsets

#' Exhaustive ROC evaluation of all item subsets
#'
#' Enumerates all possible item combinations up to `max_items`, computes
#' simple sum scores for each, and evaluates predictive performance via ROC
#' analysis on each combination.
#'
#' The sum score of each item combination is computed as `rowSums()`.
#' **Higher scores are assumed to indicate higher probability of a positive
#' outcome.** Users must reverse-code items beforehand if needed.
#'
#' The cutoff rule is `predicted_positive = score >= cutoff`.
#'
#' @param data A data.frame containing item columns and a binary outcome column.
#' @param outcome Character string naming the binary outcome column.
#' @param items Character vector of item column names. If NULL (default), uses
#'   all columns except `outcome`.
#' @param min_items Integer, minimum number of items per combination (default 1).
#' @param max_items Integer, maximum number of items per combination (default 4).
#' @param positive_label Value in `outcome` representing a positive case (default 1).
#' @param negative_label Value in `outcome` representing a negative case (default 0).
#' @param cutoff_method Method for determining the optimal cutoff. One of
#'   `"youden"` (maximize Youden index) or `"closest_topleft"` (minimize
#'   Euclidean distance to (0,1) in ROC space). Default `"youden"`.
#' @param rank_by Metric for ranking models. One of `"auc"`, `"youden"`,
#'   `"sensitivity"`, `"specificity"`, or `"accuracy"`. Default `"auc"`.
#' @param top_n Integer, return only the top N models. `NULL` returns all models.
#' @param prefer_fewer_items Logical. If `TRUE` and multiple models tie on
#'   `rank_by`, models with fewer items are ranked higher. Default `TRUE`.
#' @param engine Character, computation engine. `"R"` (default) or `"Rcpp"`.
#' @param progress Logical, show progress bar? Default `TRUE`.
#'
#' @return A data.frame with columns: `rank`, `items` (comma-separated string),
#'   `n_items`, `auc`, `cutoff`, `sensitivity`, `specificity`, `youden`,
#'   `accuracy`, `ppv`, `npv`, `n_positive`, `n_negative`.
#'   Sorted by the chosen `rank_by` metric in descending order.
#'
#' @examples
#' d <- data.frame(
#'   y  = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
#'   q1 = c(2, 1, 2, 0, 1, 1, 2, 2, 0, 1),
#'   q2 = c(1, 2, 1, 1, 0, 0, 2, 1, 0, 1),
#'   q3 = c(2, 2, 1, 0, 1, 0, 2, 1, 1, 0)
#' )
#' exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2)
#'
#' @export
exhaustive_sum_roc <- function(data,
                               outcome,
                               items,
                               min_items = 1,
                               max_items = 4,
                               positive_label = 1,
                               negative_label = 0,
                               cutoff_method = c("youden", "closest_topleft"),
                               rank_by = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                               top_n = NULL,
                               prefer_fewer_items = TRUE,
                               engine = c("R", "Rcpp"),
                               progress = TRUE) {

  # ---- Argument validation ----
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)
  engine <- match.arg(engine)

  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || top_n <= 0) {
      stop("`top_n` must be a positive integer or NULL.", call. = FALSE)
    }
    top_n <- as.integer(top_n)
  }

  # ---- Validate inputs (converts outcome to 0/1) ----
  validated <- validate_inputs(data, outcome, items, positive_label, negative_label)

  x     <- validated$data
  y     <- validated$y          # 0/1 numeric vector
  items <- validated$items

  n_total <- length(y)
  n_pos   <- sum(y == 1L)
  n_neg   <- sum(y == 0L)

  # ---- Enumerate all item combinations ----
  combos <- enumerate_combinations(items, min_items = min_items, max_items = max_items)
  n_combos <- length(combos)

  if (engine == "Rcpp") {
    # ---- Rcpp engine ----
    x_mat <- as.matrix(x[, items, drop = FALSE])
    combo_indices <- lapply(combos, function(v) match(v, items) - 1L)

    results <- evaluate_combos_cpp(x_mat, y, combo_indices, cutoff_method)
    results$items <- sapply(combos, format_items)

  } else {
    # ---- R engine ----

    # Progress bar
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = n_combos, style = 3)
      on.exit(close(pb), add = TRUE)
    }

    results <- vector("list", n_combos)

    for (i in seq_len(n_combos)) {
      combo_items <- combos[[i]]
      k <- length(combo_items)

      # Simple sum score
      scores <- rowSums(x[, combo_items, drop = FALSE])

      # Frequency table
      freq <- compute_score_frequencies(scores, y)

      # AUC
      auc_val <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)

      # Full ROC metrics at all cutoffs
      metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)

      # Optimal cutoff
      best <- find_optimal_cutoff(metrics, method = cutoff_method)

      results[[i]] <- data.frame(
        items       = format_items(combo_items),
        n_items     = k,
        auc         = auc_val,
        cutoff      = best$cutoff,
        sensitivity = best$sensitivity,
        specificity = best$specificity,
        youden      = best$youden,
        accuracy    = best$accuracy,
        ppv         = best$ppv,
        npv         = best$npv,
        n_positive  = n_pos,
        n_negative  = n_neg,
        stringsAsFactors = FALSE
      )

      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }

    results <- do.call(rbind, results)
  }

  # ---- Sort ----
  sort_col <- results[[rank_by]]
  if (prefer_fewer_items) {
    # Descending rank_by, then ascending n_items
    ord <- order(-sort_col, results$n_items)
  } else {
    ord <- order(-sort_col)
  }
  results <- results[ord, , drop = FALSE]

  # Truncate if top_n specified
  if (!is.null(top_n)) {
    results <- utils::head(results, top_n)
  }

  # Add rank column
  results$rank <- seq_len(nrow(results))

  # Reorder columns: rank first
  col_order <- c("rank", "items", "n_items", "auc", "cutoff",
                 "sensitivity", "specificity", "youden", "accuracy",
                 "ppv", "npv", "n_positive", "n_negative")
  results <- results[, col_order, drop = FALSE]

  rownames(results) <- NULL
  results
}
