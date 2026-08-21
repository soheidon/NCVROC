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
#' @param ci Logical. If `TRUE`, compute confidence intervals for AUC (DeLong)
#'   and classification metrics (Clopper-Pearson exact binomial) for the top
#'   models. Default `FALSE`.
#' @param conf_level Numeric confidence level in (0, 1), default 0.95.
#' @param engine Character, computation engine. `"R"` (default) or `"Rcpp"`.
#' @param progress Logical, show progress bar? Default `TRUE`.
#' @param chunk_start Internal: zero-based global combination index to start
#'   from. When set together with `chunk_size`, only that range is evaluated.
#' @param chunk_size Internal: number of combinations to evaluate in this chunk.
#'
#' @details
#' When `ci = TRUE`, confidence intervals are calculated after ranking and
#' `top_n` truncation to maintain high combinatorial search speed:
#' \itemize{
#'   \item \strong{AUC CI}: Non-parametric asymptotic normal approximation using
#'     the method of DeLong et al. (1988), computed efficiently from score frequency
#'     distributions in O(K) time.
#'   \item \strong{Sensitivity, Specificity, Accuracy, PPV, NPV CI}: Exact binomial
#'     confidence intervals using the Clopper-Pearson method (Clopper & Pearson, 1934)
#'     via Beta distribution quantiles (\code{\link[stats]{qbeta}}).
#' }
#'
#' \strong{Important note on CI interpretation:} Confidence intervals for
#' performance metrics evaluated on the full dataset quantify sampling uncertainty
#' for the candidate model on that specific data. They do not account for uncertainty
#' introduced by model or cutoff selection and should not be interpreted as
#' cross-validated confidence intervals. Use \code{\link{nested_sum_roc}} for
#' cross-validated performance estimation.
#'
#' @references
#' DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas
#' under two or more correlated receiver operating characteristic curves: a
#' nonparametric approach. \emph{Biometrics}, 44(3), 837--845.
#' \doi{10.2307/2531595}
#'
#' Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. \emph{Biometrika}, 26(4),
#' 404--413. \doi{10.1093/biomet/26.4.404}
#'
#' @return A data.frame with columns: `rank`, `items` (comma-separated string),
#'   `n_items`, `auc`, `cutoff`, `sensitivity`, `specificity`, `youden`,
#'   `accuracy`, `ppv`, `npv`, `n_positive`, `n_negative`.
#'   If `ci = TRUE`, includes:
#'   \itemize{
#'     \item \code{auc_lower}, \code{auc_upper}: DeLong confidence limits for AUC.
#'     \item \code{sensitivity_lower}, \code{sensitivity_upper}: Clopper-Pearson exact limits for sensitivity.
#'     \item \code{specificity_lower}, \code{specificity_upper}: Clopper-Pearson exact limits for specificity.
#'     \item \code{accuracy_lower}, \code{accuracy_upper}: Clopper-Pearson exact limits for accuracy.
#'     \item \code{ppv_lower}, \code{ppv_upper}: Clopper-Pearson exact limits for positive predictive value.
#'     \item \code{npv_lower}, \code{npv_upper}: Clopper-Pearson exact limits for negative predictive value.
#'   }
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
                               ci = FALSE,
                               conf_level = 0.95,
                               engine = c("R", "Rcpp"),
                               progress = TRUE,
                               chunk_start = NULL,
                               chunk_size = NULL) {

  # ---- Argument validation ----
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)
  engine <- match.arg(engine)

  if (!is.logical(ci) || length(ci) != 1L || is.na(ci)) {
    stop("`ci` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single numeric value in (0, 1).", call. = FALSE)
  }

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
  n_items <- length(items)

  n_total <- length(y)
  n_pos   <- sum(y == 1L)
  n_neg   <- sum(y == 0L)

  # ---- Determine if we are in chunked mode ----
  is_chunked <- !is.null(chunk_start) && !is.null(chunk_size)

  if (is_chunked) {
    # --- Chunked evaluation path ---
    if (!is.numeric(chunk_start) || length(chunk_start) != 1 || chunk_start < 0) {
      stop("`chunk_start` must be a non-negative number.", call. = FALSE)
    }
    if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size <= 0 ||
        chunk_size != floor(chunk_size)) {
      stop("`chunk_size` must be a positive integer.", call. = FALSE)
    }
    chunk_size <- as.integer(chunk_size)
    chunk_start <- as.double(chunk_start)

    if (engine == "Rcpp") {
      x_mat <- as.matrix(x[, items, drop = FALSE])
      results <- evaluate_combos_cpp_chunk(
        x_mat, y,
        min_items = min_items,
        max_items = max_items,
        cutoff_method = cutoff_method,
        chunk_start = chunk_start,
        chunk_size = chunk_size
      )
      # Resolve column indices to item-name strings
      total_combos <- .count_total_combos(n_items, min_items, max_items)
      chunk_end <- min(chunk_start + chunk_size, total_combos)
      n_this_chunk <- as.integer(chunk_end - chunk_start)
      items_vec <- character(n_this_chunk)
      for (gi in seq_len(n_this_chunk)) {
        global_rank <- chunk_start + (gi - 1L)
        resolved <- .resolve_global_combination_rank(n_items, min_items, max_items, global_rank)
        idx <- .combination_unrank(n_items, resolved$k, resolved$rank_within_k)
        items_vec[gi] <- format_items(items[idx + 1L])
      }
      results$items <- items_vec

    } else {
      # R engine chunked: enumerate combos via combinadic, evaluate each
      combo_chunk <- .enumerate_combinations_chunk(
        items, min_items, max_items, chunk_start, chunk_size
      )
      n_this_chunk <- length(combo_chunk)

      results <- vector("list", n_this_chunk)
      for (i in seq_len(n_this_chunk)) {
        combo_items <- combo_chunk[[i]]
        k <- length(combo_items)
        scores <- rowSums(x[, combo_items, drop = FALSE])
        freq <- compute_score_frequencies(scores, y)
        auc_val <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
        metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
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
      }
      results <- do.call(rbind, results)
    }

  } else {
    # --- Full (non-chunked) evaluation path ---
    combos <- enumerate_combinations(items, min_items = min_items, max_items = max_items)
    n_combos <- length(combos)

    if (engine == "Rcpp") {
      x_mat <- as.matrix(x[, items, drop = FALSE])
      combo_indices <- lapply(combos, function(v) match(v, items) - 1L)
      results <- evaluate_combos_cpp(x_mat, y, combo_indices, cutoff_method)
      results$items <- sapply(combos, format_items)
    } else {
      if (progress) {
        pb <- utils::txtProgressBar(min = 0, max = n_combos, style = 3)
        on.exit(close(pb), add = TRUE)
      }

      results <- vector("list", n_combos)
      for (i in seq_len(n_combos)) {
        combo_items <- combos[[i]]
        k <- length(combo_items)
        scores <- rowSums(x[, combo_items, drop = FALSE])
        freq <- compute_score_frequencies(scores, y)
        auc_val <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
        metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
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
  }

  # ---- Sort ----
  sort_col <- results[[rank_by]]
  if (prefer_fewer_items) {
    ord <- order(-sort_col, results$n_items)
  } else {
    ord <- order(-sort_col)
  }
  results <- results[ord, , drop = FALSE]

  # Don't truncate in chunked mode — caller slices
  if (!is_chunked && !is.null(top_n)) {
    results <- utils::head(results, top_n)
  }

  results$rank <- seq_len(nrow(results))

  # ---- CI computation on top results ----
  if (ci) {
    results <- add_performance_cis(
      results,
      data = x,
      outcome = validated$outcome_col,
      conf_level = conf_level
    )
    col_order <- c(
      "rank", "items", "n_items",
      "auc", "auc_lower", "auc_upper",
      "cutoff",
      "sensitivity", "sensitivity_lower", "sensitivity_upper",
      "specificity", "specificity_lower", "specificity_upper",
      "youden",
      "accuracy", "accuracy_lower", "accuracy_upper",
      "ppv", "ppv_lower", "ppv_upper",
      "npv", "npv_lower", "npv_upper",
      "n_positive", "n_negative"
    )
  } else {
    col_order <- c(
      "rank", "items", "n_items", "auc", "cutoff",
      "sensitivity", "specificity", "youden", "accuracy",
      "ppv", "npv", "n_positive", "n_negative"
    )
  }
  results <- results[, col_order, drop = FALSE]

  rownames(results) <- NULL
  results
}
