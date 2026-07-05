# roc_helpers.R — Fast ROC computation from score frequency tables
#
# These functions compute ROC metrics directly from score-vs-outcome frequency
# tables, avoiding per-model calls to pROC::roc() for speed.
#
# Core assumptions:
#   - Higher score = more likely positive
#   - predicted_positive = score >= cutoff
#   - AUC = P(pos > neg) + 0.5 * P(pos == neg)

#' Compute score frequencies by outcome class
#'
#' @param scores Numeric vector of sum scores.
#' @param outcome Integer vector of 0 (negative) or 1 (positive).
#'
#' @return A list with elements `pos_counts` and `neg_counts`, each a named
#'   integer vector indexed by score value.
#' @keywords internal
compute_score_frequencies <- function(scores, outcome) {
  if (length(scores) != length(outcome)) {
    stop("`scores` and `outcome` must have the same length.", call. = FALSE)
  }

  pos_scores <- scores[outcome == 1L]
  neg_scores <- scores[outcome == 0L]

  all_scores <- sort(unique(scores))

  pos_counts <- setNames(
    vapply(all_scores, function(s) sum(pos_scores == s), integer(1)),
    as.character(all_scores)
  )
  neg_counts <- setNames(
    vapply(all_scores, function(s) sum(neg_scores == s), integer(1)),
    as.character(all_scores)
  )

  list(pos_counts = pos_counts, neg_counts = neg_counts)
}

#' Compute AUC from score frequency tables
#'
#' AUC = P(score_pos > score_neg) + 0.5 * P(score_pos == score_neg)
#'
#' @param pos_counts Named integer vector of score frequencies for positives.
#' @param neg_counts Named integer vector of score frequencies for negatives.
#'
#' @return Numeric AUC value. NA if all positives or all negatives.
#' @keywords internal
compute_auc_from_table <- function(pos_counts, neg_counts) {
  total_pos <- sum(pos_counts)
  total_neg <- sum(neg_counts)

  if (total_pos == 0 || total_neg == 0) {
    return(NA_real_)
  }

  scores <- as.numeric(names(pos_counts))
  n_scores <- length(scores)

  # Use unname to prevent name propagation through arithmetic
  pcounts <- unname(pos_counts)
  ncounts <- unname(neg_counts)

  auc_sum <- 0.0

  for (i in seq_len(n_scores)) {
    sp <- scores[i]
    for (j in seq_len(n_scores)) {
      sn <- scores[j]
      pair_count <- pcounts[i] * ncounts[j]

      if (sp > sn) {
        auc_sum <- auc_sum + pair_count
      } else if (sp == sn) {
        auc_sum <- auc_sum + 0.5 * pair_count
      }
      # sp < sn contributes 0
    }
  }

  auc_sum / (total_pos * total_neg)
}

#' Compute full ROC metrics from score frequency tables
#'
#' For every unique score treated as a cutoff (score >= cutoff = positive),
#' compute sensitivity, specificity, Youden index, accuracy, PPV, and NPV.
#'
#' @param pos_counts Named integer vector of score frequencies for positives.
#' @param neg_counts Named integer vector of score frequencies for negatives.
#'
#' @return A data.frame with columns: cutoff, tp, fp, fn, tn, sensitivity,
#'   specificity, youden, accuracy, ppv, npv.
#' @keywords internal
compute_roc_metrics_from_table <- function(pos_counts, neg_counts) {
  total_pos <- sum(pos_counts)
  total_neg <- sum(neg_counts)
  total_n <- total_pos + total_neg

  scores <- as.numeric(names(pos_counts))
  n_scores <- length(scores)

  # Work from high to low, accumulating TP and FP
  ord <- order(scores, decreasing = TRUE)
  scores_ordered <- scores[ord]
  pos_ordered <- unname(pos_counts[ord])
  neg_ordered <- unname(neg_counts[ord])

  cum_pos <- cumsum(pos_ordered)
  cum_neg <- cumsum(neg_ordered)

  # At cutoff c: TP = cum_pos up to and including scores >= c
  tp <- cum_pos
  fp <- cum_neg
  fn <- total_pos - tp
  tn <- total_neg - fp

  sensitivity <- tp / total_pos
  specificity <- tn / total_neg
  youden <- sensitivity + specificity - 1
  accuracy <- (tp + tn) / total_n

  # PPV = TP / (TP + FP), guard against division by zero
  ppv <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)

  # NPV = TN / (TN + FN), guard against division by zero
  npv <- ifelse(tn + fn > 0, tn / (tn + fn), NA_real_)

  data.frame(
    cutoff      = scores_ordered,
    tp          = tp,
    fp          = fp,
    fn          = fn,
    tn          = tn,
    sensitivity = sensitivity,
    specificity = specificity,
    youden      = youden,
    accuracy    = accuracy,
    ppv         = ppv,
    npv         = npv,
    stringsAsFactors = FALSE
  )
}

#' Find the optimal cutoff using a specified method
#'
#' @param metrics A data.frame from `compute_roc_metrics_from_table()`.
#' @param method Character, one of `"youden"` or `"closest_topleft"`.
#'
#' @return A single-row data.frame (from `metrics`) for the optimal cutoff.
#' @keywords internal
find_optimal_cutoff <- function(metrics, method = c("youden", "closest_topleft")) {
  method <- match.arg(method)

  if (method == "youden") {
    # Max Youden index; tie-break by higher sensitivity, then higher specificity
    idx <- order(
      -metrics$youden,
      -metrics$sensitivity,
      -metrics$specificity,
      metrics$cutoff
    )
    return(metrics[idx[1], , drop = FALSE])
  }

  if (method == "closest_topleft") {
    # Min Euclidean distance to (0, 1) = sqrt((1 - sens)^2 + (1 - spec)^2)
    dist <- sqrt((1 - metrics$sensitivity)^2 + (1 - metrics$specificity)^2)
    idx <- order(dist, -metrics$youden)
    return(metrics[idx[1], , drop = FALSE])
  }

  stop("Unknown cutoff_method: '", method, "'.", call. = FALSE)
}
