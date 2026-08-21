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

#' Compute Clopper-Pearson exact binomial confidence intervals
#'
#' Computes exact confidence intervals for binomial proportions using Beta quantiles.
#'
#' @param k Integer vector of number of successes.
#' @param n Integer vector of number of trials.
#' @param conf_level Numeric confidence level in (0, 1), default 0.95.
#'
#' @return A data.frame with columns `lower` and `upper`.
#' @keywords internal
compute_clopper_pearson_ci <- function(k, n, conf_level = 0.95) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single numeric value in (0, 1).", call. = FALSE)
  }

  alpha <- 1 - conf_level
  len <- max(length(k), length(n))
  k <- rep_len(k, len)
  n <- rep_len(n, len)

  lower <- rep(NA_real_, len)
  upper <- rep(NA_real_, len)

  valid <- !is.na(k) & !is.na(n) & (n > 0) & (k >= 0) & (k <= n)

  for (i in which(valid)) {
    ki <- k[i]
    ni <- n[i]
    if (ki == 0) {
      lower[i] <- 0.0
      upper[i] <- stats::qbeta(1 - alpha / 2, 1, ni)
    } else if (ki == ni) {
      lower[i] <- stats::qbeta(alpha / 2, ni, 1)
      upper[i] <- 1.0
    } else {
      lower[i] <- stats::qbeta(alpha / 2, ki, ni - ki + 1)
      upper[i] <- stats::qbeta(1 - alpha / 2, ki + 1, ni - ki)
    }
  }

  data.frame(lower = lower, upper = upper, stringsAsFactors = FALSE)
}

#' Compute DeLong confidence interval for empirical ROC AUC
#'
#' Computes the asymptotic variance and normal-approximation confidence interval
#' for empirical ROC AUC using the non-parametric method of DeLong et al. (1988).
#'
#' @param pos_counts Named integer vector of score frequencies for positives.
#' @param neg_counts Named integer vector of score frequencies for negatives.
#' @param conf_level Numeric confidence level in (0, 1), default 0.95.
#'
#' @return A list with elements `auc`, `auc_lower`, `auc_upper`, `se`, `var`.
#' @keywords internal
compute_delong_auc_ci <- function(pos_counts, neg_counts, conf_level = 0.95) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single numeric value in (0, 1).", call. = FALSE)
  }

  total_pos <- sum(pos_counts)
  total_neg <- sum(neg_counts)

  if (total_pos == 0 || total_neg == 0) {
    return(list(
      auc       = NA_real_,
      auc_lower = NA_real_,
      auc_upper = NA_real_,
      se        = NA_real_,
      var       = NA_real_
    ))
  }

  all_score_nums <- sort(unique(as.numeric(c(names(pos_counts), names(neg_counts)))))
  n_scores <- length(all_score_nums)

  pcounts <- integer(n_scores)
  ncounts <- integer(n_scores)
  names(pcounts) <- as.character(all_score_nums)
  names(ncounts) <- as.character(all_score_nums)

  pcounts[names(pos_counts)] <- unname(pos_counts)
  ncounts[names(neg_counts)] <- unname(neg_counts)

  # 1. Compute AUC and placement values V10 (for pos) and V01 (for neg)
  # For score s:
  # V10(s) = (sum_{sn < s} neg_counts(sn) + 0.5 * neg_counts(s)) / total_neg
  # V01(s) = (sum_{sp > s} pos_counts(sp) + 0.5 * pos_counts(s)) / total_pos

  # Already sorted ascending by all_score_nums
  scores_asc <- all_score_nums
  p_asc <- pcounts
  n_asc <- ncounts

  cum_n_before <- c(0, cumsum(n_asc)[-n_scores])
  v10_asc <- (cum_n_before + 0.5 * n_asc) / total_neg

  cum_p_rev <- c(0, cumsum(rev(p_asc))[-n_scores])
  cum_p_after <- rev(cum_p_rev)
  v01_asc <- (cum_p_after + 0.5 * p_asc) / total_pos

  auc_val <- sum(p_asc * v10_asc) / total_pos

  if (total_pos < 2 || total_neg < 2) {
    return(list(
      auc       = auc_val,
      auc_lower = NA_real_,
      auc_upper = NA_real_,
      se        = NA_real_,
      var       = NA_real_
    ))
  }

  # Sample variances of placement values
  s10 <- sum(p_asc * (v10_asc - auc_val)^2) / (total_pos - 1)
  s01 <- sum(n_asc * (v01_asc - auc_val)^2) / (total_neg - 1)

  var_auc <- (s10 / total_pos) + (s01 / total_neg)
  se_auc  <- sqrt(max(0, var_auc))

  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)

  auc_lower <- max(0, auc_val - z * se_auc)
  auc_upper <- min(1, auc_val + z * se_auc)

  list(
    auc       = auc_val,
    auc_lower = auc_lower,
    auc_upper = auc_upper,
    se        = se_auc,
    var       = var_auc
  )
}

#' Add performance confidence intervals to a data.frame of ROC results
#'
#' @param results data.frame with performance columns (sensitivity, specificity, etc.)
#' @param data data.frame with item columns and outcome (or NULL if scores supplied).
#' @param outcome Character string naming binary outcome column.
#' @param conf_level Numeric confidence level in (0, 1), default 0.95.
#'
#' @return data.frame with CI columns added.
#' @keywords internal
add_performance_cis <- function(results, data = NULL, outcome = NULL, conf_level = 0.95) {
  if (nrow(results) == 0) return(results)

  n_pos <- results$n_positive
  n_neg <- results$n_negative
  n_total <- n_pos + n_neg

  # Sensitivity CI
  tp <- if ("tp" %in% names(results)) results$tp else round(results$sensitivity * n_pos)
  sens_ci <- compute_clopper_pearson_ci(tp, n_pos, conf_level = conf_level)

  # Specificity CI
  tn <- if ("tn" %in% names(results)) results$tn else round(results$specificity * n_neg)
  spec_ci <- compute_clopper_pearson_ci(tn, n_neg, conf_level = conf_level)

  # Accuracy CI
  acc_ci <- compute_clopper_pearson_ci(tp + tn, n_total, conf_level = conf_level)

  # PPV CI
  fp <- if ("fp" %in% names(results)) results$fp else (n_neg - tn)
  ppv_ci <- compute_clopper_pearson_ci(tp, tp + fp, conf_level = conf_level)

  # NPV CI
  fn <- if ("fn" %in% names(results)) results$fn else (n_pos - tp)
  npv_ci <- compute_clopper_pearson_ci(tn, tn + fn, conf_level = conf_level)

  # AUC CI (DeLong)
  auc_lower <- rep(NA_real_, nrow(results))
  auc_upper <- rep(NA_real_, nrow(results))

  if (!is.null(data) && !is.null(outcome) && "items" %in% names(results)) {
    y <- data[[outcome]]
    for (i in seq_len(nrow(results))) {
      items_vec <- .parse_itemset(results$items[i])
      scores <- rowSums(data[, items_vec, drop = FALSE])
      freq <- compute_score_frequencies(scores, y)
      d_ci <- compute_delong_auc_ci(freq$pos_counts, freq$neg_counts, conf_level = conf_level)
      auc_lower[i] <- d_ci$auc_lower
      auc_upper[i] <- d_ci$auc_upper
    }
  }

  results$auc_lower         <- auc_lower
  results$auc_upper         <- auc_upper
  results$sensitivity_lower <- sens_ci$lower
  results$sensitivity_upper <- sens_ci$upper
  results$specificity_lower <- spec_ci$lower
  results$specificity_upper <- spec_ci$upper
  results$accuracy_lower    <- acc_ci$lower
  results$accuracy_upper    <- acc_ci$upper
  results$ppv_lower         <- ppv_ci$lower
  results$ppv_upper         <- ppv_ci$upper
  results$npv_lower         <- npv_ci$lower
  results$npv_upper         <- npv_ci$upper

  results
}
