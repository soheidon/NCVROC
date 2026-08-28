# candidate_stability.R — Candidate-level stability and optimism analysis
#
# Evaluates performance stability, sample dependence, rank stability,
# selection frequency, and clinical constraint feasibility for specific
# candidate item-set unweighted sum-score models across data perturbations
# (Repeated K-Fold Cross-Validation and Non-Parametric Bootstrap Resampling).
#
# Supports:
#   Mode 1: Fixed candidate sets (candidate_sets = list(...))
#   Mode 2: Combinatorial screening (items = ..., model_sizes = 1:5, screen_top_n = 200)

# Routing counters for test verification
.ncvroc_candidate_stability_routing_counters <- new.env(parent = emptyenv())
.ncvroc_candidate_stability_routing_counters$serial <- 0L
.ncvroc_candidate_stability_routing_counters$threads <- 0L
.ncvroc_candidate_stability_routing_counters$chunks <- 0L

.get_candidate_stability_routing_counters <- function() {
  list(
    serial  = .ncvroc_candidate_stability_routing_counters$serial,
    threads = .ncvroc_candidate_stability_routing_counters$threads,
    chunks  = .ncvroc_candidate_stability_routing_counters$chunks
  )
}

.reset_candidate_stability_routing_counters <- function() {
  .ncvroc_candidate_stability_routing_counters$serial <- 0L
  .ncvroc_candidate_stability_routing_counters$threads <- 0L
  .ncvroc_candidate_stability_routing_counters$chunks <- 0L
  invisible(NULL)
}

#' Candidate-Level Stability and Optimism Analysis for ROC Sum Scales
#'
#' Evaluates the performance stability, apparent-to-resampled performance gap,
#' bootstrap optimism correction, rank stability, selection frequency, and
#' clinical constraint feasibility for candidate item-combination sum-score models.
#'
#' Supports two operational modes:
#' \itemize{
#'   \item \strong{Mode 1 (Fixed Candidates)}: Evaluates a user-specified named list
#'     of candidate item sets via \code{candidate_sets}.
#'   \item \strong{Mode 2 (Combinatorial Screening)}: Performs an exact Stage-1 apparent
#'     performance screening across full combinatorial model sizes (\code{items}, \code{model_sizes})
#'     and audits the top \code{screen_top_n} candidates under resampling in Stage 2.
#' }
#'
#' @details
#' \strong{Purpose and Distinction from Nested Cross-Validation}:
#' \code{\link{cross_size_nested_cv}} evaluates the generalization performance of the
#' \emph{model-selection procedure itself} across held-out outer folds. In contrast,
#' \code{candidate_stability_roc} audits the stability and sample-dependence of
#' \emph{individual candidate models} across data perturbations. It does not replace
#' nested cross-validation when evaluating an end-to-end model selection pipeline.
#'
#' \strong{Operational Modes}:
#' \itemize{
#'   \item \strong{Mode 1 (Fixed Candidates)}: Evaluates a user-supplied named list
#'     of candidate item combinations (\code{candidate_sets}). Useful for targeted
#'     head-to-head comparisons of specific pre-selected scales.
#'   \item \strong{Mode 2 (Combinatorial Screening)}: Performs an exact Stage-1 apparent
#'     performance screening across all candidate combinations of sizes specified in \code{model_sizes}.
#'     The top \code{screen_top_n} candidates are retained and audited in Stage 2.
#' }
#'
#' \strong{Screening Conditionality (Mode 2)}:
#' In Mode 2, stability summaries, rank distributions, and selection frequencies are
#' conditional on the Stage-1 screened candidate set and do not account for uncertainty
#' in the screening step itself. Stage-1 screening ranks candidates strictly by apparent
#' \code{rank_by} performance (without applying clinical constraints) so that clinical
#' feasibility can be audited unbiasedly under resampling in Stage 2.
#'
#' \strong{Resampling Schemes and Evaluation Semantics}:
#' \itemize{
#'   \item \strong{Repeated K-Fold CV (\code{resampling = "repeated_cv"})}: In each repeat,
#'     $K$ stratified training/test splits are evaluated. Cutoffs are determined on training
#'     folds and applied to held-out test folds. Repeat-level metrics are computed across all
#'     $N$ held-out test predictions and summarized across repeats.
#'   \item \strong{Bootstrap Original (\code{bootstrap_test = "original"})}: Non-parametric bootstrap
#'     samples ($N$ drawn with replacement) serve as training sets. Cutoffs derived on the bootstrap
#'     sample are evaluated on both the bootstrap sample (training) and the original full dataset (test)
#'     to estimate Efron-style optimism \eqn{O_b(M) = M_{\text{train}, b} - M_{\text{test}, b}}.
#'   \item \strong{Bootstrap OOB (\code{bootstrap_test = "oob"})}: Cutoffs derived on bootstrap training
#'     samples are evaluated on out-of-bag (unselected) observations. Replicates where OOB samples contain
#'     only one class yield undefined OOB AUC and are tracked as invalid evaluations.
#' }
#'
#' \strong{AUC Mathematical Identities for Unweighted Sum Scores}:
#' For a fixed unweighted sum score (\eqn{\sum X_j}), the raw score vector contains no fitted
#' coefficients. Consequently:
#' \itemize{
#'   \item Under \code{resampling = "repeated_cv"}, the pooled out-of-fold score vector for each
#'     repeat is identical to the full-data score vector, yielding \eqn{\text{AUC}_1 = \dots = \text{AUC}_R = \text{Apparent AUC}}
#'     and \eqn{\text{SD}(\text{AUC}) = 0}.
#'   \item Under \code{resampling = "bootstrap"} with \code{bootstrap_test = "original"}, evaluating
#'     the fixed score on the original full dataset yields \eqn{\text{Test AUC}_b \equiv \text{Apparent AUC}}
#'     for every replicate.
#'   \item AUC variation occurs in bootstrap \emph{training} samples and \emph{OOB} test sets.
#' }
#' Zero variance in repeated-CV AUC and original-test bootstrap AUC is a mathematical identity
#' of fixed unweighted sum scores rather than empirical evidence of model robustness.
#'
#' \strong{Metric Denominator and Validity Rules}:
#' In bootstrap OOB evaluation, test metrics are marked \code{NA} if their denominator is 0:
#' \itemize{
#'   \item Sensitivity requires at least one positive test case (\eqn{n_{\text{pos}} > 0}).
#'   \item Specificity requires at least one negative test case (\eqn{n_{\text{neg}} > 0}).
#'   \item PPV requires at least one predicted positive case (\eqn{\text{TP} + \text{FP} > 0}).
#'   \item NPV requires at least one predicted negative case (\eqn{\text{TN} + \text{FN} > 0}).
#'   \item AUC requires both classes to be present in the test set (\eqn{n_{\text{pos}} > 0} and \eqn{n_{\text{neg}} > 0}).
#' }
#' Summary statistics for each metric are calculated strictly over valid, defined replicates.
#'
#' \strong{Rank and Selection Frequency Partition}:
#' In each resample replicate, candidates satisfying feasibility constraints are ranked by test-side
#' \code{rank_by} performance (with ties broken by \code{prefer_fewer_items} and global combination index).
#' If no candidates satisfy constraints, the replicate is recorded as \code{"no_feasible_candidate"}.
#' If the evaluation of the primary rank metric is invalid (e.g. OOB one-class sample), it is recorded
#' as \code{"invalid_evaluation"}. Selection frequencies strictly partition the total resamples:
#' \deqn{\sum \text{selection\_frequency\_all} + \text{no\_feasible\_frequency} + \text{invalid\_evaluation\_frequency} = 1.0}
#'
#' \strong{Candidate Optimism vs Selection Optimism}:
#' The candidate-level performance gap reported here (\eqn{\text{Apparent} - \text{Resampled}}) measures
#' the apparent-performance optimism of a \emph{single fixed candidate}. This is distinct from the
#' model-selection optimism quantified by \code{\link{compare_cv_selection}}, which measures the optimism
#' of the data-driven \emph{selection procedure} across multiple candidates.
#'
#' \strong{Parallel Execution}:
#' Supports \code{parallel = "none"} (serial), \code{parallel = "threads"} (multithreaded execution via
#' \code{RcppParallel} without inter-process communication overhead), and \code{parallel = "chunks"}
#' (PSOCK multi-process parallelism).
#'
#' @param data A \code{data.frame} containing candidate items and outcome.
#' @param outcome Unquoted column name or string specifying the binary outcome variable.
#' @param candidate_sets A named \code{list} of character vectors for Mode 1, where each vector
#'   specifies the predictor column names for a candidate item set (e.g.,
#'   \code{list("Model A" = c("Q1", "Q2"), "Model B" = c("Q1", "Q3", "Q4"))}).
#'   Mutually exclusive with \code{items}.
#' @param items Column specification for Mode 2 (unquoted expression or character vector)
#'   defining the item pool to screen combinations from. Mutually exclusive with \code{candidate_sets}.
#' @param model_sizes Integer vector of model sizes to evaluate for Mode 2 (e.g. \code{1:5} or \code{c(1, 3, 5)}).
#'   Default \code{1:5}.
#' @param screen_top_n Positive integer scalar for Mode 2 specifying the maximum number of top candidates
#'   retained from Stage-1 apparent screening for Stage-2 resampling evaluation. Default \code{200L}.
#' @param resampling Character string specifying the resampling scheme: \code{"repeated_cv"}
#'   (default) or \code{"bootstrap"}.
#' @param folds Integer scalar, number of cross-validation folds (\eqn{2 \le K < N}) for
#'   \code{resampling = "repeated_cv"}. Default \code{5L}.
#' @param repeats Integer scalar, number of independent CV repeats for
#'   \code{resampling = "repeated_cv"}. Default \code{20L}.
#' @param bootstrap_reps Integer scalar, number of bootstrap resamples for
#'   \code{resampling = "bootstrap"}. Default \code{500L}.
#' @param bootstrap_test Test evaluation set for bootstrap resampling: \code{"original"}
#'   (default, evaluates training-derived cutoff on original full data for optimism estimation)
#'   or \code{"oob"} (evaluates cutoff on out-of-bag unselected subjects).
#' @param cutoff_method Method for optimal cutoff determination: \code{"youden"} (default)
#'   or \code{"closest_topleft"}. Cutoffs are determined on training data only and applied
#'   to test observations.
#' @param sensitivity_min Optional minimum test-side sensitivity threshold in \eqn{[0, 1]}.
#' @param specificity_min Optional minimum test-side specificity threshold in \eqn{[0, 1]}.
#' @param rank_by Primary metric for ranking candidates in Stage 1 screening and in each resample replicate:
#'   \code{"youden"} (default), \code{"auc"}, \code{"sensitivity"}, \code{"specificity"},
#'   or \code{"accuracy"}. Resampling ranking always uses test-side metrics.
#' @param prefer_fewer_items Logical; if \code{TRUE} (default), ties in the primary metric
#'   are broken in favor of models with fewer items.
#' @param positive_label Value in \code{outcome} representing positive cases (default \code{1}).
#' @param negative_label Value in \code{outcome} representing negative cases (default \code{0}).
#' @param parallel Parallel execution mode: \code{"none"} (default, serial), \code{"threads"}
#'   (single-process RcppParallel multithreading), or \code{"chunks"} (PSOCK multi-process parallelism).
#' @param n_workers Optional integer scalar, number of worker processes for \code{parallel = "chunks"}
#'   or thread count limit for \code{parallel = "threads"}.
#' @param seed Optional integer random seed for reproducible fold or bootstrap generation.
#' @param engine Calculation engine for Stage 1: \code{"Rcpp"} (default) or \code{"R"}.
#' @param progress Logical; if \code{TRUE}, shows progress messages. Default \code{interactive()}.
#'
#' @return An S3 object of class \code{"candidate_stability_result"} containing:
#'   \item{candidate_summary}{A \code{data.frame} summarizing apparent performance,
#'     resampled test performance (mean, SD, median, IQR, min, max), apparent-minus-resampled
#'     gaps, optimism and optimism-corrected metrics (for bootstrap original mode),
#'     rank stability, selection frequencies, constraint feasibility pass rates,
#'     and valid replicate counts for each candidate.}
#'   \item{resample_results}{A \code{data.frame} with candidate-by-replicate detailed metrics.}
#'   \item{replicate_metadata}{A \code{data.frame} with replicate-level selection status, sample sizes, and winner IDs.}
#'   \item{rank_stability}{A \code{data.frame} of candidate rank distribution summaries.}
#'   \item{selection_frequency}{A \code{data.frame} of selection counts and partitioned frequencies.}
#'   \item{constraint_stability}{A \code{data.frame} of clinical constraint feasibility pass rates.}
#'   \item{apparent_performance}{A \code{data.frame} of full-data apparent metrics.}
#'   \item{candidate_definitions}{A named list of the validated candidate item sets.}
#'   \item{settings}{A list of run configuration parameters and metadata.}
#'
#' @seealso \code{\link{cross_size_cv}}, \code{\link{cross_size_nested_cv}}, \code{\link{compare_cv_selection}}
#' @export
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   y  = sample(0:1, 40, replace = TRUE),
#'   Q1 = sample(0:2, 40, replace = TRUE),
#'   Q2 = sample(0:2, 40, replace = TRUE),
#'   Q3 = sample(0:2, 40, replace = TRUE),
#'   Q4 = sample(0:2, 40, replace = TRUE)
#' )
#'
#' # Mode 1: Fixed candidate sets
#' cand_list <- list(
#'   "M1" = "Q1",
#'   "M2" = c("Q1", "Q2"),
#'   "M3" = c("Q1", "Q3", "Q4")
#' )
#' res_cv <- candidate_stability_roc(
#'   data           = dat,
#'   outcome        = y,
#'   candidate_sets = cand_list,
#'   resampling     = "repeated_cv",
#'   folds          = 3,
#'   repeats        = 2,
#'   seed           = 42
#' )
#' print(res_cv)
#'
#' # Mode 2: Combinatorial screening
#' res_screen <- candidate_stability_roc(
#'   data           = dat,
#'   outcome        = y,
#'   items          = Q1:Q4,
#'   model_sizes    = 1:2,
#'   screen_top_n   = 5L,
#'   resampling     = "repeated_cv",
#'   folds          = 3,
#'   repeats        = 2,
#'   seed           = 42
#' )
#' print(res_screen)
candidate_stability_roc <- function(data,
                                    outcome,
                                    candidate_sets      = NULL,
                                    items               = NULL,
                                    model_sizes         = 1:5,
                                    screen_top_n        = 200L,
                                    resampling          = c("repeated_cv", "bootstrap"),
                                    folds               = 5L,
                                    repeats             = 20L,
                                    bootstrap_reps      = 500L,
                                    bootstrap_test      = c("original", "oob"),
                                    cutoff_method       = c("youden", "closest_topleft"),
                                    sensitivity_min     = NULL,
                                    specificity_min     = NULL,
                                    rank_by             = c("youden", "auc", "sensitivity", "specificity", "accuracy"),
                                    prefer_fewer_items  = TRUE,
                                    positive_label      = 1,
                                    negative_label      = 0,
                                    parallel            = c("none", "threads", "chunks"),
                                    n_workers           = NULL,
                                    seed                = NULL,
                                    engine              = c("Rcpp", "R"),
                                    progress            = interactive()) {
  call_matched <- match.call()
  env <- parent.frame()

  # 1. Resolve outcome
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  if (!outcome_name %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in `data`.", outcome_name), call. = FALSE)
  }

  y_raw <- data[[outcome_name]]
  if (any(is.na(y_raw))) {
    stop("`outcome` contains missing (NA) values. Remove or impute missing values first.", call. = FALSE)
  }

  unique_y <- unique(y_raw)
  if (!all(unique_y %in% c(positive_label, negative_label))) {
    stop(sprintf("`outcome` contains values other than positive_label (%s) and negative_label (%s).",
                 positive_label, negative_label), call. = FALSE)
  }
  if (length(unique(unique_y)) < 2L) {
    stop("`outcome` must contain both positive and negative cases.", call. = FALSE)
  }

  y <- ifelse(y_raw == positive_label, 1L, 0L)
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  n_obs <- length(y)

  if (n_pos < 2L || n_neg < 2L) {
    stop("At least 2 positive and 2 negative cases are required for candidate stability evaluation.", call. = FALSE)
  }

  # 2. Resolve arguments
  resampling <- match.arg(resampling)
  bootstrap_test <- match.arg(bootstrap_test)
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)
  parallel <- match.arg(parallel)
  engine <- match.arg(engine)

  if (!is.null(sensitivity_min)) {
    if (!is.numeric(sensitivity_min) || length(sensitivity_min) != 1L ||
        is.na(sensitivity_min) || !is.finite(sensitivity_min) || sensitivity_min < 0 || sensitivity_min > 1) {
      stop("`sensitivity_min` must be a numeric scalar in [0, 1] or NULL.", call. = FALSE)
    }
  }
  if (!is.null(specificity_min)) {
    if (!is.numeric(specificity_min) || length(specificity_min) != 1L ||
        is.na(specificity_min) || !is.finite(specificity_min) || specificity_min < 0 || specificity_min > 1) {
      stop("`specificity_min` must be a numeric scalar in [0, 1] or NULL.", call. = FALSE)
    }
  }

  if (!is.null(n_workers)) {
    if (!is.numeric(n_workers) || length(n_workers) != 1L ||
        is.na(n_workers) || !is.finite(n_workers) || n_workers != as.integer(n_workers) || n_workers < 1L) {
      stop("`n_workers` must be a positive integer scalar or NULL.", call. = FALSE)
    }
    n_workers <- as.integer(n_workers)
  }

  # 3. Input Mode Resolution (Mode 1: candidate_sets vs Mode 2: items + model_sizes)
  items_expr <- substitute(items)
  has_items <- !is.null(items_expr) && !identical(items_expr, quote(NULL))
  has_cands <- !is.null(candidate_sets)

  if (!has_cands && !has_items) {
    stop("Either `candidate_sets` (Mode 1) or `items` (Mode 2) must be provided.", call. = FALSE)
  }
  if (has_cands && has_items) {
    stop("`candidate_sets` and `items` are mutually exclusive. Specify only one.", call. = FALSE)
  }

  # ---- Mode 2: Combinatorial Screening (Stage 1) ----
  if (has_items) {
    item_names <- .resolve_items(data, items_expr, env)
    if (length(item_names) == 0L) {
      stop("`items` must specify at least one predictor column.", call. = FALSE)
    }

    # Validate item columns
    for (col in item_names) {
      col_v <- data[[col]]
      if (!is.numeric(col_v)) {
        stop(sprintf("Item column '%s' must be numeric.", col), call. = FALSE)
      }
      if (anyNA(col_v)) {
        stop(sprintf("Item column '%s' contains NA values. Missing values are not supported.", col), call. = FALSE)
      }
      if (any(!is.finite(col_v))) {
        stop(sprintf("Item column '%s' contains non-finite values (Inf/-Inf/NaN).", col), call. = FALSE)
      }
    }

    # Validate model_sizes using existing canonical contract
    sizes <- .resolve_model_sizes(model_sizes = model_sizes, n_available = length(item_names))

    # Validate screen_top_n
    if (is.null(screen_top_n) || !is.numeric(screen_top_n) || length(screen_top_n) != 1L ||
        is.na(screen_top_n) || !is.finite(screen_top_n) || screen_top_n != as.integer(screen_top_n) || screen_top_n < 1L) {
      stop("`screen_top_n` must be a positive integer scalar.", call. = FALSE)
    }
    screen_top_n <- as.integer(screen_top_n)

    total_candidates <- sum(choose(length(item_names), sizes))

    if (progress) {
      message(sprintf("Stage 1: Screening top %d candidate(s) from %s total combinations across model size(s) %s by apparent %s...",
                      min(screen_top_n, total_candidates), format(total_candidates, big.mark = ","),
                      paste(sizes, collapse = ", "), rank_by))
    }

    # Stage 1: Exact Apparent Performance Screening across sizes (Bounded Memory)
    stage1_parallel <- if (parallel == "none") FALSE else parallel
    stage1_cand_list <- vector("list", length(sizes))
    cum_offset <- 0L
    n_items_total <- length(item_names)

    for (si in seq_along(sizes)) {
      s <- sizes[si]
      res_s <- exhaustive_sum_roc(
        data               = data,
        outcome            = outcome_name,
        items              = item_names,
        min_items          = s,
        max_items          = s,
        cutoff_method      = cutoff_method,
        rank_by            = rank_by,
        top_n              = screen_top_n,
        prefer_fewer_items = prefer_fewer_items,
        engine             = engine,
        parallel           = stage1_parallel,
        n_workers          = n_workers,
        progress           = FALSE
      )
      if (nrow(res_s) > 0L) {
        g_idx_s <- integer(nrow(res_s))
        for (j in seq_len(nrow(res_s))) {
          items_j <- .parse_itemset(res_s$items[j])
          combo_0based <- sort(as.integer(match(items_j, item_names) - 1L))
          local_rank_0based <- .combination_rank(n_items_total, s, combo_0based)
          g_idx_s[j] <- as.integer(cum_offset + local_rank_0based + 1L)
        }
        res_s$.global_combo_index <- g_idx_s
      }
      cum_offset <- cum_offset + choose(n_items_total, s)
      stage1_cand_list[[si]] <- res_s
    }

    merged_top <- do.call(rbind, stage1_cand_list)
    ranked_top <- .order_and_rank_candidates(
      df                 = merged_top,
      rank_by            = rank_by,
      prefer_fewer_items = prefer_fewer_items
    )
    ranked_top$rank <- seq_len(nrow(ranked_top))
    k_screen <- min(screen_top_n, nrow(ranked_top))
    screened_df <- utils::head(ranked_top, k_screen)

    # Convert screened candidates into candidate_sets list for Stage 2
    pad_width <- max(2L, nchar(as.character(k_screen)))
    cand_labels <- sprintf(paste0("M%0", pad_width, "d"), seq_len(k_screen))
    candidate_sets_screened <- vector("list", k_screen)
    for (i in seq_len(k_screen)) {
      candidate_sets_screened[[i]] <- .parse_itemset(screened_df$items[i])
    }
    names(candidate_sets_screened) <- cand_labels

    candidate_sets <- candidate_sets_screened
    conditional_on_screen <- TRUE
    screening_metric <- rank_by
    screen_top_n_requested <- screen_top_n
    screened_candidate_count <- k_screen
    global_combo_indices <- screened_df$.global_combo_index
  } else {
    # ---- Mode 1: Fixed Candidate Sets ----
    conditional_on_screen <- FALSE
    screening_metric <- NA_character_
    screen_top_n_requested <- NA_integer_
    screened_candidate_count <- NA_integer_
    total_candidates <- length(candidate_sets)
    sizes <- NULL
    global_combo_indices <- seq_along(candidate_sets)
  }

  # 4. Validate candidate_sets (both Mode 1 and screened Mode 2)
  if (!is.list(candidate_sets) || length(candidate_sets) == 0L) {
    stop("`candidate_sets` must be a non-empty named list of character vectors.", call. = FALSE)
  }

  labels <- names(candidate_sets)
  if (is.null(labels) || any(is.na(labels)) || any(labels == "") || length(unique(labels)) != length(labels)) {
    stop("`candidate_sets` must be a named list with unique, non-empty names.", call. = FALSE)
  }

  m_candidates <- length(candidate_sets)
  canonical_keys <- character(m_candidates)
  validated_candidates <- vector("list", m_candidates)
  names(validated_candidates) <- labels

  for (i in seq_len(m_candidates)) {
    cand <- candidate_sets[[i]]
    lbl <- labels[i]

    if (!is.character(cand) || length(cand) == 0L) {
      stop(sprintf("Candidate '%s' must be a non-empty character vector of item names.", lbl), call. = FALSE)
    }
    if (any(is.na(cand)) || any(cand == "")) {
      stop(sprintf("Candidate '%s' contains NA or empty item names.", lbl), call. = FALSE)
    }
    if (anyDuplicated(cand)) {
      stop(sprintf("Candidate '%s' contains duplicated items: %s.",
                   lbl, paste(cand[duplicated(cand)], collapse = ", ")), call. = FALSE)
    }
    missing_items <- setdiff(cand, names(data))
    if (length(missing_items) > 0L) {
      stop(sprintf("Candidate '%s' references items not found in `data`: %s.",
                   lbl, paste(missing_items, collapse = ", ")), call. = FALSE)
    }

    validated_candidates[[i]] <- as.character(cand)
    canonical_keys[i] <- paste(sort(cand), collapse = "|||")
  }

  # Check for duplicate canonical candidate definitions
  dup_idx <- which(duplicated(canonical_keys))
  if (length(dup_idx) > 0L) {
    first_dup_key <- canonical_keys[dup_idx[1]]
    orig_idx <- which(canonical_keys == first_dup_key)[1]
    stop(sprintf("Duplicate candidate definitions found in `candidate_sets`: '%s' and '%s' define the same item combination.",
                 labels[orig_idx], labels[dup_idx[1]]), call. = FALSE)
  }

  # Validate item columns data integrity
  all_items <- unique(unlist(validated_candidates))
  for (col in all_items) {
    col_v <- data[[col]]
    if (!is.numeric(col_v)) {
      stop(sprintf("Item column '%s' must be numeric.", col), call. = FALSE)
    }
    if (anyNA(col_v)) {
      stop(sprintf("Item column '%s' contains NA values. Missing values are not supported.", col), call. = FALSE)
    }
    if (any(!is.finite(col_v))) {
      stop(sprintf("Item column '%s' contains non-finite values (Inf/-Inf/NaN).", col), call. = FALSE)
    }
  }

  # 5. Validate scalar parameters for resampling
  if (resampling == "repeated_cv") {
    if (is.null(folds) || !is.numeric(folds) || length(folds) != 1L ||
        is.na(folds) || !is.finite(folds) || folds != as.integer(folds)) {
      stop("`folds` must be an integer-valued scalar.", call. = FALSE)
    }
    folds <- as.integer(folds)

    if (folds < 2L || folds >= n_obs) {
      stop(sprintf("`folds` (%d) must satisfy 2 <= folds < n_obs (%d).", folds, n_obs), call. = FALSE)
    }

    if (is.null(repeats) || !is.numeric(repeats) || length(repeats) != 1L ||
        is.na(repeats) || !is.finite(repeats) || repeats != as.integer(repeats) || repeats < 1L) {
      stop("`repeats` must be a positive integer scalar.", call. = FALSE)
    }
    repeats <- as.integer(repeats)
    total_resamples <- repeats
  } else {
    # Bootstrap validation
    if (is.null(bootstrap_reps) || !is.numeric(bootstrap_reps) || length(bootstrap_reps) != 1L ||
        is.na(bootstrap_reps) || !is.finite(bootstrap_reps) || bootstrap_reps != as.integer(bootstrap_reps) || bootstrap_reps < 1L) {
      stop("`bootstrap_reps` must be a positive integer scalar.", call. = FALSE)
    }
    bootstrap_reps <- as.integer(bootstrap_reps)
    total_resamples <- bootstrap_reps
  }

  # 6. Evaluate Full-Sample Apparent Performance for Candidates
  if (progress) {
    message(sprintf("Evaluating apparent performance for %d candidate model(s)...", m_candidates))
  }

  apparent_rows <- vector("list", m_candidates)
  for (i in seq_len(m_candidates)) {
    items_i <- validated_candidates[[i]]
    scores_i <- rowSums(data[, items_i, drop = FALSE])
    freq_i <- compute_score_frequencies(scores_i, y)
    auc_i <- compute_auc_from_table(freq_i$pos_counts, freq_i$neg_counts)
    roc_table_i <- compute_roc_metrics_from_table(freq_i$pos_counts, freq_i$neg_counts)
    best_cut_row <- find_optimal_cutoff(roc_table_i, method = cutoff_method)

    app_row_data <- list(
      candidate_id         = i,
      global_combo_index   = global_combo_indices[i],
      label                = labels[i],
      items                = paste(items_i, collapse = ", "),
      n_items              = length(items_i),
      apparent_auc         = auc_i,
      apparent_cutoff      = best_cut_row$cutoff,
      apparent_sensitivity = best_cut_row$sensitivity,
      apparent_specificity = best_cut_row$specificity,
      apparent_youden      = best_cut_row$youden,
      apparent_accuracy    = best_cut_row$accuracy,
      apparent_ppv         = best_cut_row$ppv,
      apparent_npv         = best_cut_row$npv
    )
    if (conditional_on_screen) {
      app_row_data$stage1_screen_rank <- i
    }
    apparent_rows[[i]] <- as.data.frame(app_row_data, stringsAsFactors = FALSE)
  }
  apparent_df <- do.call(rbind, apparent_rows)
  rownames(apparent_df) <- NULL

  x_mat <- as.matrix(data[, all_items, drop = FALSE])
  mode(x_mat) <- "double"

  combo_col_indices <- lapply(validated_candidates, function(cand) {
    as.integer(match(cand, all_items) - 1L)
  })

  # 7. Resampling Execution (Repeated CV or Bootstrap)
  if (resampling == "repeated_cv") {
    cv_folds <- .build_cv_folds(y, folds = folds, repeats = repeats, stratified = TRUE, seed = seed)
    test_row_indices <- lapply(cv_folds, function(fold_idx) {
      as.integer(fold_idx - 1L)
    })
    total_folds <- length(cv_folds)

    if (progress) {
      message(sprintf("Running repeated %d-fold CV (%d repeat(s)) across %d candidates...",
                      folds, repeats, m_candidates))
    }

    if (parallel == "none") {
      .ncvroc_candidate_stability_routing_counters$serial <-
        .ncvroc_candidate_stability_routing_counters$serial + 1L
      raw_resample_df <- evaluate_candidate_stability_cv_cpp(
        x             = x_mat,
        y             = y,
        combo_indices = combo_col_indices,
        test_indices  = test_row_indices,
        n_folds       = total_folds,
        repeats       = repeats,
        cutoff_method = cutoff_method,
        num_threads   = 1L
      )
    } else if (parallel == "threads") {
      .ncvroc_candidate_stability_routing_counters$threads <-
        .ncvroc_candidate_stability_routing_counters$threads + 1L
      threads_to_use <- if (is.null(n_workers)) .resolve_n_workers("threads", NULL) else as.integer(n_workers)
      raw_resample_df <- evaluate_candidate_stability_cv_cpp(
        x             = x_mat,
        y             = y,
        combo_indices = combo_col_indices,
        test_indices  = test_row_indices,
        n_folds       = total_folds,
        repeats       = repeats,
        cutoff_method = cutoff_method,
        num_threads   = threads_to_use
      )
    } else if (parallel == "chunks") {
      .ncvroc_candidate_stability_routing_counters$chunks <-
        .ncvroc_candidate_stability_routing_counters$chunks + 1L
      workers <- if (is.null(n_workers)) .resolve_n_workers("chunks", NULL) else as.integer(n_workers)
      cl <- parallel::makePSOCKcluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterEvalQ(cl, {
        Sys.setenv(RCPP_PARALLEL_NUM_THREADS = "1")
        library(NCVROC)
      })

      chunk_splits <- split(seq_len(m_candidates), cut(seq_len(m_candidates), breaks = min(m_candidates, workers), labels = FALSE))

      worker_task_cv <- function(chunk_cands, x_mat, y, combo_col_indices, test_row_indices, total_folds, repeats, cutoff_method) {
        sub_combos <- combo_col_indices[chunk_cands]
        chunk_res <- evaluate_candidate_stability_cv_cpp(
          x             = x_mat,
          y             = y,
          combo_indices = sub_combos,
          test_indices  = test_row_indices,
          n_folds       = total_folds,
          repeats       = repeats,
          cutoff_method = cutoff_method,
          num_threads   = 1L
        )
        chunk_res$candidate_id <- chunk_cands[chunk_res$candidate_id]
        chunk_res
      }

      chunk_results <- parallel::parLapply(
        cl,
        chunk_splits,
        worker_task_cv,
        x_mat             = x_mat,
        y                 = y,
        combo_col_indices = combo_col_indices,
        test_row_indices  = test_row_indices,
        total_folds       = total_folds,
        repeats           = repeats,
        cutoff_method     = cutoff_method
      )
      raw_resample_df <- do.call(rbind, chunk_results)
      ord_raw <- order(raw_resample_df$candidate_id, raw_resample_df$repeat_id)
      raw_resample_df <- raw_resample_df[ord_raw, , drop = FALSE]
      rownames(raw_resample_df) <- NULL
    }

    resample_df <- data.frame(
      candidate_id     = raw_resample_df$candidate_id,
      label            = labels[raw_resample_df$candidate_id],
      replicate_id     = raw_resample_df$repeat_id,
      repeat_id        = raw_resample_df$repeat_id,
      test_auc         = raw_resample_df$auc,
      test_sensitivity = raw_resample_df$sensitivity,
      test_specificity = raw_resample_df$specificity,
      test_youden      = raw_resample_df$youden,
      test_accuracy    = raw_resample_df$accuracy,
      test_ppv         = raw_resample_df$ppv,
      test_npv         = raw_resample_df$npv,
      test_valid       = TRUE,
      stringsAsFactors = FALSE
    )
    resample_df$auc         <- resample_df$test_auc
    resample_df$sensitivity <- resample_df$test_sensitivity
    resample_df$specificity <- resample_df$test_specificity
    resample_df$youden      <- resample_df$test_youden
    resample_df$accuracy    <- resample_df$test_accuracy
    resample_df$ppv         <- resample_df$test_ppv
    resample_df$npv         <- resample_df$test_npv
  } else {
    # Bootstrap Resampling
    if (!is.null(seed)) {
      set.seed(seed)
    }
    boot_train_list <- vector("list", bootstrap_reps)
    boot_oob_list   <- vector("list", bootstrap_reps)
    tr_valid_vec    <- logical(bootstrap_reps)
    n_oob_vec       <- integer(bootstrap_reps)
    n_oob_pos_vec   <- integer(bootstrap_reps)
    n_oob_neg_vec   <- integer(bootstrap_reps)

    for (b in seq_len(bootstrap_reps)) {
      tr_idx <- sample.int(n_obs, size = n_obs, replace = TRUE)
      oob_idx <- setdiff(seq_len(n_obs), unique(tr_idx))

      boot_train_list[[b]] <- as.integer(tr_idx - 1L)
      boot_oob_list[[b]]   <- as.integer(oob_idx - 1L)

      tr_y <- y[tr_idx]
      tr_pos <- sum(tr_y == 1L)
      tr_neg <- sum(tr_y == 0L)
      tr_valid_vec[b] <- (tr_pos > 0L && tr_neg > 0L)

      n_oob_vec[b] <- length(oob_idx)
      if (length(oob_idx) > 0L) {
        oob_y <- y[oob_idx]
        n_oob_pos_vec[b] <- sum(oob_y == 1L)
        n_oob_neg_vec[b] <- sum(oob_y == 0L)
      } else {
        n_oob_pos_vec[b] <- 0L
        n_oob_neg_vec[b] <- 0L
      }
    }

    app_auc_vec <- apparent_df$apparent_auc

    if (progress) {
      message(sprintf("Running bootstrap resampling (B = %d replicates, test = '%s') across %d candidates...",
                      bootstrap_reps, bootstrap_test, m_candidates))
    }

    if (parallel == "none") {
      .ncvroc_candidate_stability_routing_counters$serial <-
        .ncvroc_candidate_stability_routing_counters$serial + 1L
      raw_resample_df <- evaluate_candidate_stability_bootstrap_cpp(
        x              = x_mat,
        y              = y,
        combo_indices  = combo_col_indices,
        train_indices  = boot_train_list,
        oob_indices    = boot_oob_list,
        bootstrap_test = bootstrap_test,
        cutoff_method  = cutoff_method,
        apparent_auc   = app_auc_vec,
        num_threads    = 1L
      )
    } else if (parallel == "threads") {
      .ncvroc_candidate_stability_routing_counters$threads <-
        .ncvroc_candidate_stability_routing_counters$threads + 1L
      threads_to_use <- if (is.null(n_workers)) .resolve_n_workers("threads", NULL) else as.integer(n_workers)
      raw_resample_df <- evaluate_candidate_stability_bootstrap_cpp(
        x              = x_mat,
        y              = y,
        combo_indices  = combo_col_indices,
        train_indices  = boot_train_list,
        oob_indices    = boot_oob_list,
        bootstrap_test = bootstrap_test,
        cutoff_method  = cutoff_method,
        apparent_auc   = app_auc_vec,
        num_threads    = threads_to_use
      )
    } else if (parallel == "chunks") {
      .ncvroc_candidate_stability_routing_counters$chunks <-
        .ncvroc_candidate_stability_routing_counters$chunks + 1L
      workers <- if (is.null(n_workers)) .resolve_n_workers("chunks", NULL) else as.integer(n_workers)
      cl <- parallel::makePSOCKcluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterEvalQ(cl, {
        Sys.setenv(RCPP_PARALLEL_NUM_THREADS = "1")
        library(NCVROC)
      })

      chunk_splits <- split(seq_len(m_candidates), cut(seq_len(m_candidates), breaks = min(m_candidates, workers), labels = FALSE))

      worker_task_boot <- function(chunk_cands, x_mat, y, combo_col_indices, boot_train_list, boot_oob_list, bootstrap_test, cutoff_method, app_auc_vec) {
        sub_combos <- combo_col_indices[chunk_cands]
        sub_app_auc <- app_auc_vec[chunk_cands]
        chunk_res <- evaluate_candidate_stability_bootstrap_cpp(
          x              = x_mat,
          y              = y,
          combo_indices  = sub_combos,
          train_indices  = boot_train_list,
          oob_indices    = boot_oob_list,
          bootstrap_test = bootstrap_test,
          cutoff_method  = cutoff_method,
          apparent_auc   = sub_app_auc,
          num_threads    = 1L
        )
        chunk_res$candidate_id <- chunk_cands[chunk_res$candidate_id]
        chunk_res
      }

      chunk_results <- parallel::parLapply(
        cl,
        chunk_splits,
        worker_task_boot,
        x_mat             = x_mat,
        y                 = y,
        combo_col_indices = combo_col_indices,
        boot_train_list   = boot_train_list,
        boot_oob_list     = boot_oob_list,
        bootstrap_test    = bootstrap_test,
        cutoff_method     = cutoff_method,
        app_auc_vec       = app_auc_vec
      )
      raw_resample_df <- do.call(rbind, chunk_results)
      ord_raw <- order(raw_resample_df$candidate_id, raw_resample_df$replicate_id)
      raw_resample_df <- raw_resample_df[ord_raw, , drop = FALSE]
      rownames(raw_resample_df) <- NULL
    }

    resample_df <- data.frame(
      candidate_id      = raw_resample_df$candidate_id,
      label             = labels[raw_resample_df$candidate_id],
      replicate_id      = raw_resample_df$replicate_id,
      training_valid    = as.logical(raw_resample_df$training_valid),
      train_auc         = raw_resample_df$train_auc,
      train_cutoff      = raw_resample_df$train_cutoff,
      train_sensitivity = raw_resample_df$train_sensitivity,
      train_specificity = raw_resample_df$train_specificity,
      train_youden      = raw_resample_df$train_youden,
      train_accuracy    = raw_resample_df$train_accuracy,
      train_ppv         = raw_resample_df$train_ppv,
      train_npv         = raw_resample_df$train_npv,
      test_valid        = as.logical(raw_resample_df$test_valid),
      test_auc          = raw_resample_df$test_auc,
      test_sensitivity  = raw_resample_df$test_sensitivity,
      test_specificity  = raw_resample_df$test_specificity,
      test_youden       = raw_resample_df$test_youden,
      test_accuracy     = raw_resample_df$test_accuracy,
      test_ppv          = raw_resample_df$test_ppv,
      test_npv          = raw_resample_df$test_npv,
      n_train           = rep(n_obs, nrow(raw_resample_df)),
      n_test            = if (bootstrap_test == "original") rep(n_obs, nrow(raw_resample_df)) else n_oob_vec[raw_resample_df$replicate_id],
      stringsAsFactors  = FALSE
    )
    resample_df$auc         <- resample_df$test_auc
    resample_df$sensitivity <- resample_df$test_sensitivity
    resample_df$specificity <- resample_df$test_specificity
    resample_df$youden      <- resample_df$test_youden
    resample_df$accuracy    <- resample_df$test_accuracy
    resample_df$ppv         <- resample_df$test_ppv
    resample_df$npv         <- resample_df$test_npv
  }

  rownames(resample_df) <- NULL

  # 8. Post-Resampling Replicate Ranking, Selection Status & Feasibility
  resample_df$rank <- NA_integer_
  resample_df$feasible <- FALSE

  replicate_status <- character(total_resamples)
  winner_ids <- rep(NA_integer_, total_resamples)
  winner_labels <- rep(NA_character_, total_resamples)
  n_feasible_vec <- integer(total_resamples)

  for (r in seq_len(total_resamples)) {
    sub_indices <- which(resample_df$replicate_id == r)
    sub_df <- resample_df[sub_indices, , drop = FALSE]

    # Check training validity if bootstrap
    is_tr_valid <- if (resampling == "bootstrap") all(sub_df$training_valid) else TRUE

    if (!is_tr_valid) {
      replicate_status[r] <- "invalid_evaluation"
      next
    }

    # Feasibility check on test-side repeat metrics
    sens_pass <- if (is.null(sensitivity_min)) rep(TRUE, m_candidates) else (!is.na(sub_df$test_sensitivity) & sub_df$test_sensitivity >= sensitivity_min)
    spec_pass <- if (is.null(specificity_min)) rep(TRUE, m_candidates) else (!is.na(sub_df$test_specificity) & sub_df$test_specificity >= specificity_min)
    is_feasible <- sens_pass & spec_pass
    resample_df$feasible[sub_indices] <- is_feasible
    n_feasible_vec[r] <- sum(is_feasible)

    # Candidate ranking using canonical tie-breaking on test-side rank_by metric
    rank_metric_col <- paste0("test_", rank_by)
    rank_metric_vals <- sub_df[[rank_metric_col]]
    valid_rank_mask <- !is.na(rank_metric_vals)

    if (any(valid_rank_mask)) {
      valid_sub <- sub_df[valid_rank_mask, , drop = FALSE]
      cand_sizes <- apparent_df$n_items[valid_sub$candidate_id]
      cand_ids   <- valid_sub$candidate_id

      # Canonical sorting: 1. primary metric desc, 2. fewer items asc (if prefer_fewer_items), 3. candidate_id asc
      ord <- if (prefer_fewer_items) {
        order(-valid_sub[[rank_metric_col]], cand_sizes, cand_ids)
      } else {
        order(-valid_sub[[rank_metric_col]], cand_ids)
      }

      ranks_assigned <- integer(nrow(valid_sub))
      ranks_assigned[ord] <- seq_along(ord)

      valid_idx_in_resample <- sub_indices[valid_rank_mask]
      resample_df$rank[valid_idx_in_resample] <- ranks_assigned
    }

    # Selection status for replicate r
    if (!any(valid_rank_mask)) {
      replicate_status[r] <- "invalid_evaluation"
    } else if (sum(is_feasible) == 0L) {
      replicate_status[r] <- "no_feasible_candidate"
    } else {
      replicate_status[r] <- "selected"
      feasible_sub <- sub_df[is_feasible, , drop = FALSE]
      feas_sizes <- apparent_df$n_items[feasible_sub$candidate_id]
      feas_ids   <- feasible_sub$candidate_id

      ord_feas <- if (prefer_fewer_items) {
        order(-feasible_sub[[rank_metric_col]], feas_sizes, feas_ids)
      } else {
        order(-feasible_sub[[rank_metric_col]], feas_ids)
      }

      winner_row <- feasible_sub[ord_feas[1], , drop = FALSE]
      winner_ids[r] <- winner_row$candidate_id
      winner_labels[r] <- winner_row$label
    }
  }

  if (resampling == "repeated_cv") {
    replicate_metadata <- data.frame(
      repeat_id           = seq_len(total_resamples),
      selection_status    = replicate_status,
      winner_candidate_id = winner_ids,
      winner_label        = winner_labels,
      n_feasible          = n_feasible_vec,
      stringsAsFactors    = FALSE
    )
  } else {
    replicate_metadata <- data.frame(
      replicate_id        = seq_len(total_resamples),
      selection_status    = replicate_status,
      winner_candidate_id = winner_ids,
      winner_label        = winner_labels,
      n_feasible          = n_feasible_vec,
      training_valid      = tr_valid_vec,
      n_train             = rep(n_obs, total_resamples),
      n_oob               = n_oob_vec,
      n_oob_positive      = n_oob_pos_vec,
      n_oob_negative      = n_oob_neg_vec,
      stringsAsFactors    = FALSE
    )
  }

  # 9. Aggregate Metrics & Construct candidate_summary
  summary_rows <- vector("list", m_candidates)
  metrics_to_agg <- c("auc", "youden", "sensitivity", "specificity", "accuracy", "ppv", "npv")

  n_selected_replicates <- sum(replicate_status == "selected")

  # Tracking for low-validity warning in bootstrap mode
  low_validity_warnings <- list()

  for (i in seq_len(m_candidates)) {
    sub_cand <- resample_df[resample_df$candidate_id == i, , drop = FALSE]
    app_row  <- apparent_df[apparent_df$candidate_id == i, , drop = FALSE]

    # Test-side metrics distribution
    agg_stats <- list()
    for (m in metrics_to_agg) {
      vals <- sub_cand[[paste0("test_", m)]]
      valid_vals <- vals[!is.na(vals)]
      n_v <- length(valid_vals)

      is_auc_zero_sd <- (resampling == "repeated_cv" && m == "auc") ||
                        (resampling == "bootstrap" && bootstrap_test == "original" && m == "auc")

      agg_stats[[paste0("resampled_", m, "_mean")]]   <- if (n_v > 0) mean(valid_vals) else NA_real_
      agg_stats[[paste0("resampled_", m, "_sd")]]     <- if (is_auc_zero_sd) 0.0 else (if (n_v > 1) stats::sd(valid_vals) else NA_real_)
      agg_stats[[paste0("resampled_", m, "_median")]] <- if (n_v > 0) stats::median(valid_vals) else NA_real_
      agg_stats[[paste0("resampled_", m, "_iqr")]]    <- if (n_v > 0) stats::IQR(valid_vals) else NA_real_
      agg_stats[[paste0("resampled_", m, "_min")]]    <- if (n_v > 0) min(valid_vals) else NA_real_
      agg_stats[[paste0("resampled_", m, "_max")]]    <- if (n_v > 0) max(valid_vals) else NA_real_
      agg_stats[[paste0("n_valid_", m)]]              <- n_v

      if (resampling == "bootstrap" && (n_v / total_resamples < 0.80)) {
        low_validity_warnings[[length(low_validity_warnings) + 1L]] <- list(
          metric = m, candidate = labels[i], n_valid = n_v, total = total_resamples
        )
      }
    }

    # Apparent minus resampled gap (classification metrics)
    gap_stats <- list()
    for (m in c("youden", "sensitivity", "specificity", "accuracy", "ppv", "npv")) {
      app_val <- app_row[[paste0("apparent_", m)]]
      res_m   <- agg_stats[[paste0("resampled_", m, "_mean")]]
      gap_stats[[paste0("apparent_minus_resampled_", m)]] <- if (!is.na(app_val) && !is.na(res_m)) app_val - res_m else NA_real_
    }

    # Optimism stats for bootstrap original mode
    opt_stats <- list()
    boot_tr_stats <- list()
    if (resampling == "bootstrap" && bootstrap_test == "original") {
      for (m in c("youden", "sensitivity", "specificity", "accuracy", "ppv", "npv")) {
        tr_vals <- sub_cand[[paste0("train_", m)]]
        ts_vals <- sub_cand[[paste0("test_", m)]]
        paired_valid <- !is.na(tr_vals) & !is.na(ts_vals)
        n_opt_v <- sum(paired_valid)

        if (n_opt_v > 0) {
          diffs <- tr_vals[paired_valid] - ts_vals[paired_valid]
          opt_mean <- mean(diffs)
          opt_sd   <- if (n_opt_v > 1) stats::sd(diffs) else NA_real_
          opt_corr <- app_row[[paste0("apparent_", m)]] - opt_mean
        } else {
          opt_mean <- NA_real_
          opt_sd   <- NA_real_
          opt_corr <- NA_real_
        }
        opt_stats[[paste0("optimism_", m, "_mean")]]      <- opt_mean
        opt_stats[[paste0("optimism_", m, "_sd")]]        <- opt_sd
        opt_stats[[paste0("optimism_corrected_", m)]]     <- opt_corr
        opt_stats[[paste0("n_valid_optimism_", m)]]       <- n_opt_v
      }

      # Training bootstrap metric distributions
      for (m in metrics_to_agg) {
        tr_vals <- sub_cand[[paste0("train_", m)]]
        v_tr <- tr_vals[!is.na(tr_vals)]
        boot_tr_stats[[paste0("bootstrap_train_", m, "_mean")]] <- if (length(v_tr) > 0) mean(v_tr) else NA_real_
        boot_tr_stats[[paste0("bootstrap_train_", m, "_sd")]]   <- if (length(v_tr) > 1) stats::sd(v_tr) else NA_real_
      }
    }

    # Rank statistics
    ranks_v <- sub_cand$rank[!is.na(sub_cand$rank)]
    n_rank_reps <- length(ranks_v)
    rank_stats <- list(
      mean_rank         = if (n_rank_reps > 0) mean(ranks_v) else NA_real_,
      median_rank       = if (n_rank_reps > 0) stats::median(ranks_v) else NA_real_,
      rank_sd           = if (n_rank_reps > 1) stats::sd(ranks_v) else NA_real_,
      rank_iqr          = if (n_rank_reps > 0) stats::IQR(ranks_v) else NA_real_,
      best_rank         = if (n_rank_reps > 0) min(ranks_v) else NA_integer_,
      worst_rank        = if (n_rank_reps > 0) max(ranks_v) else NA_integer_,
      top_1_frequency   = if (n_rank_reps > 0) sum(ranks_v == 1L) / total_resamples else NA_real_,
      top_5_frequency   = if (n_rank_reps > 0) sum(ranks_v <= 5L) / total_resamples else NA_real_,
      top_10_frequency  = if (n_rank_reps > 0) sum(ranks_v <= 10L) / total_resamples else NA_real_,
      n_valid_rank_reps = n_rank_reps
    )

    # Selection frequencies
    sel_count <- sum(replicate_metadata$winner_candidate_id == i, na.rm = TRUE)
    sel_freq_all  <- sel_count / total_resamples
    sel_freq_cond <- if (n_selected_replicates > 0) sel_count / n_selected_replicates else NA_real_

    # Constraint pass frequencies
    sens_v <- sub_cand$test_sensitivity[!is.na(sub_cand$test_sensitivity)]
    spec_v <- sub_cand$test_specificity[!is.na(sub_cand$test_specificity)]
    both_v_mask <- !is.na(sub_cand$test_sensitivity) & !is.na(sub_cand$test_specificity)

    sens_pass_freq <- if (!is.null(sensitivity_min)) {
      if (length(sens_v) > 0) sum(sens_v >= sensitivity_min) / length(sens_v) else NA_real_
    } else {
      NA_real_
    }
    spec_pass_freq <- if (!is.null(specificity_min)) {
      if (length(spec_v) > 0) sum(spec_v >= specificity_min) / length(spec_v) else NA_real_
    } else {
      NA_real_
    }
    both_pass_freq <- if (!is.null(sensitivity_min) && !is.null(specificity_min)) {
      if (sum(both_v_mask) > 0) sum(sub_cand$test_sensitivity[both_v_mask] >= sensitivity_min &
                                      sub_cand$test_specificity[both_v_mask] >= specificity_min) / sum(both_v_mask) else NA_real_
    } else {
      NA_real_
    }

    cand_row_data <- list(
      candidate_id                   = i,
      global_combo_index             = global_combo_indices[i],
      label                          = labels[i],
      items                          = app_row$items,
      n_items                        = app_row$n_items,
      apparent_auc                   = app_row$apparent_auc,
      apparent_cutoff                = app_row$apparent_cutoff,
      apparent_sensitivity           = app_row$apparent_sensitivity,
      apparent_specificity           = app_row$apparent_specificity,
      apparent_youden                = app_row$apparent_youden,
      apparent_accuracy              = app_row$apparent_accuracy,
      apparent_ppv                   = app_row$apparent_ppv,
      apparent_npv                   = app_row$apparent_npv,
      resampled_auc_mean             = agg_stats$resampled_auc_mean,
      resampled_auc_sd               = agg_stats$resampled_auc_sd,
      resampled_auc_median           = agg_stats$resampled_auc_median,
      resampled_auc_iqr              = agg_stats$resampled_auc_iqr,
      resampled_youden_mean          = agg_stats$resampled_youden_mean,
      resampled_youden_sd            = agg_stats$resampled_youden_sd,
      resampled_youden_median        = agg_stats$resampled_youden_median,
      resampled_youden_iqr           = agg_stats$resampled_youden_iqr,
      resampled_sensitivity_mean     = agg_stats$resampled_sensitivity_mean,
      resampled_sensitivity_sd       = agg_stats$resampled_sensitivity_sd,
      resampled_specificity_mean     = agg_stats$resampled_specificity_mean,
      resampled_specificity_sd       = agg_stats$resampled_specificity_sd,
      resampled_accuracy_mean        = agg_stats$resampled_accuracy_mean,
      resampled_accuracy_sd          = agg_stats$resampled_accuracy_sd,
      resampled_ppv_mean             = agg_stats$resampled_ppv_mean,
      resampled_ppv_sd               = agg_stats$resampled_ppv_sd,
      resampled_npv_mean             = agg_stats$resampled_npv_mean,
      resampled_npv_sd               = agg_stats$resampled_npv_sd,
      apparent_minus_resampled_youden = gap_stats$apparent_minus_resampled_youden,
      apparent_minus_resampled_sens  = gap_stats$apparent_minus_resampled_sensitivity,
      apparent_minus_resampled_spec  = gap_stats$apparent_minus_resampled_specificity,
      apparent_minus_resampled_acc   = gap_stats$apparent_minus_resampled_accuracy,
      mean_rank                      = rank_stats$mean_rank,
      median_rank                    = rank_stats$median_rank,
      rank_sd                        = rank_stats$rank_sd,
      rank_iqr                       = rank_stats$rank_iqr,
      best_rank                      = rank_stats$best_rank,
      worst_rank                     = rank_stats$worst_rank,
      top_1_frequency                = rank_stats$top_1_frequency,
      top_5_frequency                = rank_stats$top_5_frequency,
      top_10_frequency               = rank_stats$top_10_frequency,
      selection_count                = sel_count,
      selection_frequency_all        = sel_freq_all,
      selection_frequency_conditional = sel_freq_cond,
      sens_pass_frequency            = sens_pass_freq,
      spec_pass_frequency            = spec_pass_freq,
      both_pass_frequency            = both_pass_freq,
      n_valid_auc                    = agg_stats$n_valid_auc,
      n_valid_sensitivity            = agg_stats$n_valid_sensitivity,
      n_valid_specificity            = agg_stats$n_valid_specificity,
      n_valid_youden                 = agg_stats$n_valid_youden,
      n_valid_accuracy               = agg_stats$n_valid_accuracy,
      n_valid_ppv                    = agg_stats$n_valid_ppv,
      n_valid_npv                    = agg_stats$n_valid_npv,
      n_valid_rank_reps              = rank_stats$n_valid_rank_reps
    )

    if (conditional_on_screen) {
      cand_row_data$stage1_screen_rank <- i
    }

    if (resampling == "bootstrap" && bootstrap_test == "original") {
      cand_row_data$optimism_youden_mean           <- opt_stats$optimism_youden_mean
      cand_row_data$optimism_youden_sd             <- opt_stats$optimism_youden_sd
      cand_row_data$optimism_corrected_youden      <- opt_stats$optimism_corrected_youden
      cand_row_data$optimism_sensitivity_mean      <- opt_stats$optimism_sensitivity_mean
      cand_row_data$optimism_sensitivity_sd        <- opt_stats$optimism_sensitivity_sd
      cand_row_data$optimism_corrected_sensitivity <- opt_stats$optimism_corrected_sensitivity
      cand_row_data$optimism_specificity_mean      <- opt_stats$optimism_specificity_mean
      cand_row_data$optimism_specificity_sd        <- opt_stats$optimism_specificity_sd
      cand_row_data$optimism_corrected_specificity <- opt_stats$optimism_corrected_specificity
      cand_row_data$optimism_accuracy_mean         <- opt_stats$optimism_accuracy_mean
      cand_row_data$optimism_accuracy_sd           <- opt_stats$optimism_accuracy_sd
      cand_row_data$optimism_corrected_accuracy    <- opt_stats$optimism_corrected_accuracy
      cand_row_data$optimism_ppv_mean              <- opt_stats$optimism_ppv_mean
      cand_row_data$optimism_ppv_sd                <- opt_stats$optimism_ppv_sd
      cand_row_data$optimism_corrected_ppv         <- opt_stats$optimism_corrected_ppv
      cand_row_data$optimism_npv_mean              <- opt_stats$optimism_npv_mean
      cand_row_data$optimism_npv_sd                <- opt_stats$optimism_npv_sd
      cand_row_data$optimism_corrected_npv         <- opt_stats$optimism_corrected_npv

      cand_row_data$n_valid_optimism_youden        <- opt_stats$n_valid_optimism_youden
      cand_row_data$n_valid_optimism_sensitivity   <- opt_stats$n_valid_optimism_sensitivity
      cand_row_data$n_valid_optimism_specificity   <- opt_stats$n_valid_optimism_specificity
      cand_row_data$n_valid_optimism_accuracy      <- opt_stats$n_valid_optimism_accuracy
      cand_row_data$n_valid_optimism_ppv           <- opt_stats$n_valid_optimism_ppv
      cand_row_data$n_valid_optimism_npv           <- opt_stats$n_valid_optimism_npv

      cand_row_data$bootstrap_train_auc_mean       <- boot_tr_stats$bootstrap_train_auc_mean
      cand_row_data$bootstrap_train_auc_sd         <- boot_tr_stats$bootstrap_train_auc_sd
      cand_row_data$bootstrap_train_youden_mean    <- boot_tr_stats$bootstrap_train_youden_mean
      cand_row_data$bootstrap_train_youden_sd      <- boot_tr_stats$bootstrap_train_youden_sd
      cand_row_data$bootstrap_train_sens_mean      <- boot_tr_stats$bootstrap_train_sensitivity_mean
      cand_row_data$bootstrap_train_sens_sd        <- boot_tr_stats$bootstrap_train_sensitivity_sd
      cand_row_data$bootstrap_train_spec_mean      <- boot_tr_stats$bootstrap_train_specificity_mean
      cand_row_data$bootstrap_train_spec_sd        <- boot_tr_stats$bootstrap_train_specificity_sd
      cand_row_data$bootstrap_train_acc_mean       <- boot_tr_stats$bootstrap_train_accuracy_mean
      cand_row_data$bootstrap_train_acc_sd         <- boot_tr_stats$bootstrap_train_accuracy_sd
      cand_row_data$bootstrap_train_ppv_mean       <- boot_tr_stats$bootstrap_train_ppv_mean
      cand_row_data$bootstrap_train_ppv_sd         <- boot_tr_stats$bootstrap_train_ppv_sd
      cand_row_data$bootstrap_train_npv_mean       <- boot_tr_stats$bootstrap_train_npv_mean
      cand_row_data$bootstrap_train_npv_sd         <- boot_tr_stats$bootstrap_train_npv_sd
    }

    summary_rows[[i]] <- as.data.frame(cand_row_data, stringsAsFactors = FALSE)
  }

  # Emit concise aggregated warning if any metric had < 80% validity in bootstrap
  if (length(low_validity_warnings) > 0L) {
    metric_map <- list()
    for (w in low_validity_warnings) {
      if (is.null(metric_map[[w$metric]])) metric_map[[w$metric]] <- list()
      metric_map[[w$metric]][[w$candidate]] <- sprintf("%d/%d", w$n_valid, w$total)
    }
    desc_parts <- vapply(names(metric_map), function(m) {
      c_strs <- vapply(names(metric_map[[m]]), function(cand) {
        sprintf("%s (%s)", cand, metric_map[[m]][[cand]])
      }, character(1))
      sprintf("%s: %s", m, paste(c_strs, collapse = ", "))
    }, character(1))

    warn_msg <- sprintf(
      "Resampling validity rate < 80%% observed for: %s. Interpret affected metric distributions with caution.",
      paste(desc_parts, collapse = "; ")
    )
    warning(warn_msg, call. = FALSE)
  }

  candidate_summary_df <- do.call(rbind, summary_rows)
  rownames(candidate_summary_df) <- NULL

  # Sort candidate_summary by mean_rank ascending
  ord_sum <- order(candidate_summary_df$mean_rank, candidate_summary_df$candidate_id)
  candidate_summary_df <- candidate_summary_df[ord_sum, , drop = FALSE]
  rownames(candidate_summary_df) <- NULL

  # 10. Extract specialized component tables
  rank_cols <- c("candidate_id", "global_combo_index")
  if (conditional_on_screen) rank_cols <- c(rank_cols, "stage1_screen_rank")
  rank_cols <- c(rank_cols, "label", "items", "n_items",
                 "mean_rank", "median_rank", "rank_sd", "rank_iqr",
                 "best_rank", "worst_rank", "top_1_frequency", "top_5_frequency", "top_10_frequency",
                 "n_valid_rank_reps")
  rank_stability_df <- candidate_summary_df[, rank_cols, drop = FALSE]

  sel_cols <- c("candidate_id", "global_combo_index")
  if (conditional_on_screen) sel_cols <- c(sel_cols, "stage1_screen_rank")
  sel_cols <- c(sel_cols, "label", "items", "n_items",
                "selection_count", "selection_frequency_all", "selection_frequency_conditional")
  selection_frequency_df <- candidate_summary_df[, sel_cols, drop = FALSE]

  const_cols <- c("candidate_id", "global_combo_index")
  if (conditional_on_screen) const_cols <- c(const_cols, "stage1_screen_rank")
  const_cols <- c(const_cols, "label", "items", "n_items",
                  "sens_pass_frequency", "spec_pass_frequency", "both_pass_frequency",
                  "n_valid_sensitivity", "n_valid_specificity")
  constraint_stability_df <- candidate_summary_df[, const_cols, drop = FALSE]

  # Global partitioned frequency accounting
  no_feasible_freq <- sum(replicate_status == "no_feasible_candidate") / total_resamples
  invalid_eval_freq <- sum(replicate_status == "invalid_evaluation") / total_resamples

  # 11. Build settings and return object
  settings <- list(
    mode                         = if (conditional_on_screen) "combinatorial_screen" else "fixed_candidates",
    conditional_on_screen        = conditional_on_screen,
    screening_metric             = screening_metric,
    screen_top_n_requested       = screen_top_n_requested,
    screened_candidate_count     = screened_candidate_count,
    total_candidate_count        = total_candidates,
    model_sizes                  = sizes,
    resampling                   = resampling,
    folds                        = if (resampling == "repeated_cv") folds else NA_integer_,
    repeats                      = if (resampling == "repeated_cv") repeats else NA_integer_,
    bootstrap_reps               = if (resampling == "bootstrap") bootstrap_reps else NA_integer_,
    bootstrap_test               = if (resampling == "bootstrap") bootstrap_test else NA_character_,
    total_requested_resamples    = total_resamples,
    n_candidates                 = m_candidates,
    cutoff_method                = cutoff_method,
    rank_by                      = rank_by,
    sensitivity_min              = sensitivity_min,
    specificity_min              = specificity_min,
    prefer_fewer_items           = prefer_fewer_items,
    positive_label               = positive_label,
    negative_label               = negative_label,
    parallel                     = parallel,
    seed                         = seed,
    no_feasible_frequency        = no_feasible_freq,
    invalid_evaluation_frequency = invalid_eval_freq,
    n_resamples_with_selected_winner = n_selected_replicates,
    call                         = call_matched
  )

  result <- list(
    candidate_summary     = candidate_summary_df,
    resample_results      = resample_df,
    replicate_metadata    = replicate_metadata,
    rank_stability        = rank_stability_df,
    selection_frequency   = selection_frequency_df,
    constraint_stability  = constraint_stability_df,
    apparent_performance = apparent_df,
    candidate_definitions = validated_candidates,
    settings              = settings
  )

  class(result) <- "candidate_stability_result"
  result
}
