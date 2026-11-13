# bootstrap_select_sum_roc.R — Harrell-style Bootstrap Internal Validation & Selection Stability
#
# Provides:
#   - bootstrap_select_sum_roc()
#   - Internal helpers for candidate evaluation and validation

#' Bootstrap Internal Validation and Selection Stability for Item-Set Scores
#'
#' Performs Harrell-style bootstrap internal validation, Efron's .632, and Efron–Tibshirani's
#' .632+ bootstrap corrections of the complete unweighted sum-score item selection and
#' cutoff determination procedure, and evaluates item selection stability across bootstrap resamples.
#'
#' @details
#' \strong{Correction Methods:}
#' \itemize{
#'   \item \code{"harrell"}: Standard Harrell-style optimism correction. On each resample,
#'     the complete candidate selection and cutoff determination procedure is rerun, and the
#'     frozen selected model is evaluated on the original dataset. Optimism is calculated as
#'     bootstrap performance minus original data performance.
#'   \item \code{"632"}: Efron's .632 bootstrap correction: \eqn{M_{.632} = 0.368 M_{app} + 0.632 M_{OOB}}.
#'     Out-of-bag (OOB) metric values are aggregated as the arithmetic mean across valid
#'     replicates (replicate-level mean). For sensitivity, specificity, and Youden index,
#'     a common valid replicate set is used so the identity \eqn{J_{.632} = Se_{.632} + Sp_{.632} - 1}
#'     is strictly preserved.
#'   \item \code{"632plus"}: Efron & Tibshirani's (1997) .632+ bootstrap correction.
#'     Relative overfitting rate \eqn{R = (M_{app} - M_{OOB}) / (M_{app} - \gamma)} is bounded
#'     to \eqn{[0, 1]}, yielding weight \eqn{w = 0.632 / (1 - 0.368 R) \in [0.632, 1]}, and
#'     corrected metric \eqn{M_{.632+} = (1 - w) M_{app} + w M_{OOB}}.
#'     For AUC, \eqn{\gamma = 0.5}.
#'     For sensitivity, specificity, and Youden index, an NCVROC experimental extension is applied:
#'     a common \eqn{R} and \eqn{w} are derived from the Youden index (\eqn{\gamma_J = 0}), and that
#'     weight is applied to sensitivity and specificity, ensuring \eqn{J_{.632+} = Se_{.632+} + Sp_{.632+} - 1}.
#'     This is an experimental extension, not an established standard .632+ estimator for sensitivity or specificity.
#' }
#'
#' @param data A data.frame containing item responses and the binary outcome column.
#'   Missing values in item columns or outcome are rejected by default.
#' @param outcome Character, name of the binary outcome column in \code{data}.
#' @param items Character vector of item column names to include in exhaustive search.
#' @param model_sizes Integer vector of combination sizes to explore. Default \code{1:6}.
#' @param bootstrap_reps Integer scalar, number of bootstrap resamples. Default \code{1000L}.
#' @param resampling Character, resampling scheme: \code{"stratified"} (preserves class counts, default)
#'   or \code{"ordinary"} (standard unstratified bootstrap).
#' @param selection_metric Character, candidate ranking metric: \code{"auc"} (default),
#'   \code{"youden"}, \code{"sensitivity"}, \code{"specificity"}, or \code{"accuracy"}.
#' @param cutoff_method Character, cutoff determination method: \code{"clinical_constraint"} (default),
#'   \code{"youden"}, or \code{"closest_topleft"}.
#' @param sensitivity_min Numeric threshold in \code{[0, 1]}. Required when \code{cutoff_method = "clinical_constraint"}.
#' @param specificity_min Numeric threshold in \code{[0, 1]}. Required when \code{cutoff_method = "clinical_constraint"}.
#' @param prefer_fewer_items Logical, whether to break metric ties in favor of models with fewer items. Default \code{TRUE}.
#' @param no_feasible Character, action when no candidate meets feasibility constraints:
#'   \code{"record_failure"} (default, records replicate as failed and treats metrics as NA) or
#'   \code{"fallback_youden"} (selects optimal Youden model as fallback and applies the same fallback rule to full data).
#' @param correction_methods Character vector specifying one or more bootstrap correction methods:
#'   \code{"harrell"} (default, Harrell-style optimism correction), \code{"632"} (Efron's .632 bootstrap correction),
#'   and/or \code{"632plus"} (Efron & Tibshirani's .632+ bootstrap correction). Default \code{"harrell"}.
#' @param positive_label Value in \code{outcome} indicating positive diagnosis. Default \code{1}.
#' @param negative_label Value in \code{outcome} indicating negative diagnosis. Default \code{0}.
#' @param engine Character, computation engine: \code{"Rcpp"} (default) or \code{"R"}.
#' @param parallel Character, parallel execution mode: \code{"none"} (default) or \code{"threads"}.
#' @param n_workers Integer, number of worker threads for parallel execution. Default \code{NULL} (auto-detect).
#' @param seed Integer scalar or NULL, random seed for reproducible bootstrap sampling.
#'   When supplied, the caller's pre-existing \code{.Random.seed} is preserved and restored on exit.
#' @param result_file Character path to RDS file for saving final results and periodic checkpoints. Default \code{NULL}.
#' @param checkpoint_every Integer, checkpoint interval in completed replicates. Default \code{25L}.
#' @param force_recompute Logical, if \code{TRUE} ignore existing compatible result files and recompute. Default \code{FALSE}.
#' @param progress Logical, whether to display progress. Default \code{interactive()}.
#'
#' @return An S3 object of class \code{"bootstrap_select_sum_roc_result"}.
#' @export
bootstrap_select_sum_roc <- function(
    data,
    outcome,
    items,
    model_sizes        = 1:6,
    bootstrap_reps     = 1000L,
    resampling         = c("stratified", "ordinary"),
    selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
    cutoff_method      = c("clinical_constraint", "youden", "closest_topleft"),
    sensitivity_min    = NULL,
    specificity_min    = NULL,
    prefer_fewer_items = TRUE,
    no_feasible        = c("record_failure", "fallback_youden"),
    correction_methods = "harrell",
    positive_label     = 1,
    negative_label     = 0,
    engine             = c("Rcpp", "R"),
    parallel           = c("none", "threads"),
    n_workers          = NULL,
    seed               = NULL,
    result_file        = NULL,
    checkpoint_every   = 25L,
    force_recompute    = FALSE,
    progress           = interactive()
) {
  # ---- 1. Argument validation ----
  resampling       <- match.arg(resampling)
  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  no_feasible      <- match.arg(no_feasible)
  engine           <- match.arg(engine)
  parallel         <- match.arg(parallel)

  valid_methods <- c("harrell", "632", "632plus")
  if (!is.character(correction_methods) || length(correction_methods) < 1L ||
      !all(correction_methods %in% valid_methods)) {
    stop("`correction_methods` must be a character vector containing one or more of: 'harrell', '632', '632plus'.", call. = FALSE)
  }
  correction_methods <- unique(correction_methods)
  need_oob <- any(c("632", "632plus") %in% correction_methods)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L || !(outcome %in% names(data))) {
    stop("`outcome` must name a single column in `data`.", call. = FALSE)
  }
  if (!is.character(items) || length(items) < 1L || !all(items %in% names(data))) {
    stop("`items` must be a character vector of column names in `data`.", call. = FALSE)
  }
  if (outcome %in% items) {
    stop("`outcome` column must not be included in `items`.", call. = FALSE)
  }

  # Reject missing values by default
  na_outcome <- is.na(data[[outcome]])
  na_items   <- rowSums(is.na(data[, items, drop = FALSE])) > 0
  if (any(na_outcome) || any(na_items)) {
    stop(sprintf("Missing values detected in data (outcome NAs: %d, item NAs: %d rows). Missing values must be addressed prior to calling `bootstrap_select_sum_roc()`.",
                 sum(na_outcome), sum(na_items)), call. = FALSE)
  }

  y_raw <- data[[outcome]]
  unique_y <- unique(y_raw)
  if (!all(unique_y %in% c(positive_label, negative_label))) {
    stop("`outcome` contains values outside `positive_label` and `negative_label`.", call. = FALSE)
  }
  y <- ifelse(y_raw == positive_label, 1L, 0L)
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  n_obs <- length(y)
  if (n_pos < 1L || n_neg < 1L) {
    stop("Data must contain at least one positive and one negative observation.", call. = FALSE)
  }

  # Model sizes validation
  model_sizes <- as.integer(sort(unique(model_sizes)))
  if (any(model_sizes < 1L) || any(model_sizes > length(items))) {
    stop(sprintf("`model_sizes` must be integers between 1 and length(items) (%d).", length(items)), call. = FALSE)
  }

  bootstrap_reps <- as.integer(bootstrap_reps)
  if (length(bootstrap_reps) != 1L || is.na(bootstrap_reps) || bootstrap_reps < 1L) {
    stop("`bootstrap_reps` must be a positive integer.", call. = FALSE)
  }

  # Constraints validation
  if (cutoff_method == "clinical_constraint") {
    if (is.null(sensitivity_min) || !is.numeric(sensitivity_min) || sensitivity_min < 0 || sensitivity_min > 1) {
      stop("`sensitivity_min` in [0, 1] is required when `cutoff_method = \"clinical_constraint\"`.", call. = FALSE)
    }
    if (is.null(specificity_min) || !is.numeric(specificity_min) || specificity_min < 0 || specificity_min > 1) {
      stop("`specificity_min` in [0, 1] is required when `cutoff_method = \"clinical_constraint\"`.", call. = FALSE)
    }
  }

  checkpoint_every <- as.integer(checkpoint_every)
  if (length(checkpoint_every) != 1L || is.na(checkpoint_every) || checkpoint_every < 1L) {
    checkpoint_every <- 25L
  }

  # Prepare numeric matrix for items
  x_mat <- as.matrix(data[, items, drop = FALSE])
  mode(x_mat) <- "numeric"

  # RNG state preservation
  has_seed <- !is.null(seed)
  if (has_seed) {
    seed <- as.integer(seed)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
      }
    }, add = TRUE)
  }

  # ---- 2. Result file compatibility check and resumption ----
  canonical_result_file <- NULL
  if (!is.null(result_file) && nzchar(result_file)) {
    canonical_result_file <- normalizePath(result_file, mustWork = FALSE)
  }

  settings_list <- list(
    outcome            = outcome,
    items              = items,
    model_sizes        = model_sizes,
    bootstrap_reps     = bootstrap_reps,
    resampling         = resampling,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    prefer_fewer_items = prefer_fewer_items,
    no_feasible        = no_feasible,
    correction_methods = correction_methods,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = parallel,
    n_workers          = n_workers,
    seed               = seed,
    checkpoint_every   = checkpoint_every
  )

  # Checkpoint identity signature
  raw_data_bytes <- serialize(list(x_mat, y), NULL, version = 3)
  raw_file <- tempfile()
  writeBin(raw_data_bytes, raw_file)
  data_hash <- unname(tools::md5sum(raw_file))
  unlink(raw_file)

  compat_key <- list(
    data_digest      = data_hash,
    settings         = settings_list,
    pkg_version      = as.character(utils::packageVersion("NCVROC"))
  )

  # Check if complete cached result exists
  if (!is.null(canonical_result_file) && file.exists(canonical_result_file) && !force_recompute) {
    cached_obj <- tryCatch(readRDS(canonical_result_file), error = function(e) NULL)
    if (is.list(cached_obj) && inherits(cached_obj$result, "bootstrap_select_sum_roc_result") &&
        .is_checkpoint_compatible(cached_obj$compat_key, compat_key)) {
      if (isTRUE(progress)) {
        message(sprintf("Loaded completed bootstrap_select_sum_roc result from '%s'.", canonical_result_file))
      }
      return(cached_obj$result)
    }
  }

  # Check if resumable checkpoint exists
  checkpoint_file <- NULL
  resumed_replicates <- list()
  if (!is.null(canonical_result_file)) {
    checkpoint_file <- paste0(canonical_result_file, ".checkpoint.rds")
    if (file.exists(checkpoint_file) && !force_recompute) {
      chk_obj <- tryCatch(readRDS(checkpoint_file), error = function(e) NULL)
      if (is.list(chk_obj) && .is_checkpoint_compatible(chk_obj$compat_key, compat_key)) {
        resumed_replicates <- chk_obj$replicates
        if (isTRUE(progress)) {
          message(sprintf("Resuming bootstrap analysis from checkpoint '%s' (%d of %d replicates completed).",
                          checkpoint_file, length(resumed_replicates), bootstrap_reps))
        }
      }
    }
  }

  # ---- 3. Precompute combinations and structures ----
  n_items <- length(items)
  all_combos_list <- list()
  for (sz in model_sizes) {
    sz_combos <- utils::combn(seq_len(n_items), sz, simplify = FALSE)
    all_combos_list <- c(all_combos_list, sz_combos)
  }
  n_combos_total <- length(all_combos_list)
  combo_names <- vapply(all_combos_list, function(idx) format_items(items[idx]), character(1L))
  combo_sizes <- vapply(all_combos_list, length, integer(1L))
  # 0-based column indices for C++
  combo_0based <- lapply(all_combos_list, function(idx) as.integer(idx - 1L))

  # ---- 4. Reference Model Selection on Full Data ----
  full_selection <- .select_optimal_model_internal(
    x_mat              = x_mat,
    y                  = y,
    combo_0based       = combo_0based,
    combo_names        = combo_names,
    combo_sizes        = combo_sizes,
    cutoff_method      = cutoff_method,
    selection_metric   = selection_metric,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    prefer_fewer_items = prefer_fewer_items,
    no_feasible        = no_feasible,
    engine             = engine
  )

  ref_model <- full_selection$selected_model
  apparent_perf <- full_selection$apparent_performance

  # ---- 5. Deterministic Bootstrap Sampling Generation ----
  if (has_seed) {
    set.seed(seed)
  }

  bootstrap_indices <- vector("list", bootstrap_reps)
  pos_indices <- which(y == 1L)
  neg_indices <- which(y == 0L)

  for (b in seq_len(bootstrap_reps)) {
    if (resampling == "stratified") {
      b_pos <- sample(pos_indices, size = length(pos_indices), replace = TRUE)
      b_neg <- sample(neg_indices, size = length(neg_indices), replace = TRUE)
      bootstrap_indices[[b]] <- sample(c(b_pos, b_neg)) # shuffle
    } else {
      bootstrap_indices[[b]] <- sample.int(n_obs, size = n_obs, replace = TRUE)
    }
  }

  # ---- 6. Bootstrap Replicate Evaluation ----
  replicate_results <- vector("list", bootstrap_reps)
  # Pre-fill already completed replicates from checkpoint
  if (length(resumed_replicates) > 0L) {
    for (i in seq_along(resumed_replicates)) {
      replicate_results[[i]] <- resumed_replicates[[i]]
    }
  }

  start_rep <- length(resumed_replicates) + 1L

  # Progress handler
  prg <- NULL
  if (isTRUE(progress) && start_rep <= bootstrap_reps) {
    prg <- .progress_make(total = bootstrap_reps, enabled = TRUE)
    if (start_rep > 1L) {
      prg$tick(start_rep - 1L)
    }
  }

  # Worker task function
  run_one_replicate <- function(b) {
    boot_rows <- bootstrap_indices[[b]]
    y_boot <- y[boot_rows]
    x_boot <- x_mat[boot_rows, , drop = FALSE]

    # OOB rows absent from bootstrap resample
    oob_rows <- which(!(seq_len(n_obs) %in% boot_rows))
    n_oob <- length(oob_rows)
    n_pos_oob <- if (n_oob > 0L) sum(y[oob_rows] == 1L) else 0L
    n_neg_oob <- if (n_oob > 0L) sum(y[oob_rows] == 0L) else 0L

    # Check if both classes present
    if (length(unique(y_boot)) < 2L) {
      return(list(
        replicate_id           = b,
        selected_items         = NA_character_,
        selected_n_items       = NA_integer_,
        selected_cutoff        = NA_real_,
        selection_status       = "invalid_evaluation",
        fallback_used          = FALSE,
        n_feasible_candidates  = 0L,
        training_auc           = NA_real_,
        training_sensitivity   = NA_real_,
        training_specificity   = NA_real_,
        training_youden        = NA_real_,
        test_auc               = NA_real_,
        test_sensitivity       = NA_real_,
        test_specificity       = NA_real_,
        test_youden            = NA_real_,
        optimism_auc           = NA_real_,
        optimism_sensitivity   = NA_real_,
        optimism_specificity   = NA_real_,
        optimism_youden        = NA_real_,
        training_feasible      = FALSE,
        test_feasible          = FALSE,
        exact_match            = FALSE,
        jaccard_similarity     = NA_real_,
        oob_n_total            = n_oob,
        oob_n_pos              = n_pos_oob,
        oob_n_neg              = n_neg_oob,
        oob_valid_auc          = FALSE,
        oob_valid_joint        = FALSE,
        oob_auc                = NA_real_,
        oob_sensitivity        = NA_real_,
        oob_specificity        = NA_real_,
        oob_youden             = NA_real_,
        oob_invalid_reason     = "selection_failed"
      ))
    }

    # Model selection on bootstrap sample
    b_sel <- .select_optimal_model_internal(
      x_mat              = x_boot,
      y                  = y_boot,
      combo_0based       = combo_0based,
      combo_names        = combo_names,
      combo_sizes        = combo_sizes,
      cutoff_method      = cutoff_method,
      selection_metric   = selection_metric,
      sensitivity_min    = sensitivity_min,
      specificity_min    = specificity_min,
      prefer_fewer_items = prefer_fewer_items,
      no_feasible        = no_feasible,
      engine             = engine
    )

    if (b_sel$status %in% c("no_feasible_candidate", "no_feasible_cutoff_on_full_data")) {
      return(list(
        replicate_id           = b,
        selected_items         = NA_character_,
        selected_n_items       = NA_integer_,
        selected_cutoff        = NA_real_,
        selection_status       = "no_feasible_candidate",
        fallback_used          = FALSE,
        n_feasible_candidates  = 0L,
        training_auc           = NA_real_,
        training_sensitivity   = NA_real_,
        training_specificity   = NA_real_,
        training_youden        = NA_real_,
        test_auc               = NA_real_,
        test_sensitivity       = NA_real_,
        test_specificity       = NA_real_,
        test_youden            = NA_real_,
        optimism_auc           = NA_real_,
        optimism_sensitivity   = NA_real_,
        optimism_specificity   = NA_real_,
        optimism_youden        = NA_real_,
        training_feasible      = FALSE,
        test_feasible          = FALSE,
        exact_match            = FALSE,
        jaccard_similarity     = NA_real_,
        oob_n_total            = n_oob,
        oob_n_pos              = n_pos_oob,
        oob_n_neg              = n_neg_oob,
        oob_valid_auc          = FALSE,
        oob_valid_joint        = FALSE,
        oob_auc                = NA_real_,
        oob_sensitivity        = NA_real_,
        oob_specificity        = NA_real_,
        oob_youden             = NA_real_,
        oob_invalid_reason     = "selection_failed"
      ))
    }

    # Selected model on bootstrap data
    b_mod <- b_sel$selected_model
    b_app <- b_sel$apparent_performance
    sel_items_str <- b_mod$items
    sel_items_vec <- .parse_itemset(sel_items_str)
    sel_cutoff    <- b_mod$cutoff

    # Apply selected model & cutoff to original data
    orig_scores <- rowSums(x_mat[, sel_items_vec, drop = FALSE])
    orig_freq   <- compute_score_frequencies(orig_scores, y)
    orig_auc    <- compute_auc_from_table(orig_freq$pos_counts, orig_freq$neg_counts)

    # Cutoff metrics on original data (strict score >= cutoff)
    orig_pred <- ifelse(orig_scores >= sel_cutoff, 1L, 0L)
    tp <- sum(orig_pred == 1L & y == 1L)
    fp <- sum(orig_pred == 1L & y == 0L)
    fn <- sum(orig_pred == 0L & y == 1L)
    tn <- sum(orig_pred == 0L & y == 0L)

    orig_sens <- tp / n_pos
    orig_spec <- tn / n_neg
    orig_youd <- orig_sens + orig_spec - 1.0

    # Optimism = Bootstrap - Original
    opt_auc  <- b_app$auc - orig_auc
    opt_sens <- b_app$sensitivity - orig_sens
    opt_spec <- b_app$specificity - orig_spec
    opt_youd <- b_app$youden - orig_youd

    # Feasibility on original data
    tr_feas <- if (cutoff_method == "clinical_constraint") {
      b_app$sensitivity >= sensitivity_min && b_app$specificity >= specificity_min
    } else TRUE

    test_feas <- if (cutoff_method == "clinical_constraint") {
      orig_sens >= sensitivity_min && orig_spec >= specificity_min
    } else TRUE

    # Exact match & Jaccard vs Reference Model
    ref_items_vec <- if (!is.null(ref_model) && !is.na(ref_model$items)) .parse_itemset(ref_model$items) else character(0)
    exact_m <- if (length(ref_items_vec) > 0) identical(sort(sel_items_vec), sort(ref_items_vec)) else FALSE
    jaccard <- if (length(ref_items_vec) > 0) {
      length(intersect(sel_items_vec, ref_items_vec)) / length(union(sel_items_vec, ref_items_vec))
    } else NA_real_

    # OOB evaluation
    oob_valid_auc <- FALSE
    oob_valid_joint <- FALSE
    oob_auc <- NA_real_
    oob_sens <- NA_real_
    oob_spec <- NA_real_
    oob_youd <- NA_real_
    oob_reason <- NA_character_

    if (!need_oob) {
      oob_reason <- "not_requested"
    } else if (n_oob == 0L) {
      oob_reason <- "empty_oob"
    } else if (n_pos_oob == 0L && n_neg_oob == 0L) {
      oob_reason <- "no_oob_samples"
    } else if (n_pos_oob == 0L) {
      oob_reason <- "no_oob_positives"
      oob_scores <- orig_scores[oob_rows]
      oob_pred <- ifelse(oob_scores >= sel_cutoff, 1L, 0L)
      oob_spec <- sum(oob_pred == 0L & y[oob_rows] == 0L) / n_neg_oob
    } else if (n_neg_oob == 0L) {
      oob_reason <- "no_oob_negatives"
      oob_scores <- orig_scores[oob_rows]
      oob_pred <- ifelse(oob_scores >= sel_cutoff, 1L, 0L)
      oob_sens <- sum(oob_pred == 1L & y[oob_rows] == 1L) / n_pos_oob
    } else {
      # Both classes present in OOB
      oob_scores <- orig_scores[oob_rows]
      y_oob <- y[oob_rows]

      # Unthresholded sum score AUC on OOB
      oob_freq <- compute_score_frequencies(oob_scores, y_oob)
      oob_auc <- compute_auc_from_table(oob_freq$pos_counts, oob_freq$neg_counts)
      oob_valid_auc <- !is.na(oob_auc)

      # Fixed selected cutoff for sensitivity and specificity
      oob_pred <- ifelse(oob_scores >= sel_cutoff, 1L, 0L)
      tp_oob <- sum(oob_pred == 1L & y_oob == 1L)
      tn_oob <- sum(oob_pred == 0L & y_oob == 0L)
      oob_sens <- tp_oob / n_pos_oob
      oob_spec <- tn_oob / n_neg_oob
      oob_youd <- oob_sens + oob_spec - 1.0
      oob_valid_joint <- TRUE
    }

    list(
      replicate_id           = b,
      selected_items         = sel_items_str,
      selected_n_items       = b_mod$n_items,
      selected_cutoff        = sel_cutoff,
      selection_status       = b_sel$status,
      fallback_used          = b_sel$fallback_used,
      n_feasible_candidates  = b_sel$n_feasible,
      training_auc           = b_app$auc,
      training_sensitivity   = b_app$sensitivity,
      training_specificity   = b_app$specificity,
      training_youden        = b_app$youden,
      test_auc               = orig_auc,
      test_sensitivity       = orig_sens,
      test_specificity       = orig_spec,
      test_youden            = orig_youd,
      optimism_auc           = opt_auc,
      optimism_sensitivity   = opt_sens,
      optimism_specificity   = opt_spec,
      optimism_youden        = opt_youd,
      training_feasible      = tr_feas,
      test_feasible          = test_feas,
      exact_match            = exact_m,
      jaccard_similarity     = jaccard,
      oob_n_total            = n_oob,
      oob_n_pos              = n_pos_oob,
      oob_n_neg              = n_neg_oob,
      oob_valid_auc          = oob_valid_auc,
      oob_valid_joint        = oob_valid_joint,
      oob_auc                = oob_auc,
      oob_sensitivity        = oob_sens,
      oob_specificity        = oob_spec,
      oob_youden             = oob_youd,
      oob_invalid_reason     = oob_reason
    )
  }

  # Execution loop (Native C++/TBB when engine == "Rcpp", else R Serial/PSOCK)
  if (start_rep <= bootstrap_reps) {
    if (identical(engine, "Rcpp")) {
      eff_threads <- if (identical(parallel, "threads")) {
        nc <- if (is.null(n_workers)) parallel::detectCores(logical = FALSE) else as.integer(n_workers)
        if (is.na(nc) || nc < 1L) 1L else nc
      } else {
        1L
      }
      sens_min_c <- if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else -1.0
      spec_min_c <- if (!is.null(specificity_min)) as.numeric(specificity_min) else -1.0
      ref_items_vec <- if (!is.null(ref_model) && !is.na(ref_model$items)) .parse_itemset(ref_model$items) else character(0)
      ref_items_0based <- if (length(ref_items_vec) > 0L) as.integer(match(ref_items_vec, items) - 1L) else integer(0)
      status_labels <- c("selected", "fallback_youden", "no_feasible_candidate", "invalid_evaluation")
      oob_reason_labels <- c(
        NA_character_, "not_requested", "empty_oob", "no_oob_samples",
        "no_oob_positives", "no_oob_negatives", "selection_failed"
      )

      cur_start <- start_rep
      while (cur_start <= bootstrap_reps) {
        if (isTRUE(check_user_interrupt_cpp())) {
          stop("Bootstrap evaluation interrupted by user.", call. = FALSE)
        }
        cur_end <- min(cur_start + checkpoint_every - 1L, bootstrap_reps)
        batch_ids <- cur_start:cur_end
        boot_wave_0 <- lapply(bootstrap_indices[batch_ids], function(v) as.integer(v) - 1L)

        wave_df <- evaluate_bootstrap_replicates_native_cpp(
          x                 = x_mat,
          y                 = as.integer(y),
          boot_indices_list = boot_wave_0,
          combo_0based      = combo_0based,
          cutoff_method     = cutoff_method,
          selection_metric  = selection_metric,
          sensitivity_min   = sens_min_c,
          specificity_min   = spec_min_c,
          prefer_fewer_items = isTRUE(prefer_fewer_items),
          fallback_youden   = identical(no_feasible, "fallback_youden"),
          need_oob          = isTRUE(need_oob),
          ref_items_0based  = ref_items_0based,
          num_threads       = as.integer(eff_threads)
        )

        for (idx in seq_along(batch_ids)) {
          b_id <- batch_ids[idx]
          c_idx <- wave_df$selected_combo_idx[idx]
          has_sel <- !is.na(c_idx)
          replicate_results[[b_id]] <- list(
            replicate_id           = b_id,
            selected_items         = if (has_sel) combo_names[c_idx] else NA_character_,
            selected_n_items       = if (has_sel) as.integer(combo_sizes[c_idx]) else NA_integer_,
            selected_cutoff        = wave_df$selected_cutoff[idx],
            selection_status       = status_labels[wave_df$status_code[idx] + 1L],
            fallback_used          = wave_df$fallback_used[idx],
            n_feasible_candidates  = wave_df$n_feasible_candidates[idx],
            training_auc           = wave_df$training_auc[idx],
            training_sensitivity   = wave_df$training_sensitivity[idx],
            training_specificity   = wave_df$training_specificity[idx],
            training_youden        = wave_df$training_youden[idx],
            test_auc               = wave_df$test_auc[idx],
            test_sensitivity       = wave_df$test_sensitivity[idx],
            test_specificity       = wave_df$test_specificity[idx],
            test_youden            = wave_df$test_youden[idx],
            optimism_auc           = wave_df$optimism_auc[idx],
            optimism_sensitivity   = wave_df$optimism_sensitivity[idx],
            optimism_specificity   = wave_df$optimism_specificity[idx],
            optimism_youden        = wave_df$optimism_youden[idx],
            training_feasible      = wave_df$training_feasible[idx],
            test_feasible          = wave_df$test_feasible[idx],
            exact_match            = wave_df$exact_match[idx],
            jaccard_similarity     = wave_df$jaccard_similarity[idx],
            oob_n_total            = wave_df$oob_n_total[idx],
            oob_n_pos              = wave_df$oob_n_pos[idx],
            oob_n_neg              = wave_df$oob_n_neg[idx],
            oob_valid_auc          = wave_df$oob_valid_auc[idx],
            oob_valid_joint        = wave_df$oob_valid_joint[idx],
            oob_auc                = wave_df$oob_auc[idx],
            oob_sensitivity        = wave_df$oob_sensitivity[idx],
            oob_specificity        = wave_df$oob_specificity[idx],
            oob_youden             = wave_df$oob_youden[idx],
            oob_invalid_reason     = oob_reason_labels[wave_df$oob_reason_code[idx] + 1L]
          )
          if (!is.null(prg)) prg$tick()
        }

        if (!is.null(checkpoint_file)) {
          .save_atomic_rds(
            list(compat_key = compat_key, settings = settings_list, replicates = replicate_results[1:cur_end]),
            checkpoint_file
          )
        }
        cur_start <- cur_end + 1L
      }
    } else if (parallel == "threads") {
      n_cores <- if (is.null(n_workers)) parallel::detectCores(logical = FALSE) else as.integer(n_workers)
      if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L

      # Batch across replicates to allow checkpoints
      cur_start <- start_rep
      while (cur_start <= bootstrap_reps) {
        cur_end <- min(cur_start + checkpoint_every - 1L, bootstrap_reps)
        batch_ids <- cur_start:cur_end

        # Run batch in parallel using parLapply or mclapply (PSOCK for Windows compatibility)
        cl <- parallel::makePSOCKcluster(min(n_cores, length(batch_ids)))
        parallel::clusterExport(
          cl,
          varlist = c("x_mat", "y", "combo_0based", "combo_names", "combo_sizes",
                      "cutoff_method", "selection_metric", "sensitivity_min", "specificity_min",
                      "prefer_fewer_items", "no_feasible", "need_oob", "engine", "bootstrap_indices",
                      "ref_model", "n_pos", "n_neg", "n_obs", ".select_optimal_model_internal",
                      ".parse_itemset", "compute_score_frequencies", "compute_auc_from_table",
                      "compute_roc_metrics_from_table", "find_optimal_cutoff",
                      "evaluate_combos_cpp", "format_items"),
          envir = environment()
        )

        batch_res <- parallel::parLapply(cl, batch_ids, run_one_replicate)
        parallel::stopCluster(cl)

        for (idx in seq_along(batch_ids)) {
          b_id <- batch_ids[idx]
          replicate_results[[b_id]] <- batch_res[[idx]]
          if (!is.null(prg)) prg$tick()
        }

        # Checkpoint save
        if (!is.null(checkpoint_file)) {
          .save_atomic_rds(
            list(compat_key = compat_key, settings = settings_list, replicates = replicate_results[1:cur_end]),
            checkpoint_file
          )
        }

        cur_start <- cur_end + 1L
      }
    } else {
      # Serial execution
      for (b in start_rep:bootstrap_reps) {
        replicate_results[[b]] <- run_one_replicate(b)
        if (!is.null(prg)) prg$tick()

        if (!is.null(checkpoint_file) && (b %% checkpoint_every == 0L || b == bootstrap_reps)) {
          .save_atomic_rds(
            list(compat_key = compat_key, settings = settings_list, replicates = replicate_results[1:b]),
            checkpoint_file
          )
        }
      }
    }
  }

  if (!is.null(prg)) prg$finish()

  # ---- 7. Aggregate Results & Summaries ----
  replicates_df <- do.call(rbind, lapply(replicate_results, as.data.frame, stringsAsFactors = FALSE))

  # Categorize replicates
  is_valid <- replicates_df$selection_status %in% c("selected", "fallback_youden")
  n_valid  <- sum(is_valid)
  n_failed <- sum(!is_valid)
  n_fallback <- sum(replicates_df$fallback_used)

  # Mean optimism over valid replicates
  mean_optimism <- c(
    auc         = if (n_valid > 0L) mean(replicates_df$optimism_auc[is_valid]) else NA_real_,
    sensitivity = if (n_valid > 0L) mean(replicates_df$optimism_sensitivity[is_valid]) else NA_real_,
    specificity = if (n_valid > 0L) mean(replicates_df$optimism_specificity[is_valid]) else NA_real_,
    youden      = if (n_valid > 0L) mean(replicates_df$optimism_youden[is_valid]) else NA_real_
  )

  # Corrected performance (Harrell)
  # If any replicate failed under record_failure, unconditional corrected estimate is NA
  has_unconditional_estimate <- (n_failed == 0L) && !is.null(ref_model) && !is.na(ref_model$items)

  harrell_perf <- list(
    auc         = if (has_unconditional_estimate) apparent_perf$auc - mean_optimism["auc"] else NA_real_,
    sensitivity = if (has_unconditional_estimate) apparent_perf$sensitivity - mean_optimism["sensitivity"] else NA_real_,
    specificity = if (has_unconditional_estimate) apparent_perf$specificity - mean_optimism["specificity"] else NA_real_,
    youden      = if (has_unconditional_estimate) apparent_perf$youden - mean_optimism["youden"] else NA_real_,
    is_conditional = (n_failed > 0L),
    unconditional_available = has_unconditional_estimate,
    requested   = "harrell" %in% correction_methods
  )

  # For backwards compatibility, result$corrected_performance remains Harrell
  corrected_perf <- harrell_perf

  # Check theoretical boundary warnings for Harrell
  if (!is.na(corrected_perf$auc) && (corrected_perf$auc < 0 || corrected_perf$auc > 1)) {
    warning(sprintf("Corrected AUC (%.4f) falls outside theoretical range [0, 1]. Retaining raw value.", corrected_perf$auc), call. = FALSE)
  }
  if (!is.na(corrected_perf$sensitivity) && (corrected_perf$sensitivity < 0 || corrected_perf$sensitivity > 1)) {
    warning(sprintf("Corrected sensitivity (%.4f) falls outside [0, 1]. Retaining raw value.", corrected_perf$sensitivity), call. = FALSE)
  }
  if (!is.na(corrected_perf$specificity) && (corrected_perf$specificity < 0 || corrected_perf$specificity > 1)) {
    warning(sprintf("Corrected specificity (%.4f) falls outside [0, 1]. Retaining raw value.", corrected_perf$specificity), call. = FALSE)
  }
  if (!is.na(corrected_perf$youden) && (corrected_perf$youden < -1 || corrected_perf$youden > 1)) {
    warning(sprintf("Corrected Youden index (%.4f) falls outside [-1, 1]. Retaining raw value.", corrected_perf$youden), call. = FALSE)
  }

  # OOB aggregation
  oob_valid_auc_mask   <- is_valid & replicates_df$oob_valid_auc
  oob_valid_joint_mask <- is_valid & replicates_df$oob_valid_joint

  n_oob_valid_auc   <- sum(oob_valid_auc_mask)
  n_oob_valid_joint <- sum(oob_valid_joint_mask)

  mean_oob_auc  <- if (n_oob_valid_auc > 0L) mean(replicates_df$oob_auc[oob_valid_auc_mask]) else NA_real_
  mean_oob_sens <- if (n_oob_valid_joint > 0L) mean(replicates_df$oob_sensitivity[oob_valid_joint_mask]) else NA_real_
  mean_oob_spec <- if (n_oob_valid_joint > 0L) mean(replicates_df$oob_specificity[oob_valid_joint_mask]) else NA_real_
  mean_oob_youd <- if (n_oob_valid_joint > 0L) mean(replicates_df$oob_youden[oob_valid_joint_mask]) else NA_real_

  mean_oob <- c(
    auc         = mean_oob_auc,
    sensitivity = mean_oob_sens,
    specificity = mean_oob_spec,
    youden      = mean_oob_youd
  )

  oob_invalid_counts <- table(replicates_df$oob_invalid_reason)

  oob_summary <- list(
    mean_oob            = mean_oob,
    n_valid_auc         = n_oob_valid_auc,
    n_valid_joint       = n_oob_valid_joint,
    n_pos_distribution  = summary(replicates_df$oob_n_pos),
    n_neg_distribution  = summary(replicates_df$oob_n_neg),
    invalid_reasons     = as.list(oob_invalid_counts)
  )

  # .632 calculation
  b632_requested <- "632" %in% correction_methods
  b632_auc  <- if (b632_requested && !is.null(ref_model) && !is.na(ref_model$items) && !is.na(mean_oob_auc)) {
    if (n_failed == 0L) 0.368 * apparent_perf$auc + 0.632 * mean_oob_auc else NA_real_
  } else NA_real_

  b632_sens <- if (b632_requested && !is.null(ref_model) && !is.na(ref_model$items) && !is.na(mean_oob_sens)) {
    if (n_failed == 0L) 0.368 * apparent_perf$sensitivity + 0.632 * mean_oob_sens else NA_real_
  } else NA_real_

  b632_spec <- if (b632_requested && !is.null(ref_model) && !is.na(ref_model$items) && !is.na(mean_oob_spec)) {
    if (n_failed == 0L) 0.368 * apparent_perf$specificity + 0.632 * mean_oob_spec else NA_real_
  } else NA_real_

  b632_youd <- if (b632_requested && !is.null(ref_model) && !is.na(ref_model$items) && !is.na(mean_oob_youd)) {
    if (n_failed == 0L) 0.368 * apparent_perf$youden + 0.632 * mean_oob_youd else NA_real_
  } else NA_real_

  b632_perf <- list(
    auc                     = b632_auc,
    sensitivity             = b632_sens,
    specificity             = b632_spec,
    youden                  = b632_youd,
    is_conditional          = (n_failed > 0L) || (n_oob_valid_joint < bootstrap_reps),
    unconditional_available = (n_failed == 0L) && !is.na(b632_youd) && !is.na(b632_auc),
    requested               = b632_requested
  )

  if (b632_requested) {
    if (!is.na(b632_perf$auc) && (b632_perf$auc < 0 || b632_perf$auc > 1)) {
      warning(sprintf(".632 AUC (%.4f) falls outside theoretical range [0, 1]. Retaining raw value.", b632_perf$auc), call. = FALSE)
    }
    if (!is.na(b632_perf$sensitivity) && (b632_perf$sensitivity < 0 || b632_perf$sensitivity > 1)) {
      warning(sprintf(".632 sensitivity (%.4f) falls outside [0, 1]. Retaining raw value.", b632_perf$sensitivity), call. = FALSE)
    }
    if (!is.na(b632_perf$specificity) && (b632_perf$specificity < 0 || b632_perf$specificity > 1)) {
      warning(sprintf(".632 specificity (%.4f) falls outside [0, 1]. Retaining raw value.", b632_perf$specificity), call. = FALSE)
    }
    if (!is.na(b632_perf$youden) && (b632_perf$youden < -1 || b632_perf$youden > 1)) {
      warning(sprintf(".632 Youden index (%.4f) falls outside [-1, 1]. Retaining raw value.", b632_perf$youden), call. = FALSE)
    }
  }

  # .632+ calculation & boundary handling
  b632plus_requested <- "632plus" %in% correction_methods

  boundary_counts <- list(
    auc_r_clipped_zero    = 0L,
    auc_r_clipped_one     = 0L,
    auc_denom_nonpos      = 0L,
    youden_r_clipped_zero = 0L,
    youden_r_clipped_one  = 0L,
    youden_denom_nonpos   = 0L
  )
  boundary_notes <- character(0)

  # AUC: gamma = 0.5
  R_auc <- NA_real_
  w_auc <- NA_real_
  b632plus_auc <- NA_real_

  if (b632plus_requested && !is.null(ref_model) && !is.na(ref_model$items)) {
    if (is.na(apparent_perf$auc) || is.na(mean_oob_auc)) {
      boundary_notes <- c(boundary_notes, "AUC .632+: OOB or apparent AUC is NA.")
    } else {
      denom_auc <- apparent_perf$auc - 0.5
      if (denom_auc <= 1e-12) {
        boundary_counts$auc_denom_nonpos <- 1L
        boundary_notes <- c(boundary_notes, "AUC .632+: Apparent AUC does not exceed chance gamma=0.5; R is undefined.")
        warning("Apparent AUC does not exceed chance gamma = 0.5; .632+ relative overfitting rate is undefined.", call. = FALSE)
      } else {
        r_raw_auc <- (apparent_perf$auc - mean_oob_auc) / denom_auc
        if (r_raw_auc <= 0.0) {
          boundary_counts$auc_r_clipped_zero <- 1L
          R_auc <- 0.0
        } else if (r_raw_auc >= 1.0) {
          boundary_counts$auc_r_clipped_one <- 1L
          R_auc <- 1.0
        } else {
          R_auc <- r_raw_auc
        }
        w_auc <- 0.632 / (1.0 - 0.368 * R_auc)
        w_auc <- min(max(w_auc, 0.632), 1.0)
        if (n_failed == 0L) {
          b632plus_auc <- (1.0 - w_auc) * apparent_perf$auc + w_auc * mean_oob_auc
        }
      }
    }
  }

  # Sensitivity, Specificity, Youden: common R and w derived from Youden (gamma_J = 0)
  R_youden <- NA_real_
  w_youden <- NA_real_
  b632plus_sens <- NA_real_
  b632plus_spec <- NA_real_
  b632plus_youd <- NA_real_

  if (b632plus_requested && !is.null(ref_model) && !is.na(ref_model$items)) {
    if (is.na(apparent_perf$youden) || is.na(mean_oob_youd)) {
      boundary_notes <- c(boundary_notes, "Joint .632+: OOB or apparent Youden is NA.")
    } else {
      denom_youd <- apparent_perf$youden - 0.0
      if (denom_youd <= 1e-12) {
        boundary_counts$youden_denom_nonpos <- 1L
        boundary_notes <- c(boundary_notes, "Joint .632+: Apparent Youden does not exceed chance gamma_J=0; R is undefined.")
        warning("Apparent Youden index does not exceed chance gamma = 0; .632+ relative overfitting rate is undefined.", call. = FALSE)
      } else {
        r_raw_youd <- (apparent_perf$youden - mean_oob_youd) / denom_youd
        if (r_raw_youd <= 0.0) {
          boundary_counts$youden_r_clipped_zero <- 1L
          R_youden <- 0.0
        } else if (r_raw_youd >= 1.0) {
          boundary_counts$youden_r_clipped_one <- 1L
          R_youden <- 1.0
        } else {
          R_youden <- r_raw_youd
        }
        w_youden <- 0.632 / (1.0 - 0.368 * R_youden)
        w_youden <- min(max(w_youden, 0.632), 1.0)
        if (n_failed == 0L && !is.na(mean_oob_sens) && !is.na(mean_oob_spec)) {
          b632plus_sens <- (1.0 - w_youden) * apparent_perf$sensitivity + w_youden * mean_oob_sens
          b632plus_spec <- (1.0 - w_youden) * apparent_perf$specificity + w_youden * mean_oob_spec
          b632plus_youd <- b632plus_sens + b632plus_spec - 1.0
        }
      }
    }
  }

  b632plus_perf <- list(
    auc                     = b632plus_auc,
    sensitivity             = b632plus_sens,
    specificity             = b632plus_spec,
    youden                  = b632plus_youd,
    is_conditional          = (n_failed > 0L) || (n_oob_valid_joint < bootstrap_reps),
    unconditional_available = (n_failed == 0L) && !is.na(b632plus_youd) && !is.na(b632plus_auc),
    r_auc                   = R_auc,
    w_auc                   = w_auc,
    r_youden                = R_youden,
    w_youden                = w_youden,
    boundary_notes          = boundary_notes,
    requested               = b632plus_requested
  )

  if (b632plus_requested) {
    if (!is.na(b632plus_perf$auc) && (b632plus_perf$auc < 0 || b632plus_perf$auc > 1)) {
      warning(sprintf(".632+ AUC (%.4f) falls outside theoretical range [0, 1]. Retaining raw value.", b632plus_perf$auc), call. = FALSE)
    }
    if (!is.na(b632plus_perf$sensitivity) && (b632plus_perf$sensitivity < 0 || b632plus_perf$sensitivity > 1)) {
      warning(sprintf(".632+ sensitivity (%.4f) falls outside [0, 1]. Retaining raw value.", b632plus_perf$sensitivity), call. = FALSE)
    }
    if (!is.na(b632plus_perf$specificity) && (b632plus_perf$specificity < 0 || b632plus_perf$specificity > 1)) {
      warning(sprintf(".632+ specificity (%.4f) falls outside [0, 1]. Retaining raw value.", b632plus_perf$specificity), call. = FALSE)
    }
    if (!is.na(b632plus_perf$youden) && (b632plus_perf$youden < -1 || b632plus_perf$youden > 1)) {
      warning(sprintf(".632+ Youden index (%.4f) falls outside [-1, 1]. Retaining raw value.", b632plus_perf$youden), call. = FALSE)
    }
  }

  overfitting_diag <- list(
    r_auc            = R_auc,
    w_auc            = w_auc,
    r_youden         = R_youden,
    w_youden         = w_youden,
    boundary_counts  = boundary_counts,
    boundary_notes   = boundary_notes
  )

  corrections_list <- list(
    harrell  = harrell_perf,
    b632     = b632_perf,
    b632plus = b632plus_perf
  )

  # Selection stability calculations
  # Combination frequencies
  sel_combos_all <- replicates_df$selected_items
  combo_counts <- table(sel_combos_all[!is.na(sel_combos_all)])
  combo_freq_df <- data.frame(
    items            = names(combo_counts),
    count            = as.integer(combo_counts),
    proportion_valid = if (n_valid > 0L) as.numeric(combo_counts) / n_valid else 0.0,
    proportion_all   = as.numeric(combo_counts) / bootstrap_reps,
    stringsAsFactors = FALSE
  )
  combo_freq_df$n_items <- vapply(combo_freq_df$items, function(s) length(.parse_itemset(s)), integer(1L))
  combo_freq_df <- combo_freq_df[order(-combo_freq_df$count, combo_freq_df$n_items, combo_freq_df$items), , drop = FALSE]
  rownames(combo_freq_df) <- NULL

  # Item frequencies
  item_counts_valid <- integer(length(items))
  names(item_counts_valid) <- items
  if (n_valid > 0L) {
    for (it_str in replicates_df$selected_items[is_valid]) {
      v <- .parse_itemset(it_str)
      item_counts_valid[v] <- item_counts_valid[v] + 1L
    }
  }

  item_freq_df <- data.frame(
    item            = items,
    count_valid     = unname(item_counts_valid),
    frequency_valid = if (n_valid > 0L) unname(item_counts_valid) / n_valid else 0.0,
    frequency_all   = unname(item_counts_valid) / bootstrap_reps,
    stringsAsFactors = FALSE
  )
  item_freq_df <- item_freq_df[order(-item_freq_df$count_valid, item_freq_df$item), , drop = FALSE]
  rownames(item_freq_df) <- NULL

  # Stability statistics
  jacc_vals <- replicates_df$jaccard_similarity[is_valid]
  jacc_vals <- jacc_vals[!is.na(jacc_vals)]

  exact_m_count <- sum(replicates_df$exact_match[is_valid])
  exact_m_rate  <- if (n_valid > 0L) exact_m_count / n_valid else 0.0

  stability_summary <- list(
    exact_match_rate   = exact_m_rate,
    n_exact_match      = exact_m_count,
    n_reps_denominator = n_valid,
    jaccard_mean       = if (length(jacc_vals) > 0L) mean(jacc_vals) else NA_real_,
    jaccard_sd         = if (length(jacc_vals) > 1L) stats::sd(jacc_vals) else 0.0,
    jaccard_median     = if (length(jacc_vals) > 0L) stats::median(jacc_vals) else NA_real_,
    jaccard_iqr        = if (length(jacc_vals) > 0L) stats::IQR(jacc_vals) else NA_real_,
    jaccard_min        = if (length(jacc_vals) > 0L) min(jacc_vals) else NA_real_,
    jaccard_max        = if (length(jacc_vals) > 0L) max(jacc_vals) else NA_real_
  )

  # Constraint stability
  constraint_summary <- list(
    training_feasible_count    = sum(replicates_df$training_feasible),
    training_feasible_rate_all = mean(replicates_df$training_feasible),
    test_feasible_count        = sum(replicates_df$test_feasible),
    test_feasible_rate_all     = mean(replicates_df$test_feasible),
    fallback_count             = n_fallback,
    fallback_rate_all          = n_fallback / bootstrap_reps
  )

  # Cutoff distribution
  cutoff_dist <- table(replicates_df$selected_cutoff[is_valid])
  cutoff_dist_df <- data.frame(
    cutoff    = as.numeric(names(cutoff_dist)),
    count     = as.integer(cutoff_dist),
    frequency = as.numeric(cutoff_dist) / n_valid,
    stringsAsFactors = FALSE
  )
  cutoff_dist_df <- cutoff_dist_df[order(cutoff_dist_df$cutoff), , drop = FALSE]
  rownames(cutoff_dist_df) <- NULL

  # Result construction
  result <- list(
    final_selected_model     = ref_model,
    apparent_performance     = apparent_perf,
    bootstrap_summary        = list(
      n_reps_requested       = bootstrap_reps,
      n_reps_valid           = n_valid,
      n_reps_failed          = n_failed,
      n_reps_fallback        = n_fallback,
      mean_optimism          = mean_optimism,
      n_reps_oob_valid_auc   = n_oob_valid_auc,
      n_reps_oob_valid_joint = n_oob_valid_joint
    ),
    corrected_performance    = corrected_perf,
    corrections              = corrections_list,
    oob_summary              = oob_summary,
    overfitting_diagnostics  = overfitting_diag,
    bootstrap_results        = replicates_df,
    selection_frequency      = combo_freq_df,
    item_selection_frequency = item_freq_df,
    selection_stability      = stability_summary,
    constraint_stability     = constraint_summary,
    cutoff_distribution      = cutoff_dist_df,
    settings                 = settings_list
  )
  class(result) <- "bootstrap_select_sum_roc_result"

  # Final result persistence
  if (!is.null(canonical_result_file)) {
    .save_atomic_rds(list(compat_key = compat_key, settings = settings_list, result = result), canonical_result_file)
    # Clean up checkpoint file if final result written successfully
    if (!is.null(checkpoint_file) && file.exists(checkpoint_file)) {
      unlink(checkpoint_file, force = TRUE)
    }
  }

  result
}

# ------------------------------------------------------------------------------
# Internal helper functions
# ------------------------------------------------------------------------------

#' Internal model selection helper for exhaustive combination search
#' @keywords internal
#' @noRd
.select_optimal_model_internal <- function(
    x_mat,
    y,
    combo_0based,
    combo_names,
    combo_sizes,
    cutoff_method,
    selection_metric,
    sensitivity_min,
    specificity_min,
    prefer_fewer_items,
    no_feasible,
    engine
) {
  n_combos <- length(combo_0based)
  n_obs    <- length(y)
  n_pos    <- sum(y == 1L)
  n_neg    <- sum(y == 0L)

  eval_df <- NULL

  if (engine == "Rcpp") {
    # Call exported evaluate_combos_cpp with constraints
    sens_min_c <- if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else -1.0
    spec_min_c <- if (!is.null(specificity_min)) as.numeric(specificity_min) else -1.0
    eval_df <- evaluate_combos_cpp(x_mat, y, combo_0based, cutoff_method, sens_min_c, spec_min_c)
  } else {
    # Pure R fallback
    res_list <- vector("list", n_combos)
    for (i in seq_len(n_combos)) {
      c_cols <- combo_0based[[i]] + 1L
      scores <- rowSums(x_mat[, c_cols, drop = FALSE])
      freq <- compute_score_frequencies(scores, y)
      auc_val <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)
      metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
      best_cut <- find_optimal_cutoff(metrics, method = cutoff_method,
                                      sensitivity_min = sensitivity_min,
                                      specificity_min = specificity_min)

      res_list[[i]] <- data.frame(
        n_items     = combo_sizes[i],
        auc         = auc_val,
        cutoff      = if (!is.null(best_cut)) best_cut$cutoff else NA_real_,
        sensitivity = if (!is.null(best_cut)) best_cut$sensitivity else NA_real_,
        specificity = if (!is.null(best_cut)) best_cut$specificity else NA_real_,
        youden      = if (!is.null(best_cut)) best_cut$youden else NA_real_,
        accuracy    = if (!is.null(best_cut)) best_cut$accuracy else NA_real_,
        ppv         = if (!is.null(best_cut)) best_cut$ppv else NA_real_,
        npv         = if (!is.null(best_cut)) best_cut$npv else NA_real_,
        n_positive  = n_pos,
        n_negative  = n_neg,
        stringsAsFactors = FALSE
      )
    }
    eval_df <- do.call(rbind, res_list)
  }

  eval_df$items <- combo_names
  eval_df$.global_combo_index <- seq_len(n_combos)

  # Check clinical constraints
  feasible_mask <- rep(TRUE, n_combos)
  if (cutoff_method == "clinical_constraint") {
    feasible_mask <- !is.na(eval_df$cutoff) &
      (eval_df$sensitivity >= sensitivity_min) &
      (eval_df$specificity >= specificity_min)
  }

  n_feasible <- sum(feasible_mask)
  fallback_used <- FALSE
  status <- "selected"

  if (n_feasible == 0L) {
    if (no_feasible == "fallback_youden") {
      # Select model by Youden optimal cutoff
      fallback_used <- TRUE
      status <- "fallback_youden"

      if (cutoff_method == "clinical_constraint") {
        # Recalculate cutoffs using Youden method
        youd_res <- evaluate_combos_cpp(x_mat, y, combo_0based, "youden")
        youd_res$items <- combo_names
        youd_res$.global_combo_index <- seq_len(n_combos)
        ranked <- .order_and_rank_candidates(youd_res, rank_by = selection_metric, prefer_fewer_items = prefer_fewer_items)
      } else {
        ranked <- .order_and_rank_candidates(eval_df, rank_by = selection_metric, prefer_fewer_items = prefer_fewer_items)
      }
      best_row <- ranked[1, , drop = FALSE]
    } else {
      # record_failure
      return(list(
        selected_model       = NULL,
        apparent_performance = NULL,
        status               = "no_feasible_candidate",
        fallback_used        = FALSE,
        n_feasible           = 0L
      ))
    }
  } else {
    feas_candidates <- eval_df[feasible_mask, , drop = FALSE]
    ranked <- .order_and_rank_candidates(feas_candidates, rank_by = selection_metric, prefer_fewer_items = prefer_fewer_items)
    best_row <- ranked[1, , drop = FALSE]
  }

  best_model <- list(
    items       = best_row$items,
    n_items     = as.integer(best_row$n_items),
    cutoff      = as.numeric(best_row$cutoff),
    global_rank = as.integer(best_row$.global_combo_index)
  )

  apparent_perf <- list(
    auc         = as.numeric(best_row$auc),
    sensitivity = as.numeric(best_row$sensitivity),
    specificity = as.numeric(best_row$specificity),
    youden      = as.numeric(best_row$youden),
    accuracy    = as.numeric(best_row$accuracy),
    ppv         = as.numeric(best_row$ppv),
    npv         = as.numeric(best_row$npv)
  )

  list(
    selected_model       = best_model,
    apparent_performance = apparent_perf,
    status               = status,
    fallback_used        = fallback_used,
    n_feasible           = n_feasible
  )
}

#' Check if checkpoint compatibility key matches current invocation
#' @keywords internal
#' @noRd
.is_checkpoint_compatible <- function(chk_key, cur_key) {
  if (is.null(chk_key) || is.null(cur_key)) return(FALSE)

  # 1. Compare package specification version
  if (!identical(chk_key$pkg_version, cur_key$pkg_version)) {
    return(FALSE)
  }

  # 2. Compare data content digest
  if (!identical(chk_key$data_digest, cur_key$data_digest)) {
    return(FALSE)
  }

  # 3. Compare analysis settings
  chk_sett <- chk_key$settings
  cur_sett <- cur_key$settings
  if (is.null(chk_sett) || is.null(cur_sett)) return(FALSE)

  keys_to_compare <- c("outcome", "items", "model_sizes", "bootstrap_reps",
                       "resampling", "selection_metric", "cutoff_method",
                       "sensitivity_min", "specificity_min", "prefer_fewer_items",
                       "no_feasible", "correction_methods", "positive_label", "negative_label",
                       "engine", "parallel", "n_workers", "seed")
  for (k in keys_to_compare) {
    if (!identical(chk_sett[[k]], cur_sett[[k]])) {
      return(FALSE)
    }
  }
  TRUE
}

#' Atomically write RDS file via temporary file replacement
#' @keywords internal
#' @noRd
.save_atomic_rds <- function(object, target_path) {
  parent_dir <- dirname(target_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
  temp_file <- paste0(target_path, ".tmp.", Sys.getpid())
  saveRDS(object, file = temp_file)
  if (file.exists(target_path)) {
    unlink(target_path, force = TRUE)
  }
  file.rename(temp_file, target_path)
  invisible(target_path)
}
