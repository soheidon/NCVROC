# exhaustive_sum_roc.R — Exhaustive ROC evaluation of all item subsets

# Exported symbols required by chunk PSOCK workers
.CHUNK_WORKER_EXPORT_SYMBOLS <- c(
  ".evaluate_chunk_serial",
  ".execute_chunk_worker_task",
  ".resolve_global_combination_rank",
  ".combination_unrank",
  ".enumerate_combinations_chunk",
  ".count_total_combos",
  "format_items",
  "compute_score_frequencies",
  "compute_auc_from_table",
  "compute_roc_metrics_from_table",
  "find_optimal_cutoff",
  ".write_chunk_rds",
  ".order_and_rank_candidates"
)

#' Pure serial evaluation of a single combination chunk
#'
#' Evaluates combinations in range [chunk_start, chunk_start + chunk_size).
#' Contains zero cluster code to prevent recursive nested parallelism.
#'
#' @param x_mat Matrix or data.frame of item values.
#' @param y Integer binary outcome vector (0/1).
#' @param items Character vector of item column names.
#' @param min_items Integer, minimum items per combo.
#' @param max_items Integer, maximum items per combo.
#' @param cutoff_method Method for cutoff: "youden" or "closest_topleft".
#' @param chunk_start Numeric, zero-based global combo index to start from.
#' @param chunk_size Integer, number of combinations in chunk.
#' @param engine Computation engine: "Rcpp" or "R".
#' @param n_pos Optional count of positive samples.
#' @param n_neg Optional count of negative samples.
#' @param num_threads Integer, number of C++ threads for Rcpp parallel evaluation. Default 1L.
#' @return data.frame of evaluated candidates with .global_combo_index column.
#' @keywords internal
.evaluate_chunk_serial <- function(x_mat,
                                   y,
                                   items,
                                   min_items,
                                   max_items,
                                   cutoff_method,
                                   chunk_start,
                                   chunk_size,
                                   engine = "Rcpp",
                                   n_pos = NULL,
                                   n_neg = NULL,
                                   num_threads = 1L) {
  n_items <- length(items)
  if (is.null(n_pos)) n_pos <- sum(y == 1L)
  if (is.null(n_neg)) n_neg <- sum(y == 0L)

  total_combos <- .count_total_combos(n_items, min_items, max_items)
  chunk_end <- min(as.double(chunk_start) + as.double(chunk_size), total_combos)
  n_this_chunk <- as.integer(chunk_end - chunk_start)
  if (n_this_chunk <= 0L) {
    return(data.frame())
  }

  # 1-based global combination index in the exhaustive sequence
  g_idx_1based <- as.double(chunk_start) + seq_len(n_this_chunk)

  if (engine == "Rcpp") {
    if (!is.matrix(x_mat)) {
      x_mat <- as.matrix(x_mat[, items, drop = FALSE])
    }
    if (!is.null(num_threads) && num_threads > 1L) {
      results <- evaluate_combos_cpp_chunk_parallel(
        x_mat, y,
        min_items = min_items,
        max_items = max_items,
        cutoff_method = cutoff_method,
        chunk_start = as.double(chunk_start),
        chunk_size = as.integer(chunk_size),
        num_threads = as.integer(num_threads),
        grain_size = 1000L
      )
    } else {
      results <- evaluate_combos_cpp_chunk(
        x_mat, y,
        min_items = min_items,
        max_items = max_items,
        cutoff_method = cutoff_method,
        chunk_start = as.double(chunk_start),
        chunk_size = as.integer(chunk_size)
      )
    }
    items_vec <- character(n_this_chunk)
    for (gi in seq_len(n_this_chunk)) {
      global_rank <- as.double(chunk_start) + (gi - 1L)
      resolved <- .resolve_global_combination_rank(n_items, min_items, max_items, global_rank)
      idx <- .combination_unrank(n_items, resolved$k, resolved$rank_within_k)
      items_vec[gi] <- format_items(items[idx + 1L])
    }
    results$items <- items_vec
  } else {
    combo_chunk <- .enumerate_combinations_chunk(
      items, min_items, max_items, chunk_start, chunk_size
    )
    results <- vector("list", n_this_chunk)
    for (i in seq_len(n_this_chunk)) {
      combo_items <- combo_chunk[[i]]
      k <- length(combo_items)
      scores <- rowSums(x_mat[, combo_items, drop = FALSE])
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

  results$.global_combo_index <- g_idx_1based
  results
}

#' Worker task execution function for PSOCK chunk workers
#'
#' Reads immutable data from worker environment and evaluates one chunk task.
#'
#' @param task List with chunk_idx, chunk_start, chunk_size, rank_by, top_n_local,
#'   prefer_fewer_items, save_rds, chunk_dir.
#' @return data.frame of candidates or lightweight metadata list.
#' @keywords internal
.execute_chunk_worker_task <- function(task) {
  # Retrieve immutable inputs stored once in worker global environment
  x_mat         <- get(".WORKER_X_MAT", envir = .GlobalEnv)
  y             <- get(".WORKER_Y", envir = .GlobalEnv)
  items         <- get(".WORKER_ITEMS", envir = .GlobalEnv)
  min_items     <- get(".WORKER_MIN_ITEMS", envir = .GlobalEnv)
  max_items     <- get(".WORKER_MAX_ITEMS", envir = .GlobalEnv)
  cutoff_method <- get(".WORKER_CUTOFF_METHOD", envir = .GlobalEnv)
  engine        <- get(".WORKER_ENGINE", envir = .GlobalEnv)

  chunk_res <- .evaluate_chunk_serial(
    x_mat         = x_mat,
    y             = y,
    items         = items,
    min_items     = min_items,
    max_items     = max_items,
    cutoff_method = cutoff_method,
    chunk_start   = task$chunk_start,
    chunk_size    = task$chunk_size,
    engine        = engine
  )

  if (isTRUE(task$save_rds) && !is.null(task$chunk_dir)) {
    # Remove internal column before writing to RDS
    clean_res <- chunk_res
    clean_res$.global_combo_index <- NULL
    rds_path <- .write_chunk_rds(clean_res, task$chunk_dir, task$chunk_idx)

    local_top <- if (!is.null(task$top_n_local) && task$top_n_local > 0 && nrow(chunk_res) > 0) {
      .order_and_rank_candidates(chunk_res, task$rank_by, task$prefer_fewer_items)[seq_len(min(task$top_n_local, nrow(chunk_res))), , drop = FALSE]
    } else {
      NULL
    }
    return(list(
      chunk_idx    = task$chunk_idx,
      rds_path     = rds_path,
      n_candidates = nrow(chunk_res),
      local_top_n  = local_top
    ))
  }

  if (!is.null(task$top_n_local) && task$top_n_local > 0 && nrow(chunk_res) > task$top_n_local) {
    chunk_res <- .order_and_rank_candidates(chunk_res, task$rank_by, task$prefer_fewer_items)[seq_len(task$top_n_local), , drop = FALSE]
  }

  chunk_res
}

#' Execute chunk-parallel exhaustive ROC evaluation
#'
#' @param x Matrix or data.frame of items.
#' @param y Integer binary outcome vector.
#' @param items Character vector of item names.
#' @param min_items Integer, minimum items per combo.
#' @param max_items Integer, maximum items per combo.
#' @param cutoff_method Cutoff method.
#' @param rank_by Metric for ranking candidates.
#' @param top_n Number of top models to return (NULL returns all).
#' @param prefer_fewer_items Logical.
#' @param engine Computation engine.
#' @param chunk_size Combinations per chunk (default 200000).
#' @param n_workers Worker count or NULL.
#' @param save_rds Logical, save chunked RDS files. Default FALSE.
#' @param chunk_dir Directory for chunked RDS files.
#' @param cl Optional existing PSOCK cluster. If NULL, a cluster is created and stopped.
#' @return data.frame of ranked candidate models, or list with candidates and rds metadata.
#' @keywords internal
.parallel_chunk_exhaustive <- function(x,
                                       y,
                                       items,
                                       min_items,
                                       max_items,
                                       cutoff_method,
                                       rank_by,
                                       top_n = NULL,
                                       prefer_fewer_items = TRUE,
                                       engine = "Rcpp",
                                       chunk_size = 200000L,
                                       n_workers = NULL,
                                       save_rds = FALSE,
                                       chunk_dir = NULL,
                                       cl = NULL) {
  n_items <- length(items)
  total_combos <- .count_total_combos(n_items, min_items, max_items)
  chunk_size <- as.integer(chunk_size)
  n_chunks <- ceiling(total_combos / chunk_size)

  if (n_chunks <= 1L) {
    # Only 1 chunk: evaluate sequentially
    x_mat <- as.matrix(x[, items, drop = FALSE])
    res <- .evaluate_chunk_serial(
      x_mat, y, items, min_items, max_items, cutoff_method,
      chunk_start = 0, chunk_size = chunk_size, engine = engine
    )
    if (save_rds && !is.null(chunk_dir)) {
      clean_res <- res
      clean_res$.global_combo_index <- NULL
      .write_chunk_rds(clean_res, chunk_dir, 0L)
    }
    res <- .order_and_rank_candidates(res, rank_by, prefer_fewer_items)
    if (!is.null(top_n) && top_n > 0 && nrow(res) > top_n) {
      res <- utils::head(res, top_n)
    }
    res$.global_combo_index <- NULL
    return(res)
  }

  # Resolve actual workers
  actual_workers <- .resolve_n_workers(n_workers, n_chunks)
  created_cl <- FALSE

  if (is.null(cl)) {
    cl <- parallel::makePSOCKcluster(actual_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    created_cl <- TRUE

    # Initialize workers
    lib_paths <- .libPaths()
    parallel::clusterExport(cl, "lib_paths", envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(lib_paths)
      if (requireNamespace("NCVROC", quietly = TRUE)) {
        try(library(NCVROC), silent = TRUE)
      }
      NULL
    })

    ns <- asNamespace("NCVROC")
    avail <- intersect(.CHUNK_WORKER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
    if (length(avail) > 0) {
      parallel::clusterExport(cl, varlist = avail, envir = ns)
    }
  }

  # One-time export of immutable dataset and search settings to cluster
  x_mat <- as.matrix(x[, items, drop = FALSE])
  worker_env <- new.env(parent = emptyenv())
  worker_env$.WORKER_X_MAT         <- x_mat
  worker_env$.WORKER_Y             <- y
  worker_env$.WORKER_ITEMS         <- items
  worker_env$.WORKER_MIN_ITEMS     <- min_items
  worker_env$.WORKER_MAX_ITEMS     <- max_items
  worker_env$.WORKER_CUTOFF_METHOD <- cutoff_method
  worker_env$.WORKER_ENGINE        <- engine

  parallel::clusterExport(
    cl,
    varlist = c(".WORKER_X_MAT", ".WORKER_Y", ".WORKER_ITEMS",
                ".WORKER_MIN_ITEMS", ".WORKER_MAX_ITEMS",
                ".WORKER_CUTOFF_METHOD", ".WORKER_ENGINE"),
    envir = worker_env
  )

  # Determine local top-N reduction size
  # Invariant: top_n_local >= top_n
  top_n_local <- if (!is.null(top_n) && top_n > 0) {
    as.integer(max(top_n, 50L))
  } else {
    NULL
  }

  # Build lightweight task list
  tasks <- vector("list", n_chunks)
  for (i in seq_len(n_chunks)) {
    tasks[[i]] <- list(
      chunk_idx          = i - 1L,
      chunk_start        = as.double((i - 1L) * chunk_size),
      chunk_size         = chunk_size,
      rank_by            = rank_by,
      top_n_local        = top_n_local,
      prefer_fewer_items = prefer_fewer_items,
      save_rds           = save_rds,
      chunk_dir          = chunk_dir
    )
  }

  # Execute chunk tasks across PSOCK workers
  results_list <- parallel::parLapply(cl, tasks, .execute_chunk_worker_task)

  if (save_rds) {
    # Combine local top-N from worker metadata
    local_tops <- lapply(results_list, function(r) r$local_top_n)
    local_tops <- local_tops[!vapply(local_tops, is.null, logical(1))]
    combined <- if (length(local_tops) > 0) do.call(rbind, local_tops) else data.frame()
  } else {
    combined <- do.call(rbind, results_list)
  }

  # Deterministic merge preserving exact serial tie-breaking
  ordered_res <- .order_and_rank_candidates(combined, rank_by, prefer_fewer_items)
  if (!is.null(top_n) && top_n > 0 && nrow(ordered_res) > top_n) {
    ordered_res <- utils::head(ordered_res, top_n)
  }

  ordered_res$.global_combo_index <- NULL
  rownames(ordered_res) <- NULL

  ordered_res
}

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
#' @param parallel Logical or character. If `TRUE` or `"chunks"`, evaluate
#'   combination chunks in parallel across socket workers. If `"threads"`,
#'   evaluate combinations in parallel using C++ multi-threading within
#'   the current R process via RcppParallel. Default `FALSE`.
#' @param n_workers Integer, number of worker processes (for `"chunks"`) or
#'   threads (for `"threads"`), or `NULL` (default) for automatic detection.
#'   Ignored when `parallel = FALSE` or `"none"`.
#' @param progress Logical, show progress bar? Default `TRUE`.
#' @param chunk_start Internal: zero-based global combination index to start
#'   from. When set together with `chunk_size`, only that range is evaluated.
#' @param chunk_size Combinations per chunk (default 200000 when chunking).
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
#'   If `ci = TRUE`, includes confidence bounds (`auc_lower`, `auc_upper`, etc.).
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
                               parallel = FALSE,
                               n_workers = NULL,
                               progress = TRUE,
                               chunk_start = NULL,
                               chunk_size = NULL) {

  # ---- Argument validation ----
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)
  engine <- match.arg(engine)

  parallel_mode <- .resolve_parallel_mode(
    parallel,
    context = "exhaustive",
    allowed = c("none", "chunks", "threads")
  )

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

  # ---- Determine if we are in single-chunk sub-call mode ----
  is_single_chunk <- !is.null(chunk_start) && !is.null(chunk_size)

  if (is_single_chunk) {
    if (!is.numeric(chunk_start) || length(chunk_start) != 1 || chunk_start < 0) {
      stop("`chunk_start` must be a non-negative number.", call. = FALSE)
    }
    if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size <= 0 ||
        chunk_size != floor(chunk_size)) {
      stop("`chunk_size` must be a positive integer.", call. = FALSE)
    }
    chunk_size <- as.integer(chunk_size)
    chunk_start <- as.double(chunk_start)

    x_mat <- as.matrix(x[, items, drop = FALSE])
    threads_for_chunk <- if (parallel_mode == "threads") {
      .resolve_n_workers(parallel = TRUE, n_workers = n_workers)
    } else {
      1L
    }

    results <- .evaluate_chunk_serial(
      x_mat         = x_mat,
      y             = y,
      items         = items,
      min_items     = min_items,
      max_items     = max_items,
      cutoff_method = cutoff_method,
      chunk_start   = chunk_start,
      chunk_size    = chunk_size,
      engine        = engine,
      n_pos         = n_pos,
      n_neg         = n_neg,
      num_threads   = threads_for_chunk
    )
    # Remove internal tracking column for public return
    results$.global_combo_index <- NULL

  } else if (parallel_mode == "threads") {
    # --- Multi-threaded C++ Evaluation Path ---
    eff_n_threads <- .resolve_n_workers(parallel = TRUE, n_workers = n_workers)
    total_combos <- .count_total_combos(n_items, min_items, max_items)
    x_mat <- as.matrix(x[, items, drop = FALSE])

    results <- .evaluate_chunk_serial(
      x_mat         = x_mat,
      y             = y,
      items         = items,
      min_items     = min_items,
      max_items     = max_items,
      cutoff_method = cutoff_method,
      chunk_start   = 0.0,
      chunk_size    = total_combos,
      engine        = engine,
      n_pos         = n_pos,
      n_neg         = n_neg,
      num_threads   = eff_n_threads
    )
    results$.global_combo_index <- NULL

    results <- .order_and_rank_candidates(results, rank_by, prefer_fewer_items)

    if (!is.null(top_n)) {
      results <- utils::head(results, top_n)
    }

  } else if (parallel_mode == "chunks") {
    # --- Parallel Chunk Execution Path ---
    eff_chunk_size <- if (!is.null(chunk_size) && chunk_size > 0) as.integer(chunk_size) else 200000L
    results <- .parallel_chunk_exhaustive(
      x                  = x,
      y                  = y,
      items              = items,
      min_items          = min_items,
      max_items          = max_items,
      cutoff_method      = cutoff_method,
      rank_by            = rank_by,
      top_n              = top_n,
      prefer_fewer_items = prefer_fewer_items,
      engine             = engine,
      chunk_size         = eff_chunk_size,
      n_workers          = n_workers,
      save_rds           = FALSE,
      chunk_dir          = NULL,
      cl                 = NULL
    )

  } else {
    # --- Full Sequential Evaluation Path ---
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

    results <- .order_and_rank_candidates(results, rank_by, prefer_fewer_items)

    if (!is.null(top_n)) {
      results <- utils::head(results, top_n)
    }
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
