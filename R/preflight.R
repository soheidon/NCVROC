# preflight.R — Preflight Feasibility & Diagnostics for NCVROC Workloads
#
# Non-statistical, observational preflight calculator and diagnostic layer.
# Invariants:
# - Strictly observational: does not alter candidate space, models, folds, repeats,
#   metrics, cutoffs, seeds, or RNG stream.
# - Safe numerical handling: avoids integer overflow via double storage.
# - Model-size-stratified bounded pilot: benchmarks across every requested model size.
# - Strict timing-quality gate: suppresses artificial throughput numbers on near-zero elapsed times.
# - Hard-capped pilot budget: pilot combinations never exceed user-configured pilot_candidates.
# - Bounded memory: does not materialize full candidate tables.

.PREFLIGHT_THRESHOLDS <- list(
  small      = 10000,
  moderate   = 500000,
  large      = 10000000
)

.PREFLIGHT_MIN_STRATUM_SECS <- 0.01       # 10ms minimum for valid per-stratum rate
.PREFLIGHT_MIN_TOTAL_PILOT_SECS <- 0.5    # 500ms minimum total pilot time for extrapolated estimate

#' Classify a candidate workload into heuristic feasibility tiers
#'
#' @param count Numeric candidate or evaluation count.
#' @return Character scalar: "small", "moderate", "large", or "very_large".
#' @keywords internal
#' @noRd
.preflight_classify_workload <- function(count) {
  if (!is.numeric(count) || length(count) != 1L ||
      is.na(count) || count < 0) {
    return("unknown")
  }
  if (count <= .PREFLIGHT_THRESHOLDS$small) {
    "small"
  } else if (count <= .PREFLIGHT_THRESHOLDS$moderate) {
    "moderate"
  } else if (count <= .PREFLIGHT_THRESHOLDS$large) {
    "large"
  } else {
    "very_large"
  }
}

#' Calculate exact candidate workload and evaluation multiplier
#'
#' @param p Total number of available predictors/items.
#' @param model_sizes Vector of integer model sizes.
#' @param workflow Workflow name.
#' @param selection_metric Selection metric name.
#' @param folds Number of CV folds.
#' @param repeats Number of CV repeats.
#' @param outer_folds Number of outer nested CV folds.
#' @param outer_repeats Number of outer nested CV repeats.
#' @param inner_folds Number of inner nested CV folds.
#' @param inner_repeats Number of inner nested CV repeats.
#' @param top_n Top-N truncation value.
#' @param has_constraints Logical indicating whether clinical constraints exist.
#' @param stability_mode Mode for candidate_stability_roc ("bootstrap", "subsample", "cv").
#' @param b_resamples Number of bootstrap/subsample resamples.
#' @return A list with candidate_space, candidate_count_by_size, and candidate_evaluations.
#' @keywords internal
#' @noRd
.preflight_calculate_workload <- function(p,
                                         model_sizes,
                                         workflow = c("cross_size_cv", "exhaustive_sum_roc",
                                                      "cross_size_nested_cv", "nested_sum_roc",
                                                      "compare_cv_selection", "candidate_stability_roc"),
                                         selection_metric = "auc",
                                         folds = 5L,
                                         repeats = 1L,
                                         outer_folds = 5L,
                                         outer_repeats = 1L,
                                         inner_folds = 5L,
                                         inner_repeats = 1L,
                                         top_n = 20L,
                                         has_constraints = FALSE,
                                         stability_mode = c("bootstrap", "subsample", "cv"),
                                         b_resamples = 50L) {
  workflow <- match.arg(workflow)
  stability_mode <- match.arg(stability_mode)

  if (!is.numeric(p) || length(p) != 1L || is.na(p) || p < 1 || p != floor(p)) {
    stop("`p` (item count) must be a positive integer.", call. = FALSE)
  }
  p <- as.integer(p)

  if (!is.numeric(model_sizes) || length(model_sizes) == 0L || anyNA(model_sizes)) {
    stop("`model_sizes` must be a non-empty numeric vector.", call. = FALSE)
  }
  sizes <- sort(unique(as.integer(model_sizes)))
  if (any(sizes < 1L) || any(sizes > p)) {
    stop(sprintf("`model_sizes` must contain integers between 1 and %d.", p), call. = FALSE)
  }

  counts_by_size <- vapply(sizes, function(s) choose(as.double(p), as.double(s)), numeric(1))
  names(counts_by_size) <- as.character(sizes)
  w_candidates <- sum(counts_by_size)

  # Per-size evaluations
  evals_by_size <- switch(
    workflow,
    exhaustive_sum_roc = counts_by_size,

    cross_size_cv = {
      if (identical(selection_metric, "auc") && !isTRUE(has_constraints)) {
        # Strategy 1: base search on full data = W_s; single global best model CV = folds * repeats
        counts_by_size
      } else {
        # Strategy 2: all candidates evaluated across all folds x repeats
        counts_by_size * as.double(folds) * as.double(repeats)
      }
    },

    cross_size_nested_cv = {
      n_outer <- as.double(outer_folds) * as.double(outer_repeats)
      if (identical(selection_metric, "auc") && !isTRUE(has_constraints)) {
        # Inner Strategy 1: base search on training fold = W_s; single inner best model CV = inner_folds * inner_repeats
        counts_by_size * n_outer
      } else {
        counts_by_size * n_outer * as.double(inner_folds) * as.double(inner_repeats)
      }
    },

    nested_sum_roc = {
      n_outer <- as.double(outer_folds) * as.double(outer_repeats)
      counts_by_size * n_outer * as.double(inner_folds) * as.double(inner_repeats)
    },

    compare_cv_selection = {
      counts_by_size * as.double(folds) * as.double(repeats)
    },

    candidate_stability_roc = {
      if (identical(stability_mode, "cv")) {
        counts_by_size * as.double(folds) * as.double(repeats)
      } else {
        counts_by_size * as.double(b_resamples)
      }
    }
  )

  names(evals_by_size) <- as.character(sizes)

  post_selection_evals <- 0.0
  if (workflow == "cross_size_cv" && identical(selection_metric, "auc") && !isTRUE(has_constraints)) {
    post_selection_evals <- as.double(folds) * as.double(repeats)
  } else if (workflow == "cross_size_nested_cv" && identical(selection_metric, "auc") && !isTRUE(has_constraints)) {
    post_selection_evals <- as.double(outer_folds) * as.double(outer_repeats) * as.double(inner_folds) * as.double(inner_repeats)
  }

  w_evaluations <- sum(evals_by_size) + post_selection_evals

  list(
    p                       = p,
    model_sizes             = sizes,
    candidate_space         = w_candidates,
    candidate_count_by_size = counts_by_size,
    evaluations_by_size     = evals_by_size,
    candidate_evaluations   = w_evaluations,
    workflow                = workflow
  )
}

#' Resolve expected execution pathway and memory profile
#'
#' @param workflow Workflow name.
#' @param parallel Parallel mode string.
#' @param engine Engine string ("Rcpp" or "R").
#' @param top_n Top-N truncation value.
#' @param save_rds Logical indicating whether chunk RDS files are saved.
#' @param n_workers Number of workers.
#' @return A list describing the expected execution path and memory profile.
#' @keywords internal
#' @noRd
.preflight_resolve_execution <- function(workflow, parallel, engine, top_n,
                                         save_rds = FALSE, n_workers = NULL) {
  eff_workers <- .resolve_n_workers(parallel = !identical(parallel, "none"), n_workers = n_workers)

  expected_path <- if (isTRUE(save_rds) || identical(parallel, "chunks")) {
    "chunked_rds"
  } else if (!is.null(top_n) && top_n > 0 && identical(engine, "Rcpp") && parallel %in% c("none", "threads")) {
    "streaming_top_n"
  } else if (workflow %in% c("cross_size_nested_cv", "nested_sum_roc") && identical(parallel, "outer")) {
    "outer_parallel"
  } else if (workflow %in% c("cross_size_nested_cv", "nested_sum_roc") && identical(parallel, "hybrid")) {
    "hybrid_parallel"
  } else if (identical(parallel, "threads")) {
    "threads_batched"
  } else {
    "serial_eval"
  }

  memory_profile <- switch(
    expected_path,
    streaming_top_n  = "bounded candidate-result memory (O(top_n))",
    chunked_rds      = "disk-backed chunk storage (bounded RAM, chunked RDS on disk)",
    threads_batched  = "batched thread evaluation",
    outer_parallel   = "outer fold parallel (bounded memory per worker)",
    hybrid_parallel  = "hybrid outer x inner parallel (bounded memory)",
    serial_eval      = if (is.null(top_n)) "full in-memory candidate table (O(W) RAM)" else "bounded candidate-result memory"
  )

  list(
    engine          = engine,
    parallel        = parallel,
    workers         = eff_workers,
    expected_path   = expected_path,
    memory_profile  = memory_profile
  )
}

#' Run a deterministic model-size-stratified bounded pilot benchmark
#'
#' @param data Matrix or data.frame.
#' @param outcome Outcome vector or column name.
#' @param items Item names vector.
#' @param model_sizes Integer model sizes.
#' @param workflow Workflow name.
#' @param engine Engine name.
#' @param parallel Parallel mode.
#' @param n_workers Worker count.
#' @param pilot_candidates Maximum candidate quota across all sizes (hard cap).
#' @param workload Pre-calculated workload object.
#' @return A list containing stratified pilot timing metrics and estimated runtime.
#' @keywords internal
#' @noRd
.preflight_run_pilot <- function(data,
                                 outcome,
                                 items,
                                 model_sizes,
                                 workflow = "cross_size_cv",
                                 engine = "Rcpp",
                                 parallel = "threads",
                                 n_workers = NULL,
                                 pilot_candidates = 10000L,
                                 workload = NULL) {

  p <- length(items)
  sizes <- sort(unique(as.integer(model_sizes)))
  counts_by_size <- workload$candidate_count_by_size
  evals_by_size <- workload$evaluations_by_size
  w_total <- workload$candidate_space
  w_eval_total <- workload$candidate_evaluations

  if (w_total <= 0) {
    return(list(
      pilot_candidates    = 0L,
      pilot_elapsed_sec   = 0.0,
      candidates_per_sec  = NA_real_,
      estimated_total_sec = NA_real_,
      estimated_human     = "unavailable",
      per_size_results    = list()
    ))
  }

  eff_workers <- .resolve_n_workers(parallel = !identical(parallel, "none"), n_workers = n_workers)

  # Extract numeric matrix and binary outcome
  x_mat <- as.matrix(data[, items, drop = FALSE])
  y_vec <- if (is.character(outcome) && length(outcome) == 1L) data[[outcome]] else outcome
  y_vec <- as.integer(y_vec)

  # Hard-cap allocation: distribute quota across sizes strictly <= pilot_candidates
  max_cap <- max(1L, as.integer(pilot_candidates))
  size_quotas <- integer(length(sizes))
  names(size_quotas) <- as.character(sizes)

  if (w_total <= max_cap) {
    # Full candidate space fits within budget
    for (s_char in names(size_quotas)) {
      size_quotas[s_char] <- as.integer(counts_by_size[s_char])
    }
  } else {
    # Size-proportional allocation
    raw_shares <- vapply(sizes, function(s) {
      s_char <- as.character(s)
      w_s <- counts_by_size[s_char]
      floor(max_cap * (w_s / w_total))
    }, numeric(1))
    names(raw_shares) <- as.character(sizes)

    # Ensure every non-empty stratum receives at least 1 candidate if max_cap >= length(sizes)
    if (max_cap >= length(sizes)) {
      for (s_char in names(raw_shares)) {
        if (raw_shares[s_char] < 1L && counts_by_size[s_char] >= 1L) {
          raw_shares[s_char] <- 1L
        }
      }
    }

    # Ensure sum of quotas does NOT exceed max_cap (hard cap)
    while (sum(raw_shares) > max_cap) {
      max_idx <- which.max(raw_shares)
      raw_shares[max_idx] <- raw_shares[max_idx] - 1L
    }

    for (s_char in names(size_quotas)) {
      size_quotas[s_char] <- as.integer(min(counts_by_size[s_char], raw_shares[s_char]))
    }
  }

  # Execute size-stratified pilot with RNG preservation
  pilot_res <- .planner_with_preserved_rng({
    per_size <- vector("list", length(sizes))
    names(per_size) <- as.character(sizes)

    total_pilot_evals <- 0L
    total_pilot_time <- 0.0

    for (i in seq_along(sizes)) {
      s <- sizes[i]
      s_char <- as.character(s)
      q_s <- size_quotas[s_char]

      if (q_s <= 0L) {
        per_size[[s_char]] <- list(
          size               = s,
          pilot_candidates   = 0L,
          pilot_elapsed_sec  = 0.0,
          candidates_per_sec = NA_real_,
          all_evaluated      = FALSE,
          timing_valid       = FALSE
        )
        next
      }

      t0_s <- proc.time()[["elapsed"]]

      if (identical(engine, "Rcpp")) {
        # Streaming C++ kernel for size s
        evaluate_combos_cpp_chunk_parallel_topn(
          x                  = x_mat,
          y                  = y_vec,
          min_items          = s,
          max_items          = s,
          cutoff_method      = "youden",
          rank_by            = "auc",
          top_n              = 10L,
          prefer_fewer_items = TRUE,
          chunk_start        = 0.0,
          chunk_size         = as.integer(q_s),
          num_threads        = if (identical(parallel, "threads")) eff_workers else 1L
        )
      } else {
        # R fallback pilot for size s
        combos_s <- enumerate_combinations(items, min_items = s, max_items = s)
        c_sub <- utils::head(combos_s, q_s)
        lapply(c_sub, function(ci) {
          score <- rowSums(x_mat[, ci, drop = FALSE])
          fr <- compute_score_frequencies(score, y_vec)
          compute_roc_metrics_from_table(fr$pos_counts, fr$neg_counts)
        })
      }

      t_elapsed_s <- proc.time()[["elapsed"]] - t0_s
      is_valid_timing <- (t_elapsed_s >= .PREFLIGHT_MIN_STRATUM_SECS)
      rate_s <- if (is_valid_timing) (q_s / t_elapsed_s) else NA_real_

      all_eval_s <- (q_s >= counts_by_size[s_char])

      per_size[[s_char]] <- list(
        size               = s,
        pilot_candidates   = q_s,
        pilot_elapsed_sec  = t_elapsed_s,
        candidates_per_sec = rate_s,
        all_evaluated      = all_eval_s,
        timing_valid       = is_valid_timing
      )

      total_pilot_evals <- total_pilot_evals + q_s
      total_pilot_time <- total_pilot_time + t_elapsed_s
    }

    list(
      per_size          = per_size,
      total_pilot_evals = total_pilot_evals,
      total_pilot_time  = total_pilot_time
    )
  })

  all_evaluated <- (pilot_res$total_pilot_evals >= w_total)
  timing_valid <- (pilot_res$total_pilot_time >= .PREFLIGHT_MIN_TOTAL_PILOT_SECS)

  # Stratified runtime extrapolation:
  # Check whether every materially contributing stratum has valid timing or is fully evaluated
  est_secs_by_size <- numeric(length(sizes))
  names(est_secs_by_size) <- as.character(sizes)
  extrapolation_possible <- TRUE

  for (s_char in as.character(sizes)) {
    ps <- pilot_res$per_size[[s_char]]
    w_eval_s <- evals_by_size[s_char]
    stratum_weight <- if (w_eval_total > 0) (w_eval_s / w_eval_total) else 0.0

    if (isTRUE(ps$all_evaluated)) {
      # 100% of this stratum was evaluated in the pilot
      est_secs_by_size[s_char] <- ps$pilot_elapsed_sec
    } else if (isTRUE(ps$timing_valid) && !is.na(ps$candidates_per_sec) && ps$candidates_per_sec > 0) {
      # Valid stratum rate
      est_secs_by_size[s_char] <- w_eval_s / ps$candidates_per_sec
    } else {
      # Missing rate for uncompleted stratum
      if (stratum_weight >= 0.01) {
        # Material stratum missing stable rate -> cannot extrapolate reliably
        extrapolation_possible <- FALSE
      } else {
        # Negligible stratum (< 1% of total evaluations)
        est_secs_by_size[s_char] <- 0.0
      }
    }
  }

  total_est_sec <- sum(est_secs_by_size)

  est_human <- if (isTRUE(all_evaluated)) {
    "< 10 sec"
  } else if (!isTRUE(timing_valid)) {
    "unavailable (insufficient stable timing data; pilot duration < 0.5 sec)"
  } else if (!isTRUE(extrapolation_possible)) {
    "unavailable (insufficient stable timing data on material model sizes)"
  } else if (is.finite(total_est_sec) && total_est_sec > 0) {
    .format_eta(total_est_sec)
  } else {
    "unavailable"
  }

  overall_rate <- if (isTRUE(timing_valid) && pilot_res$total_pilot_time > 0) {
    pilot_res$total_pilot_evals / pilot_res$total_pilot_time
  } else NA_real_

  list(
    pilot_candidates    = as.integer(pilot_res$total_pilot_evals),
    pilot_elapsed_sec   = pilot_res$total_pilot_time,
    candidates_per_sec  = overall_rate,
    estimated_total_sec = if (isTRUE(all_evaluated) || (isTRUE(timing_valid) && isTRUE(extrapolation_possible))) total_est_sec else NA_real_,
    estimated_human     = est_human,
    per_size_results    = pilot_res$per_size
  )
}

#' Preflight Feasibility & Diagnostics for NCVROC Workloads
#'
#' Evaluates the exact combinatorial candidate space, calculates workflow-specific
#' evaluation multipliers, executes a deterministic model-size-stratified bounded pilot
#' benchmark without RNG contamination, and provides machine-specific runtime estimates
#' and execution diagnostics.
#'
#' @param data A data frame or matrix containing the predictor items and outcome.
#' @param outcome Character name of the binary outcome column, or binary outcome vector.
#' @param items Character vector of item/predictor column names.
#' @param model_sizes Integer vector of requested model sizes (default `1:4`).
#' @param workflow Character specifying the workflow: `"cross_size_cv"`, `"exhaustive_sum_roc"`,
#'   `"cross_size_nested_cv"`, `"nested_sum_roc"`, `"compare_cv_selection"`, or `"candidate_stability_roc"`.
#' @param selection_metric Character model selection metric (default `"auc"`).
#' @param folds Number of CV folds (default `5L`).
#' @param repeats Number of CV repeats (default `1L`).
#' @param outer_folds Number of outer nested CV folds (default `5L`).
#' @param outer_repeats Number of outer nested CV repeats (default `1L`).
#' @param inner_folds Number of inner nested CV folds (default `5L`).
#' @param inner_repeats Number of inner nested CV repeats (default `1L`).
#' @param top_n Top-N truncation count (default `20L`).
#' @param prefer_fewer_items Logical indicating whether smaller item sets break ties.
#' @param engine Execution engine: `"Rcpp"` (default) or `"R"`.
#' @param parallel Parallel mode: `"threads"`, `"chunks"`, `"none"`, etc.
#' @param n_workers Number of worker threads/processes.
#' @param pilot Logical indicating whether to run a deterministic pilot benchmark (default `TRUE`).
#' @param pilot_candidates Maximum candidate combinations to benchmark across model sizes in pilot (default `10000L`).
#' @param ... Additional arguments passed to methods.
#'
#' @return An S3 object of class `ncvroc_preflight` containing:
#'   \itemize{
#'     \item `workflow`: Requested workflow name.
#'     \item `p`: Number of candidate predictor items.
#'     \item `model_sizes`: Integer model sizes.
#'     \item `candidate_space`: Total unique candidate combinations \eqn{W = \sum \binom{p}{k}}{W = sum(choose(p, k))}.
#'     \item `candidate_space_class`: Static classification of candidate space (`"small"`, `"moderate"`, `"large"`, `"very_large"`).
#'     \item `candidate_evaluations`: Estimated total candidate evaluations across CV folds.
#'     \item `effective_workload_class`: Static classification of effective evaluation workload.
#'     \item `pilot`: List of stratified pilot benchmark results.
#'     \item `runtime_estimate`: List with `seconds` and `human` estimated total duration.
#'     \item `execution`: Expected execution pathway, engine, parallelism, and memory behavior.
#'     \item `advisories`: Character vector of diagnostic notes and advisories.
#'   }
#' @export
#'
#' @examples
#' d <- data.frame(
#'   y  = sample(0:1, 50, replace = TRUE),
#'   q1 = sample(0:2, 50, replace = TRUE),
#'   q2 = sample(0:2, 50, replace = TRUE),
#'   q3 = sample(0:2, 50, replace = TRUE),
#'   q4 = sample(0:2, 50, replace = TRUE)
#' )
#' pf <- ncvroc_preflight(d, "y", c("q1", "q2", "q3", "q4"), model_sizes = 1:3)
#' print(pf)
ncvroc_preflight <- function(data = NULL,
                             outcome = NULL,
                             items,
                             model_sizes = 1:4,
                             workflow = c("cross_size_cv", "exhaustive_sum_roc",
                                          "cross_size_nested_cv", "nested_sum_roc",
                                          "compare_cv_selection", "candidate_stability_roc"),
                             selection_metric = "auc",
                             folds = 5L,
                             repeats = 1L,
                             outer_folds = 5L,
                             outer_repeats = 1L,
                             inner_folds = 5L,
                             inner_repeats = 1L,
                             top_n = 20L,
                             prefer_fewer_items = TRUE,
                             engine = c("Rcpp", "R"),
                             parallel = c("threads", "none", "chunks", "outer", "hybrid"),
                             n_workers = NULL,
                             pilot = TRUE,
                             pilot_candidates = 10000L,
                             ...) {

  workflow <- match.arg(workflow)
  engine <- match.arg(engine)
  parallel <- match.arg(parallel)

  p <- length(items)
  workload <- .preflight_calculate_workload(
    p                = p,
    model_sizes      = model_sizes,
    workflow         = workflow,
    selection_metric = selection_metric,
    folds            = folds,
    repeats          = repeats,
    outer_folds      = outer_folds,
    outer_repeats    = outer_repeats,
    inner_folds      = inner_folds,
    inner_repeats    = inner_repeats,
    top_n            = top_n
  )

  candidate_space_class <- .preflight_classify_workload(workload$candidate_space)
  effective_workload_class <- .preflight_classify_workload(workload$candidate_evaluations)

  execution_diag <- .preflight_resolve_execution(
    workflow  = workflow,
    parallel  = parallel,
    engine    = engine,
    top_n     = top_n,
    n_workers = n_workers
  )

  pilot_res <- if (isTRUE(pilot) && !is.null(data) && !is.null(outcome)) {
    .preflight_run_pilot(
      data               = data,
      outcome            = outcome,
      items              = items,
      model_sizes        = workload$model_sizes,
      workflow           = workflow,
      engine             = engine,
      parallel           = parallel,
      n_workers          = execution_diag$workers,
      pilot_candidates   = pilot_candidates,
      workload           = workload
    )
  } else {
    list(
      pilot_candidates    = 0L,
      pilot_elapsed_sec   = 0.0,
      candidates_per_sec  = NA_real_,
      estimated_total_sec = NA_real_,
      estimated_human     = "not measured",
      per_size_results    = list()
    )
  }

  advisories <- character()
  if (candidate_space_class == "very_large" || effective_workload_class == "very_large") {
    advisories <- c(advisories,
      sprintf("This is a very large exact search (%s candidate combinations, %s estimated evaluations).",
              format(workload$candidate_space, big.mark = ","),
              format(workload$candidate_evaluations, big.mark = ",")))
  }
  if (is.null(top_n) && workload$candidate_space > .PREFLIGHT_THRESHOLDS$moderate) {
    advisories <- c(advisories,
      "Full candidate result requested (top_n = NULL) for large search; consider top_n truncation to preserve bounded memory.")
  }
  if (identical(parallel, "none") && workload$candidate_space > .PREFLIGHT_THRESHOLDS$small) {
    advisories <- c(advisories,
      "Serial execution selected for non-trivial search; multi-threaded execution (parallel = 'threads') is recommended.")
  }

  structure(
    list(
      workflow                 = workflow,
      p                        = p,
      model_sizes              = workload$model_sizes,
      candidate_space          = workload$candidate_space,
      candidate_count_by_size  = workload$candidate_count_by_size,
      candidate_space_class    = candidate_space_class,
      candidate_evaluations    = workload$candidate_evaluations,
      evaluations_by_size      = workload$evaluations_by_size,
      effective_workload_class = effective_workload_class,
      feasibility_class        = effective_workload_class, # backward compatibility
      pilot                    = pilot_res,
      runtime_estimate         = list(
        seconds = pilot_res$estimated_total_sec,
        human   = pilot_res$estimated_human
      ),
      execution                = execution_diag,
      advisories               = advisories
    ),
    class = "ncvroc_preflight"
  )
}

#' Print Method for NCVROC Preflight Diagnostics
#'
#' @param x An object of class `ncvroc_preflight`.
#' @param ... Additional arguments.
#' @return Invisibly returns the input object `x`.
#' @export
print.ncvroc_preflight <- function(x, ...) {
  cat("=================================================================\n")
  cat("NCVROC Preflight Diagnostics\n")
  cat("=================================================================\n")
  cat(sprintf("Workflow:                         %s\n", x$workflow))
  cat(sprintf("Predictors (p):                   %d\n", x$p))
  cat(sprintf("Model sizes:                      %s\n", paste(range(x$model_sizes), collapse = ":")))
  cat(sprintf("Unique candidate space:           %s (%s)\n",
              format(x$candidate_space, big.mark = ","), x$candidate_space_class))
  cat(sprintf("Estimated candidate evaluations:  %s (%s)\n\n",
              format(x$candidate_evaluations, big.mark = ","), x$effective_workload_class))

  if (x$pilot$pilot_candidates > 0L) {
    cat("Stratified Pilot Benchmark:\n")
    if (!is.na(x$pilot$candidates_per_sec)) {
      cat(sprintf("  Evaluated %s candidates across sizes in %.3f sec (~%s candidates/sec)\n",
                  format(x$pilot$pilot_candidates, big.mark = ","),
                  x$pilot$pilot_elapsed_sec,
                  format(round(x$pilot$candidates_per_sec), big.mark = ",")))
    } else {
      cat(sprintf("  Evaluated %s candidates across sizes in %.3f sec\n",
                  format(x$pilot$pilot_candidates, big.mark = ","),
                  x$pilot$pilot_elapsed_sec))
    }

    for (s_name in names(x$pilot$per_size_results)) {
      ps <- x$pilot$per_size_results[[s_name]]
      if (!is.na(ps$candidates_per_sec)) {
        cat(sprintf("    - Size %s: %s candidates (%.3f sec, ~%s cand/sec)\n",
                    s_name, format(ps$pilot_candidates, big.mark = ","),
                    ps$pilot_elapsed_sec, format(round(ps$candidates_per_sec), big.mark = ",")))
      } else if (isTRUE(ps$all_evaluated)) {
        cat(sprintf("    - Size %s: %s candidates (< 0.010 sec, all %s evaluated)\n",
                    s_name, format(ps$pilot_candidates, big.mark = ","),
                    format(ps$pilot_candidates, big.mark = ",")))
      } else {
        cat(sprintf("    - Size %s: %s candidates (< 0.010 sec, rate unavailable)\n",
                    s_name, format(ps$pilot_candidates, big.mark = ",")))
      }
    }
    cat(sprintf("\nEstimated Runtime:\n  %s\n\n", x$runtime_estimate$human))
  }

  cat("Execution Diagnostics:\n")
  cat(sprintf("  Engine / Backend:               %s / %s (%d workers)\n",
              x$execution$engine, x$execution$parallel, x$execution$workers))
  cat(sprintf("  Expected pathway:               %s\n", x$execution$expected_path))
  cat(sprintf("  Memory behavior:                %s\n", x$execution$memory_profile))

  if (length(x$advisories) > 0L) {
    cat("\nAdvisory:\n")
    for (adv in x$advisories) {
      cat(sprintf("  * %s\n", adv))
    }
  }
  cat("=================================================================\n")
  invisible(x)
}
