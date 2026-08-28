# execution_planner.R -- Internal automatic execution-planning primitives

.PLANNER_VERSION <- "0.2.0"
.PLANNER_SELECTION_TOLERANCE <- 0.05
.PLANNER_AUTO_RUNTIME_THRESHOLD <- 180
.PLANNER_BENCHMARK_OVERHEAD_RATIO <- 0.05
.PLANNER_MAX_EXACT_INTEGER <- 2^53 - 1

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".PLANNER_X", ".PLANNER_Y", ".PLANNER_ITEMS", ".PLANNER_MIN",
    ".PLANNER_MAX", ".PLANNER_CUTOFF", ".PLANNER_ENGINE",
    ".PLANNER_TEST_IDX", ".PLANNER_NFOLDS", ".PLANNER_REPEATS",
    ".PLANNER_SENS_MIN", ".PLANNER_SPEC_MIN"
  ))
}

.planner_is_integer_valued <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == floor(x)
}

#' Phase 1 execution-planner invariants
#'
#' @return A compact list describing the non-statistical planner contract.
#' @keywords internal
#' @noRd
.planner_contract <- function() {
  list(
    execution_only = TRUE,
    changes_candidate_space = FALSE,
    statistical_screening = FALSE,
    pilot_reduces_rows = FALSE,
    pilot_results_reused = FALSE,
    final_analysis_exact = TRUE
  )
}

#' Count a flat combinatorial workload by requested model size
#'
#' @param n_items Number of available items.
#' @param model_sizes Requested model sizes.
#' @return A list containing the total and a named count vector by size.
#' @keywords internal
#' @noRd
.planner_count_workload <- function(n_items, model_sizes) {
  if (!.planner_is_integer_valued(n_items) || n_items < 1L ||
      n_items > .Machine$integer.max) {
    stop("`n_items` must be a positive integer.", call. = FALSE)
  }
  n_items <- as.integer(n_items)
  if (!is.numeric(model_sizes) || length(model_sizes) == 0L ||
      anyNA(model_sizes) || any(!is.finite(model_sizes)) ||
      any(model_sizes != floor(model_sizes)) || any(model_sizes < 1L) ||
      any(model_sizes > n_items) ||
      any(model_sizes > .Machine$integer.max)) {
    stop("`model_sizes` must contain finite integer-valued sizes between 1 and `n_items`.",
         call. = FALSE)
  }
  sizes <- .resolve_model_sizes(
    model_sizes = as.integer(model_sizes),
    n_available = n_items
  )
  counts <- choose(n_items, sizes)
  if (any(!is.finite(counts)) || any(counts != floor(counts))) {
    stop("Requested candidate count exceeds the finite exact-integer range.",
         call. = FALSE)
  }
  if (any(counts > .PLANNER_MAX_EXACT_INTEGER)) {
    stop("Requested candidate count exceeds the exact integer range supported by the planner.",
         call. = FALSE)
  }
  names(counts) <- as.character(sizes)
  total <- sum(counts)
  if (!is.finite(total) || total > .PLANNER_MAX_EXACT_INTEGER) {
    stop("Total candidate count exceeds the exact integer range supported by the planner.",
         call. = FALSE)
  }

  list(
    total_candidates = total,
    candidate_count_by_size = counts,
    model_sizes = sizes
  )
}

#' Select deterministic ranks spread across a candidate-order range
#'
#' @param total Number of candidates in the range.
#' @param quota Number of representative ranks requested.
#' @return Numeric zero-based ranks.
#' @keywords internal
#' @noRd
.planner_evenly_spaced_ranks <- function(total, quota) {
  if (!.planner_is_integer_valued(total) || total < 1 ||
      total > .PLANNER_MAX_EXACT_INTEGER) {
    stop("`total` must be a positive finite integer-valued number.", call. = FALSE)
  }
  if (!.planner_is_integer_valued(quota) || quota < 1L ||
      quota > .PLANNER_MAX_EXACT_INTEGER) {
    stop("`quota` must be a positive integer.", call. = FALSE)
  }

  quota <- min(as.double(quota), total)
  if (quota > .Machine$integer.max) {
    stop("`quota` exceeds the maximum vector length supported by the planner.",
         call. = FALSE)
  }
  quota <- as.integer(quota)
  if (quota == 1L) {
    return(floor((total - 1) / 2))
  }

  # This quotient/remainder form never forms i * (total - 1).  The final
  # quotient below uses a base-2^16 split so that its intermediate products
  # also remain exact R doubles when quota is an R-sized vector.
  den <- as.double(quota - 1L)
  span <- total - 1
  base <- floor(span / den)
  rem <- span - base * den
  i <- as.double(seq.int(0L, quota - 1L))
  split_base <- 65536
  i_high <- floor(i / split_base)
  i_low <- i - i_high * split_base
  rem_base <- split_base * rem
  rem_quotient <- floor(rem_base / den)
  rem_remainder <- rem_base - rem_quotient * den
  remainder_term <- i_high * rem_remainder + i_low * rem
  remainder_quotient <- i_high * rem_quotient + floor(remainder_term / den)
  ranks <- i * base + remainder_quotient

  if (length(ranks) != quota || any(!is.finite(ranks)) ||
      any(ranks != floor(ranks)) || any(ranks < 0 | ranks >= total) ||
      ranks[1L] != 0 || ranks[quota] != span || any(diff(ranks) <= 0)) {
    stop("Unable to construct exact, distinct representative candidate ranks.",
         call. = FALSE)
  }
  ranks
}

#' Generate a deterministic size-stratified representative pilot set
#'
#' Candidate selection uses only combinatorial ranks and unranking. It never
#' samples observations, invokes RNG, or materializes the full candidate space.
#'
#' @param n_items Number of available items.
#' @param model_sizes Requested model sizes.
#' @param max_candidates Maximum pilot candidates across sizes.
#' @param item_names Optional item names included as a list column.
#' @return A data.frame with size, local/global rank, and combination list columns.
#' @keywords internal
#' @noRd
.planner_make_pilot_candidates <- function(n_items,
                                           model_sizes,
                                           max_candidates = 24L,
                                           item_names = NULL) {
  workload <- .planner_count_workload(n_items, model_sizes)
  sizes <- workload$model_sizes
  counts <- workload$candidate_count_by_size

  if (!.planner_is_integer_valued(max_candidates) ||
      max_candidates < length(sizes) ||
      max_candidates > .Machine$integer.max) {
    stop("`max_candidates` must be an integer at least as large as the number of model sizes.",
         call. = FALSE)
  }
  max_candidates <- as.integer(max_candidates)

  if (!is.null(item_names)) {
    if (!is.character(item_names) || length(item_names) != n_items ||
        anyNA(item_names) || anyDuplicated(item_names)) {
      stop("`item_names` must contain one unique non-missing name per item.",
           call. = FALSE)
    }
  }

  budget <- as.integer(min(as.double(max_candidates), workload$total_candidates))
  quotas <- rep.int(1L, length(sizes))
  capacities <- pmin(as.double(counts), as.double(max_candidates))
  remaining <- budget - sum(quotas)
  while (remaining > 0L) {
    eligible <- which(quotas < capacities)
    if (length(eligible) == 0L) break
    add_to <- utils::head(eligible, remaining)
    quotas[add_to] <- quotas[add_to] + 1L
    remaining <- remaining - length(add_to)
  }

  offsets <- c(0, utils::head(cumsum(counts), -1L))
  rows <- vector("list", length(sizes))
  for (i in seq_along(sizes)) {
    local_ranks <- .planner_evenly_spaced_ranks(counts[i], quotas[i])
    combinations <- lapply(
      local_ranks,
      function(rank) .combination_unrank(n_items, sizes[i], rank)
    )
    row <- data.frame(
      model_size = rep.int(sizes[i], length(local_ranks)),
      rank_within_size = local_ranks,
      global_candidate_index = offsets[i] + local_ranks + 1,
      stringsAsFactors = FALSE
    )
    row$combination <- I(combinations)
    if (!is.null(item_names)) {
      row$items <- I(lapply(combinations, function(x) item_names[x + 1L]))
    }
    rows[[i]] <- row
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "full_candidate_count") <- workload$total_candidates
  attr(result, "representative_only") <- TRUE
  result
}

#' Resolve the resource ceiling available to a flat execution planner
#'
#' @param available_resources Optional injectable machine/CRAN resource limit.
#' @param user_n_workers Optional explicit user cap.
#' @param task_count Optional number of independent tasks/chunks.
#' @return Positive integer effective resource cap.
#' @keywords internal
#' @noRd
.planner_resource_cap <- function(available_resources = NULL,
                                  user_n_workers = NULL,
                                  task_count = NULL) {
  available <- if (is.null(available_resources)) {
    .get_max_workers()
  } else {
    available_resources
  }
  values <- list(available_resources = available,
                 user_n_workers = user_n_workers,
                 task_count = task_count)
  for (name in names(values)) {
    value <- values[[name]]
    if (!is.null(value) &&
        (!.planner_is_integer_valued(value) || value < 0L ||
         value > .Machine$integer.max)) {
      stop(sprintf("`%s` must be a non-negative integer or NULL.", name),
           call. = FALSE)
    }
  }
  cap <- as.integer(available)
  if (!is.null(user_n_workers)) cap <- min(cap, as.integer(user_n_workers))
  if (!is.null(task_count)) cap <- min(cap, as.integer(task_count))
  max(0L, cap)
}

#' Generate a compact legal plan table for a Phase 1 flat API
#'
#' @param api One of `"exhaustive_sum_roc"` or `"cross_size_cv"`.
#' @param available_resources Optional injectable available resource count.
#' @param user_n_workers Optional explicit user resource cap.
#' @param task_count Optional task/chunk count cap.
#' @param engine Computation engine, `"Rcpp"` or `"R"`.
#' @return A data.frame of legal candidate plans.
#' @keywords internal
#' @noRd
.planner_generate_legal_plans <- function(api = c("exhaustive_sum_roc", "cross_size_cv"),
                                          available_resources = NULL,
                                          user_n_workers = NULL,
                                          task_count = NULL,
                                          engine = c("Rcpp", "R")) {
  api <- match.arg(api)
  engine <- match.arg(engine)
  cap <- .planner_resource_cap(
    available_resources = available_resources,
    user_n_workers = user_n_workers,
    task_count = task_count
  )

  plans <- list(data.frame(
    plan_id = "none_1",
    parallel = "none",
    n_workers = 1L,
    resource_count = 1L,
    backend_priority = 1L,
    stringsAsFactors = FALSE
  ))
  resource_levels <- if (cap >= 2L) seq.int(2L, cap) else integer()

  if (engine == "Rcpp" && length(resource_levels) > 0L) {
    plans[[length(plans) + 1L]] <- data.frame(
      plan_id = paste0("threads_", resource_levels),
      parallel = "threads",
      n_workers = resource_levels,
      resource_count = resource_levels,
      backend_priority = 2L,
      stringsAsFactors = FALSE
    )
  }
  if (length(resource_levels) > 0L) {
    plans[[length(plans) + 1L]] <- data.frame(
      plan_id = paste0("chunks_", resource_levels),
      parallel = "chunks",
      n_workers = resource_levels,
      resource_count = resource_levels,
      backend_priority = 3L,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, plans)
  rownames(result) <- NULL
  attr(result, "api") <- api
  attr(result, "resource_cap") <- cap
  result
}

#' Aggregate repeated execution-plan timings robustly
#'
#' @param timings Data.frame containing plan metadata and elapsed seconds.
#' @return One row per plan with median elapsed time and failure counts.
#' @keywords internal
#' @noRd
.planner_aggregate_timings <- function(timings) {
  if (!is.data.frame(timings) || !all(c("plan_id", "elapsed") %in% names(timings))) {
    stop("`timings` must be a data.frame with `plan_id` and `elapsed` columns.",
         call. = FALSE)
  }
  if (nrow(timings) == 0L) return(timings)
  if (is.null(timings$success)) {
    timings$success <- is.finite(timings$elapsed) & timings$elapsed >= 0
  }
  if (is.null(timings$failure_reason)) timings$failure_reason <- NA_character_

  ids <- unique(timings$plan_id)
  rows <- lapply(ids, function(id) {
    block <- timings[timings$plan_id == id, , drop = FALSE]
    explicitly_successful <- vapply(block$success, isTRUE, logical(1))
    ok <- explicitly_successful & is.finite(block$elapsed) & block$elapsed >= 0
    metadata_names <- intersect(
      c("plan_id", "parallel", "n_workers", "resource_count", "backend_priority"),
      names(block)
    )
    out <- block[1L, metadata_names, drop = FALSE]
    out$median_elapsed <- if (any(ok)) stats::median(block$elapsed[ok]) else NA_real_
    out$n_success <- sum(ok)
    out$n_failed <- sum(!ok)
    out$status <- if (any(ok)) "ok" else "failed"
    reasons <- unique(block$failure_reason[!ok & !is.na(block$failure_reason)])
    out$failure_reason <- if (length(reasons)) paste(reasons, collapse = "; ") else NA_character_
    out
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Compute empirical scaling metrics, speedup, efficiency, and saturation
#'
#' @param benchmark_table Aggregated benchmark timings data frame.
#' @param total_candidates Total workload candidate count.
#' @param pilot_candidates Number of candidates in pilot workload.
#' @return A list with updated benchmark_table, saturation info, and serial median.
#' @keywords internal
#' @noRd
.planner_fit_affine_runtime <- function(timings, total_units) {
  required <- c("workload_units", "elapsed")
  if (!is.data.frame(timings) || !all(required %in% names(timings)) ||
      !is.numeric(total_units) || length(total_units) != 1L ||
      !is.finite(total_units) || total_units < 1) {
    return(list(method = "unavailable", status = "unavailable",
                setup_seconds = NA_real_, seconds_per_unit = NA_real_,
                estimated_full_runtime = NA_real_))
  }
  success <- if ("success" %in% names(timings)) {
    vapply(timings$success, isTRUE, logical(1))
  } else {
    rep(TRUE, nrow(timings))
  }
  keep <- success & is.finite(timings$workload_units) & timings$workload_units > 0 &
    is.finite(timings$elapsed) & timings$elapsed >= 0
  observed <- timings[keep, c("workload_units", "elapsed"), drop = FALSE]
  if (nrow(observed) < 2L) {
    return(list(method = "unavailable", status = "unavailable",
                setup_seconds = NA_real_, seconds_per_unit = NA_real_,
                estimated_full_runtime = NA_real_))
  }
  medians <- stats::aggregate(elapsed ~ workload_units, data = observed, FUN = stats::median)
  medians <- medians[order(medians$workload_units), , drop = FALSE]
  if (nrow(medians) < 2L) {
    return(list(method = "unavailable", status = "unavailable",
                setup_seconds = NA_real_, seconds_per_unit = NA_real_,
                estimated_full_runtime = NA_real_))
  }
  lo <- medians[1L, , drop = FALSE]
  hi <- medians[nrow(medians), , drop = FALSE]
  denominator <- hi$workload_units - lo$workload_units
  b <- (hi$elapsed - lo$elapsed) / denominator
  a <- lo$elapsed - b * lo$workload_units
  stable <- is.finite(a) && is.finite(b) && is.finite(denominator) && denominator > 0 &&
    a >= 0 && b > 0 && is.finite(a + b * total_units) &&
    abs(b * denominator) > .Machine$double.eps * max(1, abs(lo$elapsed), abs(hi$elapsed))
  if (!stable) {
    return(list(method = "unavailable", status = "unavailable",
                setup_seconds = NA_real_, seconds_per_unit = NA_real_,
                estimated_full_runtime = NA_real_))
  }
  list(method = "empirical_affine", status = "ok", setup_seconds = as.double(a),
       seconds_per_unit = as.double(b),
       estimated_full_runtime = as.double(a + b * total_units))
}

.planner_compute_scaling_metrics <- function(benchmark_table, total_candidates, pilot_candidates,
                                             affine_timings = NULL) {
  if (!is.data.frame(benchmark_table) || nrow(benchmark_table) == 0L) {
    return(list(
      benchmark_table = benchmark_table,
      saturation = list(),
      scaling_summary = list()
    ))
  }
  serial_row <- benchmark_table[benchmark_table$parallel == "none", , drop = FALSE]
  ok_medians <- benchmark_table$median_elapsed[benchmark_table$status == "ok" & is.finite(benchmark_table$median_elapsed)]
  serial_median <- if (nrow(serial_row) > 0L && is.finite(serial_row$median_elapsed[[1L]])) {
    serial_row$median_elapsed[[1L]]
  } else if (length(ok_medians) > 0L) {
    min(ok_medians)
  } else {
    NA_real_
  }

  tbl <- benchmark_table
  eff_serial <- if (is.finite(serial_median) && serial_median > 0) serial_median else 1e-4
  eff_elapsed <- ifelse(is.finite(tbl$median_elapsed) & tbl$median_elapsed > 0,
                        tbl$median_elapsed, 1e-4)
  tbl$speedup <- ifelse(is.finite(tbl$median_elapsed) & is.finite(serial_median),
                        pmin(tbl$resource_count * 1.5, eff_serial / eff_elapsed), NA_real_)
  tbl$speedup[tbl$parallel == "none" & tbl$resource_count == 1L] <- 1.0
  tbl$parallel_efficiency <- ifelse(is.finite(tbl$speedup) & tbl$resource_count > 0,
                                    tbl$speedup / tbl$resource_count, NA_real_)

  # Setup-aware extrapolation is derived solely from measured two-point timings.
  # In particular, PSOCK lifecycle cost is represented by the measured intercept
  # and is never multiplied by the full candidate count.
  est_runtimes <- rep(NA_real_, nrow(tbl))
  estimate_method <- rep("unavailable", nrow(tbl))
  estimate_status <- rep("unavailable", nrow(tbl))
  setup_seconds <- rep(NA_real_, nrow(tbl))
  seconds_per_unit <- rep(NA_real_, nrow(tbl))
  for (i in seq_len(nrow(tbl))) {
    if (!is.null(affine_timings) && is.data.frame(affine_timings) &&
        "plan_id" %in% names(affine_timings)) {
      fit <- .planner_fit_affine_runtime(
        affine_timings[affine_timings$plan_id == tbl$plan_id[i], , drop = FALSE],
        total_candidates
      )
      est_runtimes[i] <- fit$estimated_full_runtime
      estimate_method[i] <- fit$method
      estimate_status[i] <- fit$status
      setup_seconds[i] <- fit$setup_seconds
      seconds_per_unit[i] <- fit$seconds_per_unit
    }
  }
  tbl$estimated_full_runtime <- est_runtimes
  tbl$estimate_method <- estimate_method
  tbl$estimate_status <- estimate_status
  tbl$setup_seconds <- setup_seconds
  tbl$seconds_per_unit <- seconds_per_unit

  # Descriptive Saturation detection per backend
  saturation_info <- list()
  backends <- unique(tbl$parallel[tbl$status == "ok"])
  for (b in backends) {
    b_rows <- tbl[tbl$parallel == b & tbl$status == "ok" & is.finite(tbl$speedup), , drop = FALSE]
    b_rows <- b_rows[order(b_rows$resource_count), , drop = FALSE]
    if (nrow(b_rows) >= 2L) {
      eff <- b_rows$parallel_efficiency
      knee_idx <- which(eff < 0.50 | c(1, diff(b_rows$speedup) / diff(b_rows$resource_count)) < 0.10)
      if (length(knee_idx) > 0L) {
        knee_res <- b_rows$resource_count[knee_idx[1L]]
        saturation_info[[b]] <- sprintf("Scaling flattens beyond %d resources (efficiency: %.2f)",
                                        knee_res, eff[knee_idx[1L]])
      }
    }
  }

  list(
    benchmark_table = tbl,
    saturation = saturation_info,
    serial_median = serial_median,
    scaling_summary = list(
      speedup = tbl$speedup,
      parallel_efficiency = tbl$parallel_efficiency
    )
  )
}

#' Estimate serial runtime from size-stratified pilot timings
#'
#' @param workload Output from `.planner_count_workload()` or a named count vector.
#' @param pilot_timings Data.frame with `model_size`, `n_candidates`, and `elapsed`.
#' @return Approximate runtime estimate and per-size components.
#' @keywords internal
#' @noRd
.planner_estimate_runtime <- function(workload, pilot_timings) {
  counts <- if (is.list(workload)) workload$candidate_count_by_size else workload
  if (is.null(counts) || is.null(names(counts))) {
    stop("`workload` must contain named candidate counts by model size.",
         call. = FALSE)
  }
  required <- c("model_size", "n_candidates", "elapsed")
  if (!is.data.frame(pilot_timings) || !all(required %in% names(pilot_timings))) {
    stop("`pilot_timings` must contain `model_size`, `n_candidates`, and `elapsed`.",
         call. = FALSE)
  }
  valid <- is.finite(pilot_timings$elapsed) & pilot_timings$elapsed >= 0 &
    is.finite(pilot_timings$n_candidates) & pilot_timings$n_candidates > 0
  observed <- pilot_timings[valid, , drop = FALSE]
  if (nrow(observed) == 0L) {
    return(list(
      estimated_serial_runtime = NA_real_,
      runtime_estimation_method = "size_stratified_candidate_cost",
      by_size = data.frame(),
      fallback_reason = "no successful micro-pilot timings"
    ))
  }
  observed$seconds_per_candidate <- observed$elapsed / observed$n_candidates
  global_rate <- stats::median(observed$seconds_per_candidate)

  sizes <- suppressWarnings(as.double(names(counts)))
  if (any(!is.finite(sizes)) || any(sizes != floor(sizes))) {
    stop("`workload` candidate-count names must be integer-valued model sizes.",
         call. = FALSE)
  }
  rows <- lapply(seq_along(sizes), function(i) {
    rates <- observed$seconds_per_candidate[
      !is.na(observed$model_size) & observed$model_size == sizes[i]
    ]
    rate <- if (length(rates)) stats::median(rates) else global_rate
    source <- if (length(rates)) "size_specific" else "global_fallback"
    data.frame(
      model_size = sizes[i],
      candidate_count = as.double(counts[i]),
      seconds_per_candidate = rate,
      estimated_seconds = as.double(counts[i]) * rate,
      rate_source = source,
      stringsAsFactors = FALSE
    )
  })
  by_size <- do.call(rbind, rows)
  rownames(by_size) <- NULL
  list(
    estimated_serial_runtime = sum(by_size$estimated_seconds),
    runtime_estimation_method = "size_stratified_candidate_cost",
    by_size = by_size,
    fallback_reason = if (any(by_size$rate_source == "global_fallback")) {
      "one or more model sizes used the global median candidate cost"
    } else {
      NA_character_
    }
  )
}

#' Decide whether full backend benchmarking is justified
#'
#' @param estimated_serial_runtime Approximate serial runtime in seconds.
#' @param threshold Internal decision threshold in seconds.
#' @return A list with the decision, threshold, and stable reason text.
#' @keywords internal
#' @noRd
.planner_should_benchmark <- function(
    estimated_serial_runtime,
    threshold = .PLANNER_AUTO_RUNTIME_THRESHOLD) {
  values <- list(estimated_serial_runtime = estimated_serial_runtime,
                 threshold = threshold)
  for (name in names(values)) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0) {
      stop(sprintf("`%s` must be a non-negative finite number.", name),
           call. = FALSE)
    }
  }
  perform <- estimated_serial_runtime >= threshold
  list(
    backend_benchmark_required = perform,
    auto_runtime_threshold = threshold,
    reason = if (perform) {
      "estimated runtime justifies backend benchmarking"
    } else {
      "estimated workload too small for backend benchmarking"
    }
  )
}

#' Decide whether a complete resource sweep fits the approved overhead budget
#' @keywords internal
#' @noRd
.planner_sweep_gate <- function(estimated_serial_runtime, expected_sweep_seconds,
                                threshold = .PLANNER_AUTO_RUNTIME_THRESHOLD,
                                overhead_ratio = .PLANNER_BENCHMARK_OVERHEAD_RATIO) {
  primary <- .planner_should_benchmark(estimated_serial_runtime, threshold)
  if (!isTRUE(primary$backend_benchmark_required)) {
    return(c(primary, list(allowed = FALSE, overhead_budget_seconds = NA_real_,
                           reason = primary$reason)))
  }
  if (!is.numeric(expected_sweep_seconds) || length(expected_sweep_seconds) != 1L ||
      !is.finite(expected_sweep_seconds) || expected_sweep_seconds < 0 ||
      !is.numeric(overhead_ratio) || length(overhead_ratio) != 1L ||
      !is.finite(overhead_ratio) || overhead_ratio <= 0) {
    return(c(primary, list(allowed = FALSE, overhead_budget_seconds = NA_real_,
                           reason = "benchmark_budget_insufficient")))
  }
  budget <- estimated_serial_runtime * overhead_ratio
  list(backend_benchmark_required = TRUE, allowed = expected_sweep_seconds <= budget,
       auto_runtime_threshold = threshold, overhead_budget_seconds = budget,
       expected_sweep_seconds = expected_sweep_seconds,
       reason = if (expected_sweep_seconds <= budget) {
         "estimated runtime justifies backend benchmarking"
       } else "benchmark_budget_insufficient")
}

#' Classify whether a deterministic pilot can exercise a legal plan
#' @keywords internal
#' @noRd
.planner_plan_workload_status <- function(plan, candidate_count, grain_size = 1L,
                                          task_count = candidate_count,
                                          outer_task_count = 1L) {
  workers <- as.integer(plan$n_workers[[1L]])
  parallel_mode <- plan$parallel[[1L]]
  candidate_tasks <- floor(as.double(candidate_count) / max(1L, as.integer(grain_size)))
  task_count <- as.double(task_count)
  outer_task_count <- as.double(outer_task_count)
  sufficient <- switch(parallel_mode,
    none = TRUE,
    threads = candidate_tasks >= workers,
    chunks = task_count >= workers,
    outer = outer_task_count >= workers,
    hybrid = outer_task_count >= as.integer(plan$outer_workers[[1L]]) &&
      candidate_tasks >= as.integer(plan$threads_per_worker[[1L]]),
    FALSE
  )
  list(
    status = if (sufficient) "pending" else "insufficient_workload",
    sufficient = sufficient,
    failure_reason = if (sufficient) NA_character_ else sprintf(
      "insufficient_workload: candidates=%s, candidate_tasks=%s, tasks=%s, outer_tasks=%s",
      as.character(candidate_count), as.character(candidate_tasks),
      as.character(task_count), as.character(outer_task_count)
    )
  )
}

.planner_plan_status_table <- function(plans, candidate_count, grain_size = 1L,
                                       task_count = candidate_count,
                                       outer_task_count = 1L) {
  rows <- lapply(seq_len(nrow(plans)), function(i) {
    status <- .planner_plan_workload_status(plans[i, , drop = FALSE], candidate_count,
                                            grain_size, task_count, outer_task_count)
    cbind(plans[i, , drop = FALSE], status = status$status,
          failure_reason = status$failure_reason, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.planner_append_unmeasured_plans <- function(aggregated, plan_statuses) {
  if (!is.data.frame(aggregated) || nrow(aggregated) == 0L) {
    out <- plan_statuses
    out$median_elapsed <- rep(NA_real_, nrow(out))
    out$n_success <- rep(0L, nrow(out))
    out$n_failed <- rep(0L, nrow(out))
    return(out)
  }
  unmeasured <- plan_statuses[plan_statuses$status != "pending", , drop = FALSE]
  if (nrow(unmeasured) == 0L) return(aggregated)
  unmeasured$median_elapsed <- NA_real_
  unmeasured$n_success <- 0L
  unmeasured$n_failed <- 0L
  keep <- intersect(c("plan_id", "parallel", "n_workers", "outer_workers", "threads_per_worker", "resource_count", "backend_priority",
                      "median_elapsed", "n_success", "n_failed", "status", "failure_reason"),
                    union(names(aggregated), names(unmeasured)))
  for (name in setdiff(keep, names(aggregated))) aggregated[[name]] <- NA
  for (name in setdiff(keep, names(unmeasured))) unmeasured[[name]] <- NA
  rbind(aggregated[, keep, drop = FALSE], unmeasured[, keep, drop = FALSE])
}

#' Select a near-best execution plan with a lower-resource preference
#'
#' @param benchmark_table Aggregated benchmark table.
#' @param tolerance Relative near-best tolerance (default 5 percent).
#' @param fallback_plan Optional one-row fallback plan.
#' @return Selection metadata including a safe fallback reason when needed.
#' @keywords internal
#' @noRd
.planner_select_plan <- function(benchmark_table,
                                 tolerance = .PLANNER_SELECTION_TOLERANCE,
                                 fallback_plan = NULL,
                                 allow_nested = FALSE) {
  if (!is.data.frame(benchmark_table) ||
      !all(c("parallel", "n_workers", "resource_count", "median_elapsed") %in%
           names(benchmark_table))) {
    stop("`benchmark_table` is missing required plan-selection columns.",
         call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || anyNA(tolerance) ||
      tolerance < 0 || !is.finite(tolerance)) {
    stop("`tolerance` must be a non-negative finite number.", call. = FALSE)
  }
  if (isTRUE(allow_nested)) {
    benchmark_table <- .planner_validate_nested_plan(
      benchmark_table,
      argument_name = "benchmark_table"
    )
    if (is.null(benchmark_table$status)) {
      benchmark_table$status <- ifelse(is.finite(benchmark_table$median_elapsed),
                                       "ok", "failed")
    }
    if (is.null(benchmark_table$backend_priority)) {
      benchmark_table$backend_priority <- match(
        benchmark_table$parallel, c("none", "threads", "outer", "chunks", "hybrid")
      )
    }
    if (is.null(benchmark_table$plan_id)) {
      benchmark_table$plan_id <- paste0(
        benchmark_table$parallel, "_", benchmark_table$n_workers
      )
    }
    if (!is.null(fallback_plan)) {
      fallback_plan <- .planner_validate_nested_fallback_plan(fallback_plan)
    }
  } else {
    benchmark_table <- .planner_validate_flat_plan(
      benchmark_table,
      argument_name = "benchmark_table"
    )
    if (is.null(benchmark_table$status)) {
      benchmark_table$status <- ifelse(is.finite(benchmark_table$median_elapsed),
                                       "ok", "failed")
    }
    if (is.null(benchmark_table$backend_priority)) {
      benchmark_table$backend_priority <- match(
        benchmark_table$parallel, c("none", "threads", "chunks")
      )
    }
    if (is.null(benchmark_table$plan_id)) {
      benchmark_table$plan_id <- paste0(
        benchmark_table$parallel, "_", benchmark_table$n_workers
      )
    }
    if (!is.null(fallback_plan)) {
      fallback_plan <- .planner_validate_fallback_plan(fallback_plan)
    }
  }

  successful <- benchmark_table[
    benchmark_table$status == "ok" &
      is.finite(benchmark_table$median_elapsed) &
      benchmark_table$median_elapsed >= 0,
    , drop = FALSE
  ]
  fallback_reason <- NA_character_
  fastest <- NA_real_

  if (nrow(successful) == 0L) {
    if (is.null(fallback_plan)) {
      selected <- data.frame(
        plan_id = "none_1", parallel = "none", n_workers = 1L,
        resource_count = 1L, backend_priority = 1L,
        median_elapsed = NA_real_, stringsAsFactors = FALSE
      )
    } else {
      selected <- fallback_plan
    }
    fallback_reason <- "all benchmark plans failed; using safe fallback plan"
  } else {
    fastest <- min(successful$median_elapsed)
    near_best <- successful[
      successful$median_elapsed <= fastest * (1 + tolerance),
      , drop = FALSE
    ]
    ord <- order(
      near_best$resource_count,
      near_best$backend_priority,
      near_best$median_elapsed,
      near_best$plan_id
    )
    selected <- near_best[ord[1L], , drop = FALSE]
  }

  selected_values <- list(
    n_workers = selected$n_workers[1L],
    resource_count = selected$resource_count[1L]
  )
  for (name in names(selected_values)) {
    value <- selected_values[[name]]
    if (!.planner_is_integer_valued(value) || value < 1L ||
        value > .Machine$integer.max) {
      stop(sprintf("Selected plan `%s` must be a positive R-sized integer.", name),
           call. = FALSE)
    }
  }

  list(
    selected_plan = selected,
    selected_parallel = selected$parallel[1L],
    selected_n_workers = as.integer(selected$n_workers[1L]),
    selected_resource_count = as.integer(selected$resource_count[1L]),
    fastest_observed_elapsed = fastest,
    selection_tolerance = tolerance,
    fallback_reason = fallback_reason
  )
}

.planner_validate_nested_plan <- function(plan, argument_name = "plan") {
  required <- c("parallel", "n_workers", "resource_count")
  if (!is.data.frame(plan) || !all(required %in% names(plan))) {
    stop(sprintf("`%s` must be a data.frame with `parallel`, `n_workers`, and `resource_count`.",
                 argument_name),
         call. = FALSE)
  }
  n_plan <- nrow(plan)
  parallel_mode <- plan$parallel
  if (!is.character(parallel_mode) || length(parallel_mode) != n_plan ||
      anyNA(parallel_mode) || any(!(parallel_mode %in% c("none", "threads", "outer", "chunks", "hybrid")))) {
    stop(sprintf("`%s$parallel` must contain only valid execution modes.",
                 argument_name),
         call. = FALSE)
  }
  for (name in c("n_workers", "resource_count")) {
    value <- plan[[name]]
    if (!is.numeric(value) || length(value) != n_plan || anyNA(value) ||
        any(!is.finite(value)) || any(value != floor(value)) ||
        any(value < 1L) || any(value > .Machine$integer.max)) {
      stop(sprintf("`%s$%s` must contain positive R-sized integers.",
                   argument_name, name),
           call. = FALSE)
    }
  }
  plan
}

.planner_validate_nested_fallback_plan <- function(fallback_plan) {
  if (!is.data.frame(fallback_plan) || nrow(fallback_plan) != 1L) {
    stop("`fallback_plan` must be a one-row data.frame.", call. = FALSE)
  }
  .planner_validate_nested_plan(fallback_plan, argument_name = "fallback_plan")
}

#' Validate one or more legal flat execution plans without repairing them
#'
#' @keywords internal
#' @noRd
.planner_validate_flat_plan <- function(plan, argument_name = "plan") {
  required <- c("parallel", "n_workers", "resource_count")
  if (!is.data.frame(plan) || !all(required %in% names(plan))) {
    stop(sprintf("`%s` must be a data.frame with `parallel`, `n_workers`, and `resource_count`.",
                 argument_name),
         call. = FALSE)
  }
  n_plan <- nrow(plan)
  parallel_mode <- plan$parallel
  if (!is.character(parallel_mode) || length(parallel_mode) != n_plan ||
      anyNA(parallel_mode) || any(!(parallel_mode %in% c("none", "threads", "chunks")))) {
    stop(sprintf("`%s$parallel` must contain only `none`, `threads`, or `chunks`.",
                 argument_name),
         call. = FALSE)
  }
  for (name in c("n_workers", "resource_count")) {
    value <- plan[[name]]
    if (!is.numeric(value) || length(value) != n_plan || anyNA(value) ||
        any(!is.finite(value)) || any(value != floor(value)) ||
        any(value < 1L) || any(value > .Machine$integer.max)) {
      stop(sprintf("`%s$%s` must contain positive R-sized integers.",
                   argument_name, name),
           call. = FALSE)
    }
  }
  n_workers <- plan$n_workers
  resource_count <- plan$resource_count
  if (any(parallel_mode == "none" &
          (n_workers != 1L | resource_count != 1L)) ||
      any(parallel_mode != "none" & n_workers != resource_count)) {
    stop(sprintf("`%s` must describe only legal flat execution plans.",
                  argument_name),
         call. = FALSE)
  }
  plan
}

#' Validate a custom fallback plan without changing its requested semantics
#'
#' @keywords internal
#' @noRd
.planner_validate_fallback_plan <- function(fallback_plan) {
  if (!is.data.frame(fallback_plan) || nrow(fallback_plan) != 1L) {
    stop("`fallback_plan` must be a one-row data.frame.", call. = FALSE)
  }
  .planner_validate_flat_plan(fallback_plan, argument_name = "fallback_plan")
}

#' Evaluate an expression without changing the caller's global RNG state
#'
#' @param expr Expression to evaluate.
#' @return The expression result.
#' @keywords internal
#' @noRd
.planner_with_preserved_rng <- function(expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  eval.parent(substitute(expr))
}

#' Compact non-identifying execution environment summary
#'
#' @return A list suitable for future execution-plan metadata.
#' @keywords internal
#' @noRd
.planner_environment_summary <- function(package_version = NULL,
                                         logical_cores = NULL,
                                         physical_cores = NULL) {
  normalise_cores <- function(value) {
    if (!.planner_is_integer_valued(value) || value < 1L ||
        value > .Machine$integer.max) {
      return(NA_integer_)
    }
    as.integer(value)
  }
  detect_cores <- function(logical) {
    normalise_cores(tryCatch(
      parallel::detectCores(logical = logical),
      error = function(e) NA_integer_
    ))
  }
  version <- if (!is.null(package_version)) {
    if (!is.character(package_version) || length(package_version) != 1L ||
        is.na(package_version) || !nzchar(package_version)) {
      stop("`package_version` must be a non-missing character scalar or NULL.",
           call. = FALSE)
    }
    list(value = package_version, source = "injected")
  } else if ("NCVROC" %in% loadedNamespaces()) {
    list(value = as.character(getNamespaceVersion("NCVROC")),
         source = "active_namespace")
  } else {
    installed <- tryCatch(utils::packageVersion("NCVROC"), error = function(e) NULL)
    list(value = if (is.null(installed)) NA_character_ else as.character(installed),
         source = if (is.null(installed)) "unavailable" else "installed_package")
  }
  list(
    os = unname(Sys.info()["sysname"]),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    detected_logical_cores = if (is.null(logical_cores)) detect_cores(TRUE) else normalise_cores(logical_cores),
    detected_physical_cores = if (is.null(physical_cores)) detect_cores(FALSE) else normalise_cores(physical_cores),
    rcppparallel_default_threads = if (requireNamespace("RcppParallel", quietly = TRUE)) {
      RcppParallel::defaultNumThreads()
    } else {
      NA_integer_
    },
    package_version = version$value,
    package_version_source = version$source
  )
}

# Phase 1.1 exhaustive-search integration helpers.  These are intentionally
# internal: public callers only opt in with exhaustive_sum_roc(tuning = ...).
.PLANNER_EXHAUSTIVE_PILOT_BUDGET <- 96L

.planner_or <- function(value, default) if (is.null(value)) default else value

.planner_manual_exhaustive_plan <- function(parallel_mode, n_workers,
                                            total_candidates, chunk_size) {
  if (identical(parallel_mode, "none")) {
    return(data.frame(parallel = "none", n_workers = 1L,
                      resource_count = 1L, stringsAsFactors = FALSE))
  }
  if (identical(parallel_mode, "threads")) {
    workers <- .resolve_n_workers(parallel = TRUE, n_workers = n_workers)
  } else {
    n_chunks <- ceiling(total_candidates / as.integer(chunk_size))
    workers <- .resolve_n_workers(
      parallel = "chunks", n_workers = n_workers, max_tasks = n_chunks
    )
  }
  data.frame(parallel = parallel_mode, n_workers = as.integer(workers),
             resource_count = as.integer(workers), stringsAsFactors = FALSE)
}

.planner_exhaustive_ranks <- function(pilot) {
  as.double(pilot$global_candidate_index - 1)
}

.planner_evaluate_exhaustive_ranks <- function(x_mat, y, items, min_items,
                                                max_items, cutoff_method,
                                                global_ranks, engine,
                                                parallel = "none", n_workers = 1L) {
  global_ranks <- as.double(global_ranks)
  if (!length(global_ranks)) return(invisible(NULL))
  combos <- lapply(global_ranks, function(rank) {
    resolved <- .resolve_global_combination_rank(
      length(items), min_items, max_items, rank
    )
    .combination_unrank(length(items), resolved$k, resolved$rank_within_k)
  })
  # Match the production evaluator's candidate-label formatting work.
  invisible(vapply(combos, function(idx) format_items(items[idx + 1L]), character(1)))
  if (identical(engine, "Rcpp")) {
    if (identical(parallel, "threads") && n_workers > 1L) {
      invisible(evaluate_combos_cpp_sparse_parallel(
        x_mat, y, min_items, max_items, cutoff_method, global_ranks,
        as.integer(n_workers)
      ))
    } else {
      invisible(evaluate_combos_cpp(
        x_mat, y, combos, cutoff_method
      ))
    }
    return(invisible(NULL))
  }
  for (idx in combos) {
    combo_items <- items[idx + 1L]
    scores <- rowSums(x_mat[, combo_items, drop = FALSE])
    freq <- compute_score_frequencies(scores, y)
    metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
    find_optimal_cutoff(metrics, method = cutoff_method)
  }
  invisible(NULL)
}

.planner_default_timer <- function(expr) {
  started <- proc.time()[["elapsed"]]
  force(expr)
  proc.time()[["elapsed"]] - started
}

.planner_default_clock <- function() proc.time()[["elapsed"]]

.planner_with_chunk_benchmark_lifecycle <- function(plan, setup, evaluate,
                                                     cluster_factory = parallel::makePSOCKcluster,
                                                     cluster_stopper = parallel::stopCluster) {
  cl <- cluster_factory(plan$n_workers[[1L]])
  on.exit(cluster_stopper(cl), add = TRUE)
  setup(cl)
  evaluate(cl)
}

.planner_benchmark_exhaustive_candidates <- function(x_mat, y, items,
                                                      min_items, max_items,
                                                      cutoff_method, engine,
                                                      global_ranks, plan,
                                                      timer = .planner_default_timer,
                                                      lifecycle = .planner_with_chunk_benchmark_lifecycle) {
  plan <- .planner_validate_fallback_plan(plan)
  run_once <- function() {
    if (!identical(plan$parallel[[1L]], "chunks")) {
      return(timer(.planner_evaluate_exhaustive_ranks(
        x_mat, y, items, min_items, max_items, cutoff_method, global_ranks,
        engine, plan$parallel[[1L]], plan$n_workers[[1L]]
      )))
    }
    data_env <- new.env(parent = emptyenv())
    data_env$.PLANNER_X <- x_mat; data_env$.PLANNER_Y <- y
    data_env$.PLANNER_ITEMS <- items; data_env$.PLANNER_MIN <- min_items
    data_env$.PLANNER_MAX <- max_items; data_env$.PLANNER_CUTOFF <- cutoff_method
    data_env$.PLANNER_ENGINE <- engine
    splits <- split(global_ranks, rep(seq_len(plan$n_workers[[1L]]), length.out = length(global_ranks)))
    # The timed expression owns startup, exports, evaluation, and shutdown.
    timer(lifecycle(
      plan,
      setup = function(cl) {
        lib_paths <- .libPaths()
        parallel::clusterExport(cl, "lib_paths", envir = environment())
        parallel::clusterEvalQ(cl, {
          .libPaths(lib_paths)
          if (requireNamespace("NCVROC", quietly = TRUE)) library(NCVROC)
          NULL
        })
        ns <- asNamespace("NCVROC")
        helpers <- c(".planner_evaluate_exhaustive_ranks", ".resolve_global_combination_rank",
                     ".combination_unrank", "compute_score_frequencies",
                     "compute_roc_metrics_from_table", "find_optimal_cutoff",
                     "evaluate_combos_cpp", "evaluate_combos_cpp_sparse_parallel")
        helpers <- intersect(helpers, ls(ns, all.names = TRUE))
        if (length(helpers)) parallel::clusterExport(cl, helpers, envir = ns)
        parallel::clusterExport(cl, ls(data_env, all.names = TRUE), envir = data_env)
      },
      evaluate = function(cl) parallel::parLapply(cl, splits, function(ranks) {
        .planner_evaluate_exhaustive_ranks(
          .PLANNER_X, .PLANNER_Y, .PLANNER_ITEMS, .PLANNER_MIN, .PLANNER_MAX,
          .PLANNER_CUTOFF, ranks, .PLANNER_ENGINE, "none", 1L
        )
      })
    ))
  }
  tryCatch(list(elapsed = as.double(run_once()), success = TRUE,
                failure_reason = NA_character_),
           error = function(e) list(elapsed = NA_real_, success = FALSE,
                                     failure_reason = conditionMessage(e)))
}

.planner_exhaustive_controller <- function(x_mat, y, items, min_items, max_items,
                                           cutoff_method, engine, tuning,
                                           manual_parallel_mode, manual_n_workers,
                                           chunk_size, dependencies = list()) {
  timer <- .planner_or(dependencies$timer, .planner_default_timer)
  resource_detector <- .planner_or(dependencies$resource_detector, .get_max_workers)
  benchmark_executor <- .planner_or(
    dependencies$benchmark_executor, .planner_benchmark_exhaustive_candidates
  )
  threshold <- .planner_or(
    dependencies$auto_runtime_threshold, .PLANNER_AUTO_RUNTIME_THRESHOLD
  )
  clock <- .planner_or(dependencies$clock, .planner_default_clock)
  effective_chunk_size <- if (is.null(chunk_size) || !is.numeric(chunk_size) ||
                              length(chunk_size) != 1L || is.na(chunk_size) ||
                              chunk_size <= 0) 200000L else as.integer(chunk_size)
  model_sizes <- seq.int(as.integer(min_items), min(as.integer(max_items), length(items)))
  workload <- .planner_count_workload(length(items), model_sizes)
  manual_plan <- .planner_manual_exhaustive_plan(
    manual_parallel_mode, manual_n_workers, workload$total_candidates, effective_chunk_size
  )
  metadata <- list(
    planner_version             = .PLANNER_VERSION,
    target_api                  = "exhaustive_sum_roc",
    tuning_mode                 = tuning,
    tuning_performed            = TRUE,
    backend_benchmark_performed = FALSE,
    total_candidates            = workload$total_candidates,
    candidate_count_by_size     = workload$candidate_count_by_size,
    micro_pilot_candidates      = list(total = 0L, by_size = integer()),
    micro_pilot_elapsed         = NA_real_,
    estimated_serial_runtime    = NA_real_,
    auto_runtime_threshold      = threshold,
    benchmark_table             = data.frame(),
    selected_parallel           = manual_plan$parallel[[1L]],
    selected_n_workers          = manual_plan$n_workers[[1L]],
    selected_resource_count     = manual_plan$resource_count[[1L]],
    estimated_runtime           = NA_real_,
    runtime_estimation_method   = "size_stratified_candidate_cost",
    runtime_estimate_status     = "fallback",
    selection_tolerance         = .PLANNER_SELECTION_TOLERANCE,
    decision_reason             = NA_character_,
    fallback_reason             = NA_character_,
    planner_elapsed             = NA_real_,
    benchmark_repeat_count      = integer(),
    warmup_performed            = FALSE,
    manual_parallel_requested   = manual_parallel_mode,
    manual_n_workers_requested  = manual_n_workers,
    environment_summary         = .planner_environment_summary(),
    tuning_budget_seconds       = NA_real_,
    tuning_budget_exhausted     = FALSE
  )
  started <- clock()
  outcome <- tryCatch(.planner_with_preserved_rng({
    pilot <- .planner_make_pilot_candidates(length(items), model_sizes,
                                             .PLANNER_EXHAUSTIVE_PILOT_BUDGET)
    metadata$micro_pilot_candidates <- list(
      total = as.integer(nrow(pilot)),
      by_size = as.integer(table(factor(pilot$model_size, levels = model_sizes)) )
    )
    names(metadata$micro_pilot_candidates$by_size) <- as.character(model_sizes)
    ranks <- .planner_exhaustive_ranks(pilot)
    # Untimed serial warm-up uses a deterministic subset.
    .planner_evaluate_exhaustive_ranks(x_mat, y, items, min_items, max_items,
                                       cutoff_method, ranks[seq_len(min(length(ranks), length(model_sizes)))], engine)
    metadata$warmup_performed <- list(serial = TRUE, plans = character())
    timing_rows <- lapply(model_sizes, function(k) {
      selected <- ranks[pilot$model_size == k]
      result <- benchmark_executor(x_mat, y, items, min_items, max_items,
                                   cutoff_method, engine, selected,
                                   data.frame(parallel = "none", n_workers = 1L,
                                              resource_count = 1L), timer)
      data.frame(model_size = k, n_candidates = length(selected), elapsed = result$elapsed,
                 success = isTRUE(result$success),
                 stringsAsFactors = FALSE)
    })
    pilot_timing <- do.call(rbind, timing_rows)
    valid_pilot <- pilot_timing[pilot_timing$success & is.finite(pilot_timing$elapsed) &
                                  pilot_timing$elapsed >= 0, , drop = FALSE]
    metadata$micro_pilot_elapsed <- sum(valid_pilot$elapsed, na.rm = TRUE)
    estimate <- .planner_estimate_runtime(workload, valid_pilot)
    metadata$estimated_serial_runtime <- estimate$estimated_serial_runtime
    metadata$runtime_estimation_method <- estimate$runtime_estimation_method
    if (!is.finite(estimate$estimated_serial_runtime)) {
      metadata$fallback_reason <- "runtime estimate unavailable; using manual plan"
      metadata$decision_reason <- metadata$fallback_reason
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }
    detected <- tryCatch(resource_detector(), error = function(e) NA_integer_)
    if (!.planner_is_integer_valued(detected) || detected < 1L) detected <- .get_max_workers()
    task_chunks <- ceiling(workload$total_candidates / effective_chunk_size)
    thread_tasks <- max(1L, ceiling(workload$total_candidates / 1000L))
    thread_plans <- .planner_generate_legal_plans(
      "exhaustive_sum_roc", detected, manual_n_workers, thread_tasks, engine
    )
    chunk_plans <- .planner_generate_legal_plans(
      "exhaustive_sum_roc", detected, manual_n_workers, task_chunks, engine
    )
    all_plans <- rbind(
      thread_plans[thread_plans$parallel != "chunks", , drop = FALSE],
      chunk_plans[chunk_plans$parallel == "chunks", , drop = FALSE]
    )
    all_plans <- all_plans[!duplicated(paste(all_plans$parallel, all_plans$n_workers)), , drop = FALSE]
    status_plans <- .planner_plan_status_table(
      all_plans, length(ranks), grain_size = 1L, task_count = length(ranks)
    )
    nonserial <- status_plans$parallel != "none"
    degenerate <- workload$total_candidates < 8 || !any(nonserial)
    if (degenerate || !all(status_plans$status == "pending")) {
      status_plans$status[status_plans$status == "pending"] <- "not_benchmarked"
      status_plans$failure_reason[status_plans$status == "not_benchmarked"] <-
        "benchmark not launched because a legal plan has insufficient_workload"
      metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
      metadata$decision_reason <- if (degenerate) "degenerate workload; using manual plan" else "benchmark_budget_insufficient"
      metadata$fallback_reason <- metadata$decision_reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    small_n <- max(1L, as.integer(floor(length(ranks) / 2L)))
    workloads <- list(ranks[seq_len(small_n)], ranks)
    expected_sweep <- (metadata$micro_pilot_elapsed / length(ranks)) *
      sum(vapply(workloads, length, integer(1))) * nrow(status_plans)
    gate <- .planner_sweep_gate(estimate$estimated_serial_runtime, expected_sweep, threshold)
    metadata$tuning_budget_seconds <- gate$overhead_budget_seconds
    if (!isTRUE(gate$allowed)) {
      status_plans$status <- "not_benchmarked"
      status_plans$failure_reason <- gate$reason
      metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
      metadata$decision_reason <- gate$reason
      metadata$fallback_reason <- gate$reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    metadata$backend_benchmark_performed <- TRUE
    benchmark_started <- clock()
    rows <- list(); row_i <- 0L
    for (i in seq_len(nrow(status_plans))) {
      plan <- status_plans[i, c("parallel", "n_workers", "resource_count"), drop = FALSE]
      if (plan$parallel != "chunks") {
        .planner_evaluate_exhaustive_ranks(
          x_mat, y, items, min_items, max_items, cutoff_method,
          workloads[[1L]], engine, plan$parallel, plan$n_workers
        )
        metadata$warmup_performed$plans <- c(metadata$warmup_performed$plans, status_plans$plan_id[i])
      }
      for (units in workloads) {
        result <- benchmark_executor(x_mat, y, items, min_items, max_items,
                                     cutoff_method, engine, units, plan, timer)
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(plan_id = status_plans$plan_id[i], parallel = plan$parallel,
          n_workers = plan$n_workers, resource_count = plan$resource_count,
          backend_priority = status_plans$backend_priority[i], workload_units = length(units),
          elapsed = result$elapsed, success = result$success, failure_reason = result$failure_reason,
          stringsAsFactors = FALSE)
      }
    }
    raw <- do.call(rbind, rows)
    if (clock() - benchmark_started > metadata$tuning_budget_seconds) {
      metadata$tuning_budget_exhausted <- TRUE
      metadata$decision_reason <- "benchmark_budget_insufficient"
      metadata$fallback_reason <- metadata$decision_reason
      metadata$benchmark_table <- .planner_compute_scaling_metrics(
        .planner_aggregate_timings(raw[raw$workload_units == length(ranks), , drop = FALSE]),
        workload$total_candidates, length(ranks), raw
      )$benchmark_table
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }
    metadata$benchmark_repeat_count <- table(raw$plan_id)
    raw_aggregated <- .planner_aggregate_timings(raw[raw$workload_units == length(ranks), , drop = FALSE])
    scaling_res <- .planner_compute_scaling_metrics(raw_aggregated, workload$total_candidates, length(ranks), raw)
    metadata$benchmark_table <- scaling_res$benchmark_table
    metadata$saturation_summary <- scaling_res$saturation
    metadata$scaling_summary <- scaling_res$scaling_summary
    valid_estimates <- metadata$benchmark_table$estimate_status[metadata$benchmark_table$status == "ok"]
    metadata$runtime_estimation_method <- if (length(valid_estimates) > 0L && all(valid_estimates == "ok")) "empirical_affine" else "unavailable"
    metadata$runtime_estimate_status <- metadata$runtime_estimation_method
    if (identical(metadata$runtime_estimation_method, "unavailable")) {
      metadata$decision_reason <- "runtime estimate unavailable; using manual plan"
      metadata$fallback_reason <- metadata$decision_reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    selected <- .planner_select_plan(metadata$benchmark_table, fallback_plan = manual_plan)
    metadata$selected_parallel <- selected$selected_parallel
    metadata$selected_n_workers <- selected$selected_n_workers
    metadata$selected_resource_count <- selected$selected_resource_count
    sel_row <- metadata$benchmark_table[metadata$benchmark_table$plan_id == selected$selected_plan_id, , drop = FALSE]
    metadata$estimated_runtime <- if (nrow(sel_row) > 0L && is.finite(sel_row$estimated_full_runtime[[1L]])) {
      sel_row$estimated_full_runtime[[1L]]
    } else if (length(selected$selected_plan$median_elapsed) == 1L && is.finite(selected$selected_plan$median_elapsed[[1L]])) {
      selected$selected_plan$median_elapsed[[1L]] / length(ranks) * workload$total_candidates
    } else NA_real_
    metadata$fallback_reason <- selected$fallback_reason
    metadata$decision_reason <- if (is.na(selected$fallback_reason)) {
      "selected near-best benchmark plan"
    } else {
      selected$fallback_reason
    }
    list(plan = selected$selected_plan[, c("parallel", "n_workers", "resource_count"), drop = FALSE],
         metadata = metadata, warn = !is.na(selected$fallback_reason))
  }), error = function(e) list(plan = manual_plan, metadata = within(metadata, {
    fallback_reason <- paste("planner failure; using manual plan:", conditionMessage(e))
    decision_reason <- fallback_reason
  }), warn = TRUE))
  outcome$metadata$planner_elapsed <- clock() - started
  outcome
}

# ============================================================================
# cross_size_cv Execution Planner Primitives (Phase 1.2)
# ============================================================================

.PLANNER_CROSS_SIZE_CV_PILOT_BUDGET <- 96L

.planner_manual_cross_size_cv_plan <- function(parallel_mode, n_workers) {
  if (identical(parallel_mode, "none")) {
    return(data.frame(parallel = "none", n_workers = 1L,
                      resource_count = 1L, stringsAsFactors = FALSE))
  }
  if (identical(parallel_mode, "threads")) {
    workers <- .resolve_n_workers(parallel = "threads", n_workers = n_workers)
  } else {
    workers <- .resolve_n_workers(parallel = "chunks", n_workers = n_workers)
  }
  data.frame(parallel = parallel_mode, n_workers = as.integer(workers),
             resource_count = as.integer(workers), stringsAsFactors = FALSE)
}

.planner_evaluate_cv_pilot_combos <- function(x_mat, y, combo_indices_list,
                                              test_indices_0based, n_folds_total,
                                              repeats, cutoff_method,
                                              sens_min, spec_min, engine,
                                              parallel = "none", n_workers = 1L) {
  if (!length(combo_indices_list)) return(invisible(NULL))
  if (identical(engine, "Rcpp")) {
    if (identical(parallel, "threads") && n_workers > 1L) {
      pilot_grain <- max(1L, as.integer(floor(length(combo_indices_list) / (n_workers * 2L))))
      invisible(evaluate_combos_cv_cpp(
        x               = x_mat,
        y               = y,
        combo_indices   = combo_indices_list,
        test_indices    = test_indices_0based,
        n_folds         = n_folds_total,
        repeats         = repeats,
        cutoff_method   = cutoff_method,
        sensitivity_min = sens_min,
        specificity_min = spec_min,
        num_threads     = as.integer(n_workers),
        grain_size      = pilot_grain
      ))
    } else {
      invisible(evaluate_combos_cv_cpp(
        x               = x_mat,
        y               = y,
        combo_indices   = combo_indices_list,
        test_indices    = test_indices_0based,
        n_folds         = n_folds_total,
        repeats         = repeats,
        cutoff_method   = cutoff_method,
        sensitivity_min = sens_min,
        specificity_min = spec_min,
        num_threads     = 1L,
        grain_size      = 64L
      ))
    }
    return(invisible(NULL))
  }
  # Engine = "R"
  folds_total <- length(test_indices_0based)
  folds_per_rep <- folds_total %/% max(1L, as.integer(repeats))
  for (idx in combo_indices_list) {
    items_1based <- idx + 1L
    scores <- rowSums(x_mat[, items_1based, drop = FALSE])
    tps <- numeric(folds_total)
    tns <- numeric(folds_total)
    fps <- numeric(folds_total)
    fns <- numeric(folds_total)
    for (f in seq_len(folds_total)) {
      test_idx <- test_indices_0based[[f]] + 1L
      train_scores <- scores[-test_idx]
      train_y <- y[-test_idx]
      freq <- compute_score_frequencies(train_scores, train_y)
      metrics <- compute_roc_metrics_from_table(freq$pos_counts, freq$neg_counts)
      best_cut <- find_optimal_cutoff(metrics, method = cutoff_method)
      test_preds <- ifelse(scores[test_idx] >= best_cut$cutoff, 1L, 0L)
      test_y <- y[test_idx]
      tps[f] <- sum(test_preds == 1L & test_y == 1L)
      tns[f] <- sum(test_preds == 0L & test_y == 0L)
      fps[f] <- sum(test_preds == 1L & test_y == 0L)
      fns[f] <- sum(test_preds == 0L & test_y == 1L)
    }
    rep_sens <- numeric(repeats)
    rep_spec <- numeric(repeats)
    for (r in seq_len(repeats)) {
      r_idx <- ((r - 1L) * folds_per_rep + 1L):(r * folds_per_rep)
      tot_tp <- sum(tps[r_idx])
      tot_tn <- sum(tns[r_idx])
      tot_fp <- sum(fps[r_idx])
      tot_fn <- sum(fns[r_idx])
      rep_sens[r] <- if (tot_tp + tot_fn > 0) tot_tp / (tot_tp + tot_fn) else NA_real_
      rep_spec[r] <- if (tot_tn + tot_fp > 0) tot_tn / (tot_tn + tot_fp) else NA_real_
    }
    mean_sens <- mean(rep_sens, na.rm = TRUE)
    mean_spec <- mean(rep_spec, na.rm = TRUE)
    if (!is.null(sens_min) && (is.na(mean_sens) || mean_sens < sens_min)) next
    if (!is.null(spec_min) && (is.na(mean_spec) || mean_spec < spec_min)) next
    full_freq <- compute_score_frequencies(scores, y)
    full_metrics <- compute_roc_metrics_from_table(full_freq$pos_counts, full_freq$neg_counts)
    best_full <- find_optimal_cutoff(full_metrics, method = cutoff_method)
  }
  invisible(NULL)
}

.planner_benchmark_cv_candidates <- function(x_mat, y, combo_indices_list,
                                             test_indices_0based, n_folds_total,
                                             repeats, cutoff_method,
                                             sens_min, spec_min, engine,
                                             plan,
                                             timer = .planner_default_timer,
                                             lifecycle = .planner_with_chunk_benchmark_lifecycle) {
  plan <- .planner_validate_fallback_plan(plan)
  run_once <- function() {
    if (!identical(plan$parallel[[1L]], "chunks")) {
      return(timer(.planner_evaluate_cv_pilot_combos(
        x_mat               = x_mat,
        y                   = y,
        combo_indices_list  = combo_indices_list,
        test_indices_0based = test_indices_0based,
        n_folds_total       = n_folds_total,
        repeats             = repeats,
        cutoff_method       = cutoff_method,
        sens_min            = sens_min,
        spec_min            = spec_min,
        engine              = engine,
        parallel            = plan$parallel[[1L]],
        n_workers           = plan$n_workers[[1L]]
      )))
    }
    data_env <- new.env(parent = emptyenv())
    data_env$.PLANNER_X <- x_mat
    data_env$.PLANNER_Y <- y
    data_env$.PLANNER_TEST_IDX <- test_indices_0based
    data_env$.PLANNER_NFOLDS <- n_folds_total
    data_env$.PLANNER_REPEATS <- repeats
    data_env$.PLANNER_CUTOFF <- cutoff_method
    data_env$.PLANNER_SENS_MIN <- sens_min
    data_env$.PLANNER_SPEC_MIN <- spec_min
    data_env$.PLANNER_ENGINE <- engine

    splits <- split(
      combo_indices_list,
      rep(seq_len(plan$n_workers[[1L]]), length.out = length(combo_indices_list))
    )

    # The timed expression owns startup, exports, evaluation, and shutdown.
    timer(lifecycle(
      plan,
      setup = function(cl) {
        lib_paths <- .libPaths()
        parallel::clusterExport(cl, "lib_paths", envir = environment())
        parallel::clusterEvalQ(cl, {
          .libPaths(lib_paths)
          if (requireNamespace("NCVROC", quietly = TRUE)) library(NCVROC)
          NULL
        })
        ns <- asNamespace("NCVROC")
        helpers <- c(".planner_evaluate_cv_pilot_combos", ".eval_single_combo_cv",
                     "evaluate_combos_cv_cpp", "compute_score_frequencies",
                     "compute_roc_metrics_from_table", "find_optimal_cutoff")
        helpers <- intersect(helpers, ls(ns, all.names = TRUE))
        if (length(helpers)) parallel::clusterExport(cl, helpers, envir = ns)
        parallel::clusterExport(cl, ls(data_env, all.names = TRUE), envir = data_env)
      },
      evaluate = function(cl) parallel::parLapply(cl, splits, function(sub_combos) {
        .planner_evaluate_cv_pilot_combos(
          x_mat               = .PLANNER_X,
          y                   = .PLANNER_Y,
          combo_indices_list  = sub_combos,
          test_indices_0based = .PLANNER_TEST_IDX,
          n_folds_total       = .PLANNER_NFOLDS,
          repeats             = .PLANNER_REPEATS,
          cutoff_method       = .PLANNER_CUTOFF,
          sens_min            = .PLANNER_SENS_MIN,
          spec_min            = .PLANNER_SPEC_MIN,
          engine              = .PLANNER_ENGINE,
          parallel            = "none",
          n_workers           = 1L
        )
      })
    ))
  }
  tryCatch(
    list(elapsed = as.double(run_once()), success = TRUE, failure_reason = NA_character_),
    error = function(e) list(elapsed = NA_real_, success = FALSE, failure_reason = conditionMessage(e))
  )
}

.planner_cross_size_cv_controller <- function(data_matrix,
                                              y,
                                              item_names,
                                              sizes,
                                              cv_folds,
                                              folds,
                                              repeats,
                                              stratified,
                                              cv_method,
                                              selection_metric,
                                              cutoff_method,
                                              sensitivity_min,
                                              specificity_min,
                                              engine,
                                              tuning,
                                              manual_parallel_mode,
                                              manual_n_workers,
                                              dependencies = list()) {
  timer <- .planner_or(dependencies$timer, .planner_default_timer)
  resource_detector <- .planner_or(dependencies$resource_detector, .get_max_workers)
  benchmark_executor <- .planner_or(dependencies$benchmark_executor, .planner_benchmark_cv_candidates)
  threshold <- .planner_or(dependencies$auto_runtime_threshold, .PLANNER_AUTO_RUNTIME_THRESHOLD)
  clock <- .planner_or(dependencies$clock, .planner_default_clock)

  n_items_total <- length(item_names)
  workload <- .planner_count_workload(n_items_total, sizes)
  manual_plan <- .planner_manual_cross_size_cv_plan(manual_parallel_mode, manual_n_workers)

  n_folds_total <- length(cv_folds)
  test_indices_0based <- lapply(cv_folds, function(f_idx) as.integer(f_idx - 1L))
  sens_min <- if (is.null(sensitivity_min)) -1.0 else as.numeric(sensitivity_min)
  spec_min <- if (is.null(specificity_min)) -1.0 else as.numeric(specificity_min)
  x_mat <- as.matrix(data_matrix[, item_names, drop = FALSE])
  mode(x_mat) <- "double"
  y_int <- as.integer(y)

  metadata <- list(
    planner_version             = .PLANNER_VERSION,
    target_api                  = "cross_size_cv",
    tuning_mode                 = tuning,
    tuning_performed            = TRUE,
    backend_benchmark_performed = FALSE,
    total_candidates            = workload$total_candidates,
    candidate_count_by_size     = workload$candidate_count_by_size,
    micro_pilot_candidates      = list(total = 0L, by_size = integer()),
    micro_pilot_elapsed         = NA_real_,
    estimated_serial_runtime    = NA_real_,
    auto_runtime_threshold      = threshold,
    benchmark_table             = data.frame(),
    selected_parallel           = manual_plan$parallel[[1L]],
    selected_n_workers          = manual_plan$n_workers[[1L]],
    selected_resource_count     = manual_plan$resource_count[[1L]],
    estimated_runtime           = NA_real_,
    runtime_estimation_method   = "size_stratified_candidate_cost",
    runtime_estimate_status     = "fallback",
    selection_tolerance         = .PLANNER_SELECTION_TOLERANCE,
    decision_reason             = NA_character_,
    fallback_reason             = NA_character_,
    planner_elapsed             = NA_real_,
    benchmark_repeat_count      = integer(),
    warmup_performed            = FALSE,
    manual_parallel_requested   = manual_parallel_mode,
    manual_n_workers_requested  = manual_n_workers,
    environment_summary         = .planner_environment_summary(),
    tuning_budget_seconds       = NA_real_,
    tuning_budget_exhausted     = FALSE,
    cv_method                   = cv_method,
    k                           = as.integer(folds),
    repeats                     = as.integer(repeats),
    n_folds_total               = as.integer(n_folds_total)
  )

  started <- clock()
  outcome <- tryCatch(.planner_with_preserved_rng({
    pilot <- .planner_make_pilot_candidates(
      n_items_total, sizes, .PLANNER_CROSS_SIZE_CV_PILOT_BUDGET
    )
    metadata$micro_pilot_candidates <- list(
      total   = as.integer(nrow(pilot)),
      by_size = as.integer(table(factor(pilot$model_size, levels = sizes)))
    )
    names(metadata$micro_pilot_candidates$by_size) <- as.character(sizes)

    # Convert pilot unranked combinations to 0-based column indices
    pilot_combos <- pilot$combination

    # Untimed serial warm-up uses a small deterministic subset
    warmup_combos <- pilot_combos[seq_len(min(length(pilot_combos), length(sizes)))]
    .planner_evaluate_cv_pilot_combos(
      x_mat, y_int, warmup_combos, test_indices_0based, n_folds_total,
      repeats, cutoff_method, sens_min, spec_min, engine, "none", 1L
    )
    metadata$warmup_performed <- list(serial = TRUE, plans = character())

    # Size-stratified serial pilot timing
    timing_rows <- lapply(sizes, function(s) {
      s_indices <- which(pilot$model_size == s)
      s_combos <- pilot_combos[s_indices]
      result <- benchmark_executor(
        x_mat, y_int, s_combos, test_indices_0based, n_folds_total,
        repeats, cutoff_method, sens_min, spec_min, engine,
        data.frame(parallel = "none", n_workers = 1L, resource_count = 1L, stringsAsFactors = FALSE),
        timer
      )
      data.frame(
        model_size   = s,
        n_candidates = length(s_combos),
        elapsed      = result$elapsed,
        success      = isTRUE(result$success),
        stringsAsFactors = FALSE
      )
    })
    pilot_timing <- do.call(rbind, timing_rows)
    valid_pilot <- pilot_timing[
      pilot_timing$success & is.finite(pilot_timing$elapsed) & pilot_timing$elapsed >= 0,
      , drop = FALSE
    ]
    metadata$micro_pilot_elapsed <- sum(valid_pilot$elapsed, na.rm = TRUE)
    estimate <- .planner_estimate_runtime(workload, valid_pilot)
    metadata$estimated_serial_runtime <- estimate$estimated_serial_runtime
    metadata$runtime_estimation_method <- estimate$runtime_estimation_method

    if (!is.finite(estimate$estimated_serial_runtime)) {
      metadata$fallback_reason <- "runtime estimate unavailable; using manual plan"
      metadata$decision_reason <- metadata$fallback_reason
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }

    detected <- tryCatch(resource_detector(), error = function(e) NA_integer_)
    if (!.planner_is_integer_valued(detected) || detected < 1L) detected <- .get_max_workers()

    thread_tasks <- max(1L, as.integer(ceiling(workload$total_candidates / 64L)))
    chunk_tasks  <- max(1L, as.integer(ceiling(workload$total_candidates / 2000L)))

    thread_plans <- .planner_generate_legal_plans(
      "cross_size_cv", detected, manual_n_workers, thread_tasks, engine
    )
    chunk_plans <- .planner_generate_legal_plans(
      "cross_size_cv", detected, manual_n_workers, chunk_tasks, engine
    )

    all_plans <- rbind(
      thread_plans[thread_plans$parallel != "chunks", , drop = FALSE],
      chunk_plans[chunk_plans$parallel == "chunks", , drop = FALSE]
    )
    all_plans <- all_plans[!duplicated(paste(all_plans$parallel, all_plans$n_workers)), , drop = FALSE]

    # The C++ CV evaluator uses a grain of 64 candidates; a plan is not
    # comparable unless its requested threads can receive real grain blocks.
    status_plans <- .planner_plan_status_table(
      all_plans, length(pilot_combos), grain_size = 64L, task_count = length(pilot_combos)
    )
    nonserial <- status_plans$parallel != "none"
    degenerate <- workload$total_candidates < 8 || !any(nonserial)
    if (degenerate || !all(status_plans$status == "pending")) {
      status_plans$status[status_plans$status == "pending"] <- "not_benchmarked"
      status_plans$failure_reason[status_plans$status == "not_benchmarked"] <-
        "benchmark not launched because a legal plan has insufficient_workload"
      metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
      metadata$decision_reason <- if (degenerate) "degenerate workload; using manual plan" else "benchmark_budget_insufficient"
      metadata$fallback_reason <- metadata$decision_reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    small_n <- max(1L, as.integer(floor(length(pilot_combos) / 2L)))
    workloads <- list(pilot_combos[seq_len(small_n)], pilot_combos)
    expected_sweep <- (metadata$micro_pilot_elapsed / length(pilot_combos)) *
      sum(vapply(workloads, length, integer(1))) * nrow(status_plans)
    gate <- .planner_sweep_gate(estimate$estimated_serial_runtime, expected_sweep, threshold)
    metadata$tuning_budget_seconds <- gate$overhead_budget_seconds
    if (!isTRUE(gate$allowed)) {
      status_plans$status <- "not_benchmarked"
      status_plans$failure_reason <- gate$reason
      metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
      metadata$decision_reason <- gate$reason
      metadata$fallback_reason <- gate$reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }

    metadata$backend_benchmark_performed <- TRUE
    benchmark_started <- clock()
    rows <- list(); row_i <- 0L
    for (i in seq_len(nrow(status_plans))) {
      plan <- status_plans[i, c("parallel", "n_workers", "resource_count"), drop = FALSE]
      if (plan$parallel != "chunks") {
        .planner_evaluate_cv_pilot_combos(
          x_mat, y_int, warmup_combos, test_indices_0based, n_folds_total,
          repeats, cutoff_method, sens_min, spec_min, engine,
          plan$parallel[[1L]], plan$n_workers[[1L]]
        )
        metadata$warmup_performed$plans <- c(metadata$warmup_performed$plans, status_plans$plan_id[i])
      }
      for (units in workloads) {
        result <- benchmark_executor(
          x_mat, y_int, units, test_indices_0based, n_folds_total,
          repeats, cutoff_method, sens_min, spec_min, engine, plan, timer
        )
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(
          plan_id = status_plans$plan_id[i], parallel = plan$parallel[[1L]],
          n_workers = plan$n_workers[[1L]], resource_count = plan$resource_count[[1L]],
          backend_priority = status_plans$backend_priority[i], workload_units = length(units),
          elapsed = result$elapsed, success = result$success, failure_reason = result$failure_reason,
          stringsAsFactors = FALSE
        )
      }
    }

    raw <- do.call(rbind, rows)
    if (clock() - benchmark_started > metadata$tuning_budget_seconds) {
      metadata$tuning_budget_exhausted <- TRUE
      metadata$decision_reason <- "benchmark_budget_insufficient"
      metadata$fallback_reason <- metadata$decision_reason
      metadata$benchmark_table <- .planner_compute_scaling_metrics(
        .planner_aggregate_timings(raw[raw$workload_units == length(pilot_combos), , drop = FALSE]),
        workload$total_candidates, length(pilot_combos), raw
      )$benchmark_table
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }
    metadata$benchmark_repeat_count <- table(raw$plan_id)
    raw_aggregated <- .planner_aggregate_timings(raw[raw$workload_units == length(pilot_combos), , drop = FALSE])
    scaling_res <- .planner_compute_scaling_metrics(raw_aggregated, workload$total_candidates, length(pilot_combos), raw)
    metadata$benchmark_table <- scaling_res$benchmark_table
    metadata$saturation_summary <- scaling_res$saturation
    metadata$scaling_summary <- scaling_res$scaling_summary
    valid_estimates <- metadata$benchmark_table$estimate_status[metadata$benchmark_table$status == "ok"]
    metadata$runtime_estimation_method <- if (length(valid_estimates) > 0L && all(valid_estimates == "ok")) "empirical_affine" else "unavailable"
    metadata$runtime_estimate_status <- metadata$runtime_estimation_method
    if (identical(metadata$runtime_estimation_method, "unavailable")) {
      metadata$decision_reason <- "runtime estimate unavailable; using manual plan"
      metadata$fallback_reason <- metadata$decision_reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    selected <- .planner_select_plan(metadata$benchmark_table, fallback_plan = manual_plan)

    metadata$selected_parallel <- selected$selected_parallel
    metadata$selected_n_workers <- selected$selected_n_workers
    metadata$selected_resource_count <- selected$selected_resource_count
    sel_row <- metadata$benchmark_table[metadata$benchmark_table$plan_id == selected$selected_plan_id, , drop = FALSE]
    metadata$estimated_runtime <- if (nrow(sel_row) > 0L && is.finite(sel_row$estimated_full_runtime[[1L]])) {
      sel_row$estimated_full_runtime[[1L]]
    } else if (length(selected$selected_plan$median_elapsed) == 1L && is.finite(selected$selected_plan$median_elapsed[[1L]])) {
      selected$selected_plan$median_elapsed[[1L]] / length(pilot_combos) * workload$total_candidates
    } else NA_real_
    metadata$fallback_reason <- selected$fallback_reason
    metadata$decision_reason <- if (is.na(selected$fallback_reason)) {
      "selected near-best benchmark plan"
    } else {
      selected$fallback_reason
    }

    list(
      plan = selected$selected_plan[, c("parallel", "n_workers", "resource_count"), drop = FALSE],
      metadata = metadata,
      warn = !is.na(selected$fallback_reason)
    )
  }), error = function(e) list(
    plan = manual_plan,
    metadata = within(metadata, {
      fallback_reason <- paste("planner failure; using manual plan:", conditionMessage(e))
      decision_reason <- fallback_reason
    }),
    warn = TRUE
  ))

  outcome$metadata$planner_elapsed <- clock() - started
  outcome
}

# ============================================================================
# Nested / Hybrid Execution Planner Primitives (Phase 2)
# ============================================================================

.PLANNER_NESTED_PILOT_BUDGET <- 32L

#' Resolve the manual execution plan for a nested CV workload
#'
#' @keywords internal
#' @noRd
.planner_manual_nested_plan <- function(parallel_mode, n_workers, threads_per_worker, n_outer_tasks) {
  if (identical(parallel_mode, "none") || isFALSE(parallel_mode)) {
    return(data.frame(
      plan_id = "none_1",
      parallel = "none",
      n_workers = 1L,
      outer_workers = 1L,
      threads_per_worker = 1L,
      resource_count = 1L,
      backend_priority = 1L,
      stringsAsFactors = FALSE
    ))
  }
  if (identical(parallel_mode, "hybrid")) {
    hybrid_budget <- .resolve_hybrid_budget(n_workers, threads_per_worker, n_outer_tasks, warn = FALSE)
    return(data.frame(
      plan_id = sprintf("hybrid_%dx%d", hybrid_budget$n_workers, hybrid_budget$threads_per_worker),
      parallel = "hybrid",
      n_workers = as.integer(hybrid_budget$n_workers),
      outer_workers = as.integer(hybrid_budget$n_workers),
      threads_per_worker = as.integer(hybrid_budget$threads_per_worker),
      resource_count = as.integer(hybrid_budget$total_parallelism),
      backend_priority = 5L,
      stringsAsFactors = FALSE
    ))
  }
  if (identical(parallel_mode, "outer")) {
    workers <- .resolve_n_workers(parallel = "outer", n_workers = n_workers, n_folds = n_outer_tasks)
    return(data.frame(
      plan_id = paste0("outer_", workers),
      parallel = "outer",
      n_workers = as.integer(workers),
      outer_workers = as.integer(workers),
      threads_per_worker = 1L,
      resource_count = as.integer(workers),
      backend_priority = 3L,
      stringsAsFactors = FALSE
    ))
  }
  if (identical(parallel_mode, "threads")) {
    workers <- .resolve_n_workers(parallel = "threads", n_workers = n_workers)
    return(data.frame(
      plan_id = paste0("threads_", workers),
      parallel = "threads",
      n_workers = as.integer(workers),
      outer_workers = 1L,
      threads_per_worker = as.integer(workers),
      resource_count = as.integer(workers),
      backend_priority = 2L,
      stringsAsFactors = FALSE
    ))
  }
  # chunks
  workers <- .resolve_n_workers(parallel = "chunks", n_workers = n_workers)
  data.frame(
    plan_id = paste0("chunks_", workers),
    parallel = "chunks",
    n_workers = as.integer(workers),
    outer_workers = 1L,
    threads_per_worker = 1L,
    resource_count = as.integer(workers),
    backend_priority = 4L,
    stringsAsFactors = FALSE
  )
}

#' Generate a compact legal plan table for nested / hybrid workloads
#'
#' @keywords internal
#' @noRd
.planner_generate_nested_legal_plans <- function(
    api = c("nested_sum_roc", "cross_size_nested_cv"),
    available_resources = NULL,
    user_n_workers = NULL,
    user_threads_per_worker = 1L,
    n_outer_tasks = 5L,
    inner_candidate_tasks = 100L,
    engine = c("Rcpp", "R"),
    is_chunks_allowed = TRUE,
    is_hybrid_allowed = TRUE) {
  api <- match.arg(api)
  engine <- match.arg(engine)
  cap <- .planner_resource_cap(available_resources = available_resources,
                               user_n_workers = NULL,
                               task_count = NULL)

  plans <- list(data.frame(
    plan_id = "none_1",
    parallel = "none",
    n_workers = 1L,
    outer_workers = 1L,
    threads_per_worker = 1L,
    resource_count = 1L,
    backend_priority = 1L,
    stringsAsFactors = FALSE
  ))

  # 1. Threads plans (only if engine == Rcpp, priority = 2L)
  if (engine == "Rcpp") {
    thread_cap <- min(cap, as.integer(inner_candidate_tasks))
    if (!is.null(user_n_workers)) thread_cap <- min(thread_cap, as.integer(user_n_workers))
    thread_levels <- if (thread_cap >= 2L) seq.int(2L, thread_cap) else integer()
    if (length(thread_levels) > 0L) {
      plans[[length(plans) + 1L]] <- data.frame(
        plan_id = paste0("threads_", thread_levels),
        parallel = "threads",
        n_workers = thread_levels,
        outer_workers = 1L,
        threads_per_worker = thread_levels,
        resource_count = thread_levels,
        backend_priority = 2L,
        stringsAsFactors = FALSE
      )
    }
  }

  # 2. Outer PSOCK plans (priority = 3L)
  outer_cap <- min(cap, as.integer(n_outer_tasks))
  if (!is.null(user_n_workers)) outer_cap <- min(outer_cap, as.integer(user_n_workers))
  outer_levels <- if (outer_cap >= 2L) seq.int(2L, outer_cap) else integer()
  if (length(outer_levels) > 0L) {
    plans[[length(plans) + 1L]] <- data.frame(
      plan_id = paste0("outer_", outer_levels),
      parallel = "outer",
      n_workers = outer_levels,
      outer_workers = outer_levels,
      threads_per_worker = 1L,
      resource_count = outer_levels,
      backend_priority = 3L,
      stringsAsFactors = FALSE
    )
  }

  # 3. Chunks plans (if allowed, priority = 4L)
  if (is_chunks_allowed) {
    chunk_cap <- min(cap, as.integer(inner_candidate_tasks))
    if (!is.null(user_n_workers)) chunk_cap <- min(chunk_cap, as.integer(user_n_workers))
    chunk_levels <- if (chunk_cap >= 2L) seq.int(2L, chunk_cap) else integer()
    if (length(chunk_levels) > 0L) {
      plans[[length(plans) + 1L]] <- data.frame(
        plan_id = paste0("chunks_", chunk_levels),
        parallel = "chunks",
        n_workers = chunk_levels,
        outer_workers = 1L,
        threads_per_worker = 1L,
        resource_count = chunk_levels,
        backend_priority = 4L,
        stringsAsFactors = FALSE
      )
    }
  }

  # 4. Hybrid plans (if engine == Rcpp and is_hybrid_allowed, priority = 5L)
  if (engine == "Rcpp" && is_hybrid_allowed && cap >= 4L && n_outer_tasks >= 2L && inner_candidate_tasks >= 2L) {
    candidate_pairs <- list()
    max_k <- min(cap %/% 2L, as.integer(n_outer_tasks))
    if (max_k >= 2L) {
      for (K in 2L:max_k) {
        max_T <- min(cap %/% K, as.integer(inner_candidate_tasks))
        if (!is.null(user_threads_per_worker) && user_threads_per_worker > 1L) {
          max_T <- min(max_T, as.integer(user_threads_per_worker))
        }
        if (max_T >= 2L) {
          for (T_val in 2L:max_T) {
            candidate_pairs[[length(candidate_pairs) + 1L]] <- c(K = as.integer(K), T = as.integer(T_val))
          }
        }
      }
    }
    if (length(candidate_pairs) > 0L) {
      pairs_df <- do.call(rbind, lapply(candidate_pairs, function(p) {
        data.frame(
          plan_id = sprintf("hybrid_%dx%d", p["K"], p["T"]),
          parallel = "hybrid",
          n_workers = as.integer(p["K"]),
          outer_workers = as.integer(p["K"]),
          threads_per_worker = as.integer(p["T"]),
          resource_count = as.integer(p["K"] * p["T"]),
          backend_priority = 5L,
          stringsAsFactors = FALSE
        )
      }))
      pairs_df <- pairs_df[!duplicated(pairs_df$plan_id), , drop = FALSE]
      plans[[length(plans) + 1L]] <- pairs_df
    }
  }

  result <- do.call(rbind, plans)
  rownames(result) <- NULL
  result
}

#' Top-level execution planner controller for nested_sum_roc
#'
#' @keywords internal
#' @noRd
.planner_nested_sum_roc_controller <- function(
    full_data, y, items, outcome_col, min_items, max_items,
    positive_label, negative_label, cutoff_method,
    preselect_top_n, preselect_by, selection_criterion,
    outer_k, inner_k, outer_repeats, inner_repeats,
    stratified, seed, engine, tuning,
    manual_parallel_mode, manual_n_workers, manual_threads_per_worker,
    outer_folds, resource_detector = .get_max_workers,
    threshold = .PLANNER_AUTO_RUNTIME_THRESHOLD,
    clock = proc.time) {

  model_sizes <- as.integer(min_items:max_items)
  workload <- .planner_count_workload(length(items), model_sizes)
  n_outer_tasks <- length(outer_folds)
  manual_plan <- .planner_manual_nested_plan(
    manual_parallel_mode, manual_n_workers, manual_threads_per_worker, n_outer_tasks
  )

  metadata <- list(
    planner_version             = .PLANNER_VERSION,
    target_api                  = "nested_sum_roc",
    tuning_mode                 = tuning,
    tuning_performed            = TRUE,
    backend_benchmark_performed = FALSE,
    total_candidates            = workload$total_candidates,
    candidate_count_by_size     = workload$candidate_count_by_size,
    micro_pilot_candidates      = list(total = 0L, by_size = integer()),
    micro_pilot_elapsed         = NA_real_,
    estimated_serial_runtime    = NA_real_,
    auto_runtime_threshold      = threshold,
    benchmark_table             = data.frame(),
    selected_parallel           = manual_plan$parallel[[1L]],
    selected_n_workers          = manual_plan$n_workers[[1L]],
    selected_resource_count     = manual_plan$resource_count[[1L]],
    estimated_runtime           = NA_real_,
    runtime_estimation_method   = "size_stratified_candidate_cost",
    runtime_estimate_status     = "fallback",
    selection_tolerance         = .PLANNER_SELECTION_TOLERANCE,
    decision_reason             = NA_character_,
    fallback_reason             = NA_character_,
    planner_elapsed             = NA_real_,
    benchmark_repeat_count      = integer(),
    warmup_performed            = FALSE,
    manual_parallel_requested   = manual_parallel_mode,
    manual_n_workers_requested  = manual_n_workers,
    environment_summary         = .planner_environment_summary(),
    tuning_budget_seconds       = NA_real_,
    tuning_budget_exhausted     = FALSE,
    outer_folds                 = as.integer(outer_k),
    inner_folds                 = as.integer(inner_k),
    outer_repeats               = as.integer(outer_repeats),
    inner_repeats               = as.integer(inner_repeats),
    n_outer_tasks               = as.integer(n_outer_tasks),
    selected_outer_workers      = manual_plan$outer_workers[[1L]],
    selected_threads_per_worker = manual_plan$threads_per_worker[[1L]]
  )

  started <- clock()

  outcome <- tryCatch(.planner_with_preserved_rng({
    pilot <- .planner_make_pilot_candidates(
      length(items), model_sizes, .PLANNER_NESTED_PILOT_BUDGET
    )
    metadata$micro_pilot_candidates <- list(
      total = as.integer(nrow(pilot)),
      by_size = as.integer(table(factor(pilot$model_size, levels = model_sizes)))
    )
    names(metadata$micro_pilot_candidates$by_size) <- as.character(model_sizes)

    # 1. Serial pilot timing over complete nested fold structure
    pilot_start <- clock()
    pilot_by_size <- vector("list", length(model_sizes))
    for (si in seq_along(model_sizes)) {
      msize <- model_sizes[[si]]
      sub_pilot <- pilot[pilot$model_size == msize, , drop = FALSE]
      t0 <- clock()
      # Run serial evaluation across all outer folds for this candidate size
      for (f_i in seq_along(outer_folds)) {
        f_test <- outer_folds[[f_i]]
        f_train <- setdiff(seq_along(y), f_test)
        x_tr <- full_data[f_train, items, drop = FALSE]
        y_tr <- y[f_train]
        in_folds <- make_stratified_folds(y_tr, k = inner_k, repeats = inner_repeats, seed = seed)
        for (ci in seq_len(nrow(sub_pilot))) {
          combo_items <- items[sub_pilot$combination[[ci]] + 1L]
          for (f_in in seq_along(in_folds)) {
            in_test <- in_folds[[f_in]]
            in_train <- setdiff(seq_len(nrow(x_tr)), in_test)
            sc_tr <- rowSums(x_tr[in_train, combo_items, drop = FALSE])
            fr_tr <- compute_score_frequencies(sc_tr, y_tr[in_train])
            c_opt <- find_optimal_cutoff(
              compute_roc_metrics_from_table(fr_tr$pos_counts, fr_tr$neg_counts),
              method = cutoff_method
            )
          }
        }
      }
      elapsed_s <- max(0, as.numeric((clock() - t0)[["elapsed"]]))
      pilot_by_size[[si]] <- data.frame(
        model_size = msize,
        n_candidates = nrow(sub_pilot),
        elapsed = elapsed_s,
        stringsAsFactors = FALSE
      )
    }
    pilot_timings_df <- do.call(rbind, pilot_by_size)
    metadata$micro_pilot_elapsed <- max(0, as.numeric((clock() - pilot_start)[["elapsed"]]))

    estimate <- .planner_estimate_runtime(workload, pilot_timings_df)
    metadata$estimated_serial_runtime <- estimate$estimated_serial_runtime
    metadata$runtime_estimation_method <- estimate$runtime_estimation_method

    if (!is.finite(metadata$estimated_serial_runtime)) {
      metadata$fallback_reason <- "runtime estimate unavailable; using manual plan"
      metadata$decision_reason <- metadata$fallback_reason
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }

    # 2. Resource detection and legal plan generation
    detected <- tryCatch(resource_detector(), error = function(e) NA_integer_)
    if (!.planner_is_integer_valued(detected) || detected < 1L) {
      detected <- .get_max_workers()
    }

    inner_tasks <- max(1L, ceiling(workload$total_candidates / 1000L))
    all_plans <- .planner_generate_nested_legal_plans(
      api = "nested_sum_roc",
      available_resources = detected,
      user_n_workers = manual_n_workers,
      user_threads_per_worker = manual_threads_per_worker,
      n_outer_tasks = n_outer_tasks,
      inner_candidate_tasks = inner_tasks,
      engine = engine,
      is_chunks_allowed = identical(manual_parallel_mode, "chunks"),
      is_hybrid_allowed = (engine == "Rcpp")
    )

    # Nested production evaluators currently enumerate their own full candidate
    # space.  They deliberately do not accept the representative pilot ranks.
    # Running them here would therefore make the pilot cosmetic and launch a
    # near-full nested search once per plan.  Keep the full fold/repeat pilot
    # above for the serial estimate, record every legal resource plan, and use
    # the validated manual plan until a rank-bounded nested evaluator exists.
    status_plans <- .planner_plan_status_table(
      all_plans, nrow(pilot), grain_size = 1L, task_count = nrow(pilot),
      outer_task_count = n_outer_tasks
    )
    status_plans$status[status_plans$status == "pending"] <- "insufficient_workload"
    status_plans$failure_reason <- "insufficient_workload: nested pilot has no rank-bounded evaluator"
    metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
    gate <- .planner_sweep_gate(metadata$estimated_serial_runtime, 0, threshold)
    metadata$tuning_budget_seconds <- gate$overhead_budget_seconds
    metadata$decision_reason <- if (nrow(all_plans) <= 1L) {
      "degenerate workload; using manual plan"
    } else if (!isTRUE(gate$backend_benchmark_required)) {
      gate$reason
    } else {
      "benchmark_budget_insufficient"
    }
    metadata$fallback_reason <- metadata$decision_reason
    return(list(plan = manual_plan, metadata = metadata, warn = FALSE))

    # 3. Benchmark legal plans on pilot candidates over complete nested structure
    metadata$backend_benchmark_performed <- TRUE
    raw_timings <- vector("list", nrow(all_plans))

    for (p_idx in seq_len(nrow(all_plans))) {
      p_row <- all_plans[p_idx, , drop = FALSE]
      t_run_start <- clock()
      err_msg <- NA_character_
      bench_ok <- TRUE

      tryCatch({
        p_mode <- p_row$parallel[[1L]]
        p_outer_w <- p_row$outer_workers[[1L]]
        p_threads_w <- p_row$threads_per_worker[[1L]]

        if (p_mode == "none") {
          for (f_i in seq_along(outer_folds)) {
            .evaluate_single_outer_fold(
              i = f_i, outer_folds = outer_folds, full_data = full_data, y = y,
              n_total = length(y), items = items, outcome_col = outcome_col,
              min_items = min_items, max_items = max_items,
              positive_label = positive_label, negative_label = negative_label,
              cutoff_method = cutoff_method, preselect_top_n = min(nrow(pilot), preselect_top_n),
              preselect_by = preselect_by, selection_criterion = selection_criterion,
              inner_k = inner_k, inner_repeats = inner_repeats,
              use_streaming_ncv = FALSE, engine = engine, seed = seed,
              progress = FALSE, verbose = FALSE, cl_chunk = NULL,
              parallel_inner = "none", n_workers_inner = 1L
            )
          }
        } else if (p_mode %in% c("outer", "hybrid") && p_outer_w > 1L) {
          cl_bench <- parallel::makePSOCKcluster(p_outer_w)
          on.exit(try(parallel::stopCluster(cl_bench), silent = TRUE), add = TRUE)
          lib_paths <- .libPaths()
          parallel::clusterExport(cl_bench, "lib_paths", envir = environment())
          parallel::clusterEvalQ(cl_bench, {
            .libPaths(lib_paths)
            if (requireNamespace("NCVROC", quietly = TRUE)) try(library(NCVROC), silent = TRUE)
            NULL
          })
          ns <- asNamespace("NCVROC")
          available_symbols <- intersect(.OUTER_WORKER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
          parallel::clusterExport(cl_bench, varlist = available_symbols, envir = ns)

          parallel::parLapply(cl_bench, seq_along(outer_folds), function(i) {
            .evaluate_single_outer_fold(
              i = i, outer_folds = outer_folds, full_data = full_data, y = y,
              n_total = length(y), items = items, outcome_col = outcome_col,
              min_items = min_items, max_items = max_items,
              positive_label = positive_label, negative_label = negative_label,
              cutoff_method = cutoff_method, preselect_top_n = min(nrow(pilot), preselect_top_n),
              preselect_by = preselect_by, selection_criterion = selection_criterion,
              inner_k = inner_k, inner_repeats = inner_repeats,
              use_streaming_ncv = FALSE, engine = engine, seed = seed,
              progress = FALSE, verbose = FALSE, cl_chunk = NULL,
              parallel_inner = if (p_mode == "hybrid") "threads" else "none",
              n_workers_inner = if (p_mode == "hybrid") p_threads_w else 1L
            )
          })
          parallel::stopCluster(cl_bench)
        } else if (p_mode == "threads") {
          for (f_i in seq_along(outer_folds)) {
            .evaluate_single_outer_fold(
              i = f_i, outer_folds = outer_folds, full_data = full_data, y = y,
              n_total = length(y), items = items, outcome_col = outcome_col,
              min_items = min_items, max_items = max_items,
              positive_label = positive_label, negative_label = negative_label,
              cutoff_method = cutoff_method, preselect_top_n = min(nrow(pilot), preselect_top_n),
              preselect_by = preselect_by, selection_criterion = selection_criterion,
              inner_k = inner_k, inner_repeats = inner_repeats,
              use_streaming_ncv = FALSE, engine = engine, seed = seed,
              progress = FALSE, verbose = FALSE, cl_chunk = NULL,
              parallel_inner = "threads", n_workers_inner = p_threads_w
            )
          }
        }
      }, error = function(e) {
        bench_ok <<- FALSE
        err_msg <<- conditionMessage(e)
      })

      t_run_elapsed <- max(0, as.numeric((clock() - t_run_start)[["elapsed"]]))
      raw_timings[[p_idx]] <- data.frame(
        plan_id = p_row$plan_id[[1L]],
        parallel = p_row$parallel[[1L]],
        n_workers = p_row$n_workers[[1L]],
        outer_workers = p_row$outer_workers[[1L]],
        threads_per_worker = p_row$threads_per_worker[[1L]],
        resource_count = p_row$resource_count[[1L]],
        backend_priority = p_row$backend_priority[[1L]],
        elapsed = t_run_elapsed,
        success = bench_ok,
        failure_reason = err_msg,
        stringsAsFactors = FALSE
      )
    }

    all_raw <- do.call(rbind, raw_timings)
    bench_table <- .planner_aggregate_timings(all_raw)
    extra_cols <- unique(all_raw[, c("plan_id", "outer_workers", "threads_per_worker"), drop = FALSE])
    bench_table <- merge(bench_table, extra_cols, by = "plan_id", all.x = TRUE, sort = FALSE)
    scaling_res <- .planner_compute_scaling_metrics(bench_table, workload$total_candidates, nrow(pilot))
    metadata$benchmark_table <- scaling_res$benchmark_table
    metadata$saturation_summary <- scaling_res$saturation
    metadata$scaling_summary <- scaling_res$scaling_summary

    selected <- .planner_select_plan(metadata$benchmark_table, tolerance = .PLANNER_SELECTION_TOLERANCE, fallback_plan = manual_plan, allow_nested = TRUE)
    chosen_plan <- selected$selected_plan
    metadata$selected_parallel <- chosen_plan$parallel[[1L]]
    metadata$selected_n_workers <- chosen_plan$n_workers[[1L]]
    metadata$selected_resource_count <- chosen_plan$resource_count[[1L]]
    metadata$selected_outer_workers <- if (!is.null(chosen_plan$outer_workers)) chosen_plan$outer_workers[[1L]] else chosen_plan$n_workers[[1L]]
    metadata$selected_threads_per_worker <- if (!is.null(chosen_plan$threads_per_worker)) chosen_plan$threads_per_worker[[1L]] else 1L
    sel_row <- metadata$benchmark_table[metadata$benchmark_table$plan_id == selected$selected_plan_id, , drop = FALSE]
    metadata$estimated_runtime <- if (nrow(sel_row) > 0L && is.finite(sel_row$estimated_full_runtime[[1L]])) {
      sel_row$estimated_full_runtime[[1L]]
    } else if (length(chosen_plan$median_elapsed) == 1L && is.finite(chosen_plan$median_elapsed[[1L]])) {
      chosen_plan$median_elapsed[[1L]] / nrow(pilot) * workload$total_candidates
    } else NA_real_

    metadata$fallback_reason <- selected$fallback_reason
    metadata$decision_reason <- if (is.na(selected$fallback_reason)) {
      "selected near-best benchmark plan"
    } else {
      selected$fallback_reason
    }

    list(
      plan = chosen_plan,
      metadata = metadata,
      warn = !is.na(selected$fallback_reason)
    )
  }), error = function(e) {
    list(
      plan = manual_plan,
      metadata = within(metadata, {
        fallback_reason <- paste("planner failure; using manual plan:", conditionMessage(e))
        decision_reason <- fallback_reason
      }),
      warn = TRUE
    )
  })

  outcome$metadata$planner_elapsed <- max(0, as.numeric((clock() - started)[["elapsed"]]))
  outcome
}

#' Top-level execution planner controller for cross_size_nested_cv
#'
#' @keywords internal
#' @noRd
.planner_cross_size_nested_cv_controller <- function(
    dat_prep, y, outcome_name, item_names, sizes,
    outer_fold_indices, inner_folds, inner_repeats,
    stratified, selection_metric, cutoff_method,
    sensitivity_min, specificity_min, prefer_fewer_items,
    positive_label, negative_label, engine, tuning,
    manual_parallel_mode, manual_n_workers, manual_threads_per_worker,
    fold_seeds, resource_detector = .get_max_workers,
    threshold = .PLANNER_AUTO_RUNTIME_THRESHOLD,
    clock = proc.time) {

  workload <- .planner_count_workload(length(item_names), sizes)
  n_outer_tasks <- length(outer_fold_indices)
  manual_plan <- .planner_manual_nested_plan(
    manual_parallel_mode, manual_n_workers, manual_threads_per_worker, n_outer_tasks
  )

  metadata <- list(
    planner_version             = .PLANNER_VERSION,
    target_api                  = "cross_size_nested_cv",
    tuning_mode                 = tuning,
    tuning_performed            = TRUE,
    backend_benchmark_performed = FALSE,
    total_candidates            = workload$total_candidates,
    candidate_count_by_size     = workload$candidate_count_by_size,
    micro_pilot_candidates      = list(total = 0L, by_size = integer()),
    micro_pilot_elapsed         = NA_real_,
    estimated_serial_runtime    = NA_real_,
    auto_runtime_threshold      = threshold,
    benchmark_table             = data.frame(),
    selected_parallel           = manual_plan$parallel[[1L]],
    selected_n_workers          = manual_plan$n_workers[[1L]],
    selected_resource_count     = manual_plan$resource_count[[1L]],
    estimated_runtime           = NA_real_,
    runtime_estimation_method   = "size_stratified_candidate_cost",
    runtime_estimate_status     = "fallback",
    selection_tolerance         = .PLANNER_SELECTION_TOLERANCE,
    decision_reason             = NA_character_,
    fallback_reason             = NA_character_,
    planner_elapsed             = NA_real_,
    benchmark_repeat_count      = integer(),
    warmup_performed            = FALSE,
    manual_parallel_requested   = manual_parallel_mode,
    manual_n_workers_requested  = manual_n_workers,
    environment_summary         = .planner_environment_summary(),
    tuning_budget_seconds       = NA_real_,
    tuning_budget_exhausted     = FALSE,
    outer_folds                 = as.integer(inner_folds),
    inner_folds                 = as.integer(inner_folds),
    outer_repeats               = 1L,
    inner_repeats               = as.integer(inner_repeats),
    n_outer_tasks               = as.integer(n_outer_tasks),
    selected_outer_workers      = manual_plan$outer_workers[[1L]],
    selected_threads_per_worker = manual_plan$threads_per_worker[[1L]]
  )

  started <- clock()

  outcome <- tryCatch(.planner_with_preserved_rng({
    pilot <- .planner_make_pilot_candidates(
      length(item_names), sizes, .PLANNER_NESTED_PILOT_BUDGET
    )
    metadata$micro_pilot_candidates <- list(
      total = as.integer(nrow(pilot)),
      by_size = as.integer(table(factor(pilot$model_size, levels = sizes)))
    )
    names(metadata$micro_pilot_candidates$by_size) <- as.character(sizes)

    # 1. Serial pilot timing across complete outer folds
    pilot_start <- clock()
    pilot_by_size <- vector("list", length(sizes))
    for (si in seq_along(sizes)) {
      msize <- sizes[[si]]
      sub_pilot <- pilot[pilot$model_size == msize, , drop = FALSE]
      t0 <- clock()
      for (f_i in seq_along(outer_fold_indices)) {
        f_test <- outer_fold_indices[[f_i]]
        f_train <- setdiff(seq_along(y), f_test)
        x_tr <- as.matrix(dat_prep[f_train, item_names, drop = FALSE])
        y_tr <- y[f_train]
        in_cv_folds <- .make_stratified_cv_folds(
          y = y_tr, k = inner_folds, repeats = inner_repeats, seed = fold_seeds[[f_i]]
        )
        for (ci in seq_len(nrow(sub_pilot))) {
          combo_cols <- sub_pilot$combination[[ci]] + 1L
          sc <- rowSums(x_tr[, combo_cols, drop = FALSE])
          for (f_in in seq_along(in_cv_folds)) {
            in_te <- in_cv_folds[[f_in]]
            in_tr <- setdiff(seq_len(nrow(x_tr)), in_te)
            fr_tr <- compute_score_frequencies(sc[in_tr], y_tr[in_tr])
          }
        }
      }
      elapsed_s <- max(0, as.numeric((clock() - t0)[["elapsed"]]))
      pilot_by_size[[si]] <- data.frame(
        model_size = msize,
        n_candidates = nrow(sub_pilot),
        elapsed = elapsed_s,
        stringsAsFactors = FALSE
      )
    }
    pilot_timings_df <- do.call(rbind, pilot_by_size)
    metadata$micro_pilot_elapsed <- max(0, as.numeric((clock() - pilot_start)[["elapsed"]]))

    estimate <- .planner_estimate_runtime(workload, pilot_timings_df)
    metadata$estimated_serial_runtime <- estimate$estimated_serial_runtime
    metadata$runtime_estimation_method <- estimate$runtime_estimation_method

    if (!is.finite(metadata$estimated_serial_runtime)) {
      metadata$fallback_reason <- "runtime estimate unavailable; using manual plan"
      metadata$decision_reason <- metadata$fallback_reason
      return(list(plan = manual_plan, metadata = metadata, warn = TRUE))
    }

    # 2. Resource detection and legal plan generation
    detected <- tryCatch(resource_detector(), error = function(e) NA_integer_)
    if (!.planner_is_integer_valued(detected) || detected < 1L) {
      detected <- .get_max_workers()
    }

    inner_tasks <- max(1L, ceiling(workload$total_candidates / 1000L))
    all_plans <- .planner_generate_nested_legal_plans(
      api = "cross_size_nested_cv",
      available_resources = detected,
      user_n_workers = manual_n_workers,
      user_threads_per_worker = manual_threads_per_worker,
      n_outer_tasks = n_outer_tasks,
      inner_candidate_tasks = inner_tasks,
      engine = engine,
      is_chunks_allowed = FALSE,
      is_hybrid_allowed = (engine == "Rcpp")
    )

    # See nested_sum_roc: a planner pilot must bound the candidate search
    # itself.  This evaluator cannot consume pilot ranks, so do not time the
    # full nested search once per resource plan.
    status_plans <- .planner_plan_status_table(
      all_plans, nrow(pilot), grain_size = 1L, task_count = nrow(pilot),
      outer_task_count = n_outer_tasks
    )
    status_plans$status[status_plans$status == "pending"] <- "insufficient_workload"
    status_plans$failure_reason <- "insufficient_workload: nested pilot has no rank-bounded evaluator"
    metadata$benchmark_table <- .planner_append_unmeasured_plans(data.frame(), status_plans)
    gate <- .planner_sweep_gate(metadata$estimated_serial_runtime, 0, threshold)
    metadata$tuning_budget_seconds <- gate$overhead_budget_seconds
    metadata$decision_reason <- if (nrow(all_plans) <= 1L) {
      "degenerate workload; using manual plan"
    } else if (!isTRUE(gate$backend_benchmark_required)) {
      gate$reason
    } else {
      "benchmark_budget_insufficient"
    }
    metadata$fallback_reason <- metadata$decision_reason
    return(list(plan = manual_plan, metadata = metadata, warn = FALSE))

    # 3. Benchmark legal plans on pilot candidates over complete nested structure
    metadata$backend_benchmark_performed <- TRUE
    raw_timings <- vector("list", nrow(all_plans))

    for (p_idx in seq_len(nrow(all_plans))) {
      p_row <- all_plans[p_idx, , drop = FALSE]
      t_run_start <- clock()
      err_msg <- NA_character_
      bench_ok <- TRUE

      tryCatch({
        p_mode <- p_row$parallel[[1L]]
        p_outer_w <- p_row$outer_workers[[1L]]
        p_threads_w <- p_row$threads_per_worker[[1L]]

        if (p_mode == "none") {
          for (f_i in seq_along(outer_fold_indices)) {
            test_idx <- outer_fold_indices[[f_i]]
            train_idx <- setdiff(seq_len(nrow(dat_prep)), test_idx)
            .evaluate_cross_size_outer_fold(
              train_data = dat_prep[train_idx, , drop = FALSE],
              test_data = dat_prep[test_idx, , drop = FALSE],
              train_y = y[train_idx], test_y = y[test_idx],
              outcome_name = outcome_name, item_names = item_names,
              sizes = sizes, inner_folds = inner_folds, inner_repeats = inner_repeats,
              stratified = stratified, selection_metric = selection_metric,
              cutoff_method = cutoff_method, sensitivity_min = sensitivity_min,
              specificity_min = specificity_min, prefer_fewer_items = prefer_fewer_items,
              positive_label = positive_label, negative_label = negative_label,
              engine = engine, parallel_mode = "none", n_workers_res = 1L,
              threads_per_worker = 1L, seed = fold_seeds[[f_i]],
              fold_name = paste0("Fold", f_i), rep_id = 1L, f_id = f_i, test_idx = test_idx
            )
          }
        } else if (p_mode %in% c("outer", "hybrid") && p_outer_w > 1L) {
          cl_bench <- parallel::makePSOCKcluster(p_outer_w)
          on.exit(try(parallel::stopCluster(cl_bench), silent = TRUE), add = TRUE)
          lib_paths <- .libPaths()
          parallel::clusterExport(cl_bench, "lib_paths", envir = environment())
          parallel::clusterEvalQ(cl_bench, {
            .libPaths(lib_paths)
            if (requireNamespace("NCVROC", quietly = TRUE)) try(library(NCVROC), silent = TRUE)
            NULL
          })
          ns <- asNamespace("NCVROC")
          available_symbols <- intersect(.CROSS_SIZE_OUTER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
          parallel::clusterExport(cl_bench, varlist = available_symbols, envir = ns)

          parallel::parLapply(cl_bench, seq_along(outer_fold_indices), function(f_i) {
            test_idx <- outer_fold_indices[[f_i]]
            train_idx <- setdiff(seq_len(nrow(dat_prep)), test_idx)
            .evaluate_cross_size_outer_fold(
              train_data = dat_prep[train_idx, , drop = FALSE],
              test_data = dat_prep[test_idx, , drop = FALSE],
              train_y = y[train_idx], test_y = y[test_idx],
              outcome_name = outcome_name, item_names = item_names,
              sizes = sizes, inner_folds = inner_folds, inner_repeats = inner_repeats,
              stratified = stratified, selection_metric = selection_metric,
              cutoff_method = cutoff_method, sensitivity_min = sensitivity_min,
              specificity_min = specificity_min, prefer_fewer_items = prefer_fewer_items,
              positive_label = positive_label, negative_label = negative_label,
              engine = engine,
              parallel_mode = if (p_mode == "hybrid") "hybrid" else "none",
              n_workers_res = if (p_mode == "hybrid") p_threads_w else 1L,
              threads_per_worker = if (p_mode == "hybrid") p_threads_w else 1L,
              seed = fold_seeds[[f_i]],
              fold_name = paste0("Fold", f_i), rep_id = 1L, f_id = f_i, test_idx = test_idx
            )
          })
          parallel::stopCluster(cl_bench)
        } else if (p_mode == "threads") {
          for (f_i in seq_along(outer_fold_indices)) {
            test_idx <- outer_fold_indices[[f_i]]
            train_idx <- setdiff(seq_len(nrow(dat_prep)), test_idx)
            .evaluate_cross_size_outer_fold(
              train_data = dat_prep[train_idx, , drop = FALSE],
              test_data = dat_prep[test_idx, , drop = FALSE],
              train_y = y[train_idx], test_y = y[test_idx],
              outcome_name = outcome_name, item_names = item_names,
              sizes = sizes, inner_folds = inner_folds, inner_repeats = inner_repeats,
              stratified = stratified, selection_metric = selection_metric,
              cutoff_method = cutoff_method, sensitivity_min = sensitivity_min,
              specificity_min = specificity_min, prefer_fewer_items = prefer_fewer_items,
              positive_label = positive_label, negative_label = negative_label,
              engine = engine, parallel_mode = "threads", n_workers_res = p_threads_w,
              threads_per_worker = 1L, seed = fold_seeds[[f_i]],
              fold_name = paste0("Fold", f_i), rep_id = 1L, f_id = f_i, test_idx = test_idx
            )
          }
        }
      }, error = function(e) {
        bench_ok <<- FALSE
        err_msg <<- conditionMessage(e)
      })

      t_run_elapsed <- max(0, as.numeric((clock() - t_run_start)[["elapsed"]]))
      raw_timings[[p_idx]] <- data.frame(
        plan_id = p_row$plan_id[[1L]],
        parallel = p_row$parallel[[1L]],
        n_workers = p_row$n_workers[[1L]],
        outer_workers = p_row$outer_workers[[1L]],
        threads_per_worker = p_row$threads_per_worker[[1L]],
        resource_count = p_row$resource_count[[1L]],
        backend_priority = p_row$backend_priority[[1L]],
        elapsed = t_run_elapsed,
        success = bench_ok,
        failure_reason = err_msg,
        stringsAsFactors = FALSE
      )
    }

    all_raw <- do.call(rbind, raw_timings)
    bench_table <- .planner_aggregate_timings(all_raw)
    extra_cols <- unique(all_raw[, c("plan_id", "outer_workers", "threads_per_worker"), drop = FALSE])
    bench_table <- merge(bench_table, extra_cols, by = "plan_id", all.x = TRUE, sort = FALSE)
    scaling_res <- .planner_compute_scaling_metrics(bench_table, workload$total_candidates, nrow(pilot))
    metadata$benchmark_table <- scaling_res$benchmark_table
    metadata$saturation_summary <- scaling_res$saturation
    metadata$scaling_summary <- scaling_res$scaling_summary

    selected <- .planner_select_plan(metadata$benchmark_table, tolerance = .PLANNER_SELECTION_TOLERANCE, fallback_plan = manual_plan, allow_nested = TRUE)
    chosen_plan <- selected$selected_plan
    metadata$selected_parallel <- chosen_plan$parallel[[1L]]
    metadata$selected_n_workers <- chosen_plan$n_workers[[1L]]
    metadata$selected_resource_count <- chosen_plan$resource_count[[1L]]
    metadata$selected_outer_workers <- if (!is.null(chosen_plan$outer_workers)) chosen_plan$outer_workers[[1L]] else chosen_plan$n_workers[[1L]]
    metadata$selected_threads_per_worker <- if (!is.null(chosen_plan$threads_per_worker)) chosen_plan$threads_per_worker[[1L]] else 1L
    sel_row <- metadata$benchmark_table[metadata$benchmark_table$plan_id == selected$selected_plan_id, , drop = FALSE]
    metadata$estimated_runtime <- if (nrow(sel_row) > 0L && is.finite(sel_row$estimated_full_runtime[[1L]])) {
      sel_row$estimated_full_runtime[[1L]]
    } else if (length(chosen_plan$median_elapsed) == 1L && is.finite(chosen_plan$median_elapsed[[1L]])) {
      chosen_plan$median_elapsed[[1L]] / nrow(pilot) * workload$total_candidates
    } else NA_real_

    metadata$fallback_reason <- selected$fallback_reason
    metadata$decision_reason <- if (is.na(selected$fallback_reason)) {
      "selected near-best benchmark plan"
    } else {
      selected$fallback_reason
    }

    list(
      plan = chosen_plan,
      metadata = metadata,
      warn = !is.na(selected$fallback_reason)
    )
  }), error = function(e) {
    list(
      plan = manual_plan,
      metadata = within(metadata, {
        fallback_reason <- paste("planner failure; using manual plan:", conditionMessage(e))
        decision_reason <- fallback_reason
      }),
      warn = TRUE
    )
  })

  outcome$metadata$planner_elapsed <- max(0, as.numeric((clock() - started)[["elapsed"]]))
  outcome
}

#' Format automatic execution plan metadata as human-readable text
#'
#' @param x An execution plan metadata list or an S3 object containing one.
#' @return A character string summarizing the execution plan.
#' @keywords internal
#' @noRd
.planner_format_execution_plan <- function(x) {
  metadata <- if (is.list(x) && is.list(x$settings) && !is.null(x$settings$execution_plan)) {
    x$settings$execution_plan
  } else if (is.list(x) && !is.null(x$execution_plan)) {
    x$execution_plan
  } else if (!is.null(attr(x, "execution_plan", exact = TRUE))) {
    attr(x, "execution_plan", exact = TRUE)
  } else if (is.list(x) && !is.null(x$planner_version)) {
    x
  } else {
    stop("`x` does not contain execution_plan metadata.", call. = FALSE)
  }

  target_api <- if (!is.null(metadata$target_api)) metadata$target_api else "unknown"
  tuning_mode <- if (!is.null(metadata$tuning_mode)) metadata$tuning_mode else "off"
  decision <- if (!is.null(metadata$decision_reason)) metadata$decision_reason else "none"
  n_workers <- if (!is.null(metadata$selected_n_workers)) as.integer(metadata$selected_n_workers) else 1L
  worker_str <- if (n_workers > 1L) sprintf(" (%d workers)", n_workers) else ""
  backend_str <- sprintf("%s%s", metadata$selected_parallel, worker_str)

  lines <- c(
    "=== Automatic Execution Plan ===",
    sprintf("Target API:               %s", target_api),
    sprintf("Tuning Mode:              %s", tuning_mode),
    sprintf("Total Candidates:         %s", format(metadata$total_candidates, big.mark = ",")),
    sprintf("Decision Reason:          %s", decision),
    sprintf("Selected Backend:         %s", backend_str)
  )

  if (is.numeric(metadata$estimated_serial_runtime) && is.finite(metadata$estimated_serial_runtime)) {
    lines <- c(lines, sprintf("Estimated Serial Runtime: ~%.2f s (approximate estimate)", metadata$estimated_serial_runtime))
  }
  if (is.numeric(metadata$estimated_runtime) && is.finite(metadata$estimated_runtime)) {
    lines <- c(lines, sprintf("Estimated Plan Runtime:   ~%.2f s (approximate estimate)", metadata$estimated_runtime))
  }
  if (isTRUE(metadata$backend_benchmark_performed)) {
    n_bench <- if (is.data.frame(metadata$benchmark_table)) nrow(metadata$benchmark_table) else 0L
    lines <- c(lines, sprintf("Backend Benchmarking:     Performed across %d candidate configurations", n_bench))
  } else {
    lines <- c(lines, "Backend Benchmarking:     Skipped (workload below threshold or degenerate)")
  }
  if (!is.null(metadata$cv_method)) {
    lines <- c(lines, sprintf("Cross-Validation Method:  %s (k = %d, repeats = %d)",
                            metadata$cv_method, metadata$k, metadata$repeats))
  }
  if (!is.null(metadata$outer_folds)) {
    lines <- c(lines, sprintf("Nested CV Structure:      %d-fold outer x %d repeat(s), %d-fold inner",
                              metadata$outer_folds,
                              if (!is.null(metadata$outer_repeats)) metadata$outer_repeats else 1L,
                              metadata$inner_folds))
    if (identical(metadata$selected_parallel, "hybrid")) {
      lines <- c(lines, sprintf("Hybrid Allocation:        %d outer workers x %d threads = %d total resources",
                                if (!is.null(metadata$selected_outer_workers)) metadata$selected_outer_workers else metadata$selected_n_workers,
                                if (!is.null(metadata$selected_threads_per_worker)) metadata$selected_threads_per_worker else 1L,
                                metadata$selected_resource_count))
    }
  }
  paste(lines, collapse = "\n")
}
