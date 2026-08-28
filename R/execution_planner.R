# execution_planner.R -- Internal automatic execution-planning primitives

.PLANNER_VERSION <- "0.1.0"
.PLANNER_SELECTION_TOLERANCE <- 0.05
.PLANNER_AUTO_RUNTIME_THRESHOLD <- 30
.PLANNER_MAX_EXACT_INTEGER <- 2^53 - 1

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

  offsets <- c(0, head(cumsum(counts), -1L))
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
        (!.planner_is_integer_valued(value) || value < 1L ||
         value > .Machine$integer.max)) {
      stop(sprintf("`%s` must be a positive integer or NULL.", name),
           call. = FALSE)
    }
  }
  cap <- as.integer(available)
  if (!is.null(user_n_workers)) cap <- min(cap, as.integer(user_n_workers))
  if (!is.null(task_count)) cap <- min(cap, as.integer(task_count))
  max(1L, cap)
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
  resource_levels <- sort(unique(c(2L, 4L, cap)))
  resource_levels <- resource_levels[resource_levels >= 2L & resource_levels <= cap]

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
                                 fallback_plan = NULL) {
  if (!is.data.frame(benchmark_table) ||
      !all(c("parallel", "n_workers", "resource_count", "median_elapsed") %in%
           names(benchmark_table))) {
    stop("`benchmark_table` is missing required plan-selection columns.",
         call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
      tolerance < 0 || !is.finite(tolerance)) {
    stop("`tolerance` must be a non-negative finite number.", call. = FALSE)
  }
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
    planner_version = .PLANNER_VERSION, tuning_mode = tuning, tuning_performed = TRUE,
    backend_benchmark_performed = FALSE, total_candidates = workload$total_candidates,
    candidate_count_by_size = workload$candidate_count_by_size,
    micro_pilot_candidates = list(total = 0L, by_size = integer()),
    micro_pilot_elapsed = NA_real_, estimated_serial_runtime = NA_real_,
    auto_runtime_threshold = threshold, benchmark_table = data.frame(),
    selected_parallel = manual_plan$parallel[[1L]], selected_n_workers = manual_plan$n_workers[[1L]],
    selected_resource_count = manual_plan$resource_count[[1L]], estimated_runtime = NA_real_,
    runtime_estimation_method = "size_stratified_candidate_cost",
    selection_tolerance = .PLANNER_SELECTION_TOLERANCE, decision_reason = NA_character_,
    fallback_reason = NA_character_, planner_elapsed = NA_real_, benchmark_repeat_count = integer(),
    warmup_performed = FALSE, manual_parallel_requested = manual_parallel_mode,
    manual_n_workers_requested = manual_n_workers,
    environment_summary = .planner_environment_summary(), tuning_budget_seconds = NA_real_,
    tuning_budget_exhausted = FALSE
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
    pick_levels <- function(table, backend) {
      rows <- table[table$parallel == backend & table$n_workers >= 2L, , drop = FALSE]
      if (!nrow(rows)) return(rows)
      rows[match(unique(c(min(rows$n_workers), max(rows$n_workers))), rows$n_workers), , drop = FALSE]
    }
    all_plans <- rbind(
      thread_plans[thread_plans$parallel == "none", , drop = FALSE],
      pick_levels(thread_plans, "threads"), pick_levels(chunk_plans, "chunks")
    )
    all_plans <- all_plans[!duplicated(paste(all_plans$parallel, all_plans$n_workers)), , drop = FALSE]
    nonserial <- all_plans$parallel != "none"
    degenerate <- workload$total_candidates < 8 || !any(nonserial)
    should <- identical(tuning, "always") && !degenerate ||
      identical(tuning, "auto") && !degenerate &&
      .planner_should_benchmark(estimate$estimated_serial_runtime, threshold)$backend_benchmark_required
    if (!should) {
      metadata$decision_reason <- if (degenerate) "degenerate workload; using manual plan" else
        .planner_should_benchmark(estimate$estimated_serial_runtime, threshold)$reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }
    metadata$backend_benchmark_performed <- TRUE
    metadata$tuning_budget_seconds <- min(10, max(2, estimate$estimated_serial_runtime * 0.10))
    benchmark_started <- clock()
    rows <- list(); row_i <- 0L
    for (i in seq_len(nrow(all_plans))) {
      plan <- all_plans[i, c("parallel", "n_workers", "resource_count"), drop = FALSE]
      repeats <- if (plan$parallel == "chunks") 2L else 3L
      if (plan$parallel != "chunks") {
        .planner_evaluate_exhaustive_ranks(
          x_mat, y, items, min_items, max_items, cutoff_method,
          ranks[seq_len(min(length(ranks), length(model_sizes)))], engine,
          plan$parallel, plan$n_workers
        )
        metadata$warmup_performed$plans <- c(
          metadata$warmup_performed$plans, all_plans$plan_id[i]
        )
      }
      for (repeat_i in seq_len(repeats)) {
        if (repeat_i > 1L && clock() - benchmark_started > metadata$tuning_budget_seconds) {
          metadata$tuning_budget_exhausted <- TRUE; next
        }
        result <- benchmark_executor(x_mat, y, items, min_items, max_items,
                                     cutoff_method, engine, ranks, plan, timer)
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(plan_id = all_plans$plan_id[i], parallel = plan$parallel,
          n_workers = plan$n_workers, resource_count = plan$resource_count,
          backend_priority = all_plans$backend_priority[i], elapsed = result$elapsed,
          success = result$success, failure_reason = result$failure_reason,
          stringsAsFactors = FALSE)
      }
    }
    raw <- do.call(rbind, rows)
    metadata$benchmark_repeat_count <- table(raw$plan_id)
    metadata$benchmark_table <- .planner_aggregate_timings(raw)
    selected <- .planner_select_plan(metadata$benchmark_table, fallback_plan = manual_plan)
    metadata$selected_parallel <- selected$selected_parallel
    metadata$selected_n_workers <- selected$selected_n_workers
    metadata$selected_resource_count <- selected$selected_resource_count
    selected_elapsed <- selected$selected_plan$median_elapsed
    metadata$estimated_runtime <- if (length(selected_elapsed) == 1L &&
                                      is.finite(selected_elapsed[[1L]])) {
      selected_elapsed[[1L]] / length(ranks) * workload$total_candidates
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
  for (idx in combo_indices_list) {
    .eval_single_combo_cv(
      x_mat         = x_mat,
      y             = y,
      items_0based  = idx,
      folds         = test_indices_0based,
      repeats       = repeats,
      cutoff_method = cutoff_method,
      sens_min      = sens_min,
      spec_min      = spec_min
    )
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

    pick_levels <- function(table, backend) {
      rows <- table[table$parallel == backend & table$n_workers >= 2L, , drop = FALSE]
      if (!nrow(rows)) return(rows)
      rows[match(unique(c(min(rows$n_workers), max(rows$n_workers))), rows$n_workers), , drop = FALSE]
    }

    all_plans <- rbind(
      thread_plans[thread_plans$parallel == "none", , drop = FALSE],
      pick_levels(thread_plans, "threads"),
      pick_levels(chunk_plans, "chunks")
    )
    all_plans <- all_plans[!duplicated(paste(all_plans$parallel, all_plans$n_workers)), , drop = FALSE]

    nonserial <- all_plans$parallel != "none"
    degenerate <- workload$total_candidates < 8 || !any(nonserial)
    should <- identical(tuning, "always") && !degenerate ||
      identical(tuning, "auto") && !degenerate &&
      .planner_should_benchmark(estimate$estimated_serial_runtime, threshold)$backend_benchmark_required

    if (!should) {
      metadata$decision_reason <- if (degenerate) "degenerate workload; using manual plan" else
        .planner_should_benchmark(estimate$estimated_serial_runtime, threshold)$reason
      return(list(plan = manual_plan, metadata = metadata, warn = FALSE))
    }

    metadata$backend_benchmark_performed <- TRUE
    metadata$tuning_budget_seconds <- min(10, max(2, estimate$estimated_serial_runtime * 0.10))
    benchmark_started <- clock()
    rows <- list(); row_i <- 0L

    for (i in seq_len(nrow(all_plans))) {
      plan <- all_plans[i, c("parallel", "n_workers", "resource_count"), drop = FALSE]
      repeats_bm <- if (plan$parallel == "chunks") 2L else 2L

      if (plan$parallel != "chunks") {
        .planner_evaluate_cv_pilot_combos(
          x_mat, y_int, warmup_combos, test_indices_0based, n_folds_total,
          repeats, cutoff_method, sens_min, spec_min, engine,
          plan$parallel[[1L]], plan$n_workers[[1L]]
        )
        metadata$warmup_performed$plans <- c(
          metadata$warmup_performed$plans, all_plans$plan_id[i]
        )
      }

      for (repeat_i in seq_len(repeats_bm)) {
        if (repeat_i > 1L && clock() - benchmark_started > metadata$tuning_budget_seconds) {
          metadata$tuning_budget_exhausted <- TRUE
          next
        }
        result <- benchmark_executor(
          x_mat, y_int, pilot_combos, test_indices_0based, n_folds_total,
          repeats, cutoff_method, sens_min, spec_min, engine, plan, timer
        )
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(
          plan_id          = all_plans$plan_id[i],
          parallel         = plan$parallel[[1L]],
          n_workers        = plan$n_workers[[1L]],
          resource_count   = plan$resource_count[[1L]],
          backend_priority = all_plans$backend_priority[i],
          elapsed          = result$elapsed,
          success          = result$success,
          failure_reason   = result$failure_reason,
          stringsAsFactors = FALSE
        )
      }
    }

    raw <- do.call(rbind, rows)
    metadata$benchmark_repeat_count <- table(raw$plan_id)
    metadata$benchmark_table <- .planner_aggregate_timings(raw)
    selected <- .planner_select_plan(metadata$benchmark_table, fallback_plan = manual_plan)

    metadata$selected_parallel <- selected$selected_parallel
    metadata$selected_n_workers <- selected$selected_n_workers
    metadata$selected_resource_count <- selected$selected_resource_count
    selected_elapsed <- selected$selected_plan$median_elapsed
    metadata$estimated_runtime <- if (length(selected_elapsed) == 1L &&
                                      is.finite(selected_elapsed[[1L]])) {
      selected_elapsed[[1L]] / length(pilot_combos) * workload$total_candidates
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
