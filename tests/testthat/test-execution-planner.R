# Phase 1.0 internal execution-planner tests

test_that("workload counting is exact and stratified by model size", {
  workload <- .planner_count_workload(6L, c(1L, 3L, 5L))

  expect_identical(workload$model_sizes, c(1L, 3L, 5L))
  expect_equal(
    workload$candidate_count_by_size,
    c(`1` = 6, `3` = 20, `5` = 6)
  )
  expect_equal(workload$total_candidates, 32)
})

test_that("workload counting rejects invalid model sizes at the planner boundary", {
  expect_error(.planner_count_workload(6L, 2^31), "model_sizes")
  expect_error(.planner_count_workload(6L, 7L), "model_sizes")
  expect_error(.planner_count_workload(6L, 1.5), "model_sizes")
})

test_that("representative pilot candidates are deterministic and size-stratified", {
  first <- .planner_make_pilot_candidates(
    8L, c(1L, 3L, 5L), max_candidates = 9L,
    item_names = paste0("q", seq_len(8L))
  )
  second <- .planner_make_pilot_candidates(
    8L, c(1L, 3L, 5L), max_candidates = 9L,
    item_names = paste0("q", seq_len(8L))
  )

  expect_identical(first, second)
  expect_identical(sort(unique(first$model_size)), c(1L, 3L, 5L))
  expect_equal(as.integer(table(first$model_size)), c(3L, 3L, 3L))
  expect_identical(anyDuplicated(first$global_candidate_index), 0L)

  counts <- choose(8L, first$model_size)
  expect_true(all(first$rank_within_size >= 0))
  expect_true(all(first$rank_within_size < counts))
  expect_true(all(vapply(first$combination, function(x) {
    length(x) >= 1L && all(diff(x) > 0L) &&
      min(x) >= 0L && max(x) < 8L
  }, logical(1))))
  expect_identical(vapply(first$combination, length, integer(1)),
                   first$model_size)

  for (size in c(1L, 3L, 5L)) {
    ranks <- first$rank_within_size[first$model_size == size]
    expect_equal(min(ranks), 0)
    expect_equal(max(ranks), choose(8L, size) - 1)
  }
})

test_that("evenly spaced ranks remain exact near the double integer ceiling", {
  total <- 2^53 - 1
  ranks <- .planner_evenly_spaced_ranks(total, quota = 24L)

  expect_length(ranks, 24L)
  expect_identical(ranks[1L], 0)
  expect_identical(ranks[24L], total - 1)
  expect_true(all(is.finite(ranks)))
  expect_true(all(ranks == floor(ranks)))
  expect_true(all(ranks >= 0 & ranks < total))
  expect_identical(anyDuplicated(ranks), 0L)
  expect_true(all(diff(ranks) > 0))
})

test_that("pilot quotas stay balanced until a model size is exhausted", {
  sizes <- c(1L, 3L, 5L)
  workload <- .planner_count_workload(5L, sizes)

  for (budget in c(4L, 5L, 8L)) {
    pilot <- .planner_make_pilot_candidates(5L, sizes, max_candidates = budget)
    repeat_pilot <- .planner_make_pilot_candidates(5L, sizes, max_candidates = budget)
    quotas <- as.integer(table(factor(pilot$model_size, levels = sizes)))
    active <- quotas < workload$candidate_count_by_size

    expect_identical(pilot, repeat_pilot)
    expect_true(all(quotas >= 1L))
    expect_equal(sum(quotas), min(budget, workload$total_candidates))
    expect_true(all(quotas <= workload$candidate_count_by_size))
    if (sum(active) > 1L) {
      expect_lte(max(quotas[active]) - min(quotas[active]), 1L)
    }
  }
})

test_that("pilot memory scales with pilot size rather than full candidate space", {
  pilot <- .planner_make_pilot_candidates(
    60L, c(1L, 3L, 5L), max_candidates = 9L
  )

  expect_equal(nrow(pilot), 9L)
  expect_gt(attr(pilot, "full_candidate_count"), 5e6)
  expect_lt(as.numeric(object.size(pilot)), 100000)
  expect_true(isTRUE(attr(pilot, "representative_only")))
})

test_that("large real combination spaces retain exact pilot ranks", {
  n_items <- 35L
  model_size <- 17L
  workload <- .planner_count_workload(n_items, model_size)
  total <- choose(n_items, model_size)
  pilot <- .planner_make_pilot_candidates(
    n_items, model_size, max_candidates = 3L
  )

  expect_gt(total, 2^31)
  expect_lt(total, 2^53)
  expect_identical(workload$total_candidates, total)
  expect_length(pilot$combination, 3L)
  expect_type(pilot$global_candidate_index, "double")
  expect_true(all(pilot$global_candidate_index ==
                  floor(pilot$global_candidate_index)))
  expect_equal(pilot$global_candidate_index, pilot$rank_within_size + 1)
  expect_identical(pilot$combination[[1L]], .combination_unrank(n_items, model_size, 0))
  expect_identical(
    pilot$combination[[3L]],
    .combination_unrank(n_items, model_size, total - 1)
  )
  expect_lt(as.numeric(object.size(pilot)), 100000)
})

test_that("legal plan generation proposes only flat API backends", {
  for (api in c("exhaustive_sum_roc", "cross_size_cv")) {
    plans <- .planner_generate_legal_plans(
      api, available_resources = 8L, engine = "Rcpp"
    )
    expect_setequal(unique(plans$parallel), c("none", "threads", "chunks"))
    expect_false(any(plans$parallel %in% c("outer", "hybrid")))
    expect_equal(unique(plans$n_workers[plans$parallel != "none"]),
                 2L:8L)
  }

  r_plans <- .planner_generate_legal_plans(
    "exhaustive_sum_roc", available_resources = 4L, engine = "R"
  )
  expect_setequal(unique(r_plans$parallel), c("none", "chunks"))
})

test_that("legal plans respect machine, task, and explicit user caps", {
  plans <- .planner_generate_legal_plans(
    "cross_size_cv",
    available_resources = 16L,
    user_n_workers = 3L,
    task_count = 10L,
    engine = "Rcpp"
  )

  expect_identical(attr(plans, "resource_cap"), 3L)
  expect_lte(max(plans$resource_count), 3L)
  expect_true(3L %in% plans$resource_count)
  expect_identical(
    .planner_resource_cap(available_resources = 2L, user_n_workers = 8L),
    2L
  )
})

test_that("timing aggregation uses medians and records failed plans", {
  raw <- data.frame(
    plan_id = rep(c("none_1", "threads_2"), each = 3L),
    parallel = rep(c("none", "threads"), each = 3L),
    n_workers = rep(c(1L, 2L), each = 3L),
    resource_count = rep(c(1L, 2L), each = 3L),
    backend_priority = rep(c(1L, 2L), each = 3L),
    elapsed = c(1.8, 1.7, 4.5, NA, NA, NA),
    success = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    failure_reason = c(rep(NA_character_, 3L), rep("worker startup failed", 3L))
  )
  aggregated <- .planner_aggregate_timings(raw)

  expect_equal(aggregated$median_elapsed[aggregated$plan_id == "none_1"], 1.8)
  expect_identical(aggregated$status, c("ok", "failed"))
  expect_match(aggregated$failure_reason[aggregated$plan_id == "threads_2"],
               "worker startup failed")
})

test_that("timing aggregation accepts only explicit successful timings", {
  mixed <- data.frame(
    plan_id = "threads_2", parallel = "threads", n_workers = 2L,
    resource_count = 2L, elapsed = c(1, 2, 0.5),
    success = c(TRUE, NA, FALSE)
  )
  mixed_result <- .planner_aggregate_timings(mixed)
  expect_identical(mixed_result$n_success, 1L)
  expect_identical(mixed_result$n_failed, 2L)
  expect_identical(mixed_result$status, "ok")
  expect_identical(mixed_result$median_elapsed, 1)

  all_flag_failures <- data.frame(
    plan_id = "none_1", parallel = "none", n_workers = 1L,
    resource_count = 1L, elapsed = c(1, 2), success = c(NA, FALSE)
  )
  all_flag_result <- .planner_aggregate_timings(all_flag_failures)
  expect_identical(all_flag_result$n_success, 0L)
  expect_identical(all_flag_result$n_failed, 2L)
  expect_identical(all_flag_result$status, "failed")

  never_successful <- data.frame(
    plan_id = "chunks_2", parallel = "chunks", n_workers = 2L,
    resource_count = 2L, elapsed = c(NA_real_, Inf, -1),
    success = c(NA, TRUE, TRUE)
  )
  failed_result <- .planner_aggregate_timings(never_successful)
  expect_identical(failed_result$n_success, 0L)
  expect_identical(failed_result$n_failed, 3L)
  expect_identical(failed_result$status, "failed")
  expect_true(is.na(failed_result$median_elapsed))
})

test_that("near-best selection applies the five-percent lower-resource rule", {
  timings <- data.frame(
    plan_id = c("none_1", "threads_2", "threads_4", "threads_8"),
    parallel = c("none", "threads", "threads", "threads"),
    n_workers = c(1L, 2L, 4L, 8L),
    resource_count = c(1L, 2L, 4L, 8L),
    backend_priority = c(1L, 2L, 2L, 2L),
    median_elapsed = c(1.80, 1.20, 1.00, 0.98),
    status = "ok"
  )

  selected <- .planner_select_plan(timings, tolerance = 0.05)
  expect_identical(selected$selected_parallel, "threads")
  expect_identical(selected$selected_n_workers, 4L)
  expect_equal(selected$fastest_observed_elapsed, 0.98)
  expect_equal(selected$selection_tolerance, 0.05)
})

test_that("runtime estimation is candidate-weighted and size-stratified", {
  workload <- list(candidate_count_by_size = c(`1` = 10, `3` = 20))
  pilot <- data.frame(
    model_size = c(1L, 1L, 3L, 3L),
    n_candidates = c(2L, 2L, 4L, 4L),
    elapsed = c(0.2, 0.2, 0.8, 0.8)
  )
  estimate <- .planner_estimate_runtime(workload, pilot)

  expect_equal(estimate$by_size$seconds_per_candidate, c(0.1, 0.2))
  expect_equal(estimate$by_size$estimated_seconds, c(1, 4))
  expect_equal(estimate$estimated_serial_runtime, 5)
  expect_identical(estimate$runtime_estimation_method,
                   "size_stratified_candidate_cost")
})

test_that("runtime estimation ignores malformed rows and uses robust fallbacks", {
  workload <- list(candidate_count_by_size = c(`1` = 10, `3` = 20, `5` = 30))
  pilot <- data.frame(
    model_size = c(1L, 1L, 1L, 3L, NA_integer_, 1L, 3L, 3L, 5L),
    n_candidates = c(2L, 2L, 1L, 2L, 2L, 2L, 2L, 0L, 2L),
    elapsed = c(0.2, 0.22, 100, 0.4, 0.6, -1, NA_real_, 0.1, NA_real_)
  )
  estimate <- .planner_estimate_runtime(workload, pilot)

  expect_equal(estimate$by_size$seconds_per_candidate, c(0.11, 0.2, 0.2))
  expect_identical(estimate$by_size$rate_source,
                   c("size_specific", "size_specific", "global_fallback"))
  expect_match(estimate$fallback_reason, "global median")

  invalid <- .planner_estimate_runtime(
    workload,
    data.frame(
      model_size = c(1L, 3L, 5L),
      n_candidates = c(1L, 0L, NA_integer_),
      elapsed = c(-1, 0.1, NA_real_)
    )
  )
  expect_true(is.na(invalid$estimated_serial_runtime))
  expect_match(invalid$fallback_reason, "no successful micro-pilot timings")
})

test_that("auto benchmark decision uses estimated time and an injectable threshold", {
  small <- .planner_should_benchmark(4.2, threshold = 30)
  large <- .planner_should_benchmark(45, threshold = 30)

  expect_false(small$backend_benchmark_required)
  expect_match(small$reason, "estimated workload too small")
  expect_true(large$backend_benchmark_required)
  expect_equal(large$auto_runtime_threshold, 30)
})

test_that("failed benchmark plans fall back without aborting analysis", {
  failed <- data.frame(
    plan_id = c("threads_2", "chunks_2"),
    parallel = c("threads", "chunks"),
    n_workers = c(2L, 2L),
    resource_count = c(2L, 2L),
    backend_priority = c(2L, 3L),
    median_elapsed = c(NA_real_, NA_real_),
    status = c("failed", "failed")
  )

  selected <- .planner_select_plan(failed)
  expect_identical(selected$selected_parallel, "none")
  expect_identical(selected$selected_n_workers, 1L)
  expect_match(selected$fallback_reason, "all benchmark plans failed")
})

test_that("plan selection has a complete deterministic tie order", {
  lower_resource <- data.frame(
    plan_id = c("threads_2", "threads_4"),
    parallel = "threads", n_workers = c(2L, 4L),
    resource_count = c(2L, 4L), backend_priority = 2L,
    median_elapsed = c(1, 0.96), status = "ok"
  )
  expect_identical(.planner_select_plan(lower_resource)$selected_plan$plan_id,
                   "threads_2")

  none_within_tolerance <- data.frame(
    plan_id = c("none_1", "threads_2"),
    parallel = c("none", "threads"), n_workers = c(1L, 2L),
    resource_count = c(1L, 2L), backend_priority = c(1L, 2L),
    median_elapsed = c(1, 0.96), status = "ok"
  )
  expect_identical(.planner_select_plan(none_within_tolerance)$selected_plan$plan_id,
                   "none_1")

  same_resource <- data.frame(
    plan_id = c("threads_x", "chunks_z", "chunks_a"),
    parallel = c("threads", "chunks", "chunks"),
    n_workers = 2L, resource_count = 2L,
    backend_priority = c(2L, 3L, 3L),
    median_elapsed = 1, status = "ok"
  )
  expect_identical(.planner_select_plan(same_resource)$selected_plan$plan_id,
                   "threads_x")
  chunks_only <- same_resource[same_resource$parallel == "chunks", ]
  expect_identical(.planner_select_plan(chunks_only)$selected_plan$plan_id,
                   "chunks_a")

  zero_fastest <- rbind(
    lower_resource[1L, ],
    data.frame(plan_id = "chunks_zero", parallel = "chunks", n_workers = 2L,
               resource_count = 2L, backend_priority = 3L,
               median_elapsed = 0, status = "ok"),
    data.frame(plan_id = "failed_zero", parallel = "none", n_workers = 1L,
               resource_count = 1L, backend_priority = 1L,
               median_elapsed = 0, status = "failed")
  )
  selected_zero <- .planner_select_plan(zero_fastest)
  expect_identical(selected_zero$selected_plan$plan_id, "chunks_zero")
  expect_identical(selected_zero$fastest_observed_elapsed, 0)
})

test_that("custom fallback plans must be legal flat execution plans", {
  failed <- data.frame(
    plan_id = "failed", parallel = "threads", n_workers = 2L,
    resource_count = 2L, median_elapsed = NA_real_, status = "failed"
  )
  custom <- data.frame(parallel = "chunks", n_workers = 3L, resource_count = 3L)
  selected <- .planner_select_plan(failed, fallback_plan = custom)
  expect_identical(selected$selected_plan, custom)

  expect_error(.planner_select_plan(failed, fallback_plan =
    data.frame(parallel = "outer", n_workers = 2L, resource_count = 2L)),
    "parallel")
  expect_error(.planner_select_plan(failed, fallback_plan =
    data.frame(parallel = "hybrid", n_workers = 2L, resource_count = 2L)),
    "parallel")
  expect_error(.planner_select_plan(failed, fallback_plan =
    data.frame(parallel = "none", n_workers = 2L, resource_count = 1L)),
    "legal flat")
  expect_error(.planner_select_plan(failed, fallback_plan =
    data.frame(parallel = "threads", n_workers = 1.5, resource_count = 2L)),
    "n_workers")
  expect_error(.planner_select_plan(failed, fallback_plan =
    data.frame(parallel = "threads", n_workers = 2L)),
    "resource_count")
})

test_that("benchmark selection rejects malformed or non-flat plans", {
  plan <- function(parallel, n_workers, resource_count) {
    data.frame(
      parallel = parallel, n_workers = n_workers,
      resource_count = resource_count, median_elapsed = 1, status = "ok"
    )
  }

  expect_error(.planner_select_plan(plan("outer", 2L, 2L)), "parallel")
  expect_error(.planner_select_plan(plan(NA_character_, 2L, 2L)), "parallel")
  expect_error(.planner_select_plan(plan("threads", 1.5, 2L)), "n_workers")
  expect_error(.planner_select_plan(plan("chunks", 0L, 0L)), "n_workers")
  expect_error(.planner_select_plan(plan("threads", 2L, 3L)), "legal flat")
  expect_error(.planner_select_plan(plan("none", 1L, 2L)), "legal flat")
})

test_that("planner helpers do not interfere with the statistical RNG stream", {
  set.seed(1800)
  before <- .Random.seed
  invisible(.planner_make_pilot_candidates(10L, c(1L, 3L, 5L), 9L))
  expect_identical(.Random.seed, before)

  set.seed(1801)
  before_guard <- .Random.seed
  value <- .planner_with_preserved_rng(stats::runif(3L))
  expect_length(value, 3L)
  expect_identical(.Random.seed, before_guard)
})

test_that("planner contract forbids statistical screening or approximation", {
  contract <- .planner_contract()

  expect_true(contract$execution_only)
  expect_false(contract$changes_candidate_space)
  expect_false(contract$statistical_screening)
  expect_false(contract$pilot_reduces_rows)
  expect_false(contract$pilot_results_reused)
  expect_true(contract$final_analysis_exact)
})

test_that("environment summary excludes identifying machine fields", {
  summary <- .planner_environment_summary(
    package_version = "test-version",
    logical_cores = NA_integer_,
    physical_cores = 3L
  )

  expect_true(all(c(
    "os", "r_version", "detected_logical_cores",
    "detected_physical_cores", "rcppparallel_default_threads",
    "package_version", "package_version_source"
  ) %in% names(summary)))
  expect_identical(summary$package_version, "test-version")
  expect_identical(summary$package_version_source, "injected")
  expect_true(is.na(summary$detected_logical_cores))
  expect_identical(summary$detected_physical_cores, 3L)
  expect_false(any(c("username", "user", "hostname", "path", "home") %in%
                     names(summary)))
})
