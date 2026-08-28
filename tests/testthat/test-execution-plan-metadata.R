# test-execution-plan-metadata.R - Phase 1.3 Execution planner metadata & UX tests

.metadata_test_data <- function(n_items = 6L, n_pos = 15L, n_neg = 15L) {
  set.seed(42L)
  n_total <- n_pos + n_neg
  mat <- matrix(sample(0:2, n_total * n_items, replace = TRUE), nrow = n_total, ncol = n_items)
  df <- as.data.frame(mat)
  names(df) <- paste0("Q", seq_len(n_items))
  df$y <- rep(c(0L, 1L), c(n_neg, n_pos))
  df
}

.COMMON_METADATA_FIELDS <- c(
  "planner_version",
  "target_api",
  "tuning_mode",
  "tuning_performed",
  "backend_benchmark_performed",
  "total_candidates",
  "candidate_count_by_size",
  "micro_pilot_candidates",
  "micro_pilot_elapsed",
  "estimated_serial_runtime",
  "auto_runtime_threshold",
  "benchmark_table",
  "selected_parallel",
  "selected_n_workers",
  "selected_resource_count",
  "estimated_runtime",
  "runtime_estimation_method",
  "selection_tolerance",
  "decision_reason",
  "fallback_reason",
  "planner_elapsed",
  "benchmark_repeat_count",
  "warmup_performed",
  "manual_parallel_requested",
  "manual_n_workers_requested",
  "environment_summary",
  "tuning_budget_seconds",
  "tuning_budget_exhausted",
  "progress_mode",
  "progress_unit"
)

test_that("common metadata schema fields are identical across all four APIs", {
  d <- .metadata_test_data(5L)

  ex_res <- exhaustive_sum_roc(
    data = d, outcome = "y", items = paste0("Q", 1:5),
    max_items = 2, tuning = "auto", progress = FALSE
  )
  ex_meta <- attr(ex_res, "execution_plan")

  cv_res <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden",
    folds = 5, seed = 42, tuning = "auto", progress = FALSE
  )
  cv_meta <- cv_res$settings$execution_plan

  nested_res <- nested_sum_roc(
    data = d, outcome = "y", items = paste0("Q", 1:5),
    min_items = 1, max_items = 2, outer_k = 3, inner_k = 2,
    seed = 42, tuning = "auto", progress = FALSE, verbose = FALSE
  )
  nested_meta <- nested_res$settings$execution_plan

  cross_nested_res <- cross_size_nested_cv(
    data = d, outcome = "y", items = paste0("Q", 1:5),
    min_items = 1, max_items = 2, outer_folds = 3, inner_folds = 2,
    outer_repeats = 1, inner_repeats = 1,
    seed = 42, tuning = "auto", progress = FALSE, verbose = FALSE
  )
  cross_nested_meta <- cross_nested_res$settings$execution_plan

  expect_type(ex_meta, "list")
  expect_type(cv_meta, "list")
  expect_type(nested_meta, "list")
  expect_type(cross_nested_meta, "list")

  # All 4 must contain all 28 common metadata fields
  expect_true(all(.COMMON_METADATA_FIELDS %in% names(ex_meta)))
  expect_true(all(.COMMON_METADATA_FIELDS %in% names(cv_meta)))
  expect_true(all(.COMMON_METADATA_FIELDS %in% names(nested_meta)))
  expect_true(all(.COMMON_METADATA_FIELDS %in% names(cross_nested_meta)))

  # Target APIs are accurately stamped
  expect_identical(ex_meta$target_api, "exhaustive_sum_roc")
  expect_identical(cv_meta$target_api, "cross_size_cv")
  expect_identical(nested_meta$target_api, "nested_sum_roc")
  expect_identical(cross_nested_meta$target_api, "cross_size_nested_cv")

  # Disabled progress is represented once, in canonical execution_plan metadata.
  expect_identical(ex_meta$progress_mode, "disabled")
  expect_identical(cv_meta$progress_mode, "disabled")
  expect_identical(nested_meta$progress_unit, "none")
  expect_identical(cross_nested_meta$progress_unit, "none")

  # CV adds CV-specific fields
  expect_true(all(c("cv_method", "k", "repeats", "n_folds_total") %in% names(cv_meta)))

  # Nested APIs add nested-specific fields
  nested_specific_fields <- c("outer_folds", "inner_folds", "outer_repeats", "inner_repeats",
                              "n_outer_tasks", "selected_outer_workers", "selected_threads_per_worker")
  expect_true(all(nested_specific_fields %in% names(nested_meta)))
  expect_true(all(nested_specific_fields %in% names(cross_nested_meta)))
})

test_that("cutoff-dependent cross_size_cv records exact candidate progress", {
  d <- .metadata_test_data(5L)
  res <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden", folds = 3,
    seed = 42, tuning = "auto", progress = TRUE
  )
  meta <- res$settings$execution_plan
  expect_identical(meta$progress_mode, "exact")
  expect_identical(meta$progress_unit, "candidate")
})

test_that("tuning='off' preserves exact legacy return structure with zero metadata", {
  d <- .metadata_test_data(5L)

  ex_off <- exhaustive_sum_roc(
    data = d, outcome = "y", items = paste0("Q", 1:5),
    max_items = 2, tuning = "off", progress = FALSE
  )
  expect_null(attr(ex_off, "execution_plan"))

  cv_off <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden",
    folds = 5, seed = 42, tuning = "off", progress = FALSE
  )
  expect_null(cv_off$settings$execution_plan)
  expect_false("execution_plan" %in% names(cv_off$settings))
})

test_that("constants planner_version and auto_runtime_threshold are preserved", {
  d <- .metadata_test_data(5L)

  ex_res <- exhaustive_sum_roc(
    data = d, outcome = "y", items = paste0("Q", 1:5),
    max_items = 2, tuning = "auto", progress = FALSE
  )
  ex_meta <- attr(ex_res, "execution_plan")

  cv_res <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, selection_metric = "youden",
    folds = 5, seed = 42, tuning = "auto", progress = FALSE
  )
  cv_meta <- cv_res$settings$execution_plan

  # planner_version is "0.2.0"
  expect_identical(ex_meta$planner_version, "0.2.0")
  expect_identical(cv_meta$planner_version, "0.2.0")

  # auto_runtime_threshold is 180.0
  expect_equal(ex_meta$auto_runtime_threshold, 180.0)
  expect_equal(cv_meta$auto_runtime_threshold, 180.0)
})

test_that(".planner_should_benchmark boundary contract is exact at 30.0 seconds", {
  # Strictly below threshold (29.999s) -> skip backend benchmark
  below <- NCVROC:::.planner_should_benchmark(29.999, threshold = 30.0)
  expect_false(below$backend_benchmark_required)
  expect_identical(below$reason, "estimated workload too small for backend benchmarking")

  # Exactly at threshold (30.000s) -> perform backend benchmark
  at_threshold <- NCVROC:::.planner_should_benchmark(30.000, threshold = 30.0)
  expect_true(at_threshold$backend_benchmark_required)
  expect_identical(at_threshold$reason, "estimated runtime justifies backend benchmarking")

  # Strictly above threshold (30.001s) -> perform backend benchmark
  above <- NCVROC:::.planner_should_benchmark(30.001, threshold = 30.0)
  expect_true(above$backend_benchmark_required)
  expect_identical(above$reason, "estimated runtime justifies backend benchmarking")
})

test_that("benchmark_table columns and types match canonical schema across APIs", {
  d_large <- .metadata_test_data(14L)
  d_cv <- .metadata_test_data(8L)

  ex_res <- exhaustive_sum_roc(
    data = d_large, outcome = "y", items = paste0("Q", 1:14),
    min_items = 1, max_items = 6, engine = "Rcpp",
    tuning = "always", progress = FALSE
  )
  ex_table <- attr(ex_res, "execution_plan")$benchmark_table

  cv_res <- cross_size_cv(
    data = d_cv, outcome = y, items = paste0("Q", 1:8),
    model_sizes = 1:4, selection_metric = "youden",
    folds = 5, seed = 99, tuning = "always", progress = FALSE
  )
  cv_table <- cv_res$settings$execution_plan$benchmark_table

  expected_cols <- c(
    "plan_id", "parallel", "n_workers", "resource_count", "backend_priority",
    "median_elapsed", "n_success", "n_failed", "status", "failure_reason"
  )

  expect_s3_class(ex_table, "data.frame")
  expect_s3_class(cv_table, "data.frame")
  expect_true(all(expected_cols %in% names(ex_table)))
  expect_true(all(expected_cols %in% names(cv_table)))

  expect_type(ex_table$plan_id, "character")
  expect_type(ex_table$parallel, "character")
  expect_type(ex_table$n_workers, "integer")
  expect_type(ex_table$resource_count, "integer")
  expect_type(ex_table$backend_priority, "integer")
  expect_type(ex_table$median_elapsed, "double")
  expect_type(ex_table$status, "character")

  expect_type(cv_table$plan_id, "character")
  expect_type(cv_table$parallel, "character")
  expect_type(cv_table$n_workers, "integer")
  expect_type(cv_table$resource_count, "integer")
  expect_type(cv_table$backend_priority, "integer")
  expect_type(cv_table$median_elapsed, "double")
  expect_type(cv_table$status, "character")
})

test_that("environment_summary respects privacy and contains no host/user/path info", {
  summary <- NCVROC:::.planner_environment_summary()
  expect_type(summary, "list")

  # Key privacy fields
  expect_true(all(c("detected_logical_cores", "detected_physical_cores", "package_version") %in% names(summary)))
  summary_chars <- unlist(lapply(summary, function(x) if (is.character(x)) x else character()))
  # Ensure no paths or usernames
  expect_false(any(grepl("[\\\\/]", summary_chars)))
  expect_false(any(grepl("Users", summary_chars, ignore.case = TRUE)))
})

test_that(".planner_format_execution_plan formats metadata correctly with approximate phrasing", {
  d_large <- .metadata_test_data(14L)
  d_cv <- .metadata_test_data(8L)

  ex_res <- exhaustive_sum_roc(
    data = d_large, outcome = "y", items = paste0("Q", 1:14),
    min_items = 1, max_items = 6, engine = "Rcpp",
    tuning = "always", progress = FALSE
  )

  cv_res <- cross_size_cv(
    data = d_cv, outcome = y, items = paste0("Q", 1:8),
    model_sizes = 1:4, selection_metric = "youden",
    folds = 5, seed = 99, tuning = "always", progress = FALSE
  )

  # Format from result object
  ex_text <- NCVROC:::.planner_format_execution_plan(ex_res)
  cv_text <- NCVROC:::.planner_format_execution_plan(cv_res)

  expect_type(ex_text, "character")
  expect_type(cv_text, "character")

  expect_match(ex_text, "=== Automatic Execution Plan ===")
  expect_match(ex_text, "Target API:               exhaustive_sum_roc")
  expect_match(ex_text, "approximate estimate")

  expect_match(cv_text, "=== Automatic Execution Plan ===")
  expect_match(cv_text, "Target API:               cross_size_cv")
  expect_match(cv_text, "Cross-Validation Method:")
  expect_match(cv_text, "approximate estimate")

  # Format from metadata directly
  meta_text <- NCVROC:::.planner_format_execution_plan(attr(ex_res, "execution_plan"))
  expect_identical(meta_text, ex_text)

  # Error on object without metadata
  expect_error(
    NCVROC:::.planner_format_execution_plan(data.frame(a = 1)),
    "does not contain execution_plan metadata"
  )
})
