# test-scaling-analysis.R - Phase A scaling analysis and saturation tests

.scaling_test_data <- function(n_items = 6L, n_pos = 15L, n_neg = 15L) {
  set.seed(42L)
  n_total <- n_pos + n_neg
  mat <- matrix(sample(0:2, n_total * n_items, replace = TRUE), nrow = n_total, ncol = n_items)
  df <- as.data.frame(mat)
  names(df) <- paste0("Q", seq_len(n_items))
  df$y <- rep(c(0L, 1L), c(n_neg, n_pos))
  df
}

test_that(".planner_generate_legal_plans exhaustively generates all legal resource levels", {
  # Cap = 6
  plans_flat <- NCVROC:::.planner_generate_legal_plans(
    api = "exhaustive_sum_roc", available_resources = 6L, engine = "Rcpp"
  )
  # Should have none_1, threads_2..6, chunks_2..6 -> 1 + 5 + 5 = 11 plans
  expect_equal(nrow(plans_flat), 11L)
  expect_true(all(paste0("threads_", 2:6) %in% plans_flat$plan_id))
  expect_true(all(paste0("chunks_", 2:6) %in% plans_flat$plan_id))
})

test_that(".planner_generate_nested_legal_plans exhaustively generates nested and hybrid pairs", {
  plans_nested <- NCVROC:::.planner_generate_nested_legal_plans(
    api = "nested_sum_roc", available_resources = 6L,
    n_outer_tasks = 4L, inner_candidate_tasks = 100L, engine = "Rcpp"
  )
  # Check hybrid plans generated (e.g. 2x2, 2x3, 3x2)
  expect_true(any(grepl("^hybrid_", plans_nested$plan_id)))
  expect_true("hybrid_2x2" %in% plans_nested$plan_id)
  expect_true("hybrid_2x3" %in% plans_nested$plan_id)
  expect_true("hybrid_3x2" %in% plans_nested$plan_id)
})

test_that(".planner_compute_scaling_metrics computes speedup, efficiency, and affine runtime", {
  raw_tbl <- data.frame(
    plan_id = c("none_1", "threads_2", "threads_4", "chunks_2"),
    parallel = c("none", "threads", "threads", "chunks"),
    n_workers = c(1L, 2L, 4L, 2L),
    resource_count = c(1L, 2L, 4L, 2L),
    median_elapsed = c(10.0, 5.5, 3.0, 6.0),
    status = c("ok", "ok", "ok", "ok"),
    stringsAsFactors = FALSE
  )

  affine <- rbind(
    data.frame(plan_id = "none_1", workload_units = c(50, 100), elapsed = c(6, 11), success = TRUE),
    data.frame(plan_id = "threads_2", workload_units = c(50, 100), elapsed = c(3.5, 6), success = TRUE),
    data.frame(plan_id = "threads_4", workload_units = c(50, 100), elapsed = c(2, 3), success = TRUE),
    data.frame(plan_id = "chunks_2", workload_units = c(50, 100), elapsed = c(4, 6), success = TRUE)
  )
  res <- NCVROC:::.planner_compute_scaling_metrics(
    raw_tbl, total_candidates = 1000L, pilot_candidates = 100L, affine_timings = affine
  )

  tbl <- res$benchmark_table
  expect_true(all(c("speedup", "parallel_efficiency", "estimated_full_runtime") %in% names(tbl)))
  expect_equal(tbl$speedup[tbl$plan_id == "none_1"], 1.0)
  expect_equal(tbl$parallel_efficiency[tbl$plan_id == "none_1"], 1.0)

  expect_equal(tbl$estimated_full_runtime[tbl$plan_id == "threads_4"], 21)
  expect_equal(tbl$setup_seconds[tbl$plan_id == "chunks_2"], 2)
  expect_equal(tbl$estimated_full_runtime[tbl$plan_id == "chunks_2"], 42)
  expect_true(all(tbl$estimate_method == "empirical_affine"))
})

test_that("two-point affine timing is empirical and invalid fits remain unavailable", {
  valid <- NCVROC:::.planner_fit_affine_runtime(
    data.frame(workload_units = c(10, 30), elapsed = c(3, 7), success = c(TRUE, TRUE)), 100
  )
  expect_identical(valid$method, "empirical_affine")
  expect_equal(valid$setup_seconds, 1)
  expect_equal(valid$seconds_per_unit, .2)
  expect_equal(valid$estimated_full_runtime, 21)
  invalid <- NCVROC:::.planner_fit_affine_runtime(
    data.frame(workload_units = c(10, 30), elapsed = c(7, 3), success = c(TRUE, TRUE)), 100
  )
  expect_identical(invalid$method, "unavailable")
  expect_true(is.na(invalid$estimated_full_runtime))
})

test_that("workload insufficiency is explicit and excluded from selection", {
  plans <- data.frame(plan_id = c("none_1", "threads_2", "chunks_2"),
                      parallel = c("none", "threads", "chunks"),
                      n_workers = c(1L, 2L, 2L), resource_count = c(1L, 2L, 2L),
                      backend_priority = 1:3, stringsAsFactors = FALSE)
  statuses <- NCVROC:::.planner_plan_status_table(plans, candidate_count = 64L,
                                                   grain_size = 64L, task_count = 1L)
  expect_identical(statuses$status, c("pending", "insufficient_workload", "insufficient_workload"))
  tbl <- statuses
  tbl$median_elapsed <- c(10, .01, .02)
  selected <- NCVROC:::.planner_select_plan(tbl, fallback_plan = plans[1, ])
  expect_identical(selected$selected_parallel, "none")
})

test_that("primary 180s gate and secondary budget rule operate correctly", {
  # Below 180s threshold -> benchmark not required
  below <- NCVROC:::.planner_should_benchmark(120.0, threshold = 180.0)
  expect_false(below$backend_benchmark_required)
  expect_match(below$reason, "estimated")

  # Above 180s threshold -> benchmark required
  above <- NCVROC:::.planner_should_benchmark(240.0, threshold = 180.0)
  expect_true(above$backend_benchmark_required)
  allowed <- NCVROC:::.planner_sweep_gate(240, expected_sweep_seconds = 12, threshold = 180)
  expect_true(allowed$allowed)
  expect_equal(allowed$overhead_budget_seconds, 12)
  denied <- NCVROC:::.planner_sweep_gate(240, expected_sweep_seconds = 12.1, threshold = 180)
  expect_false(denied$allowed)
  expect_identical(denied$reason, "benchmark_budget_insufficient")
})

test_that("cross_size_cv with exhaustive sweep preserves statistical invariance and RNG", {
  d <- .scaling_test_data(6L, n_pos = 12L, n_neg = 12L)

  set.seed(123L)
  seed_before_off <- .Random.seed
  res_off <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, folds = 3, repeats = 1,
    selection_metric = "youden", tuning = "off",
    seed = 42L, progress = FALSE
  )
  seed_after_off <- .Random.seed

  set.seed(123L)
  seed_before_always <- .Random.seed
  res_always <- cross_size_cv(
    data = d, outcome = y, items = paste0("Q", 1:5),
    model_sizes = 1:2, folds = 3, repeats = 1,
    selection_metric = "youden", tuning = "always",
    seed = 42L, progress = FALSE
  )
  seed_after_always <- .Random.seed

  # Statistical equality
  expect_equal(res_off$final_selected_model, res_always$final_selected_model)
  expect_equal(res_off$candidate_ranking, res_always$candidate_ranking)
  expect_equal(res_off$cv_performance, res_always$cv_performance)
  expect_equal(res_off$oof_predictions, res_always$oof_predictions)

  # RNG determinism
  expect_identical(seed_after_off, seed_after_always)
})
