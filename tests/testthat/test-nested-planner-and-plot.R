# tests/testthat/test-nested-planner-and-plot.R
# Verification of Phase B: Nested Execution Planning & Native Result Visualization

test_that("Phase B: .nested_evaluate_candidate_ranks returns identical candidate metrics across none, threads, outer, and hybrid", {
  set.seed(42)
  n <- 60
  p <- 10
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  items <- sprintf("Q%02d", 1:p)
  names(dat) <- items
  y <- sample(0:1, n, replace = TRUE)
  outer_folds <- make_stratified_folds(y, k = 3, repeats = 1, seed = 101)

  pilot <- NCVROC:::.planner_make_pilot_candidates(p, 1:3, max_candidates = 15L)

  # 1. none
  res_none <- NCVROC:::.nested_evaluate_candidate_ranks(
    data = dat, outcome = y, items = items, candidate_ranks = pilot,
    outer_folds = outer_folds, inner_k = 3L, outer_repeats = 1L, inner_repeats = 1L,
    cutoff_method = "youden", engine = "Rcpp", parallel_mode = "none",
    n_workers = 1L, threads_per_worker = 1L, seed = 500
  )

  # 2. threads
  res_threads <- NCVROC:::.nested_evaluate_candidate_ranks(
    data = dat, outcome = y, items = items, candidate_ranks = pilot,
    outer_folds = outer_folds, inner_k = 3L, outer_repeats = 1L, inner_repeats = 1L,
    cutoff_method = "youden", engine = "Rcpp", parallel_mode = "threads",
    n_workers = 2L, threads_per_worker = 1L, seed = 500
  )

  # 3. outer (PSOCK)
  res_outer <- NCVROC:::.nested_evaluate_candidate_ranks(
    data = dat, outcome = y, items = items, candidate_ranks = pilot,
    outer_folds = outer_folds, inner_k = 3L, outer_repeats = 1L, inner_repeats = 1L,
    cutoff_method = "youden", engine = "Rcpp", parallel_mode = "outer",
    n_workers = 2L, threads_per_worker = 1L, seed = 500
  )

  # 4. hybrid (PSOCK outer x C++ threads)
  res_hybrid <- NCVROC:::.nested_evaluate_candidate_ranks(
    data = dat, outcome = y, items = items, candidate_ranks = pilot,
    outer_folds = outer_folds, inner_k = 3L, outer_repeats = 1L, inner_repeats = 1L,
    cutoff_method = "youden", engine = "Rcpp", parallel_mode = "hybrid",
    n_workers = 2L, threads_per_worker = 2L, seed = 500
  )

  expect_identical(res_threads$candidate_metrics$items, res_none$candidate_metrics$items)
  expect_identical(res_outer$candidate_metrics$items, res_none$candidate_metrics$items)
  expect_identical(res_hybrid$candidate_metrics$items, res_none$candidate_metrics$items)

  expect_equal(res_threads$candidate_metrics$mean_auc, res_none$candidate_metrics$mean_auc, tolerance = 1e-12)
  expect_equal(res_outer$candidate_metrics$mean_auc, res_none$candidate_metrics$mean_auc, tolerance = 1e-12)
  expect_equal(res_hybrid$candidate_metrics$mean_auc, res_none$candidate_metrics$mean_auc, tolerance = 1e-12)

  expect_equal(res_threads$candidate_metrics$mean_youden, res_none$candidate_metrics$mean_youden, tolerance = 1e-12)
  expect_equal(res_outer$candidate_metrics$mean_youden, res_none$candidate_metrics$mean_youden, tolerance = 1e-12)
  expect_equal(res_hybrid$candidate_metrics$mean_youden, res_none$candidate_metrics$mean_youden, tolerance = 1e-12)
})

test_that("Phase B: plan_ncvroc_execution benchmarks legal nested plans and populates benchmark_table with status ok", {
  set.seed(42)
  n <- 80
  p <- 12
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  items <- sprintf("Q%02d", 1:p)
  names(dat) <- items
  dat$y <- sample(0:1, n, replace = TRUE)

  plan_res <- plan_ncvroc_execution(
    workflow = "cross_size_nested_cv",
    data = dat,
    outcome = "y",
    items = items,
    model_sizes = 1:3,
    outer_folds = 3,
    inner_folds = 3,
    tuning = "benchmark",
    seed = 42
  )

  expect_s3_class(plan_res, "ncvroc_execution_plan")
  tb <- plan_res$benchmark_table
  expect_true(is.data.frame(tb) && nrow(tb) > 0L)
  ok_rows <- tb[tb$status == "ok", , drop = FALSE]
  expect_true(nrow(ok_rows) >= 1L)
  expect_true(all(is.finite(ok_rows$median_elapsed)))
  expect_true(all(is.finite(ok_rows$speedup)))
  expect_true(all(is.finite(ok_rows$efficiency)))

  # Verify plot.ncvroc_execution_plan renders without errors or fallback messages
  tf <- tempfile(fileext = ".pdf")
  pdf(tf)
  on.exit({ dev.off(); unlink(tf) }, add = TRUE)
  expect_silent(plot(plan_res, type = "all"))
  expect_silent(plot(plan_res, type = "runtime"))
  expect_silent(plot(plan_res, type = "speedup"))
  expect_silent(plot(plan_res, type = "efficiency"))
})

test_that("Phase B: .nested_eval_fold_candidate_ranks maintains identical metrics between R and Rcpp engines", {
  set.seed(123)
  n <- 60
  p <- 10
  x <- matrix(sample(0:2, n * p, replace = TRUE), n, p)
  colnames(x) <- sprintf("V%02d", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  ranks <- data.frame(
    model_size = c(1L, 2L, 2L, 3L),
    combination_index_0based = c(0L, 1L, 5L, 2L)
  )

  r_res <- NCVROC:::.nested_eval_fold_candidate_ranks(
    x_mat = x, y_vec = y, candidate_ranks = ranks, item_names = colnames(x),
    inner_k = 3L, engine = "R", cutoff_method = "youden", seed = 999
  )
  cpp_res <- NCVROC:::.nested_eval_fold_candidate_ranks(
    x_mat = x, y_vec = y, candidate_ranks = ranks, item_names = colnames(x),
    inner_k = 3L, engine = "Rcpp", cutoff_method = "youden", seed = 999
  )

  expect_identical(r_res$items, cpp_res$items)
  expect_identical(r_res$feasible, cpp_res$feasible)
  expect_equal(r_res$mean_auc, cpp_res$mean_auc, tolerance = 1e-12)
  expect_equal(r_res$mean_youden, cpp_res$mean_youden, tolerance = 1e-12)
  expect_equal(r_res$mean_sensitivity, cpp_res$mean_sensitivity, tolerance = 1e-12)
  expect_equal(r_res$mean_specificity, cpp_res$mean_specificity, tolerance = 1e-12)
  expect_equal(r_res$mean_accuracy, cpp_res$mean_accuracy, tolerance = 1e-12)
})

test_that("Phase B: .planner_fit_affine_runtime accurately recovers known synthetic parameters and handles degenerate fits", {
  # 1. Recover known a and b with separate cluster_setup overhead
  known_setup <- 0.25
  known_eval_a <- 0.05
  known_b <- 0.001
  total_W <- 50000
  synthetic_timings <- data.frame(
    workload_units = c(16, 32),
    elapsed = c(known_eval_a + known_b * 16, known_eval_a + known_b * 32),
    cluster_setup = c(known_setup, known_setup),
    success = c(TRUE, TRUE)
  )

  fit <- NCVROC:::.planner_fit_affine_runtime(synthetic_timings, total_units = total_W)
  expect_identical(fit$status, "ok")
  expect_equal(fit$setup_seconds, known_setup + known_eval_a, tolerance = 1e-10)
  expect_equal(fit$seconds_per_unit, known_b, tolerance = 1e-10)
  expect_equal(fit$estimated_full_runtime, known_setup + known_eval_a + known_b * total_W, tolerance = 1e-8)

  # 2. Degenerate slope (T(W2) <= T(W1) -> b <= 0) must be rejected safely
  degenerate_timings <- data.frame(
    workload_units = c(16, 32),
    elapsed = c(0.10, 0.08),
    success = c(TRUE, TRUE)
  )
  deg_fit <- NCVROC:::.planner_fit_affine_runtime(degenerate_timings, total_units = total_W)
  expect_identical(deg_fit$status, "unavailable")
  expect_true(is.na(deg_fit$estimated_full_runtime))
})

test_that("Phase B: plot.cross_size_nested_cv_result renders all diagnostic views without error", {
  set.seed(42)
  n <- 50
  p <- 8
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  items <- sprintf("Q%02d", 1:p)
  names(dat) <- items
  dat$y <- sample(0:1, n, replace = TRUE)

  nested_res <- cross_size_nested_cv(
    data = dat,
    outcome = "y",
    items = items,
    model_sizes = 1:2,
    outer_folds = 3,
    inner_folds = 3,
    parallel = "none",
    seed = 42
  )

  expect_s3_class(nested_res, "cross_size_nested_cv_result")

  tf <- tempfile(fileext = ".pdf")
  pdf(tf)
  on.exit({ dev.off(); unlink(tf) }, add = TRUE)
  expect_no_error(plot(nested_res, which = "all"))
  expect_no_error(plot(nested_res, which = "performance"))
  expect_no_error(plot(nested_res, which = "performance", metric = "youden"))
  expect_no_error(plot(nested_res, which = "model_size"))
  expect_no_error(plot(nested_res, which = "combinations"))
})

test_that("Phase B: .planner_measure_duration_cumulative enforces duration targets, budget limits, and stability", {
  # 1. Fast micro-workload dynamically scales repetition count to reach target duration
  call_count <- 0L
  fake_clock_time <- 0.0
  fake_clock <- function() fake_clock_time
  fast_fn <- function() {
    call_count <<- call_count + 1L
    fake_clock_time <<- fake_clock_time + 0.0001 # 100 us per call
  }

  res <- NCVROC:::.planner_measure_duration_cumulative(
    eval_fn = fast_fn,
    clock = fake_clock,
    target_duration = 0.05,
    max_duration_budget = 1.0
  )

  expect_identical(res$status, "ok")
  expect_true(res$is_stable)
  expect_true(res$reps >= 500L)
  expect_true(res$total_time >= 0.05)
  expect_equal(res$elapsed, 0.0001, tolerance = 1e-6)

  # 2. Hard budget limit terminates measurement early
  call_count_budget <- 0L
  fake_clock_time_budget <- 0.0
  fake_clock_budget <- function() fake_clock_time_budget
  slow_fn <- function() {
    call_count_budget <<- call_count_budget + 1L
    fake_clock_time_budget <<- fake_clock_time_budget + 0.01 # 10 ms per call
  }

  res_budget <- NCVROC:::.planner_measure_duration_cumulative(
    eval_fn = slow_fn,
    clock = fake_clock_budget,
    target_duration = 1.0,
    max_duration_budget = 0.025 # only enough budget for ~2-3 calls
  )

  expect_true(res_budget$total_time <= 0.04)
  expect_identical(res_budget$status, "unstable")
  expect_false(res_budget$is_stable)

  # 3. Single-trial long duration exits immediately without repeated calls
  single_call_count <- 0L
  fake_clock_time_single <- 0.0
  fake_clock_single <- function() fake_clock_time_single
  long_fn <- function() {
    single_call_count <<- single_call_count + 1L
    fake_clock_time_single <<- fake_clock_time_single + 0.10
  }

  res_single <- NCVROC:::.planner_measure_duration_cumulative(
    eval_fn = long_fn,
    clock = fake_clock_single,
    target_duration = 0.05
  )

  expect_identical(res_single$status, "ok")
  expect_identical(res_single$reps, 1L)
  expect_identical(single_call_count, 1L)
  expect_equal(res_single$elapsed, 0.10, tolerance = 1e-6)
})

test_that("Phase B: tuning = 'auto' skips benchmarking when nested workload is below benchmark_threshold", {
  set.seed(42)
  n <- 40
  p <- 8
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  items <- sprintf("Q%02d", 1:p)
  names(dat) <- items
  dat$y <- sample(0:1, n, replace = TRUE)

  # Total workload = sum(choose(8, 1:2)) * 3 * 1 = (8 + 28) * 3 = 108 < 5,000,000
  plan_auto <- plan_ncvroc_execution(
    workflow = "cross_size_nested_cv",
    data = dat,
    outcome = "y",
    items = items,
    model_sizes = 1:2,
    outer_folds = 3,
    inner_folds = 3,
    tuning = "auto",
    benchmark_threshold = 5000000,
    seed = 42
  )

  expect_s3_class(plan_auto, "ncvroc_execution_plan")
  expect_false(isTRUE(plan_auto$backend_benchmark_performed))
  expect_match(plan_auto$decision_reason, "workload below threshold")

  # Setting low benchmark_threshold = 10 triggers benchmark sweep
  plan_low_thresh <- plan_ncvroc_execution(
    workflow = "cross_size_nested_cv",
    data = dat,
    outcome = "y",
    items = items,
    model_sizes = 1:2,
    outer_folds = 3,
    inner_folds = 3,
    tuning = "auto",
    benchmark_threshold = 10,
    seed = 42
  )

  expect_true(isTRUE(plan_low_thresh$backend_benchmark_performed))
})

test_that("Phase B: pilot candidate generator allocates proportionally across model sizes", {
  # p=36, sizes=1:4: total 66,711 combinations, size 4 is ~88.3%
  pilot <- NCVROC:::.planner_make_pilot_candidates(36L, 1:4, max_candidates = 128L)

  expect_equal(nrow(pilot), 128L)
  quotas <- as.integer(table(factor(pilot$model_size, levels = 1:4)))

  # Every requested size with legal candidates must have >= 1 candidate
  expect_true(all(quotas >= 1L))
  # Sum equals requested budget
  expect_equal(sum(quotas), 128L)
  # Size 4 represents the vast majority of the pilot, matching the true candidate space
  expect_gt(quotas[4L], 100L)
  expect_lt(quotas[1L], 5L)
  expect_lt(quotas[2L], 5L)
})
