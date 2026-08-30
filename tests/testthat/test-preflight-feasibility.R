# test-preflight-feasibility.R — Preflight Feasibility & Diagnostics Unit Tests
#
# Covers:
# 1. Exact candidate-space counts (p=12, p=40, p=103)
# 2. Workflow evaluation multipliers (exhaustive, CV Strategy 1/2, nested CV, stability bootstrap/cv)
# 3. Static feasibility tier classifications and explicit boundaries (including p=20, sizes=1:4 -> 6,195 as small)
# 4. Dual-tier candidate space vs effective workload classification (nested CV escalation)
# 5. Model-size-stratified pilot benchmark representation across all sizes
# 6. Zero RNG contamination
# 7. Strict timing-quality gate (no fake rates on zero-duration strata, no extrapolation below 0.5s)
# 8. Hard cap on pilot_candidates (never exceeds user budget)
# 9. Execution pathway and memory profile resolution
# 10. Print method formatting with clear stratum status
# 11. Edge cases (single size, unsorted/duplicate sizes, invalid p)

test_that("Step 5: Exact candidate space calculation for canonical workloads", {
  # p = 12, sizes 1:3 -> 12 + 66 + 220 = 298
  w_12 <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "cross_size_cv")
  expect_equal(w_12$candidate_space, 298)
  expect_equal(unname(w_12$candidate_count_by_size), c(12, 66, 220))

  # p = 40, sizes 1:5 -> 40 + 780 + 9880 + 91390 + 658008 = 760098
  w_40 <- NCVROC:::.preflight_calculate_workload(40L, 1:5, workflow = "cross_size_cv")
  expect_equal(w_40$candidate_space, 760098)

  # p = 103, sizes 1:5 -> 103 + 5253 + 176851 + 4421275 + 87541245 = 92144727
  w_103 <- NCVROC:::.preflight_calculate_workload(103L, 1:5, workflow = "cross_size_cv")
  expect_equal(w_103$candidate_space, 92144727)
})

test_that("Step 5: Workflow evaluation multipliers reflect production architecture", {
  # exhaustive_sum_roc: multiplier = 1
  w_ex <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "exhaustive_sum_roc")
  expect_equal(w_ex$candidate_evaluations, 298)

  # cross_size_cv Strategy 1 (AUC): base candidate search (W=298) + global single best model CV (5 * 1 = 5) -> total = 303
  w_cv_auc <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "cross_size_cv",
                                                     selection_metric = "auc", top_n = 20L,
                                                     folds = 5L, repeats = 1L)
  expect_equal(w_cv_auc$candidate_evaluations, 298 + 5L)

  # cross_size_cv Strategy 2 (Youden, 5 folds x 2 repeats): multiplier = 10
  w_cv_youd <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "cross_size_cv",
                                                      selection_metric = "youden",
                                                      folds = 5L, repeats = 2L)
  expect_equal(w_cv_youd$candidate_evaluations, 2980)

  # cross_size_nested_cv Strategy 2 (5 outer folds x 1 repeat, inner Strategy 2 5 folds x 1 repeat): 5 * 5 = 25 multiplier
  w_ncv <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "cross_size_nested_cv",
                                                  selection_metric = "youden",
                                                  outer_folds = 5L, outer_repeats = 1L,
                                                  inner_folds = 5L, inner_repeats = 1L)
  expect_equal(w_ncv$candidate_evaluations, 298 * 25)

  # candidate_stability_roc bootstrap vs cv modes
  w_stab_boot <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "candidate_stability_roc",
                                                        stability_mode = "bootstrap", b_resamples = 50L)
  expect_equal(w_stab_boot$candidate_evaluations, 298 * 50)

  w_stab_cv <- NCVROC:::.preflight_calculate_workload(12L, 1:3, workflow = "candidate_stability_roc",
                                                     stability_mode = "cv", folds = 5L, repeats = 2L)
  expect_equal(w_stab_cv$candidate_evaluations, 298 * 10)
})

test_that("Step 5: Static warning classification categorizes tiers accurately and handles boundaries", {
  # Explicit boundary checks
  expect_equal(NCVROC:::.preflight_classify_workload(0), "small")
  expect_equal(NCVROC:::.preflight_classify_workload(298), "small")
  # p = 20, sizes 1:4 -> 20 + 190 + 1140 + 4845 = 6195 -> small (<= 10,000)
  expect_equal(NCVROC:::.preflight_classify_workload(6195), "small")
  expect_equal(NCVROC:::.preflight_classify_workload(10000), "small")
  expect_equal(NCVROC:::.preflight_classify_workload(10001), "moderate")
  expect_equal(NCVROC:::.preflight_classify_workload(500000), "moderate")
  expect_equal(NCVROC:::.preflight_classify_workload(500001), "large")
  expect_equal(NCVROC:::.preflight_classify_workload(760098), "large")
  expect_equal(NCVROC:::.preflight_classify_workload(10000000), "large")
  expect_equal(NCVROC:::.preflight_classify_workload(10000001), "very_large")
  expect_equal(NCVROC:::.preflight_classify_workload(92144727), "very_large")
})

test_that("Step 5: Dual-tier candidate space vs effective workload classification", {
  # Workload with W = 760,098 (large), but in nested CV (5 outer x 10 inner -> 50x multiplier)
  # Evaluations = 760,098 * 50 = 38,004,900 -> very_large
  pf <- ncvroc_preflight(
    data             = NULL,
    items            = paste0("q", 1:40),
    model_sizes      = 1:5,
    workflow         = "cross_size_nested_cv",
    selection_metric = "youden",
    outer_folds      = 5L,
    outer_repeats    = 1L,
    inner_folds      = 5L,
    inner_repeats    = 2L,
    pilot            = FALSE
  )

  expect_equal(pf$candidate_space, 760098)
  expect_equal(pf$candidate_space_class, "large")
  expect_equal(pf$candidate_evaluations, 760098 * 50)
  expect_equal(pf$effective_workload_class, "very_large")
})

test_that("Step 5: Preflight does NOT contaminate RNG state", {
  set.seed(501)
  n <- 50L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE)
  )

  # Capture seed state
  set.seed(999)
  s_before <- .Random.seed
  r_val1 <- runif(5)

  # Run preflight with pilot benchmark
  set.seed(999)
  pf <- ncvroc_preflight(
    data        = dat,
    outcome     = "y",
    items       = c("q1", "q2", "q3", "q4"),
    model_sizes = 1:3,
    pilot       = TRUE
  )
  s_after <- .Random.seed
  r_val2 <- runif(5)

  # Verify RNG seed was completely untouched
  expect_identical(s_after, s_before)
  expect_identical(r_val2, r_val1)
})

test_that("Step 5: Stratified pilot benchmarks across all sizes and enforces hard budget cap", {
  set.seed(502)
  n <- 100L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE),
    q5 = sample(0:2, n, replace = TRUE),
    q6 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("q", 1:6)

  pf <- ncvroc_preflight(
    data             = dat,
    outcome          = "y",
    items            = items,
    model_sizes      = 1:3,
    workflow         = "cross_size_cv",
    engine           = "Rcpp",
    parallel         = "threads",
    n_workers        = 2L,
    pilot            = TRUE,
    pilot_candidates = 25L
  )

  expect_s3_class(pf, "ncvroc_preflight")
  expect_equal(pf$candidate_space, 41) # 6 + 15 + 20

  # Hard cap: pilot candidate count must NEVER exceed requested pilot_candidates (25L)
  expect_lte(pf$pilot$pilot_candidates, 25L)

  # Check that model sizes are represented
  expect_equal(names(pf$pilot$per_size_results), c("1", "2", "3"))
})

test_that("Step 5: Strict timing gate prevents fake rates on zero-duration strata", {
  set.seed(504)
  n <- 50L
  dat <- data.frame(matrix(sample(0:2, n * 20, replace = TRUE), n, 20))
  names(dat) <- paste0("q", 1:20)
  dat$y <- sample(0:1, n, replace = TRUE)

  # Run preflight on p=20 with small pilot_candidates = 100L
  pf <- ncvroc_preflight(
    data             = dat,
    outcome          = "y",
    items            = paste0("q", 1:20),
    model_sizes      = 1:4,
    pilot_candidates = 100L,
    parallel         = "threads",
    n_workers        = 4L
  )

  # If any stratum elapsed time is below 0.010 sec, its candidates_per_sec MUST be NA
  for (s_char in names(pf$pilot$per_size_results)) {
    ps <- pf$pilot$per_size_results[[s_char]]
    if (ps$pilot_elapsed_sec < 0.010) {
      expect_true(is.na(ps$candidates_per_sec))
    }
  }

  # If total pilot time < 0.5 sec on uncompleted space, numeric estimate MUST be NA
  if (pf$pilot$pilot_elapsed_sec < 0.50 && pf$pilot$pilot_candidates < pf$candidate_space) {
    expect_true(is.na(pf$runtime_estimate$seconds))
    expect_match(pf$runtime_estimate$human, "unavailable.*insufficient stable timing data")
  }
})

test_that("Step 5: Print method formats output cleanly with stratified breakdown and stratum status", {
  set.seed(503)
  n <- 30L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE)
  )

  pf <- ncvroc_preflight(
    data        = dat,
    outcome     = "y",
    items       = c("q1", "q2"),
    model_sizes = 1:2,
    pilot       = TRUE
  )

  out <- utils::capture.output(print(pf))
  expect_true(any(grepl("NCVROC Preflight Diagnostics", out)))
  expect_true(any(grepl("Unique candidate space:", out)))
  expect_true(any(grepl("Estimated candidate evaluations:", out)))
  expect_true(any(grepl("Stratified Pilot Benchmark:", out)))
})

test_that("Step 5: Edge cases handled safely without error or mutation", {
  # Unsorted and duplicate sizes
  w_unsorted <- NCVROC:::.preflight_calculate_workload(6L, c(3, 1, 2, 2, 1))
  expect_identical(w_unsorted$model_sizes, 1:3L)
  expect_equal(w_unsorted$candidate_space, 41)

  # Single model size
  w_single <- NCVROC:::.preflight_calculate_workload(6L, 2L)
  expect_identical(w_single$model_sizes, 2L)
  expect_equal(w_single$candidate_space, 15)

  # Invalid inputs error clearly
  expect_error(NCVROC:::.preflight_calculate_workload(5L, 1:6), "between 1 and 5")
  expect_error(NCVROC:::.preflight_calculate_workload(-1L, 1:3), "positive integer")
})
