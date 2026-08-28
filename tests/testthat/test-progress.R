# test-progress.R -- Tests for Phase 3 Progress Reporting and Approximate ETA

test_that(".format_eta formats duration strings across timescales", {
  expect_identical(NCVROC:::.format_eta(NA_real_), "")
  expect_identical(NCVROC:::.format_eta(-5), "")
  expect_identical(NCVROC:::.format_eta(Inf), "")

  # Under 10 seconds
  expect_identical(NCVROC:::.format_eta(4), "< 10 sec")
  expect_identical(NCVROC:::.format_eta(9.9), "< 10 sec")

  # Under 60 seconds (rounded to nearest 5s)
  expect_identical(NCVROC:::.format_eta(12), "~10 sec")
  expect_identical(NCVROC:::.format_eta(23), "~25 sec")
  expect_identical(NCVROC:::.format_eta(44), "~45 sec")
  expect_identical(NCVROC:::.format_eta(58), "~1m")

  # Under 3600 seconds (minutes and seconds)
  expect_identical(NCVROC:::.format_eta(65), "~1m 5s")
  expect_identical(NCVROC:::.format_eta(150), "~2m 30s")
  expect_identical(NCVROC:::.format_eta(600), "~10m")

  # Over 3600 seconds (hours and minutes)
  expect_identical(NCVROC:::.format_eta(3660), "~1h 1m")
  expect_identical(NCVROC:::.format_eta(7200), "~2h")
})

test_that(".format_eta_message enforces gating and format rules", {
  # Less than minimum units (3)
  expect_identical(NCVROC:::.format_eta_message(0L, 10L, 50), "")
  expect_identical(NCVROC:::.format_eta_message(1L, 10L, 50), "")
  expect_identical(NCVROC:::.format_eta_message(2L, 10L, 50), "")

  # Invalid inputs
  expect_identical(NCVROC:::.format_eta_message(5L, 0L, 50), "")
  expect_identical(NCVROC:::.format_eta_message(12L, 10L, 50), "")
  expect_identical(NCVROC:::.format_eta_message(3L, 10L, 0), "")
  expect_identical(NCVROC:::.format_eta_message(3L, 10L, -10), "")

  # Valid call (3 of 10 done, 30s elapsed -> rate = 0.1/s, remaining 70s -> ~1m 10s)
  msg <- NCVROC:::.format_eta_message(3L, 10L, 30)
  expect_true(nzchar(msg))
  expect_match(msg, "remaining")
  expect_match(msg, "30% complete")
  expect_match(msg, "3/10 units done")
})

test_that(".progress_make finish and close are idempotent and safe", {
  # Disabled progress
  prg_off <- NCVROC:::.progress_make(10L, enabled = FALSE)
  expect_no_error(prg_off$tick())
  expect_no_error(prg_off$eta_message())
  expect_no_error(prg_off$finish())
  expect_no_error(prg_off$close())

  # Enabled progress: normal finish
  utils::capture.output({
    prg <- NCVROC:::.progress_make(5L, enabled = TRUE)
    expect_no_error(prg$tick(2L))
    expect_no_error(prg$finish())
    # Harmless second finish or subsequent close (e.g. from on.exit)
    expect_no_error(prg$finish())
    expect_no_error(prg$close())
  })

  # Enabled progress: close at partial progress (error/interrupt simulation)
  utils::capture.output({
    prg2 <- NCVROC:::.progress_make(10L, enabled = TRUE)
    expect_no_error(prg2$tick(3L))
    expect_no_error(prg2$close())
    # Repeated close is harmless
    expect_no_error(prg2$close())
  })
})

test_that(".progress_make clamps completed units to total", {
  utils::capture.output({
    prg <- NCVROC:::.progress_make(3L, enabled = TRUE)
    expect_no_error(prg$tick(100L))
    expect_no_error(prg$finish())
  })
})

test_that("simulated error with on.exit(close()) does not advance to 100%", {
  test_fn <- function(fail = TRUE) {
    prg <- NCVROC:::.progress_make(10L, enabled = TRUE)
    on.exit(prg$close(), add = TRUE)
    for (i in 1:3) {
      prg$tick(1L)
    }
    if (fail) stop("deliberate failure at 30%")
    prg$finish()
  }

  expect_error(utils::capture.output(test_fn(fail = TRUE)), "deliberate failure at 30%")
  expect_no_error(utils::capture.output(test_fn(fail = FALSE)))
})

test_that("exhaustive_sum_roc statistical and RNG identity across progress modes", {
  set.seed(42)
  d <- data.frame(
    y = c(rep(1L, 10), rep(0L, 10)),
    q1 = rnorm(20),
    q2 = rnorm(20),
    q3 = rnorm(20),
    q4 = rnorm(20)
  )

  # Progress off baseline
  set.seed(123)
  res_off <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                                min_items = 1, max_items = 2, engine = "R",
                                progress = FALSE)
  rng_off <- .Random.seed

  # Progress on
  set.seed(123)
  utils::capture.output({
    res_on <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                                 min_items = 1, max_items = 2, engine = "R",
                                 progress = TRUE)
  })
  rng_on <- .Random.seed

  expect_identical(res_off, res_on)
  expect_identical(rng_off, rng_on)

  # Rcpp engine with progress
  set.seed(123)
  res_rcpp_off <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                                     min_items = 1, max_items = 2, engine = "Rcpp",
                                     progress = FALSE)
  set.seed(123)
  utils::capture.output({
    res_rcpp_on <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                                      min_items = 1, max_items = 2, engine = "Rcpp",
                                      progress = TRUE)
  })
  expect_identical(res_rcpp_off, res_rcpp_on)
})

test_that("cross_size_cv statistical and RNG identity across progress modes", {
  set.seed(42)
  d <- data.frame(
    y = c(rep(1L, 10), rep(0L, 10)),
    q1 = rnorm(20),
    q2 = rnorm(20),
    q3 = rnorm(20),
    q4 = rnorm(20)
  )

  # Strategy 1 (AUC unconstrained)
  set.seed(456)
  res1_off <- cross_size_cv(d, "y", c("q1", "q2", "q3", "q4"),
                            model_sizes = 1:2, folds = 2, repeats = 1,
                            progress = FALSE, seed = 100)
  rng1_off <- .Random.seed

  set.seed(456)
  utils::capture.output({
    res1_on <- cross_size_cv(d, "y", c("q1", "q2", "q3", "q4"),
                             model_sizes = 1:2, folds = 2, repeats = 1,
                             progress = TRUE, seed = 100)
  })
  rng1_on <- .Random.seed

  expect_identical(res1_off$summary, res1_on$summary)
  expect_identical(res1_off$best_model, res1_on$best_model)
  expect_identical(res1_off$oof_predictions, res1_on$oof_predictions)
  expect_identical(rng1_off, rng1_on)

  # Strategy 2 (constrained)
  set.seed(789)
  res2_off <- cross_size_cv(d, "y", c("q1", "q2", "q3", "q4"),
                            model_sizes = 1:2, folds = 2, repeats = 1,
                            sensitivity_min = 0.5, specificity_min = 0.5,
                            progress = FALSE, seed = 200)
  set.seed(789)
  utils::capture.output({
    res2_on <- cross_size_cv(d, "y", c("q1", "q2", "q3", "q4"),
                             model_sizes = 1:2, folds = 2, repeats = 1,
                             sensitivity_min = 0.5, specificity_min = 0.5,
                             progress = TRUE, seed = 200)
  })
  expect_identical(res2_off$summary, res2_on$summary)
  expect_identical(res2_off$best_model, res2_on$best_model)
})

test_that("nested_sum_roc statistical and RNG identity across progress modes", {
  set.seed(42)
  d <- data.frame(
    y = c(rep(1L, 10), rep(0L, 10)),
    q1 = rnorm(20),
    q2 = rnorm(20),
    q3 = rnorm(20),
    q4 = rnorm(20)
  )

  set.seed(999)
  res_off <- nested_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                            min_items = 1, max_items = 2,
                            outer_k = 2, inner_k = 2, outer_repeats = 1,
                            progress = FALSE, verbose = FALSE, seed = 42)
  rng_off <- .Random.seed

  set.seed(999)
  utils::capture.output({
    res_on <- nested_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                             min_items = 1, max_items = 2,
                             outer_k = 2, inner_k = 2, outer_repeats = 1,
                             progress = TRUE, verbose = FALSE, seed = 42)
  })
  rng_on <- .Random.seed

  expect_identical(res_off$summary, res_on$summary)
  expect_identical(res_off$selected_models, res_on$selected_models)
  expect_identical(res_off$selected_model_frequency, res_on$selected_model_frequency)
  expect_identical(res_off$outer_predictions, res_on$outer_predictions)
  expect_identical(rng_off, rng_on)
})

test_that("cross_size_nested_cv statistical and RNG identity across progress modes", {
  set.seed(42)
  d <- data.frame(
    y = c(rep(1L, 10), rep(0L, 10)),
    q1 = rnorm(20),
    q2 = rnorm(20),
    q3 = rnorm(20),
    q4 = rnorm(20)
  )

  set.seed(888)
  res_off <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3", "q4"),
                                  min_items = 1, max_items = 2, outer_folds = 2, inner_folds = 2,
                                  outer_repeats = 1, progress = FALSE, verbose = FALSE,
                                  seed = 42)
  rng_off <- .Random.seed

  set.seed(888)
  utils::capture.output({
    res_on <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3", "q4"),
                                   min_items = 1, max_items = 2, outer_folds = 2, inner_folds = 2,
                                   outer_repeats = 1, progress = TRUE, verbose = FALSE,
                                   seed = 42)
  })
  rng_on <- .Random.seed

  expect_identical(res_off$summary, res_on$summary)
  expect_identical(res_off$outer_fold_results, res_on$outer_fold_results)
  expect_identical(res_off$outer_predictions, res_on$outer_predictions)
  expect_identical(rng_off, rng_on)
})

test_that("planner integration is preserved when progress is enabled", {
  set.seed(42)
  d <- data.frame(
    y = c(rep(1L, 12), rep(0L, 12)),
    q1 = rnorm(24),
    q2 = rnorm(24),
    q3 = rnorm(24),
    q4 = rnorm(24)
  )

  utils::capture.output({
    res_auto <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3", "q4"),
                                   min_items = 1, max_items = 2, engine = "Rcpp",
                                   tuning = "auto", progress = TRUE)
  })
  expect_false(is.null(attr(res_auto, "execution_plan")))

  utils::capture.output({
    res_always <- cross_size_cv(d, "y", c("q1", "q2", "q3", "q4"),
                                model_sizes = 1:2, folds = 2, repeats = 1,
                                tuning = "always", progress = TRUE, seed = 42)
  })
  expect_false(is.null(res_always$settings$execution_plan))
})

test_that("no clusterApplyLB exists in codebase", {
  r_files <- list.files(testthat::test_path("../../R"), pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) {
    content <- readLines(f, warn = FALSE)
    expect_false(any(grepl("clusterApplyLB", content)),
                 info = sprintf("Found clusterApplyLB in %s", basename(f)))
  }
})
