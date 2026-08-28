# test-candidate-stability-plot.R — Unit tests for plot.candidate_stability_result()

test_that("plot.candidate_stability_result executes cleanly for all plot types", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 20), rep(0, 20)),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  cands <- list(
    "Model 1" = "Q1",
    "Model 2" = c("Q1", "Q2"),
    "Model 3" = c("Q1", "Q3", "Q4")
  )

  res_cv <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = cands,
    resampling      = "repeated_cv",
    folds           = 3,
    repeats         = 2,
    sensitivity_min = 0.5,
    specificity_min = 0.5,
    seed            = 42
  )

  res_boot_orig <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = cands,
    resampling      = "bootstrap",
    bootstrap_reps  = 10,
    bootstrap_test  = "original",
    sensitivity_min = 0.5,
    specificity_min = 0.5,
    seed            = 42
  )

  res_boot_oob <- candidate_stability_roc(
    data            = dat,
    outcome         = y,
    candidate_sets  = cands,
    resampling      = "bootstrap",
    bootstrap_reps  = 10,
    bootstrap_test  = "oob",
    sensitivity_min = 0.5,
    specificity_min = 0.5,
    seed            = 42
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # 1. rank_stability
  expect_silent(plot(res_cv, type = "rank_stability"))
  expect_silent(plot(res_boot_orig, type = "rank_stability"))
  expect_silent(plot(res_boot_oob, type = "rank_stability"))

  # 2. performance across all metrics
  for (m in c("youden", "sensitivity", "specificity", "accuracy", "ppv", "npv", "auc")) {
    expect_silent(plot(res_cv, type = "performance", metric = m))
    expect_silent(plot(res_boot_orig, type = "performance", metric = m))
    expect_silent(plot(res_boot_oob, type = "performance", metric = m))
  }

  # 3. selection_frequency
  expect_silent(plot(res_cv, type = "selection_frequency"))
  expect_silent(plot(res_boot_orig, type = "selection_frequency"))
  expect_silent(plot(res_boot_oob, type = "selection_frequency"))

  # 4. constraint_stability (both constraints)
  expect_silent(plot(res_cv, type = "constraint_stability"))
  expect_silent(plot(res_boot_orig, type = "constraint_stability"))
  expect_silent(plot(res_boot_oob, type = "constraint_stability"))
})

test_that("plot top_n validation and display truncation", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"), "M3" = c("Q1", "Q3"))
  res <- candidate_stability_roc(dat, y, cands, folds = 3, repeats = 2, seed = 42)

  # Invalid top_n
  expect_error(plot(res, top_n = 0), "`top_n` must be a positive integer scalar or Inf")
  expect_error(plot(res, top_n = -1), "`top_n` must be a positive integer scalar or Inf")
  expect_error(plot(res, top_n = 1.5), "`top_n` must be a positive integer scalar or Inf")
  expect_error(plot(res, top_n = NA), "`top_n` must be a positive integer scalar or Inf")
  expect_error(plot(res, top_n = c(1, 2)), "`top_n` must be a positive integer scalar or Inf")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Valid top_n truncation
  expect_silent(plot(res, type = "rank_stability", top_n = 1L))
  expect_silent(plot(res, type = "rank_stability", top_n = 2L))
  expect_silent(plot(res, type = "rank_stability", top_n = 100L))
  expect_silent(plot(res, type = "rank_stability", top_n = Inf))
})

test_that("constraint_stability plot with partial and no constraints", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 15), rep(0, 15)),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE)
  )
  cands <- list("M1" = "Q1", "M2" = c("Q1", "Q2"))

  # No constraints -> error
  res_no_con <- candidate_stability_roc(dat, y, cands, folds = 3, repeats = 2, seed = 42)
  expect_error(plot(res_no_con, type = "constraint_stability"), "No sensitivity or specificity constraint was specified")

  # Sensitivity constraint only
  res_sens <- candidate_stability_roc(dat, y, cands, sensitivity_min = 0.6, folds = 3, repeats = 2, seed = 42)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(res_sens, type = "constraint_stability"))

  # Specificity constraint only
  res_spec <- candidate_stability_roc(dat, y, cands, specificity_min = 0.6, folds = 3, repeats = 2, seed = 42)
  expect_silent(plot(res_spec, type = "constraint_stability"))
})

test_that("plot works for Mode 2 combinatorial screening and edge cases", {
  set.seed(42)
  dat <- data.frame(
    y  = c(rep(1, 20), rep(0, 20)),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  # Mode 2 screening
  res_screen <- candidate_stability_roc(
    data         = dat,
    outcome      = y,
    items        = Q1:Q3,
    model_sizes  = 1:2,
    screen_top_n = 4L,
    resampling   = "repeated_cv",
    folds        = 3,
    repeats      = 2,
    seed         = 42
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot(res_screen, type = "rank_stability"))
  expect_silent(plot(res_screen, type = "performance"))
  expect_silent(plot(res_screen, type = "selection_frequency"))

  # Single candidate model edge case
  res_single <- candidate_stability_roc(
    data           = dat,
    outcome        = y,
    candidate_sets = list("Single" = "Q1"),
    resampling     = "repeated_cv",
    folds          = 3,
    repeats        = 2,
    seed           = 42
  )
  expect_silent(plot(res_single, type = "rank_stability"))
  expect_silent(plot(res_single, type = "performance"))
  expect_silent(plot(res_single, type = "selection_frequency"))
})
