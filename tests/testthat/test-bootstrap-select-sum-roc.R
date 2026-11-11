test_that("bootstrap_select_sum_roc statistical oracle test", {
  set.seed(42)
  # Generate small reproducible dataset
  n <- 30
  dat <- data.frame(
    I1 = rbinom(n, 1, 0.6),
    I2 = rbinom(n, 1, 0.5),
    I3 = rbinom(n, 1, 0.4),
    I4 = rbinom(n, 1, 0.3),
    y  = sample(rep(c(0, 1), each = 15))
  )

  # Run bootstrap_select_sum_roc with model_sizes = 1:2, reps = 20
  res <- bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2", "I3", "I4"),
    model_sizes      = 1:2,
    bootstrap_reps   = 20L,
    resampling       = "stratified",
    selection_metric = "auc",
    cutoff_method    = "youden",
    engine           = "Rcpp",
    parallel         = "none",
    seed             = 12345L,
    progress         = FALSE
  )

  expect_s3_class(res, "bootstrap_select_sum_roc_result")
  expect_equal(res$bootstrap_summary$n_reps_requested, 20L)
  expect_equal(res$bootstrap_summary$n_reps_valid, 20L)
  expect_equal(res$bootstrap_summary$n_reps_failed, 0L)

  # Check mathematical identity: Corrected = Apparent - Mean(Optimism)
  app <- res$apparent_performance
  opt <- res$bootstrap_summary$mean_optimism
  cor <- res$corrected_performance

  expect_equal(cor$auc, app$auc - opt["auc"], tolerance = 1e-7)
  expect_equal(cor$sensitivity, app$sensitivity - opt["sensitivity"], tolerance = 1e-7)
  expect_equal(cor$specificity, app$specificity - opt["specificity"], tolerance = 1e-7)
  expect_equal(cor$youden, app$youden - opt["youden"], tolerance = 1e-7)

  # Check stability summaries
  expect_true(res$selection_stability$exact_match_rate >= 0 && res$selection_stability$exact_match_rate <= 1)
  expect_true(res$selection_stability$jaccard_mean >= 0 && res$selection_stability$jaccard_mean <= 1)
  expect_equal(nrow(res$item_selection_frequency), 4L)
})

test_that("bootstrap_select_sum_roc reruns full selection per replicate", {
  # Construct dataset where I1 is best overall, but I2/I3 are best in subsets
  dat <- data.frame(
    I1 = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0),
    I2 = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 0),
    I3 = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    y  = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )

  res <- bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2", "I3"),
    model_sizes      = 1,
    bootstrap_reps   = 50L,
    resampling       = "ordinary",
    selection_metric = "auc",
    cutoff_method    = "youden",
    seed             = 999L,
    progress         = FALSE
  )

  # More than 1 distinct model should be selected across the 50 replicates
  unique_selected <- unique(res$bootstrap_results$selected_items)
  expect_gt(length(unique_selected), 1L)
})

test_that("bootstrap_select_sum_roc handles clinical constraints and record_failure vs fallback_youden", {
  dat <- data.frame(
    I1 = c(1, 1, 0, 0),
    I2 = c(0, 1, 1, 0),
    y  = c(1, 1, 0, 0)
  )

  # With strict constraints that might fail (e.g. sens >= 0.99, spec >= 0.99)
  # Test record_failure
  res_fail <- bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    cutoff_method    = "clinical_constraint",
    sensitivity_min  = 0.99,
    specificity_min  = 0.99,
    no_feasible      = "record_failure",
    seed             = 42L,
    progress         = FALSE
  )

  expect_s3_class(res_fail, "bootstrap_select_sum_roc_result")

  # Test fallback_youden
  res_fb <- bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    cutoff_method    = "clinical_constraint",
    sensitivity_min  = 0.99,
    specificity_min  = 0.99,
    no_feasible      = "fallback_youden",
    seed             = 42L,
    progress         = FALSE
  )

  expect_s3_class(res_fb, "bootstrap_select_sum_roc_result")
  expect_equal(res_fb$bootstrap_summary$n_reps_valid, 10L)
})

test_that("bootstrap_select_sum_roc preserves caller RNG state when seed is provided", {
  dat <- data.frame(
    I1 = rbinom(20, 1, 0.5),
    I2 = rbinom(20, 1, 0.5),
    y  = rep(c(0, 1), each = 10)
  )

  state_before <- get(".Random.seed", envir = .GlobalEnv)

  res <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    seed             = 12345L,
    cutoff_method    = "youden",
    progress         = FALSE
  ))

  state_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(state_before, state_after)
})

test_that("bootstrap_select_sum_roc gives identical results on repeated seeded runs", {
  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 1, 0, 0),
    I2 = c(1, 1, 0, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  run1 <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1:2,
    bootstrap_reps   = 15L,
    cutoff_method    = "youden",
    seed             = 42L,
    progress         = FALSE
  ))

  run2 <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1:2,
    bootstrap_reps   = 15L,
    cutoff_method    = "youden",
    seed             = 42L,
    progress         = FALSE
  ))

  expect_equal(run1$apparent_performance, run2$apparent_performance)
  expect_equal(run1$corrected_performance, run2$corrected_performance)
  expect_equal(run1$bootstrap_results, run2$bootstrap_results)
})

test_that("bootstrap_select_sum_roc result_file save and resume work correctly", {
  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 1, 0, 0),
    I2 = c(1, 1, 0, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  res_saved <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    cutoff_method    = "youden",
    seed             = 101L,
    result_file      = tmp_file,
    progress         = FALSE
  ))

  expect_true(file.exists(tmp_file))

  # Loading cached result
  res_loaded <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    cutoff_method    = "youden",
    seed             = 101L,
    result_file      = tmp_file,
    force_recompute  = FALSE,
    progress         = FALSE
  ))

  expect_equal(res_saved$apparent_performance, res_loaded$apparent_performance)
  expect_equal(res_saved$corrected_performance, res_loaded$corrected_performance)
})

test_that("bootstrap_select_sum_roc print, summary, and plot methods work without error", {
  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 1, 0, 0),
    I2 = c(1, 1, 0, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  res <- suppressWarnings(bootstrap_select_sum_roc(
    data             = dat,
    outcome          = "y",
    items            = c("I1", "I2"),
    model_sizes      = 1,
    bootstrap_reps   = 10L,
    cutoff_method    = "youden",
    seed             = 101L,
    progress         = FALSE
  ))

  expect_output(print(res))
  s <- summary(res)
  expect_s3_class(s, "summary.bootstrap_select_sum_roc_result")
  expect_output(print(s))

  # Test plotting to null device
  pdf_null <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_null), add = TRUE)
  grDevices::pdf(pdf_null)
  expect_silent(plot(res, ask = FALSE))
  grDevices::dev.off()
})

test_that("bootstrap_select_sum_roc engine and thread parity", {
  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 1, 0, 0),
    I2 = c(1, 1, 0, 0, 1, 0, 1, 0),
    I3 = c(0, 1, 0, 1, 0, 1, 1, 0),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  # Engine parity
  r_rcpp <- suppressWarnings(bootstrap_select_sum_roc(
    data = dat, outcome = "y", items = c("I1", "I2", "I3"),
    model_sizes = 1:2, bootstrap_reps = 10L, cutoff_method = "youden",
    engine = "Rcpp", parallel = "none", seed = 42L, progress = FALSE
  ))

  r_r <- suppressWarnings(bootstrap_select_sum_roc(
    data = dat, outcome = "y", items = c("I1", "I2", "I3"),
    model_sizes = 1:2, bootstrap_reps = 10L, cutoff_method = "youden",
    engine = "R", parallel = "none", seed = 42L, progress = FALSE
  ))

  expect_equal(r_rcpp$apparent_performance, r_r$apparent_performance)
  expect_equal(r_rcpp$bootstrap_results$selected_items, r_r$bootstrap_results$selected_items)
  expect_equal(r_rcpp$bootstrap_results$test_auc, r_r$bootstrap_results$test_auc)
  expect_equal(r_rcpp$bootstrap_summary$mean_optimism, r_r$bootstrap_summary$mean_optimism)

  # Thread parity
  r_threads <- suppressWarnings(bootstrap_select_sum_roc(
    data = dat, outcome = "y", items = c("I1", "I2", "I3"),
    model_sizes = 1:2, bootstrap_reps = 10L, cutoff_method = "youden",
    parallel = "threads", n_workers = 2L, seed = 42L, progress = FALSE
  ))

  expect_equal(r_rcpp$bootstrap_results$selected_items, r_threads$bootstrap_results$selected_items)
  expect_equal(r_rcpp$bootstrap_results$test_auc, r_threads$bootstrap_results$test_auc)
  expect_equal(r_rcpp$bootstrap_summary$mean_optimism, r_threads$bootstrap_summary$mean_optimism)
})
test_that("Finding 1: clinical constraint cutoff search finds feasible cutoff when Youden max is infeasible", {
  # Construct data where score values yield multiple cutoffs:
  # Scores: 0, 1, 2
  # Max Youden cutoff is at cutoff 1 with sens=1.0, spec=0.5 (Youden=0.5)
  # But constraint requires specificity >= 0.8, which is only satisfied by cutoff 2 (sens=0.8, spec=0.9, Youden=0.7)
  # C++ evaluate_combos_cpp must pick cutoff 2, not fail or pick cutoff 1
  scores_pos <- c(1, 1, 2, 2, 2, 2, 2, 2)
  scores_neg <- c(0, 0, 0, 0, 0, 0, 1, 1)
  y <- c(rep(1L, length(scores_pos)), rep(0L, length(scores_neg)))
  x <- matrix(c(scores_pos, scores_neg), ncol = 1)
  colnames(x) <- "I1"
  dat <- as.data.frame(x)
  dat$y <- y

  res_rcpp <- bootstrap_select_sum_roc(
    data            = dat,
    outcome         = "y",
    items           = "I1",
    model_sizes     = 1,
    bootstrap_reps  = 10L,
    cutoff_method   = "clinical_constraint",
    sensitivity_min = 0.70,
    specificity_min = 0.80,
    engine          = "Rcpp",
    seed            = 42L,
    progress        = FALSE
  )

  res_r <- bootstrap_select_sum_roc(
    data            = dat,
    outcome         = "y",
    items           = "I1",
    model_sizes     = 1,
    bootstrap_reps  = 10L,
    cutoff_method   = "clinical_constraint",
    sensitivity_min = 0.70,
    specificity_min = 0.80,
    engine          = "R",
    seed            = 42L,
    progress        = FALSE
  )

  expect_equal(res_rcpp$final_selected_model$cutoff, 2)
  expect_equal(res_r$final_selected_model$cutoff, 2)
  expect_equal(res_rcpp$final_selected_model, res_r$final_selected_model)
  expect_equal(res_rcpp$apparent_performance, res_r$apparent_performance)
})

test_that("Finding 2: checkpoint compatibility checks data digest and engine settings", {
  dat1 <- data.frame(
    I1 = c(1, 0, 1, 0),
    y  = c(1, 1, 0, 0)
  )
  dat2 <- data.frame(
    I1 = c(1, 1, 0, 0),
    y  = c(1, 1, 0, 0)
  )

  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  # Run on dat1 and save
  res1 <- suppressWarnings(bootstrap_select_sum_roc(
    data        = dat1,
    outcome     = "y",
    items       = "I1",
    model_sizes = 1,
    bootstrap_reps = 10L,
    cutoff_method = "youden",
    engine      = "Rcpp",
    seed        = 100L,
    result_file = tmp_file,
    progress    = FALSE
  ))

  # Calling with dat2 should NOT hit cache because data digest differs
  res2 <- suppressWarnings(bootstrap_select_sum_roc(
    data        = dat2,
    outcome     = "y",
    items       = "I1",
    model_sizes = 1,
    bootstrap_reps = 10L,
    cutoff_method = "youden",
    engine      = "Rcpp",
    seed        = 100L,
    result_file = tmp_file,
    force_recompute = FALSE,
    progress    = FALSE
  ))

  # dat2 has different apparent performance than dat1
  expect_false(identical(res1$apparent_performance, res2$apparent_performance))
})

test_that("Finding 3: seed = NULL advances caller RNG stream and does not restore state", {
  set.seed(1234L)
  runif(1)
  state_before <- get(".Random.seed", envir = .GlobalEnv)

  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 0, 0, 0)
  )

  res_null_seed <- suppressWarnings(bootstrap_select_sum_roc(
    data        = dat,
    outcome     = "y",
    items       = "I1",
    model_sizes = 1,
    bootstrap_reps = 5L,
    cutoff_method = "youden",
    seed        = NULL,
    progress    = FALSE
  ))

  state_after <- get(".Random.seed", envir = .GlobalEnv)
  # RNG state MUST have advanced when seed is NULL
  expect_false(identical(state_before, state_after))

  # Seeded call MUST restore state
  state_pre_seeded <- state_after
  res_seeded <- suppressWarnings(bootstrap_select_sum_roc(
    data        = dat,
    outcome     = "y",
    items       = "I1",
    model_sizes = 1,
    bootstrap_reps = 5L,
    cutoff_method = "youden",
    seed        = 888L,
    progress    = FALSE
  ))
  state_post_seeded <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(state_pre_seeded, state_post_seeded)
})

test_that("Reviewer finding: checkpoint compatibility rejects differing pkg_version", {
  key_base <- list(
    data_digest = "abc123hash",
    settings = list(
      outcome = "y", items = "I1", model_sizes = 1, bootstrap_reps = 10L,
      resampling = "stratified", selection_metric = "auc", cutoff_method = "youden",
      sensitivity_min = NULL, specificity_min = NULL, prefer_fewer_items = TRUE,
      no_feasible = "record_failure", positive_label = 1, negative_label = 0,
      engine = "Rcpp", parallel = "none", n_workers = 1L, seed = 123L
    ),
    pkg_version = "0.24.0"
  )

  key_diff_version <- key_base
  key_diff_version$pkg_version <- "0.23.2"

  # Direct internal compatibility check
  expect_false(NCVROC:::.is_checkpoint_compatible(key_base, key_diff_version))
  expect_false(NCVROC:::.is_checkpoint_compatible(key_diff_version, key_base))
  expect_true(NCVROC:::.is_checkpoint_compatible(key_base, key_base))

  # End-to-end result_file check: saved result with modified pkg_version in file is not reused
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)

  dat <- data.frame(
    I1 = c(1, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 0, 0, 0)
  )

  res1 <- suppressWarnings(bootstrap_select_sum_roc(
    data = dat, outcome = "y", items = "I1", model_sizes = 1,
    bootstrap_reps = 5L, cutoff_method = "youden", seed = 100L,
    result_file = tmp_file, progress = FALSE
  ))

  # Modify the pkg_version inside the saved file
  raw_obj <- readRDS(tmp_file)
  raw_obj$compat_key$pkg_version <- "0.23.0"
  saveRDS(raw_obj, tmp_file)

  # When called with force_recompute = FALSE, it should NOT reuse the old version's result
  res2 <- suppressWarnings(bootstrap_select_sum_roc(
    data = dat, outcome = "y", items = "I1", model_sizes = 1,
    bootstrap_reps = 5L, cutoff_method = "youden", seed = 100L,
    result_file = tmp_file, force_recompute = FALSE, progress = FALSE
  ))

  # After recomputing, file will have current pkg_version
  new_obj <- readRDS(tmp_file)
  expect_identical(new_obj$compat_key$pkg_version, as.character(utils::packageVersion("NCVROC")))
})
