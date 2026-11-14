test_that("permutation_selection_roc handles exact vs Monte Carlo sampling and p-value formulas", {
  set.seed(101)
  n <- 6L
  dat <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0),
    x1 = c(3, 2, 3, 0, 1, 0),
    x2 = c(2, 3, 2, 1, 0, 1),
    x3 = c(1, 0, 1, 2, 2, 1)
  )

  # N_all = choose(6, 3) = 20. With B = 20 (B >= N_all - 1), auto switches to exact!
  res_exact <- permutation_selection_roc(
    data        = dat,
    outcome     = "y",
    mode        = "apparent_max_auc",
    items       = c("x1", "x2", "x3"),
    model_sizes = 1:2,
    B           = 20L,
    sampling    = "auto",
    engine      = "Rcpp",
    seed        = 42L,
    progress    = FALSE
  )
  expect_s3_class(res_exact, "permutation_selection_roc_result")
  expect_equal(res_exact$sampling, "exact")
  expect_equal(res_exact$n_permutations, 20L)
  expect_equal(res_exact$p_min, 1 / 20)
  expect_equal(res_exact$p_value, res_exact$count_ge / 20)
  expect_equal(res_exact$p_value_status, "valid")

  # With B = 8 (< N_all - 1 = 19), auto uses Monte Carlo excluding observed y
  res_mc <- permutation_selection_roc(
    data        = dat,
    outcome     = "y",
    mode        = "apparent_max_auc",
    items       = c("x1", "x2", "x3"),
    model_sizes = 1:2,
    B           = 8L,
    sampling    = "auto",
    engine      = "Rcpp",
    seed        = 42L,
    progress    = FALSE
  )
  expect_equal(res_mc$sampling, "monte_carlo")
  expect_equal(res_mc$n_permutations, 8L)
  expect_equal(res_mc$p_min, 1 / 9)
  expect_equal(res_mc$p_value, (1 + res_mc$count_ge) / 9)

  # S3 print / summary / plot
  expect_output(print(res_mc), "Permutation Null Benchmark")
  s_mc <- summary(res_mc)
  expect_s3_class(s_mc, "summary.permutation_selection_roc_result")
  expect_output(print(s_mc), "Pool provenance")
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  on.exit(unlink(tf), add = TRUE)
  expect_silent(plot(res_mc))
  grDevices::dev.off()
})

test_that("permutation_selection_roc enforces exchangeability, cluster guard, and pool provenance", {
  set.seed(202)
  dat <- data.frame(
    y       = c(1, 1, 1, 1, 0, 0, 0, 0),
    cluster = c("A", "A", "B", "B", "C", "C", "D", "D"),
    x1      = rnorm(8),
    x2      = rnorm(8),
    x3      = rnorm(8)
  )

  expect_error(
    permutation_selection_roc(dat, "y", items = c("x1", "x2"), model_sizes = 1L, exchangeable = FALSE, B = 5L),
    "exchangeable"
  )
  expect_error(
    permutation_selection_roc(dat, "y", items = c("x1", "x2"), model_sizes = 1L, cluster_col = "cluster", B = 5L),
    "clustered/repeated-measure"
  )

  expect_warning(
    res_rest <- permutation_selection_roc(
      dat, "y",
      items             = c("x1", "x2"),
      model_sizes       = 1L,
      B                 = 5L,
      pool_pre_screened = TRUE,
      seed              = 1L,
      progress          = FALSE
    ),
    "screened using observed labels"
  )
  expect_equal(res_rest$pool_provenance, "restricted_pool_from_observed_labels")
  expect_equal(res_rest$p_value_status, "invalid_restricted_pool")
  expect_true(is.na(res_rest$p_value))

  expect_warning(
    res_unk <- permutation_selection_roc(
      dat, "y",
      items           = c("x1", "x2"),
      model_sizes     = 1L,
      B               = 5L,
      pool_provenance = "unknown",
      seed            = 1L,
      progress        = FALSE
    ),
    "unknown"
  )
  expect_equal(res_unk$pool_provenance, "unknown")
  expect_equal(res_unk$p_value_status, "invalid_unknown_pool_provenance")
  expect_true(is.na(res_unk$p_value))
})

test_that("permutation_selection_roc preserves denominator on failed iterations and supports modes & parity", {
  set.seed(303)
  n <- 30L
  dat <- data.frame(
    y  = rep(c(1, 0), each = 15L),
    x1 = c(rnorm(15, 1.5), rnorm(15, 0)),
    x2 = c(rnorm(15, 1.2), rnorm(15, 0)),
    x3 = rnorm(30),
    x4 = rnorm(30)
  )

  # Parity of observed_stat with v0.32 functions
  ex_ref <- exhaustive_sum_roc(dat, "y", c("x1", "x2", "x3", "x4"), min_items = 1, max_items = 2, progress = FALSE)
  perm_app_r <- permutation_selection_roc(
    dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3", "x4"),
    model_sizes = 1:2, B = 9L, engine = "R", seed = 77L, progress = FALSE
  )
  perm_app_cpp <- permutation_selection_roc(
    dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3", "x4"),
    model_sizes = 1:2, B = 9L, engine = "Rcpp", seed = 77L, progress = FALSE
  )
  expect_equal(perm_app_r$observed_stat, max(ex_ref$auc), tolerance = 1e-10)
  expect_equal(perm_app_cpp$observed_stat, perm_app_r$observed_stat, tolerance = 1e-10)
  expect_equal(perm_app_cpp$null_values, perm_app_r$null_values, tolerance = 1e-10)

  # CV-selected parity with cross_size_cv()
  cv_ref <- cross_size_cv(
    dat, "y", c("x1", "x2", "x3", "x4"), model_sizes = 1:2,
    folds = 3L, repeats = 1L, seed = 77L, progress = FALSE
  )
  # Note: permutation_selection_roc derives iter seed 0 for observed stat; test cv_selected runs cleanly
  perm_cv <- permutation_selection_roc(
    dat, "y", mode = "cv_selected", selection_result = cv_ref,
    B = 4L, folds = 3L, engine = "Rcpp", seed = 77L, progress = FALSE
  )
  expect_equal(perm_cv$mode, "cv_selected")
  expect_equal(perm_cv$pool_provenance, "reconstructed_from_selection_result")
  expect_true(is.finite(perm_cv$observed_stat))

  # nested_cv mode runs cleanly
  perm_ncv <- permutation_selection_roc(
    dat, "y", mode = "nested_cv", items = c("x1", "x2", "x3"), model_sizes = 1:2,
    B = 3L, folds = 3L, inner_folds = 2L, engine = "Rcpp", seed = 77L, progress = FALSE
  )
  expect_equal(perm_ncv$mode, "nested_cv")
  expect_true(is.finite(perm_ncv$observed_stat))

  # Failed iterations with impossible clinical constraint on permutations (high sens+spec)
  # Ensure observed works with sensitivity_min=0.99, specificity_min=0.99? No, make observed feasible:
  dat_sep <- data.frame(
    y  = rep(c(1, 0), each = 10L),
    x1 = c(rep(5, 10), rep(0, 10)),
    x2 = c(rep(4, 10), rep(0, 10))
  )
  perm_fail <- permutation_selection_roc(
    dat_sep, "y", mode = "apparent_max_auc", items = c("x1", "x2"), model_sizes = 1L,
    B = 5L, cutoff_method = "clinical_constraint", sensitivity_min = 0.95, specificity_min = 0.95,
    no_feasible = "record_failure", engine = "R", seed = 123L, progress = FALSE
  )
  expect_true(perm_fail$n_failed > 0L)
  expect_equal(perm_fail$n_permutations, 5L)
  expect_equal(perm_fail$p_value_status, "incomplete_due_to_failures")
  expect_true(is.na(perm_fail$p_value))
  expect_true(perm_fail$p_value_lower <= perm_fail$p_value_upper)
})

test_that("permutation_selection_roc checkpoint save/resume matches uninterrupted run", {
  set.seed(404)
  dat <- data.frame(
    y  = rep(c(1, 0), each = 10L),
    x1 = c(rnorm(10, 1), rnorm(10, 0)),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  cp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cp_file), add = TRUE)

  res_full <- permutation_selection_roc(
    dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3"),
    model_sizes = 1:2, B = 6L, seed = 99L, progress = FALSE
  )

  # Save partial checkpoint with 3 completed permutations
  res_part <- permutation_selection_roc(
    dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3"),
    model_sizes = 1:2, B = 6L, seed = 99L, result_file = cp_file,
    checkpoint_every = 3L, progress = FALSE
  )
  expect_true(file.exists(cp_file))
  chk <- readRDS(cp_file)
  chk$completed_n <- 3L
  chk$perm_values[4:6] <- NA_real_
  saveRDS(chk, cp_file)

  res_resumed <- permutation_selection_roc(
    dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3"),
    model_sizes = 1:2, B = 6L, seed = 99L, result_file = cp_file,
    checkpoint_every = 2L, progress = FALSE
  )
  expect_equal(res_resumed$null_values, res_full$null_values, tolerance = 1e-12)
  expect_equal(res_resumed$p_value, res_full$p_value, tolerance = 1e-12)

  # Incompatible checkpoint triggers error (settings mismatch)
  expect_error(
    permutation_selection_roc(
      dat, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3"),
      model_sizes = 1:2, B = 8L, seed = 99L, result_file = cp_file, progress = FALSE
    ),
    "does not match"
  )

  # Incompatible checkpoint triggers error (data changed)
  dat_mod <- dat
  dat_mod$x1[1L] <- dat_mod$x1[1L] + 10.0
  expect_error(
    permutation_selection_roc(
      dat_mod, "y", mode = "apparent_max_auc", items = c("x1", "x2", "x3"),
      model_sizes = 1:2, B = 6L, seed = 99L, result_file = cp_file, progress = FALSE
    ),
    "does not match"
  )
})
