test_that("nested_sum_roc produces identical results in serial and parallel modes", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    q1 = sample(0:2, 60, replace = TRUE),
    q2 = sample(0:2, 60, replace = TRUE),
    q3 = sample(0:2, 60, replace = TRUE),
    q4 = sample(0:2, 60, replace = TRUE)
  )

  res_serial <- nested_sum_roc(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2", "q3", "q4"),
    max_items     = 2,
    outer_k       = 3,
    inner_k       = 2,
    outer_repeats = 2,
    seed          = 42,
    engine        = "R",
    parallel      = FALSE,
    progress      = FALSE,
    verbose       = FALSE
  )

  res_parallel <- nested_sum_roc(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2", "q3", "q4"),
    max_items     = 2,
    outer_k       = 3,
    inner_k       = 2,
    outer_repeats = 2,
    seed          = 42,
    engine        = "R",
    parallel      = TRUE,
    n_workers     = 2,
    progress      = FALSE,
    verbose       = FALSE
  )

  # Check summary statistics
  expect_equal(res_serial$summary$auc, res_parallel$summary$auc, tolerance = 1e-12)
  expect_equal(res_serial$summary$cutoff, res_parallel$summary$cutoff)
  expect_equal(res_serial$summary$sensitivity, res_parallel$summary$sensitivity, tolerance = 1e-12)
  expect_equal(res_serial$summary$specificity, res_parallel$summary$specificity, tolerance = 1e-12)
  expect_equal(res_serial$summary$accuracy, res_parallel$summary$accuracy, tolerance = 1e-12)
  expect_equal(res_serial$summary$youden, res_parallel$summary$youden, tolerance = 1e-12)

  # Check model selections and frequencies
  expect_identical(res_serial$selected_models, res_parallel$selected_models)
  expect_identical(res_serial$selected_model_frequency, res_parallel$selected_model_frequency)

  # Check predictions
  expect_equal(res_serial$outer_predictions$score, res_parallel$outer_predictions$score)
  expect_equal(res_serial$outer_predictions$predicted, res_parallel$outer_predictions$predicted)
  expect_equal(res_serial$outer_predictions$true_outcome, res_parallel$outer_predictions$true_outcome)
})

test_that("nested_sum_roc auto-detects workers when n_workers = NULL and parallel = TRUE", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    q1 = sample(0:2, 40, replace = TRUE),
    q2 = sample(0:2, 40, replace = TRUE)
  )

  res_auto <- nested_sum_roc(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2"),
    max_items     = 2,
    outer_k       = 2,
    inner_k       = 2,
    outer_repeats = 1,
    seed          = 42,
    engine        = "R",
    parallel      = TRUE,
    n_workers     = NULL,
    progress      = FALSE,
    verbose       = FALSE
  )

  expect_s3_class(res_auto, "ncvroc_result")
  expect_equal(nrow(res_auto$summary), 2)
})

test_that("ncvroc() works with parallel = TRUE and n_workers = 2", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    q1 = sample(0:2, 60, replace = TRUE),
    q2 = sample(0:2, 60, replace = TRUE),
    q3 = sample(0:2, 60, replace = TRUE)
  )

  res_serial <- ncvroc(
    data          = d,
    outcome       = y,
    items         = q1:q3,
    max_items     = 2,
    mode          = "quick",
    outer_k       = 3,
    inner_k       = 2,
    outer_repeats = 1,
    seed          = 42,
    engine        = "R",
    parallel      = FALSE,
    final_search  = TRUE,
    progress      = FALSE,
    verbose       = FALSE
  )

  res_parallel <- ncvroc(
    data          = d,
    outcome       = y,
    items         = q1:q3,
    max_items     = 2,
    mode          = "quick",
    outer_k       = 3,
    inner_k       = 2,
    outer_repeats = 1,
    seed          = 42,
    engine        = "R",
    parallel      = TRUE,
    n_workers     = 2,
    final_search  = TRUE,
    progress      = FALSE,
    verbose       = FALSE
  )

  expect_s3_class(res_parallel, "ncvroc_analysis")
  expect_equal(res_serial$nested_cv_summary$mean_auc, res_parallel$nested_cv_summary$mean_auc, tolerance = 1e-12)
  expect_identical(res_serial$final_model$items, res_parallel$final_model$items)
  expect_equal(res_serial$final_model$auc, res_parallel$final_model$auc, tolerance = 1e-12)
  expect_equal(res_serial$final_model$sensitivity_lower, res_parallel$final_model$sensitivity_lower, tolerance = 1e-12)
})

test_that("ncvroc_config and run_ncvroc handle parallel settings properly", {
  cfg <- ncvroc_config(
    outcome    = "y",
    items      = c("q1", "q2"),
    max_items  = 2,
    mode       = "quick",
    outer_k    = 2,
    inner_k    = 2,
    parallel   = TRUE,
    n_workers  = 2
  )

  expect_true(cfg$parallel)
  expect_equal(cfg$n_workers, 2)

  out <- capture.output(print(cfg))
  expect_true(any(grepl("Parallel:\\s+TRUE", out)))

  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    q1 = sample(0:2, 40, replace = TRUE),
    q2 = sample(0:2, 40, replace = TRUE)
  )

  res <- run_ncvroc(d, c("q1", "q2"), cfg, seed = 42, progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  expect_equal(nrow(res$summary), 2 * cfg$outer_repeats)
})

test_that("nested_sum_roc gracefully handles edge cases in parallel arguments", {
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    q1 = sample(0:2, 40, replace = TRUE),
    q2 = sample(0:2, 40, replace = TRUE)
  )

  # Invalid parallel type
  expect_error(nested_sum_roc(d, "y", c("q1", "q2"), parallel = "yes"), "`parallel` must be TRUE or FALSE")
  expect_error(nested_sum_roc(d, "y", c("q1", "q2"), parallel = NA), "`parallel` must be TRUE or FALSE")

  # Invalid n_workers
  expect_error(nested_sum_roc(d, "y", c("q1", "q2"), parallel = TRUE, n_workers = -2), "`n_workers` must be a positive integer")
  expect_error(nested_sum_roc(d, "y", c("q1", "q2"), parallel = TRUE, n_workers = 2.5), "`n_workers` must be a positive integer")

  # n_workers > n_folds: capped and succeeds without error
  res_capped <- nested_sum_roc(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2"),
    outer_k       = 2,
    inner_k       = 2,
    outer_repeats = 1,
    seed          = 42,
    parallel      = TRUE,
    n_workers     = 50,
    progress      = FALSE,
    verbose       = FALSE
  )
  expect_s3_class(res_capped, "ncvroc_result")
})

test_that("PSOCK cluster workers have correct package environment and namespace objects", {
  cl <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Sync .libPaths and load NCVROC on workers
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, {
    .libPaths(lib_paths)
    if (requireNamespace("NCVROC", quietly = TRUE)) {
      try(library(NCVROC), silent = TRUE)
    }
    NULL
  })

  # Export explicit required namespace symbols
  ns <- asNamespace("NCVROC")
  available_symbols <- intersect(NCVROC:::.OUTER_WORKER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
  parallel::clusterExport(cl, varlist = available_symbols, envir = ns)

  # Check that internal helpers exist on workers
  helpers_exist <- parallel::parSapply(cl, 1:2, function(i) {
    exists(".evaluate_single_outer_fold") &&
      exists(".streaming_top_n_exhaustive") &&
      exists("validate_inputs")
  })
  expect_true(all(helpers_exist))

  # Check that workers can find and inspect NCVROC package
  worker_checks <- parallel::parLapply(cl, 1:2, function(i) {
    list(
      loaded = "NCVROC" %in% loadedNamespaces() || "package:NCVROC" %in% search(),
      pkg_path = tryCatch(find.package("NCVROC"), error = function(e) NA_character_),
      version = tryCatch(as.character(packageVersion("NCVROC")), error = function(e) NA_character_)
    )
  })

  for (chk in worker_checks) {
    if (!is.na(chk$version)) {
      expect_true(grepl("^0\\.", chk$version))
    }
  }

  # Verify Rcpp compiled function execution on workers
  rcpp_exec_results <- parallel::parLapply(cl, 1:2, function(i) {
    d_toy <- data.frame(
      y  = c(1L, 1L, 1L, 0L, 0L, 0L),
      q1 = c(2L, 2L, 1L, 0L, 0L, 1L),
      q2 = c(1L, 2L, 2L, 0L, 1L, 0L)
    )
    res_rcpp <- NCVROC::exhaustive_sum_roc(
      data      = d_toy,
      outcome   = "y",
      items     = c("q1", "q2"),
      max_items = 2,
      engine    = "Rcpp",
      progress  = FALSE
    )
    list(
      nrow = nrow(res_rcpp),
      max_auc = max(res_rcpp$auc)
    )
  })

  expect_equal(rcpp_exec_results[[1]]$nrow, 3)
  expect_gt(rcpp_exec_results[[1]]$max_auc, 0.5)
  expect_equal(rcpp_exec_results[[2]]$nrow, 3)
  expect_gt(rcpp_exec_results[[2]]$max_auc, 0.5)
})

test_that("_R_CHECK_LIMIT_CORES_ helper behaves robustly under all inputs", {
  get_max <- NCVROC:::.get_max_workers
  resolve <- NCVROC:::.resolve_n_workers

  # Backup original env var
  orig_val <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit({
    if (is.na(orig_val)) {
      Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    } else {
      Sys.setenv("_R_CHECK_LIMIT_CORES_" = orig_val)
    }
  }, add = TRUE)

  # 1. Unset / empty
  Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
  max_unconstrained <- get_max()
  expect_gte(max_unconstrained, 1L)

  # 2. "true" / "TRUE" / "1" / "warn" -> max 2L
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "true")
  expect_lte(get_max(), 2L)
  expect_gte(get_max(), 1L)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  expect_lte(get_max(), 2L)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "warn")
  expect_lte(get_max(), 2L)

  # 3. Numeric string "1" -> 1L
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "1")
  expect_equal(get_max(), 1L)

  # 4. Invalid strings -> do not crash, fallback gracefully
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "not_a_number_or_boolean")
  expect_gte(get_max(), 1L)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "")
  expect_equal(get_max(), max_unconstrained)

  # Test resolve_n_workers capping
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "true")
  # When parallel = FALSE -> always 1
  expect_equal(resolve(parallel = FALSE, n_workers = 4, n_folds = 5), 1L)
  # When parallel = TRUE with CRAN limit -> capped at 2
  expect_lte(resolve(parallel = TRUE, n_workers = 4, n_folds = 5), 2L)
  # When n_folds < max_cores -> capped at n_folds
  expect_equal(resolve(parallel = TRUE, n_workers = 4, n_folds = 1), 1L)
})
