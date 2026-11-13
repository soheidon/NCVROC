test_that("Native multi-replicate bootstrap matches R engine across methods, constraints, threads, and RNG", {
  set.seed(401)
  n <- 36L
  p <- 5L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X[1:4, 1] <- 0.25
  colnames(X) <- paste0("b", seq_len(p))
  df <- as.data.frame(X)
  df$y <- c(rep(0L, 18L), rep(1L, 18L))

  set.seed(999)
  seed_before <- .Random.seed

  res_r <- bootstrap_select_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    model_sizes = 1:2,
    bootstrap_reps = 16L,
    correction_methods = c("harrell", "632", "632plus"),
    cutoff_method = "youden",
    engine = "R",
    parallel = "none",
    seed = 123L,
    progress = FALSE
  )

  expect_identical(.Random.seed, seed_before)

  res_cpp_1 <- bootstrap_select_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    model_sizes = 1:2,
    bootstrap_reps = 16L,
    correction_methods = c("harrell", "632", "632plus"),
    cutoff_method = "youden",
    engine = "Rcpp",
    parallel = "none",
    seed = 123L,
    progress = FALSE
  )

  expect_identical(.Random.seed, seed_before)

  res_cpp_2 <- bootstrap_select_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    model_sizes = 1:2,
    bootstrap_reps = 16L,
    correction_methods = c("harrell", "632", "632plus"),
    cutoff_method = "youden",
    engine = "Rcpp",
    parallel = "threads",
    n_workers = 2L,
    seed = 123L,
    progress = FALSE
  )

  expect_equal(res_cpp_1$replicates, res_r$replicates, tolerance = 1e-12)
  expect_equal(res_cpp_1$summary, res_r$summary, tolerance = 1e-12)
  expect_equal(res_cpp_2$replicates, res_r$replicates, tolerance = 1e-12)
  expect_equal(res_cpp_2$summary, res_r$summary, tolerance = 1e-12)

  # Clinical constraint with fallback_youden and record_failure
  for (nf in c("fallback_youden", "record_failure")) {
    cc_r <- bootstrap_select_sum_roc(
      data = df,
      outcome = "y",
      items = colnames(X),
      model_sizes = 1:2,
      bootstrap_reps = 12L,
      cutoff_method = "clinical_constraint",
      sensitivity_min = 0.55,
      specificity_min = 0.55,
      no_feasible = nf,
      engine = "R",
      parallel = "none",
      seed = 321L,
      progress = FALSE
    )
    cc_cpp <- bootstrap_select_sum_roc(
      data = df,
      outcome = "y",
      items = colnames(X),
      model_sizes = 1:2,
      bootstrap_reps = 12L,
      cutoff_method = "clinical_constraint",
      sensitivity_min = 0.55,
      specificity_min = 0.55,
      no_feasible = nf,
      engine = "Rcpp",
      parallel = "threads",
      n_workers = 2L,
      seed = 321L,
      progress = FALSE
    )
    expect_equal(cc_cpp$replicates, cc_r$replicates, tolerance = 1e-12)
  }
})

test_that("Native multi-replicate bootstrap supports checkpoint_every wave resume", {
  set.seed(402)
  n <- 30L
  p <- 5L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("m", seq_len(p))
  df <- as.data.frame(X)
  df$y <- c(rep(0L, 15L), rep(1L, 15L))

  res_dir <- tempfile("boot_cp_")
  dir.create(res_dir, recursive = TRUE)
  on.exit(unlink(res_dir, recursive = TRUE), add = TRUE)
  res_file <- file.path(res_dir, "boot_res.rds")

  ref <- bootstrap_select_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    model_sizes = 1:2,
    bootstrap_reps = 12L,
    cutoff_method = "youden",
    checkpoint_every = 5L,
    engine = "Rcpp",
    parallel = "threads",
    n_workers = 2L,
    seed = 55L,
    progress = FALSE
  )

  full_run <- bootstrap_select_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    model_sizes = 1:2,
    bootstrap_reps = 12L,
    cutoff_method = "youden",
    checkpoint_every = 5L,
    result_file = res_file,
    engine = "Rcpp",
    parallel = "threads",
    n_workers = 2L,
    seed = 55L,
    progress = FALSE
  )
  expect_equal(full_run$replicates, ref$replicates, tolerance = 1e-12)
})
