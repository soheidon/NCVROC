# test-hybrid-parallel.R — Hybrid outer PSOCK x C++ thread integration

hybrid_test_data <- function(high_ties = FALSE) {
  set.seed(1401)
  n <- 40L
  if (high_ties) {
    data.frame(
      y = rep(0:1, each = n / 2L),
      q1 = rep(c(0, 1), length.out = n),
      q2 = rep(c(0, 1), each = 2L, length.out = n),
      q3 = rep(c(1, 0), length.out = n),
      q4 = rep(1, n)
    )
  } else {
    data.frame(
      y = rep(0:1, each = n / 2L),
      q1 = sample(0:2, n, TRUE),
      q2 = sample(0:2, n, TRUE),
      q3 = sample(0:2, n, TRUE),
      q4 = sample(0:2, n, TRUE)
    )
  }
}

run_hybrid_fixture <- function(dat, cutoff_method = "youden", mode = "hybrid") {
  suppressWarnings(nested_sum_roc(
    dat, "y", paste0("q", 1:4),
    max_items = 2,
    cutoff_method = cutoff_method,
    preselect_top_n = 8,
    outer_k = 2,
    inner_k = 2,
    seed = 731,
    engine = "Rcpp",
    parallel = mode,
    n_workers = 2L,
    threads_per_worker = if (identical(mode, "hybrid")) 2L else 1L,
    progress = FALSE,
    verbose = FALSE
  ))
}

hybrid_large_test_data <- function(high_ties = FALSE) {
  set.seed(1402)
  n <- 40L
  p <- 45L
  dat <- data.frame(y = rep(0:1, each = n / 2L))
  for (j in seq_len(p)) {
    dat[[paste0("q", j)]] <- if (high_ties) {
      (seq_len(n) + j) %% 3L
    } else {
      sample(0:2, n, TRUE)
    }
  }
  dat
}

run_large_hybrid_fixture <- function(dat,
                                     cutoff_method = "youden",
                                     mode = "hybrid") {
  item_names <- grep("^q", names(dat), value = TRUE)
  cran_limited <- nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  outer_workers <- if (identical(mode, "hybrid") && cran_limited) 1L else 2L

  suppressWarnings(nested_sum_roc(
    dat, "y", item_names,
    max_items = 2,
    cutoff_method = cutoff_method,
    preselect_top_n = 12,
    outer_k = 2,
    inner_k = 2,
    seed = 932,
    engine = "Rcpp",
    parallel = mode,
    n_workers = outer_workers,
    threads_per_worker = if (identical(mode, "hybrid")) 2L else 1L,
    progress = FALSE,
    verbose = FALSE
  ))
}

test_that("parallel resolver accepts hybrid only for nested CV", {
  expect_identical(.resolve_parallel_mode("hybrid", "nested"), "hybrid")
  expect_error(
    .resolve_parallel_mode("hybrid", "exhaustive"),
    "not supported"
  )
  expect_error(
    exhaustive_sum_roc(
      hybrid_test_data(), "y", paste0("q", 1:4),
      max_items = 2, parallel = "hybrid", progress = FALSE
    ),
    "not supported"
  )
})

test_that("threads_per_worker is validated and scoped to hybrid", {
  dat <- hybrid_test_data()
  expect_error(
    nested_sum_roc(dat, "y", paste0("q", 1:4),
                   threads_per_worker = 0L, progress = FALSE, verbose = FALSE),
    "positive integer"
  )
  expect_error(
    nested_sum_roc(dat, "y", paste0("q", 1:4),
                   threads_per_worker = 2L, progress = FALSE, verbose = FALSE),
    "only exceed 1"
  )
  expect_error(
    nested_sum_roc(dat, "y", paste0("q", 1:4), engine = "R",
                   parallel = "hybrid", progress = FALSE, verbose = FALSE),
    "requires `engine = 'Rcpp'`"
  )
})

test_that("hybrid budget caps total parallelism and respects CRAN limits", {
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    } else {
      do.call(Sys.setenv, setNames(list(old), "_R_CHECK_LIMIT_CORES_"))
    }
  }, add = TRUE)
  do.call(Sys.setenv, setNames(list("2"), "_R_CHECK_LIMIT_CORES_"))

  expect_warning(
    budget <- .resolve_hybrid_budget(
      n_workers = 4L, threads_per_worker = 4L, n_folds = 5L
    ),
    "capped"
  )
  expect_lte(budget$total_parallelism, 2L)
  expect_lte(budget$n_workers * budget$threads_per_worker, budget$max_cores)
  expect_identical(budget$threads_per_worker, 1L)
})

test_that("serial, outer, and hybrid nested results are exact", {
  dat <- hybrid_large_test_data()
  item_names <- grep("^q", names(dat), value = TRUE)
  expect_gt(sum(choose(length(item_names), 1:2)), 1000)

  for (method in c("youden", "closest_topleft")) {
    serial <- run_large_hybrid_fixture(dat, method, "none")
    hybrid <- run_large_hybrid_fixture(dat, method, "hybrid")

    expect_identical(hybrid$summary, serial$summary)
    expect_identical(hybrid$selected_models, serial$selected_models)
    expect_identical(hybrid$selected_model_frequency,
                     serial$selected_model_frequency)
    expect_identical(hybrid$outer_predictions, serial$outer_predictions)

    metrics <- c("auc", "cutoff", "sensitivity", "specificity",
                 "accuracy", "youden")
    expect_identical(hybrid$summary[metrics], serial$summary[metrics])
  }
})

test_that("hybrid is exact for high ties and deterministic seeds", {
  dat <- hybrid_large_test_data(high_ties = TRUE)
  item_names <- grep("^q", names(dat), value = TRUE)
  expect_gt(sum(choose(length(item_names), 1:2)), 1000)

  serial <- run_large_hybrid_fixture(dat, "youden", "none")
  hybrid1 <- run_large_hybrid_fixture(dat, "youden", "hybrid")
  hybrid2 <- run_large_hybrid_fixture(dat, "youden", "hybrid")

  expect_identical(hybrid1$summary, serial$summary)
  expect_identical(hybrid1$selected_models, serial$selected_models)
  expect_identical(hybrid1$outer_predictions, serial$outer_predictions)
  metrics <- c("auc", "cutoff", "sensitivity", "specificity",
               "accuracy", "youden")
  expect_identical(hybrid1$summary[metrics], serial$summary[metrics])
  expect_identical(hybrid1, hybrid2)
})

test_that("config and run_ncvroc preserve hybrid settings", {
  dat <- hybrid_test_data()
  cfg <- ncvroc_config(
    "y", items = paste0("q", 1:4), max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1,
    engine = "Rcpp", parallel = "hybrid", n_workers = 2L,
    threads_per_worker = 2L
  )
  expect_identical(cfg$parallel, "hybrid")
  expect_identical(cfg$n_workers, 2L)
  expect_identical(cfg$threads_per_worker, 2L)

  result <- suppressWarnings(run_ncvroc(
    dat, paste0("q", 1:4), cfg, seed = 731,
    progress = FALSE, verbose = FALSE
  ))
  expect_s3_class(result, "ncvroc_result")
  expect_identical(result$settings$parallel, "hybrid")
  expect_identical(result$settings$threads_per_worker, 2L)
  expect_identical(result$settings$requested_outer_workers, 2L)
  expect_identical(result$settings$requested_threads_per_worker, 2L)
  expect_lte(result$settings$effective_total_parallelism,
             result$settings$effective_max_cores)
})

test_that("verbose hybrid output reports a one-worker threaded budget", {
  dat <- hybrid_test_data()
  expect_message(
    suppressWarnings(nested_sum_roc(
      dat, "y", paste0("q", 1:4),
      max_items = 2, preselect_top_n = 8,
      outer_k = 2, inner_k = 2, seed = 731,
      engine = "Rcpp", parallel = "hybrid",
      n_workers = 1L, threads_per_worker = 2L,
      progress = FALSE, verbose = TRUE
    )),
    "parallel: hybrid, 1 outer worker x 2 threads = 2 total",
    fixed = TRUE
  )
})

test_that("ncvroc uses hybrid CV and completes final threaded search", {
  dat <- hybrid_test_data()
  serial <- ncvroc(
    dat, y, q1:q4,
    max_items = 2, mode = "quick", outer_k = 2, inner_k = 2,
    outer_repeats = 1, seed = 731, engine = "Rcpp",
    parallel = "none", final_search = TRUE, final_top_n = 5,
    results_storage = "memory", progress = FALSE, verbose = FALSE
  )
  hybrid <- suppressWarnings(ncvroc(
    dat, y, q1:q4,
    max_items = 2, mode = "quick", outer_k = 2, inner_k = 2,
    outer_repeats = 1, seed = 731, engine = "Rcpp",
    parallel = "hybrid", n_workers = 2L, threads_per_worker = 2L,
    final_search = TRUE, final_top_n = 5,
    results_storage = "memory", progress = FALSE, verbose = FALSE
  ))

  expect_identical(hybrid$nested_cv_summary, serial$nested_cv_summary)
  expect_identical(hybrid$final_model, serial$final_model)
  expect_identical(hybrid$final_candidates, serial$final_candidates)
})
