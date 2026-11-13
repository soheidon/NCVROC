test_that("64-bit binomial counts and tail rank/unrank slices match CBCL103 scale", {
  c4 <- choose(103, 4)
  c5 <- choose(103, 5)
  c6 <- choose(103, 6)
  expect_equal(c4, 4421275)
  expect_equal(c5, 87541245)
  expect_equal(c6, 1429840335)
  total_1_to_6 <- sum(vapply(1:6, function(k) choose(103, k), numeric(1)))
  expect_equal(total_1_to_6, 1521985062)

  # Bounded slice at the very end of choose(103, 6)
  set.seed(301)
  n <- 30L
  p <- 103L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- c(rep(0L, 15L), rep(1L, 15L))
  tail_start <- c6 - 25
  res_tail <- NCVROC:::native_eval_candidate_batches_cpp(
    x = X,
    y = y,
    train_indices_list = list(seq_len(n) - 1L),
    task_fold_idx = 0L,
    task_repeat_id = 1L,
    task_outer_fold_id = 1L,
    task_model_size = 6L,
    task_local_rank_start_0 = tail_start,
    task_local_rank_end_0 = c6,
    task_level_start_0 = 0,
    cutoff_method = "youden",
    rank_by = "auc",
    preselect_top_n = 5L,
    prefer_fewer_items = TRUE,
    num_threads = 2L
  )
  expect_true(isTRUE(res_tail[[1L]]$completed))
  expect_equal(res_tail[[1L]]$rank_end - res_tail[[1L]]$rank_start + 1, 25)
  expect_equal(nrow(res_tail[[1L]]$candidates), 5L)

  # The very last combination rank (c6 - 1) is the final rank of choose(103, 6)
  last_slice <- NCVROC:::native_eval_candidate_batches_cpp(
    x = X,
    y = y,
    train_indices_list = list(seq_len(n) - 1L),
    task_fold_idx = 0L,
    task_repeat_id = 1L,
    task_outer_fold_id = 1L,
    task_model_size = 6L,
    task_local_rank_start_0 = c6 - 1,
    task_local_rank_end_0 = c6,
    task_level_start_0 = 0,
    cutoff_method = "youden",
    rank_by = "auc",
    preselect_top_n = 1L,
    prefer_fewer_items = TRUE,
    num_threads = 1L
  )
  expect_true(isTRUE(last_slice[[1L]]$completed))
  expect_equal(last_slice[[1L]]$candidates$local_rank, c6 - 1)
  expect_equal(last_slice[[1L]]$rank_start, c6 - 1)
  expect_equal(last_slice[[1L]]$rank_end, c6 - 1)
})

test_that("Legacy single-pass guards retain 2,000,000 limit while native_streaming accepts >2,000,000 config", {
  set.seed(302)
  # p = 35, max_items = 6 => choose(35, 6) = 1,623,160; sum(1:6) = 2,036,935 (> 2M)
  X <- matrix(rnorm(40 * 35), nrow = 40, ncol = 35)
  colnames(X) <- paste0("x", seq_len(35))
  df <- as.data.frame(X)
  df$y <- rep(0:1, each = 20L)

  expect_error(
    nested_sum_roc(
      data = df,
      outcome = "y",
      items = colnames(X),
      max_items = 6L,
      outer_k = 2L,
      inner_k = 2L,
      parallel = "native_outer_fold",
      engine = "Rcpp",
      verbose = FALSE
    ),
    "2,000,000|limit"
  )
  expect_error(
    nested_sum_roc(
      data = df,
      outcome = "y",
      items = colnames(X),
      max_items = 6L,
      outer_k = 2L,
      inner_k = 2L,
      parallel = "native_tbb",
      engine = "Rcpp",
      verbose = FALSE
    ),
    "2,000,000|limit"
  )

  cfg <- ncvroc_config(
    outcome = "y",
    max_items = 6L,
    parallel = "native_streaming",
    engine = "Rcpp"
  )
  expect_equal(cfg$parallel, "native_streaming")
})

test_that("parallel = 'native_streaming' matches R serial and native_outer_fold across edge cases", {
  set.seed(303)
  n <- 36L
  p <- 7L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X[1:4, 1] <- 0.5
  X[5:8, 2] <- -0.25
  colnames(X) <- paste0("item_", seq_len(p))
  df <- as.data.frame(X)
  df$y <- c(rep(0L, 18L), rep(1L, 18L))

  res_r <- nested_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    preselect_top_n = 7L,
    cutoff_method = "youden",
    parallel = "none",
    engine = "R",
    seed = 42L,
    progress = FALSE,
    verbose = FALSE
  )

  res_scope_b <- nested_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    preselect_top_n = 7L,
    cutoff_method = "youden",
    parallel = "native_outer_fold",
    engine = "Rcpp",
    n_workers = 2L,
    seed = 42L,
    progress = FALSE,
    verbose = FALSE
  )

  old_opt <- getOption("ncvroc.checkpoint_batch_size")
  on.exit(options(ncvroc.checkpoint_batch_size = old_opt), add = TRUE)

  options(ncvroc.checkpoint_batch_size = 1L)
  res_stream_b1 <- nested_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    preselect_top_n = 7L,
    cutoff_method = "youden",
    parallel = "native_streaming",
    engine = "Rcpp",
    n_workers = 1L,
    seed = 42L,
    progress = FALSE,
    verbose = FALSE
  )

  options(ncvroc.checkpoint_batch_size = 11L)
  res_stream_uneven <- nested_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    preselect_top_n = 7L,
    cutoff_method = "youden",
    parallel = "native_streaming",
    engine = "Rcpp",
    n_workers = 2L,
    seed = 42L,
    progress = FALSE,
    verbose = FALSE
  )

  expect_equal(res_stream_b1$outer_results$items, res_r$outer_results$items)
  expect_equal(res_stream_b1$outer_results$outer_auc, res_r$outer_results$outer_auc, tolerance = 1e-12)
  expect_equal(res_stream_b1$outer_results$inner_cv_score, res_r$outer_results$inner_cv_score, tolerance = 1e-12)
  expect_equal(res_stream_b1$pooled_auc, res_r$pooled_auc, tolerance = 1e-12)
  expect_equal(res_stream_b1$mean_outer_auc, res_r$mean_outer_auc, tolerance = 1e-12)

  for (i in seq_along(res_stream_uneven$outer_results)) {
    expect_equal(res_stream_uneven$outer_results[[i]]$selected_items, res_scope_b$outer_results[[i]]$selected_items)
    expect_equal(res_stream_uneven$outer_results[[i]]$outer_auc, res_scope_b$outer_results[[i]]$outer_auc, tolerance = 1e-12)
    expect_equal(res_stream_uneven$outer_results[[i]]$inner_cv_score, res_scope_b$outer_results[[i]]$inner_cv_score, tolerance = 1e-12)
  }
  expect_equal(res_stream_uneven$pooled_auc, res_scope_b$pooled_auc, tolerance = 1e-12)
  inner_diag <- attr(res_stream_uneven$outer_results[[1L]], "ncvroc_inner_diag")
  expect_true(is.list(inner_diag))
  expect_equal(inner_diag$n_candidates_evaluated, 7L)

  # Also test via run_ncvroc() and ncvroc() wrappers
  cfg <- ncvroc_config(
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    preselect_top_n = 7L,
    parallel = "native_streaming",
    engine = "Rcpp",
    n_workers = 2L
  )
  res_cfg <- run_ncvroc(data = df, items = colnames(X), config = cfg, seed = 42L, progress = FALSE, verbose = FALSE)
  expect_equal(res_cfg$pooled_auc, res_r$pooled_auc, tolerance = 1e-12)

  res_api <- ncvroc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 3L,
    outer_repeats = 1L,
    inner_repeats = 1L,
    preselect_top_n = 7L,
    final_search = FALSE,
    parallel = "native_streaming",
    engine = "Rcpp",
    n_workers = 2L,
    seed = 42L,
    progress = FALSE,
    verbose = FALSE
  )
  expect_equal(res_api$nested_cv$pooled_auc, res_r$pooled_auc, tolerance = 1e-12)
})

test_that("parallel = 'native_streaming' supports checkpoint/resume across all 3 granularities and cancel flag", {
  set.seed(304)
  n <- 30L
  p <- 7L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("v", seq_len(p))
  df <- as.data.frame(X)
  df$y <- c(rep(0L, 15L), rep(1L, 15L))

  old_opt <- getOption("ncvroc.checkpoint_batch_size")
  on.exit(options(ncvroc.checkpoint_batch_size = old_opt), add = TRUE)
  options(ncvroc.checkpoint_batch_size = 7L)

  ref <- nested_sum_roc(
    data = df,
    outcome = "y",
    items = colnames(X),
    min_items = 1L,
    max_items = 3L,
    outer_k = 3L,
    inner_k = 2L,
    preselect_top_n = 5L,
    parallel = "native_streaming",
    engine = "Rcpp",
    n_workers = 2L,
    seed = 77L,
    progress = FALSE,
    verbose = FALSE
  )

  for (gran in c("outer_fold", "model_size", "candidate_batch")) {
    cp_dir <- tempfile(paste0("cp_stream_", gran, "_"))
    dir.create(cp_dir, recursive = TRUE)
    on.exit(unlink(cp_dir, recursive = TRUE), add = TRUE)

    ncvroc_request_cancel(cp_dir)
    err <- tryCatch(
      {
        nested_sum_roc(
          data = df,
          outcome = "y",
          items = colnames(X),
          min_items = 1L,
          max_items = 3L,
          outer_k = 3L,
          inner_k = 2L,
          preselect_top_n = 5L,
          parallel = "native_streaming",
          engine = "Rcpp",
          n_workers = 2L,
          checkpoint_dir = cp_dir,
          checkpoint_granularity = gran,
          seed = 77L,
          progress = FALSE,
          verbose = FALSE
        )
        NULL
      },
      ncvroc_interrupted = function(e) e
    )
    expect_s3_class(err, "ncvroc_interrupted")

    ncvroc_clear_cancel(cp_dir)
    resumed <- nested_sum_roc(
      data = df,
      outcome = "y",
      items = colnames(X),
      min_items = 1L,
      max_items = 3L,
      outer_k = 3L,
      inner_k = 2L,
      preselect_top_n = 5L,
      parallel = "native_streaming",
      engine = "Rcpp",
      n_workers = 2L,
      checkpoint_dir = cp_dir,
      checkpoint_granularity = gran,
      resume = TRUE,
      seed = 77L,
      progress = FALSE,
      verbose = FALSE
    )
    expect_equal(resumed$outer_results$items, ref$outer_results$items)
    expect_equal(resumed$outer_results$outer_auc, ref$outer_results$outer_auc, tolerance = 1e-12)
    expect_equal(resumed$pooled_auc, ref$pooled_auc, tolerance = 1e-12)
  }
})
