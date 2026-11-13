# Tests for package-level candidate-count backend routing (parallel = "auto"),
# backend provenance, native_outer_fold vs native_streaming parity, and
# native_streaming checkpoint compatibility.

.make_routing_fixture <- function(n = 48L, p = 5L, seed = 4242L) {
  set.seed(seed)
  y <- rep(c(0L, 1L), length.out = n)
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  x[, 1L] <- x[, 1L] + 1.1 * y
  x[, 2L] <- x[, 2L] + 0.8 * y
  df <- as.data.frame(x)
  colnames(df) <- paste0("q", seq_len(p))
  df$y <- y
  df$id_col <- paste0("id_", seq_len(n))
  df
}

test_that("candidate-count routing helper matches exact combinatorial math and 2,000,000 boundary", {
  # Exact n=103, k=1..5 verification against independent choose() math
  expected_counts <- c(103, 5253, 176851, 4421275, 87541245)
  for (k in 1:5) {
    expect_equal(choose(103, k), expected_counts[k])
    sel <- NCVROC:::.select_auto_parallel_backend(
      n_items = 103L, min_items = k, max_items = k
    )
    expect_equal(sel$eligible_candidate_count, expected_counts[k])
    if (expected_counts[k] <= 2000000) {
      expect_equal(sel$resolved_parallel, "native_outer_fold")
    } else {
      expect_equal(sel$resolved_parallel, "native_streaming")
    }
  }

  # Cumulative ranges 1..3 vs 1..4 vs 1..5 for n=103
  sel_1_3 <- NCVROC:::.select_auto_parallel_backend(103L, 1L, 3L)
  expect_equal(sel_1_3$eligible_candidate_count, sum(expected_counts[1:3]))
  expect_equal(sel_1_3$resolved_parallel, "native_outer_fold")

  sel_1_4 <- NCVROC:::.select_auto_parallel_backend(103L, 1L, 4L)
  expect_equal(sel_1_4$eligible_candidate_count, sum(expected_counts[1:4]))
  expect_equal(sel_1_4$resolved_parallel, "native_streaming")

  sel_1_5 <- NCVROC:::.select_auto_parallel_backend(103L, 1L, 5L)
  expect_equal(sel_1_5$eligible_candidate_count, sum(expected_counts[1:5]))
  expect_equal(sel_1_5$resolved_parallel, "native_streaming")

  # Exact boundary: <= 2,000,000 -> native_outer_fold, > 2,000,000 -> native_streaming
  # Using threshold override to test exact <, ==, > boundaries directly
  sel_lt <- NCVROC:::.select_auto_parallel_backend(
    n_items = 6L, min_items = 1L, max_items = 2L, streaming_threshold = 21L
  )
  # choose(6,1) + choose(6,2) = 6 + 15 = 21 == threshold
  expect_equal(sel_lt$eligible_candidate_count, 21)
  expect_equal(sel_lt$resolved_parallel, "native_outer_fold")

  sel_eq_minus1 <- NCVROC:::.select_auto_parallel_backend(
    n_items = 6L, min_items = 1L, max_items = 2L, streaming_threshold = 22L
  )
  expect_equal(sel_eq_minus1$resolved_parallel, "native_outer_fold")

  sel_gt <- NCVROC:::.select_auto_parallel_backend(
    n_items = 6L, min_items = 1L, max_items = 2L, streaming_threshold = 20L
  )
  expect_equal(sel_gt$resolved_parallel, "native_streaming")

  # Exact 2,000,000 threshold check via n=2000000, k=1
  sel_2m <- NCVROC:::.select_auto_parallel_backend(2000000L, 1L, 1L)
  expect_equal(sel_2m$eligible_candidate_count, 2000000)
  expect_equal(sel_2m$resolved_parallel, "native_outer_fold")

  sel_2m_plus1 <- NCVROC:::.select_auto_parallel_backend(2000001L, 1L, 1L)
  expect_equal(sel_2m_plus1$eligible_candidate_count, 2000001)
  expect_equal(sel_2m_plus1$resolved_parallel, "native_streaming")
})

test_that("parallel = 'auto' wires through ncvroc_config, run_ncvroc, ncvroc, and nested_sum_roc", {
  df <- .make_routing_fixture(n = 40L, p = 5L, seed = 101L)
  items <- paste0("q", 1:5)

  # Config accepts parallel = "auto" and preserves defaults otherwise
  cfg_default <- ncvroc_config("y", items = items, max_items = 2L)
  expect_identical(cfg_default$parallel, FALSE)

  cfg_auto <- ncvroc_config(
    "y", items = items, min_items = 1L, max_items = 2L,
    mode = "quick", outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
    parallel = "auto"
  )
  expect_equal(cfg_auto$parallel, "auto")

  # Invalid combinations fail clearly
  expect_error(
    ncvroc_config("y", items = items, engine = "R", parallel = "auto"),
    "engine = 'Rcpp'"
  )
  expect_error(
    nested_sum_roc(df, "y", items, max_items = 2L, outer_k = 3L, inner_k = 2L,
                   engine = "R", parallel = "auto", progress = FALSE, verbose = FALSE),
    "engine = 'Rcpp'"
  )
  expect_error(
    nested_sum_roc(df, "y", items, max_items = 2L, outer_k = 3L, inner_k = 2L,
                   engine = "Rcpp", tuning = "auto", parallel = "auto",
                   progress = FALSE, verbose = FALSE),
    "tuning"
  )
  expect_error(
    ncvroc_config("y", items = items, parallel = "auto", threads_per_worker = 2L),
    "threads_per_worker"
  )

  # cross_size_nested_cv remains completely separate
  expect_error(
    cross_size_nested_cv(df, "y", items, model_sizes = 1:2, parallel = "auto",
                         progress = FALSE, verbose = FALSE),
    "reserved"
  )

  # Eligible universe <= threshold -> native_outer_fold across all entry points
  res_nested <- nested_sum_roc(
    df, "y", items, min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 77L,
    engine = "Rcpp", parallel = "auto", progress = FALSE, verbose = FALSE
  )
  expect_equal(res_nested$settings$requested_parallel, "auto")
  expect_equal(res_nested$settings$resolved_parallel, "native_outer_fold")
  expect_equal(res_nested$backend_provenance$resolved_parallel, "native_outer_fold")
  expect_equal(res_nested$backend_provenance$eligible_candidate_count, 15)
  expect_match(res_nested$backend_provenance$selection_reason, "eligible_candidates \\(15\\) <= streaming_threshold \\(2000000\\)")

  res_run <- run_ncvroc(data = df, items = items, config = cfg_auto, seed = 77L, progress = FALSE, verbose = FALSE)
  expect_equal(res_run$backend_provenance$resolved_parallel, "native_outer_fold")
  expect_equal(res_run$summary, res_nested$summary)

  res_ncv <- ncvroc(
    df, "y", items = items, min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 77L,
    final_search = FALSE, parallel = "auto", progress = FALSE, verbose = FALSE
  )
  expect_equal(res_ncv$backend_provenance$resolved_parallel, "native_outer_fold")
  expect_equal(res_ncv$nested_result$settings$resolved_parallel, "native_outer_fold")

  # Eligible universe > threshold -> native_streaming (via bounded threshold option)
  old_opt <- getOption("NCVROC.auto_streaming_threshold", NULL)
  on.exit(options(NCVROC.auto_streaming_threshold = old_opt), add = TRUE)
  options(NCVROC.auto_streaming_threshold = 10L)

  res_stream_auto <- nested_sum_roc(
    df, "y", items, min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 77L,
    engine = "Rcpp", parallel = "auto", progress = FALSE, verbose = FALSE
  )
  expect_equal(res_stream_auto$settings$requested_parallel, "auto")
  expect_equal(res_stream_auto$settings$resolved_parallel, "native_streaming")
  expect_equal(res_stream_auto$backend_provenance$resolved_parallel, "native_streaming")
  expect_equal(res_stream_auto$backend_provenance$eligible_candidate_count, 15)
  expect_match(res_stream_auto$backend_provenance$selection_reason, "eligible_candidates \\(15\\) > streaming_threshold \\(10\\)")

  res_ncv_stream <- ncvroc(
    df, "y", items = items, min_items = 1L, max_items = 2L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 77L,
    final_search = FALSE, parallel = "auto", progress = FALSE, verbose = FALSE
  )
  expect_equal(res_ncv_stream$backend_provenance$resolved_parallel, "native_streaming")

  # Subset of items (3 items) with max_items = 4 clamps cleanly to eligible count = 7 <= 10
  res_sub <- nested_sum_roc(
    df, "y", items[1:3], min_items = 1L, max_items = 4L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 77L,
    engine = "Rcpp", parallel = "auto", progress = FALSE, verbose = FALSE
  )
  expect_equal(res_sub$backend_provenance$eligible_n_items, 3L)
  expect_equal(res_sub$backend_provenance$eligible_max_items, 3L)
  expect_equal(res_sub$backend_provenance$eligible_candidate_count, 7)
  expect_equal(res_sub$backend_provenance$resolved_parallel, "native_outer_fold")
})

test_that("native_outer_fold and native_streaming achieve exact parity on small fixtures", {
  df <- .make_routing_fixture(n = 54L, p = 6L, seed = 2026L)
  items <- paste0("q", 1:6)

  for (cm in c("youden", "closest_topleft")) {
    res_nof <- nested_sum_roc(
      df, "y", items, min_items = 1L, max_items = 3L,
      cutoff_method = cm, preselect_top_n = 10L,
      outer_k = 3L, inner_k = 2L, outer_repeats = 2L, seed = 99L,
      engine = "Rcpp", parallel = "native_outer_fold",
      progress = FALSE, verbose = FALSE
    )
    res_str <- nested_sum_roc(
      df, "y", items, min_items = 1L, max_items = 3L,
      cutoff_method = cm, preselect_top_n = 10L,
      outer_k = 3L, inner_k = 2L, outer_repeats = 2L, seed = 99L,
      engine = "Rcpp", parallel = "native_streaming",
      progress = FALSE, verbose = FALSE
    )

    expect_equal(res_str$summary, res_nof$summary, tolerance = 1e-12)
    expect_equal(res_str$selected_models, res_nof$selected_models)
    expect_equal(res_str$selected_model_frequency, res_nof$selected_model_frequency)
    expect_equal(res_str$outer_predictions, res_nof$outer_predictions, tolerance = 1e-12)
    expect_equal(length(res_str$outer_results), length(res_nof$outer_results))
    for (i in seq_along(res_nof$outer_results)) {
      expect_equal(res_str$outer_results[[i]]$selected_items,
                   res_nof$outer_results[[i]]$selected_items)
      expect_equal(res_str$outer_results[[i]]$cutoff,
                   res_nof$outer_results[[i]]$cutoff, tolerance = 1e-12)
      expect_equal(res_str$outer_results[[i]]$auc,
                   res_nof$outer_results[[i]]$auc, tolerance = 1e-12)
    }
  }
})

test_that("native_streaming supports checkpoint save/resume across granularities and rejects incompatible checkpoints", {
  df <- .make_routing_fixture(n = 44L, p = 5L, seed = 555L)
  items <- paste0("q", 1:5)

  ref <- nested_sum_roc(
    df, "y", items, min_items = 1L, max_items = 3L,
    preselect_top_n = 8L, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
    seed = 123L, engine = "Rcpp", parallel = "native_streaming",
    progress = FALSE, verbose = FALSE
  )

  for (gran in c("outer_fold", "model_size", "candidate_batch")) {
    cp_dir <- tempfile(paste0("cp_stream_", gran, "_"))
    on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    res_cp <- nested_sum_roc(
      df, "y", items, min_items = 1L, max_items = 3L,
      preselect_top_n = 8L, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
      seed = 123L, engine = "Rcpp", parallel = "native_streaming",
      checkpoint_dir = cp_dir, checkpoint_granularity = gran,
      resume = FALSE, progress = FALSE, verbose = FALSE
    )
    expect_equal(res_cp$summary, ref$summary, tolerance = 1e-12)
    expect_equal(res_cp$outer_predictions, ref$outer_predictions, tolerance = 1e-12)

    # Resume on completed checkpoint directory reproduces identical results
    res_resume <- nested_sum_roc(
      df, "y", items, min_items = 1L, max_items = 3L,
      preselect_top_n = 8L, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
      seed = 123L, engine = "Rcpp", parallel = "native_streaming",
      checkpoint_dir = cp_dir, checkpoint_granularity = gran,
      resume = TRUE, progress = FALSE, verbose = FALSE
    )
    expect_equal(res_resume$summary, ref$summary, tolerance = 1e-12)

    # Incompatible run (different max_items) errors cleanly without corrupting checkpoint
    expect_error(
      nested_sum_roc(
        df, "y", items, min_items = 1L, max_items = 2L,
        preselect_top_n = 8L, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
        seed = 123L, engine = "Rcpp", parallel = "native_streaming",
        checkpoint_dir = cp_dir, checkpoint_granularity = gran,
        resume = TRUE, progress = FALSE, verbose = FALSE
      )
    )
    # Original checkpoint remains intact and resumable
    res_after_err <- nested_sum_roc(
      df, "y", items, min_items = 1L, max_items = 3L,
      preselect_top_n = 8L, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
      seed = 123L, engine = "Rcpp", parallel = "native_streaming",
      checkpoint_dir = cp_dir, checkpoint_granularity = gran,
      resume = TRUE, progress = FALSE, verbose = FALSE
    )
    expect_equal(res_after_err$summary, ref$summary, tolerance = 1e-12)
  }
})

test_that("legacy result objects without backend_provenance remain compatible", {
  df <- .make_routing_fixture(n = 40L, p = 4L, seed = 321L)
  res <- nested_sum_roc(
    df, "y", paste0("q", 1:4), min_items = 1L, max_items = 2L,
    cutoff_method = "youden",
    outer_k = 3L, inner_k = 2L, outer_repeats = 1L, seed = 11L,
    engine = "Rcpp", parallel = "none", progress = FALSE, verbose = FALSE
  )
  legacy <- res
  legacy$backend_provenance <- NULL
  legacy$settings$backend_provenance <- NULL
  legacy$settings$requested_parallel <- NULL
  legacy$settings$resolved_parallel <- NULL

  expect_output(print(legacy), "NCVROC nested cross-validation result")
  expect_output(summary(legacy), "NCVROC nested cross-validation summary")
  stab <- classification_stability_roc(
    df, "y", mode = "full_procedure", selection_result = legacy,
    bootstrap_reps = 5L, seed = 11L, progress = FALSE
  )
  expect_s3_class(stab, "classification_stability_result")
})
