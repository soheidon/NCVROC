# test-native-outer-fold-scope-b.R
# Tests for NCVROC v0.26.0 Scope B: Native End-to-End Outer-Fold Execution Foundation

test_that("Scope B input contract rejects invalid dimensions, values, indices, methods, and schema", {
  set.seed(101)
  n <- 30
  p <- 4
  x <- matrix(sample(0:2, n * p, replace = TRUE), nrow = n, ncol = p)
  y <- c(rep(1L, 12), rep(0L, 18))
  outer_folds <- make_stratified_folds(y, k = 3, repeats = 1, seed = 11)
  specs <- NCVROC:::.build_native_outer_fold_specs(
    outer_folds, y, n, inner_k = 2, inner_repeats = 1L, n_candidates = 4L, seed = 11
  )

  # Unsupported schema_version
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L, schema_version = 99L
    ),
    "Unsupported schema_version"
  )

  # Non-finite x
  x_na <- x
  x_na[1, 1] <- NA_real_
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x_na, y, specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "non-finite or NA"
  )

  # Non-binary y
  y_bad <- y
  y_bad[1] <- 2L
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y_bad, specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "binary values"
  )

  # Unsupported cutoff_method / metric
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 2L, "sensitivity_target", "auc", "auc", 4L, TRUE, 1L
    ),
    "Unsupported cutoff_method"
  )
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 2L, "youden", "accuracy", "auc", 4L, TRUE, 1L
    ),
    "Unsupported preselect_by"
  )
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 2L, "youden", "auc", "accuracy", 4L, TRUE, 1L
    ),
    "Unsupported selection_criterion"
  )

  # Invalid num_threads / bounds
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 0L
    ),
    "num_threads"
  )
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 5L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "max_items"
  )

  # Overlapping train/test partition
  bad_specs <- specs
  bad_specs[[1]]$test_indices[1] <- bad_specs[[1]]$train_indices[1]
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, bad_specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "overlaps"
  )

  # Out-of-bounds inner fold index
  bad_inner <- specs
  bad_inner[[1]]$inner_folds[[1]][[1]][1] <- 999L
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, bad_inner, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "out of bounds"
  )

  # Duplicate (repeat_id, outer_fold_id)
  dup_specs <- specs
  dup_specs[[2]]$repeat_id <- dup_specs[[1]]$repeat_id
  dup_specs[[2]]$outer_fold_id <- dup_specs[[1]]$outer_fold_id
  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, dup_specs, 1L, 2L, "youden", "auc", "auc", 4L, TRUE, 1L
    ),
    "Duplicate"
  )

  # Bad fold name format
  expect_error(
    NCVROC:::.parse_outer_fold_identity("Fold_1"),
    "Rep<r>_Fold<f>"
  )
})

test_that("Scope B serial and multi-thread match R reference across metrics and cutoff methods", {
  set.seed(202)
  n <- 90
  p <- 5
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:3, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE, prob = c(0.6, 0.4))

  for (cm in c("youden", "closest_topleft")) {
    for (metric in c("auc", "youden", "sensitivity", "specificity")) {
      ref <- nested_sum_roc(
        data                = df,
        outcome             = "y",
        items               = items,
        min_items           = 1L,
        max_items           = 3L,
        cutoff_method       = cm,
        preselect_top_n     = 8L,
        preselect_by        = metric,
        selection_criterion = metric,
        outer_k             = 3L,
        inner_k             = 3L,
        outer_repeats       = 1L,
        seed                = 42L,
        engine              = "R",
        parallel            = FALSE,
        progress            = FALSE,
        verbose             = FALSE
      )

      sb_1t <- NCVROC:::.nested_sum_roc_scope_b(
        data                = df,
        outcome             = "y",
        items               = items,
        min_items           = 1L,
        max_items           = 3L,
        cutoff_method       = cm,
        preselect_top_n     = 8L,
        preselect_by        = metric,
        selection_criterion = metric,
        outer_k             = 3L,
        inner_k             = 3L,
        outer_repeats       = 1L,
        seed                = 42L,
        n_workers           = 1L
      )

      sb_2t <- NCVROC:::.nested_sum_roc_scope_b(
        data                = df,
        outcome             = "y",
        items               = items,
        min_items           = 1L,
        max_items           = 3L,
        cutoff_method       = cm,
        preselect_top_n     = 8L,
        preselect_by        = metric,
        selection_criterion = metric,
        outer_k             = 3L,
        inner_k             = 3L,
        outer_repeats       = 1L,
        seed                = 42L,
        n_workers           = 2L
      )

      expect_s3_class(sb_1t, "ncvroc_result")
      expect_equal(sb_1t$summary$outer_fold, ref$summary$outer_fold)
      expect_equal(sb_1t$summary$selected_items, ref$summary$selected_items)
      expect_equal(sb_1t$summary$n_items, ref$summary$n_items)
      expect_equal(sb_1t$summary$cutoff, ref$summary$cutoff, tolerance = 1e-12)
      expect_equal(sb_1t$summary$auc, ref$summary$auc, tolerance = 1e-12)
      expect_equal(sb_1t$summary$sensitivity, ref$summary$sensitivity, tolerance = 1e-12)
      expect_equal(sb_1t$summary$specificity, ref$summary$specificity, tolerance = 1e-12)
      expect_equal(sb_1t$summary$youden, ref$summary$youden, tolerance = 1e-12)
      expect_equal(sb_1t$summary$accuracy, ref$summary$accuracy, tolerance = 1e-12)
      expect_equal(sb_1t$summary$ppv, ref$summary$ppv, tolerance = 1e-12)
      expect_equal(sb_1t$summary$npv, ref$summary$npv, tolerance = 1e-12)
      expect_equal(sb_1t$outer_predictions, ref$outer_predictions, tolerance = 1e-12)
      expect_equal(sb_1t$selected_model_frequency, ref$selected_model_frequency)

      # Thread invariance between 1 and 2 threads
      expect_equal(sb_2t$summary, sb_1t$summary, tolerance = 0)
      expect_equal(sb_2t$outer_predictions, sb_1t$outer_predictions, tolerance = 0)
    }
  }
})

test_that("Scope B matches Scope A (native_tbb) and handles edge cases, repeats, and ties", {
  set.seed(303)
  n <- 70
  p <- 4
  items <- paste0("x", 1:p)
  # Create tied / identical columns to exercise candidate and cutoff tie-breaking
  v1 <- sample(0:1, n, replace = TRUE)
  df <- data.frame(
    x1 = v1,
    x2 = v1,
    x3 = sample(0:2, n, replace = TRUE),
    x4 = sample(0:2, n, replace = TRUE),
    y  = ifelse(v1 == 1 & runif(n) > 0.25, 1L, rbinom(n, 1, 0.25))
  )

  # Repeated outer folds + preselect_top_n > total_combos
  ref_rep <- nested_sum_roc(
    df, "y", items,
    min_items = 1L, max_items = 2L,
    preselect_top_n = 50L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 2L,
    seed = 77L, engine = "R", parallel = FALSE,
    progress = FALSE, verbose = FALSE
  )
  scope_a <- nested_sum_roc(
    df, "y", items,
    min_items = 1L, max_items = 2L,
    preselect_top_n = 50L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 2L,
    seed = 77L, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
    progress = FALSE, verbose = FALSE
  )
  scope_b <- NCVROC:::.nested_sum_roc_scope_b(
    df, "y", items,
    min_items = 1L, max_items = 2L,
    preselect_top_n = 50L,
    outer_k = 3L, inner_k = 2L, outer_repeats = 2L,
    seed = 77L, n_workers = 2L
  )

  expect_equal(scope_b$summary, ref_rep$summary, tolerance = 1e-12)
  expect_equal(scope_b$summary, scope_a$summary, tolerance = 1e-12)
  expect_equal(scope_b$outer_predictions, ref_rep$outer_predictions, tolerance = 1e-12)

  # Minimal legal folds (outer_k = 2, inner_k = 2, min_items = 1, max_items = 1)
  ref_min <- nested_sum_roc(
    df[1:24, ], "y", items,
    min_items = 1L, max_items = 1L,
    preselect_top_n = 4L,
    outer_k = 2L, inner_k = 2L, outer_repeats = 1L,
    seed = 9L, engine = "R", parallel = FALSE,
    progress = FALSE, verbose = FALSE
  )
  sb_min <- NCVROC:::.nested_sum_roc_scope_b(
    df[1:24, ], "y", items,
    min_items = 1L, max_items = 1L,
    preselect_top_n = 4L,
    outer_k = 2L, inner_k = 2L, outer_repeats = 1L,
    seed = 9L, n_workers = 1L
  )
  expect_equal(sb_min$summary, ref_min$summary, tolerance = 1e-12)
  expect_equal(sb_min$outer_predictions, ref_min$outer_predictions, tolerance = 1e-12)
})

test_that("Scope B preserves RNG state compatibility for seeded and unseeded (seed = NULL) calls", {
  set.seed(404)
  n <- 60
  p <- 4
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # Unseeded (seed = NULL): identical initial .Random.seed must yield identical results & post-call .Random.seed
  set.seed(555)
  ref_unseeded <- nested_sum_roc(
    df, "y", items, min_items = 1L, max_items = 2L,
    preselect_top_n = 5L, outer_k = 3L, inner_k = 2L,
    seed = NULL, engine = "R", parallel = FALSE,
    progress = FALSE, verbose = FALSE
  )
  rng_after_ref <- .Random.seed

  set.seed(555)
  sb_unseeded <- NCVROC:::.nested_sum_roc_scope_b(
    df, "y", items, min_items = 1L, max_items = 2L,
    preselect_top_n = 5L, outer_k = 3L, inner_k = 2L,
    seed = NULL, n_workers = 2L
  )
  rng_after_sb <- .Random.seed

  expect_identical(rng_after_sb, rng_after_ref)
  expect_equal(sb_unseeded$summary, ref_unseeded$summary, tolerance = 1e-12)
  expect_equal(sb_unseeded$outer_predictions, ref_unseeded$outer_predictions, tolerance = 1e-12)
})

test_that("Scope B transports worker errors deterministically by (repeat_id, outer_fold_id) and preserves order", {
  n <- 20
  p <- 3
  x <- matrix(rep(1.0, n * p), nrow = n, ncol = p)
  y <- c(rep(1L, 10), rep(0L, 10))
  # Construct 2 folds where BOTH outer training sets have only 1 class (all 0s or all 1s)
  outer_folds <- list(
    Rep1_Fold1 = 1:10,   # train = 11:20 (all 0s)
    Rep1_Fold2 = 11:20   # train = 1:10  (all 1s)
  )
  inner_dummy <- list(list(0:4, 5:9))
  specs <- list(
    # Intentionally pass Fold2 first in fold_specs to verify deterministic sorting by (repeat_id, outer_fold_id)
    list(
      repeat_id = 1L, outer_fold_id = 2L, fold_name = "Rep1_Fold2",
      train_indices = 0:9, test_indices = 10:19,
      inner_folds = inner_dummy, has_seed = FALSE, inner_seed_base = 0L
    ),
    list(
      repeat_id = 1L, outer_fold_id = 1L, fold_name = "Rep1_Fold1",
      train_indices = 10:19, test_indices = 0:9,
      inner_folds = inner_dummy, has_seed = FALSE, inner_seed_base = 0L
    )
  )

  expect_error(
    NCVROC:::evaluate_outer_folds_native_cpp(
      x, y, specs, 1L, 1L, "youden", "auc", "auc", 1L, TRUE, num_threads = 2L
    ),
    "Rep1_Fold1"
  )
})

test_that("Scope B test-only serialization round trip preserves complete fold payload", {
  set.seed(606)
  n <- 50
  p <- 4
  items <- paste0("q", 1:p)
  x <- matrix(sample(0:2, n * p, replace = TRUE), nrow = n, ncol = p)
  y <- sample(c(0L, 1L), n, replace = TRUE)
  outer_folds <- make_stratified_folds(y, k = 3, repeats = 1, seed = 17)
  specs <- NCVROC:::.build_native_outer_fold_specs(
    outer_folds, y, n, inner_k = 2, inner_repeats = 1L, n_candidates = 4L, seed = 17
  )

  # Pass specs in reverse order to verify deterministic output ordering by (repeat_id, outer_fold_id)
  raw_res <- NCVROC:::evaluate_outer_folds_native_cpp(
    x, y, rev(specs), 1L, 2L, "youden", "auc", "auc", 4L, TRUE, num_threads = 2L
  )

  expect_equal(vapply(raw_res, `[[`, character(1), "outer_fold"), c("Rep1_Fold1", "Rep1_Fold2", "Rep1_Fold3"))
  expect_true(all(vapply(raw_res, `[[`, logical(1), "completed")))
  expect_true(all(vapply(raw_res, `[[`, integer(1), "status_code") == 0L))

  for (res in raw_res) {
    rt <- NCVROC:::native_outer_fold_result_roundtrip_cpp(res)
    expect_equal(rt, res, tolerance = 0)
  }

  # Incomplete payload must be rejected by round-trip
  bad_res <- raw_res[[1]]
  bad_res$completed <- FALSE
  expect_error(
    NCVROC:::native_outer_fold_result_roundtrip_cpp(bad_res),
    "incomplete"
  )
})

test_that("Scope B preserves R reference which.max() first-in-order tie-breaking when inner CV criteria tie", {
  # Construct data where x1 and x2 are identical and x1+x2 has identical ROC/AUC to x1
  # so 1-item (x1) and 2-item (x1, x2) candidates tie on inner mean_auc and mean_youden
  set.seed(808)
  n <- 40
  v <- rep(c(0, 1, 2, 3), length.out = n)
  df <- data.frame(
    x1 = v,
    x2 = v,
    y  = ifelse(v >= 2, 1L, 0L)
  )

  ref <- nested_sum_roc(
    df, "y", c("x1", "x2"),
    min_items = 1L, max_items = 2L,
    preselect_top_n = 3L,
    outer_k = 2L, inner_k = 2L,
    seed = 12L, engine = "R", parallel = FALSE,
    progress = FALSE, verbose = FALSE
  )
  sb <- NCVROC:::.nested_sum_roc_scope_b(
    df, "y", c("x1", "x2"),
    min_items = 1L, max_items = 2L,
    preselect_top_n = 3L,
    outer_k = 2L, inner_k = 2L,
    seed = 12L, n_workers = 1L
  )

  expect_equal(sb$summary, ref$summary, tolerance = 1e-12)
  expect_equal(sb$outer_predictions, ref$outer_predictions, tolerance = 1e-12)
})

test_that("Public nested_sum_roc(..., parallel = 'native_outer_fold') works and matches R reference", {
  set.seed(909)
  n <- 80
  p <- 5
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:3, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # Validation rules
  expect_error(
    nested_sum_roc(df, "y", items, 1L, 2L, engine = "R", parallel = "native_outer_fold"),
    "requires `engine = 'Rcpp'`"
  )
  expect_error(
    nested_sum_roc(df, "y", items, 1L, 2L, engine = "Rcpp", parallel = "native_outer_fold", tuning = "auto"),
    "`tuning` must be 'off'"
  )
  expect_error(
    nested_sum_roc(df, "y", items, 1L, 2L, engine = "Rcpp", parallel = "native_outer_fold", threads_per_worker = 2L),
    "`threads_per_worker` can only exceed 1"
  )

  ref <- nested_sum_roc(
    df, "y", items, 1L, 3L,
    preselect_top_n = 10L, outer_k = 4L, inner_k = 3L, outer_repeats = 2L,
    seed = 33L, engine = "R", parallel = FALSE,
    progress = FALSE, verbose = FALSE
  )

  pub <- nested_sum_roc(
    df, "y", items, 1L, 3L,
    preselect_top_n = 10L, outer_k = 4L, inner_k = 3L, outer_repeats = 2L,
    seed = 33L, engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    tuning = "off", progress = FALSE, verbose = FALSE
  )

  expect_s3_class(pub, "ncvroc_result")
  expect_identical(pub$settings$parallel, "native_outer_fold")
  expect_equal(pub$summary, ref$summary, tolerance = 1e-12)
  expect_equal(pub$outer_predictions, ref$outer_predictions, tolerance = 1e-12)
  expect_equal(pub$selected_model_frequency, ref$selected_model_frequency)
})
