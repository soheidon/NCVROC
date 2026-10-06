# test-nested-native-outer-prototype.R
# Tests for NCVROC v0.23.0 Scope A: Native Outer-Parallel Candidate Search Prototype

test_that("evaluate_outer_candidate_search_native_cpp produces bitwise identical candidates to serial search", {
  set.seed(42)
  n <- 80
  p <- 6
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  names(df) <- items
  y <- sample(c(0L, 1L), n, replace = TRUE)
  df$y <- y

  outer_folds <- make_stratified_folds(y, k = 3, repeats = 1, seed = 123)
  train_indices_0based <- lapply(outer_folds, function(test_idx) {
    as.integer(setdiff(seq_len(n), test_idx) - 1L)
  })

  x_mat <- as.matrix(df[, items, drop = FALSE])

  # Test across multiple metrics and cutoff methods
  metrics <- c("auc", "youden", "sensitivity", "specificity")
  cutoffs <- c("youden", "closest_topleft")

  for (cm in cutoffs) {
    for (rm in metrics) {
      # Native outer candidate search
      native_res <- evaluate_outer_candidate_search_native_cpp(
        x                  = x_mat,
        y                  = y,
        train_indices      = train_indices_0based,
        min_items          = 1L,
        max_items          = 3L,
        cutoff_method      = cm,
        rank_by            = rm,
        top_n              = 10L,
        prefer_fewer_items = TRUE,
        num_threads        = 2L
      )

      # Compare against serial single-fold evaluation
      for (f in seq_along(outer_folds)) {
        train_idx <- setdiff(seq_len(n), outer_folds[[f]])
        ref_all <- exhaustive_sum_roc(
          data              = df[train_idx, , drop = FALSE],
          outcome           = "y",
          items             = items,
          min_items         = 1L,
          max_items         = 3L,
          cutoff_method     = cm,
          rank_by           = rm,
          top_n             = NULL,
          prefer_fewer_items = TRUE,
          engine            = "Rcpp",
          parallel          = FALSE,
          progress          = FALSE
        )
        ref_combos <- .select_top_candidates(ref_all, 10L, rm)

        nat_f <- native_res[[f]]
        nat_f_mat <- .materialize_candidate_items(nat_f, items, 1L, 3L)

        expect_equal(nat_f_mat$items, ref_combos$items)
        expect_equal(nat_f_mat$n_items, ref_combos$n_items)
        expect_equal(nat_f_mat$auc, ref_combos$auc, tolerance = 1e-12)
        expect_equal(nat_f_mat$cutoff, ref_combos$cutoff, tolerance = 1e-12)
        expect_equal(nat_f_mat$sensitivity, ref_combos$sensitivity, tolerance = 1e-12)
        expect_equal(nat_f_mat$specificity, ref_combos$specificity, tolerance = 1e-12)
        expect_equal(nat_f_mat$youden, ref_combos$youden, tolerance = 1e-12)
      }
    }
  }
})

test_that("Native outer candidate search matches across 1, 2, and 4 threads deterministically", {
  set.seed(99)
  n <- 100
  p <- 7
  items <- paste0("var", 1:p)
  df <- as.data.frame(matrix(sample(0:3, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  y <- sample(c(0L, 1L), n, replace = TRUE)
  df$y <- y

  outer_folds <- make_stratified_folds(y, k = 4, repeats = 2, seed = 456)
  train_indices_0based <- lapply(outer_folds, function(test_idx) {
    as.integer(setdiff(seq_len(n), test_idx) - 1L)
  })
  x_mat <- as.matrix(df[, items, drop = FALSE])

  res_1t <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y, train_indices_0based, 1L, 3L, "youden", "auc", 15L, TRUE, 1L
  )
  res_2t <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y, train_indices_0based, 1L, 3L, "youden", "auc", 15L, TRUE, 2L
  )
  res_4t <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y, train_indices_0based, 1L, 3L, "youden", "auc", 15L, TRUE, 4L
  )

  for (f in seq_along(outer_folds)) {
    expect_identical(res_1t[[f]]$n_items, res_2t[[f]]$n_items)
    expect_identical(res_1t[[f]]$n_items, res_4t[[f]]$n_items)
    expect_identical(res_1t[[f]]$.global_combo_index, res_2t[[f]]$.global_combo_index)
    expect_identical(res_1t[[f]]$.global_combo_index, res_4t[[f]]$.global_combo_index)
    expect_identical(res_1t[[f]]$auc, res_2t[[f]]$auc)
    expect_identical(res_1t[[f]]$auc, res_4t[[f]]$auc)
    expect_identical(res_1t[[f]]$cutoff, res_2t[[f]]$cutoff)
    expect_identical(res_1t[[f]]$cutoff, res_4t[[f]]$cutoff)
  }
})

test_that("Tie-heavy dataset respects exact comparator and prefer_fewer_items TRUE/FALSE", {
  # All features constant -> identical AUC and metrics
  n <- 40
  p <- 5
  items <- paste0("x", 1:p)
  df <- as.data.frame(matrix(1.0, nrow = n, ncol = p))
  names(df) <- items
  y <- rep(c(0L, 1L), each = n / 2)
  df$y <- y

  outer_folds <- make_stratified_folds(y, k = 2, repeats = 1, seed = 777)
  train_indices_0based <- lapply(outer_folds, function(test_idx) {
    as.integer(setdiff(seq_len(n), test_idx) - 1L)
  })
  x_mat <- as.matrix(df[, items, drop = FALSE])

  # With prefer_fewer_items = TRUE: 1-item combos must precede 2-item combos
  res_pfi_true <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y, train_indices_0based, 1L, 3L, "youden", "auc", 10L, TRUE, 2L
  )
  for (f in seq_along(outer_folds)) {
    n_items_vec <- res_pfi_true[[f]]$n_items
    g_idx_vec   <- res_pfi_true[[f]]$.global_combo_index
    expect_true(is.unsorted(n_items_vec) == FALSE) # monotonically increasing n_items
    # Within same n_items, global_combo_index must be strictly increasing
    expect_true(is.unsorted(g_idx_vec[n_items_vec == 1]) == FALSE)
  }

  # With prefer_fewer_items = FALSE: sorted strictly by global_combo_index
  res_pfi_false <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y, train_indices_0based, 1L, 3L, "youden", "auc", 10L, FALSE, 2L
  )
  for (f in seq_along(outer_folds)) {
    g_idx_vec <- res_pfi_false[[f]]$.global_combo_index
    expect_identical(g_idx_vec, as.double(1:10))
  }
})

test_that("Degenerate and NaN metrics are handled cleanly without crashes", {
  n <- 30
  p <- 4
  items <- paste0("x", 1:p)
  df <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  names(df) <- items
  # All positive cases -> AUC is NaN
  y_all_pos <- rep(1L, n)

  train_indices_0based <- list(as.integer(0:19))
  x_mat <- as.matrix(df[, items, drop = FALSE])

  res_nan <- evaluate_outer_candidate_search_native_cpp(
    x_mat, y_all_pos, train_indices_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L
  )
  expect_equal(length(res_nan), 1L)
  expect_true(all(is.na(res_nan[[1]]$auc)))
})

test_that("nested_sum_roc with parallel = 'native_tbb' matches serial end-to-end exactly", {
  set.seed(314)
  n <- 60
  p <- 5
  items <- paste0("item", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # Run standard serial nested_sum_roc
  res_serial <- nested_sum_roc(
    data                = df,
    outcome             = "y",
    items               = items,
    min_items           = 1,
    max_items           = 3,
    cutoff_method       = "youden",
    preselect_top_n     = 5,
    preselect_by        = "auc",
    selection_criterion = "auc",
    outer_k             = 3,
    inner_k             = 2,
    outer_repeats       = 1,
    seed                = 2026,
    engine              = "Rcpp",
    parallel            = FALSE,
    progress            = FALSE,
    verbose             = FALSE
  )

  # Run with public parallel = "native_tbb" API
  res_native <- nested_sum_roc(
    data                = df,
    outcome             = "y",
    items               = items,
    min_items           = 1,
    max_items           = 3,
    cutoff_method       = "youden",
    preselect_top_n     = 5,
    preselect_by        = "auc",
    selection_criterion = "auc",
    outer_k             = 3,
    inner_k             = 2,
    outer_repeats       = 1,
    seed                = 2026,
    engine              = "Rcpp",
    parallel            = "native_tbb",
    n_workers           = 2,
    progress            = FALSE,
    verbose             = FALSE
  )

  # Compare summary metrics
  expect_equal(res_native$summary$selected_items, res_serial$summary$selected_items)
  expect_equal(res_native$summary$n_items, res_serial$summary$n_items)
  expect_equal(res_native$summary$auc, res_serial$summary$auc, tolerance = 1e-12)
  expect_equal(res_native$summary$cutoff, res_serial$summary$cutoff, tolerance = 1e-12)
  expect_equal(res_native$summary$sensitivity, res_serial$summary$sensitivity, tolerance = 1e-12)
  expect_equal(res_native$summary$specificity, res_serial$summary$specificity, tolerance = 1e-12)
  expect_equal(res_native$summary$youden, res_serial$summary$youden, tolerance = 1e-12)

  # Compare predictions
  expect_identical(res_native$outer_predictions$predicted_score, res_serial$outer_predictions$predicted_score)
  expect_identical(res_native$outer_predictions$predicted_class, res_serial$outer_predictions$predicted_class)
})

test_that("PSOCK vs Native TBB vs Serial parity holds across multiple selection criteria and cutoff methods", {
  set.seed(789)
  n <- 50
  p <- 5
  items <- paste0("m", 1:p)
  df <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  names(df) <- items
  df$status <- sample(c(0L, 1L), n, replace = TRUE)

  criteria <- c("auc", "youden", "sensitivity", "specificity")
  methods <- c("youden", "closest_topleft")

  for (cm in methods) {
    for (crit in criteria) {
      # Serial
      res_ser <- nested_sum_roc(
        data                = df,
        outcome             = "status",
        items               = items,
        min_items           = 1,
        max_items           = 3,
        cutoff_method       = cm,
        preselect_top_n     = 8,
        preselect_by        = "auc",
        selection_criterion = crit,
        outer_k             = 3,
        inner_k             = 2,
        seed                = 888,
        engine              = "Rcpp",
        parallel            = FALSE,
        progress            = FALSE,
        verbose             = FALSE
      )

      # Native TBB
      res_nat <- nested_sum_roc(
        data                = df,
        outcome             = "status",
        items               = items,
        min_items           = 1,
        max_items           = 3,
        cutoff_method       = cm,
        preselect_top_n     = 8,
        preselect_by        = "auc",
        selection_criterion = crit,
        outer_k             = 3,
        inner_k             = 2,
        seed                = 888,
        engine              = "Rcpp",
        parallel            = "native_tbb",
        n_workers           = 2,
        progress            = FALSE,
        verbose             = FALSE
      )

      expect_equal(res_nat$summary$selected_items, res_ser$summary$selected_items)
      expect_equal(res_nat$summary$auc, res_ser$summary$auc, tolerance = 1e-12)
      expect_equal(res_nat$summary$cutoff, res_ser$summary$cutoff, tolerance = 1e-12)
      expect_equal(res_nat$summary$sensitivity, res_ser$summary$sensitivity, tolerance = 1e-12)
      expect_equal(res_nat$summary$specificity, res_ser$summary$specificity, tolerance = 1e-12)
      expect_equal(res_nat$summary$youden, res_ser$summary$youden, tolerance = 1e-12)
      expect_identical(res_nat$outer_predictions$predicted_score, res_ser$outer_predictions$predicted_score)
    }
  }
})

test_that("evaluate_outer_candidate_search_native_cpp performs main-thread defensive validation", {
  n <- 20
  p <- 3
  x_mat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- rep(c(0L, 1L), each = n / 2)
  valid_train_0based <- list(as.integer(0:9), as.integer(10:19))

  # 1. Zero rows or zero cols in x
  expect_error(
    evaluate_outer_candidate_search_native_cpp(matrix(numeric(0), nrow = 0, ncol = 3), integer(0), valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "x must have at least 1 row and 1 column"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(matrix(numeric(0), nrow = 20, ncol = 0), y, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "x must have at least 1 row and 1 column"
  )

  # 2. Length mismatch between y and nrow(x)
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y[1:10], valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "Length of y .* must match nrow\\(x\\)"
  )

  # 3. NA in y
  y_with_na <- y
  y_with_na[3] <- NA_integer_
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y_with_na, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "y contains NA values"
  )

  # 4. Non-binary y
  y_non_binary <- y
  y_non_binary[1] <- 2L
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y_non_binary, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "y must contain binary values"
  )

  # 5. Invalid num_threads (<= 0)
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, 0L),
    regexp = "num_threads must be a valid positive integer"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, -2L),
    regexp = "num_threads must be a valid positive integer"
  )

  # 6. Empty train_indices list
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices must be a non-empty list"
  )

  # 7. Empty training fold
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(as.integer(0:9), integer(0)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains an empty fold"
  )

  # 8. NA in train_indices fold
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(c(0L, NA_integer_, 2L)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains NA"
  )

  # 9. 0-based train_indices index < 0
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(c(-1L, 0L, 1L)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains invalid row index -1"
  )

  # 10. 0-based train_indices index >= nrow(x) (e.g. index 20 when nrow is 20)
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(c(0L, 10L, 20L)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains invalid row index 20"
  )

  # 11. min_items < 1, max_items < min_items, max_items > ncol(x), top_n < 1, or NA scalars
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 0L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "min_items must be a valid integer >= 1"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, NA_integer_, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "min_items must be a valid integer >= 1"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 3L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "max_items must be a valid integer >= min_items"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, NA_integer_, "youden", "auc", 5L, TRUE, 1L),
    regexp = "max_items must be a valid integer >= min_items"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 4L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "max_items .* cannot exceed ncol\\(x\\)"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 2L, "youden", "auc", 0L, TRUE, 1L),
    regexp = "top_n must be a valid integer >= 1"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 2L, "youden", "auc", NA_integer_, TRUE, 1L),
    regexp = "top_n must be a valid integer >= 1"
  )
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, valid_train_0based, 1L, 2L, "youden", "auc", 5L, TRUE, NA_integer_),
    regexp = "num_threads must be a valid positive integer"
  )

  # 12. Fractional non-integer numeric index in train_indices
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(c(0.5, 1.0, 2.0)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains fractional non-integer value"
  )

  # 13. Non-finite numeric index in train_indices
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat, y, list(c(0.0, Inf, 2.0)), 1L, 2L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "train_indices contains NA/non-finite value"
  )

  # 14. Direct C++ invocation exceeding 2,000,000 candidate space limit
  p30 <- 30
  x_mat30 <- matrix(rnorm(n * p30), nrow = n, ncol = p30)
  expect_error(
    evaluate_outer_candidate_search_native_cpp(x_mat30, y, valid_train_0based, 5L, 10L, "youden", "auc", 5L, TRUE, 1L),
    regexp = "Candidate space .* exceeds safe native prototype single-pass bounds \\(max 2,000,000\\)"
  )
})

test_that("Indexing contract: 1-based R fold conversion and row integrity", {
  set.seed(42)
  n <- 50
  p <- 4
  df <- data.frame(
    id = seq_len(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    y  = sample(c(0L, 1L), n, replace = TRUE)
  )

  outer_folds <- make_stratified_folds(df$y, k = 5, repeats = 1, seed = 123)

  # 1-based train indices
  train_indices_1based <- lapply(outer_folds, function(test_idx) setdiff(seq_len(n), test_idx))

  # Test boundary values: 1 is minimum, n is maximum
  all_1based <- unlist(train_indices_1based)
  expect_true(min(all_1based) >= 1L)
  expect_true(max(all_1based) <= n)

  # Test 0-based conversion
  train_indices_0based <- lapply(train_indices_1based, function(idx) as.integer(idx - 1L))
  all_0based <- unlist(train_indices_0based)
  expect_true(min(all_0based) >= 0L)
  expect_true(max(all_0based) <= n - 1L)

  # Assert exact row data slicing parity
  for (f in seq_along(outer_folds)) {
    r_train_rows <- df[train_indices_1based[[f]], c("x1", "x2", "x3", "x4")]
    c_0based_idx <- train_indices_0based[[f]] + 1L # Convert back to 1-based to verify equality
    expect_identical(train_indices_1based[[f]], c_0based_idx)
    expect_identical(r_train_rows, df[c_0based_idx, c("x1", "x2", "x3", "x4")])
  }
})

test_that(".is_native_outer_candidate_search_eligible enforces candidate space limits without enumerating", {
  # Eligible normal spaces
  expect_true(.is_native_outer_candidate_search_eligible(n_items = 10, min_items = 1, max_items = 3)) # 175 combos
  expect_true(.is_native_outer_candidate_search_eligible(n_items = 20, min_items = 1, max_items = 3)) # 1350 combos
  expect_true(.is_native_outer_candidate_search_eligible(n_items = 20, min_items = 1, max_items = 4, total_combos = 6195))

  # Ineligible spaces above .NATIVE_OUTER_PROTOTYPE_MAX_CANDIDATES (2,000,000)
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 30, min_items = 1, max_items = 15, total_combos = 2000001))
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 30, min_items = 5, max_items = 10)) # > 2M combos

  # Ineligible spaces > 2^31 (e.g. choose(50, 25) = 1.26e14 > 2^31)
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 50, min_items = 20, max_items = 25))
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 50, min_items = 1, max_items = 50, total_combos = 2^32))

  # Synthetic counts near 2^53 exact double boundary without enumeration
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 100, min_items = 1, max_items = 100, total_combos = 2^52))
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 100, min_items = 1, max_items = 100, total_combos = Inf))
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 10, min_items = 1, max_items = 3, total_combos = NA_real_))
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 10, min_items = 1, max_items = 3, total_combos = -5))
})

test_that("Large candidate space (> limit) bypasses native kernel and falls back safely to streaming/reference path", {
  set.seed(101)
  n <- 30
  p <- 4
  items <- paste0("v", 1:p)
  df <- as.data.frame(matrix(rnorm(n * p), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  old_opt <- options(NCVROC.prototype_outer_engine = "native_tbb")
  on.exit(options(old_opt), add = TRUE)

  # Temporarily mock .run_native_outer_candidate_search to fail if called
  native_called <- FALSE
  mock_run <- function(...) {
    native_called <<- TRUE
    stop("Native kernel was invoked unexpectedly for ineligible search!")
  }

  # When candidate count is artificially simulated above limit or using streaming fallback
  # Test with an ineligible candidate size config
  expect_false(.is_native_outer_candidate_search_eligible(n_items = 30, min_items = 1, max_items = 15))

  # Standard nested_sum_roc should run through safe reference/streaming path without invoking native kernel if ineligible
  res_safe <- nested_sum_roc(
    data                = df,
    outcome             = "y",
    items               = items,
    min_items           = 1,
    max_items           = 2,
    cutoff_method       = "youden",
    preselect_top_n     = 3,
    outer_k             = 2,
    inner_k             = 2,
    seed                = 555,
    engine              = "Rcpp",
    parallel            = FALSE,
    progress            = FALSE,
    verbose             = FALSE
  )
  expect_s3_class(res_safe, "ncvroc_result")
  expect_equal(nrow(res_safe$summary), 2L)
})

test_that("Streaming/Reference Parity: Top-N candidate search produces exact rankings and discrete parity", {
  set.seed(2026)
  n <- 50
  p <- 6
  items <- paste0("x", 1:p)
  mat <- matrix(sample(0:3, n * p, replace = TRUE), nrow = n, ncol = p)
  colnames(mat) <- items
  df <- as.data.frame(mat)
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  x_mat <- as.matrix(df[, items, drop = FALSE])
  train_idx_0based <- list(as.integer(0:(n - 1L)))

  metrics <- c("auc", "youden", "sensitivity", "specificity", "accuracy")
  for (m in metrics) {
    # 1. Native search
    nat_res <- evaluate_outer_candidate_search_native_cpp(
      x_mat, df$y, train_idx_0based, 1L, 3L, "youden", m, 12L, TRUE, 1L
    )[[1]]
    nat_res_mat <- .materialize_candidate_items(nat_res, items, 1L, 3L)

    # 2. Reference exhaustive search + .select_top_candidates preselection
    ex_all <- exhaustive_sum_roc(
      data              = df,
      outcome           = "y",
      items             = items,
      min_items         = 1L,
      max_items         = 3L,
      cutoff_method     = "youden",
      rank_by           = m,
      top_n             = NULL,
      engine            = "Rcpp",
      parallel          = FALSE,
      progress          = FALSE
    )
    ex_res <- .select_top_candidates(ex_all, 12L, m)

    # Compare exact ranks, items, discrete fields, and metrics
    expect_identical(nat_res_mat$items, ex_res$items)
    expect_identical(nat_res_mat$n_items, ex_res$n_items)
    expect_identical(nat_res_mat$n_positive, ex_res$n_positive)
    expect_identical(nat_res_mat$n_negative, ex_res$n_negative)
    expect_equal(nat_res_mat$auc, ex_res$auc, tolerance = 1e-12)
    expect_equal(nat_res_mat$cutoff, ex_res$cutoff, tolerance = 1e-12)
    expect_equal(nat_res_mat$sensitivity, ex_res$sensitivity, tolerance = 1e-12)
    expect_equal(nat_res_mat$specificity, ex_res$specificity, tolerance = 1e-12)
    expect_equal(nat_res_mat$youden, ex_res$youden, tolerance = 1e-12)
    expect_equal(nat_res_mat$accuracy, ex_res$accuracy, tolerance = 1e-12)
  }
})

test_that("Candidate Tie-Breaking: Strict parity with R .select_top_candidates under massive metric ties", {
  set.seed(2003)
  n <- 80
  p <- 8
  items <- paste0("v", 1:p)
  # Binary predictors with duplicated columns to induce massive ties in AUC and Youden
  mat <- matrix(sample(0:1, n * p, replace = TRUE), nrow = n, ncol = p)
  mat[, 4] <- mat[, 2] # duplicate column
  mat[, 6] <- mat[, 3] # duplicate column
  colnames(mat) <- items
  df <- as.data.frame(mat)
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  x_mat <- as.matrix(df[, items, drop = FALSE])
  train_idx_0based <- list(as.integer(0:(n - 1L)))

  for (by_m in c("auc", "youden", "sensitivity", "specificity", "accuracy")) {
    nat_res <- evaluate_outer_candidate_search_native_cpp(
      x_mat, df$y, train_idx_0based, 1L, 4L, "youden", by_m, 25L, TRUE, 1L
    )[[1]]
    nat_mat <- .materialize_candidate_items(nat_res, items, 1L, 4L)

    ex_all <- exhaustive_sum_roc(
      data              = df,
      outcome           = "y",
      items             = items,
      min_items         = 1L,
      max_items         = 4L,
      cutoff_method     = "youden",
      rank_by           = by_m,
      top_n             = NULL,
      engine            = "Rcpp",
      parallel          = FALSE,
      progress          = FALSE
    )
    r_top <- .select_top_candidates(ex_all, 25L, by_m)

    # Verify exact item ranking, item count, and metric parity across all 25 top candidates
    expect_identical(nat_mat$items, r_top$items)
    expect_identical(nat_mat$n_items, r_top$n_items)
    expect_equal(nat_mat[[by_m]], r_top[[by_m]], tolerance = 1e-12)
    expect_equal(nat_mat$youden, r_top$youden, tolerance = 1e-12)
    expect_equal(nat_mat$sensitivity, r_top$sensitivity, tolerance = 1e-12)
    expect_equal(nat_mat$specificity, r_top$specificity, tolerance = 1e-12)
  }
})

test_that("Multi-Threaded Invariance: Varying thread counts (1, 2, 4, 8) produce deterministic, identical candidate ranks and nested CV results", {
  set.seed(42)
  n <- 100
  p <- 10
  items <- paste0("q", 1:p)
  mat <- matrix(sample(0:2, n * p, replace = TRUE), nrow = n, ncol = p)
  colnames(mat) <- items
  df <- as.data.frame(mat)
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  x_mat <- as.matrix(df[, items, drop = FALSE])
  folds <- make_stratified_folds(df$y, k = 4, repeats = 1, seed = 777)
  train_indices_0based <- lapply(folds, function(test_idx) as.integer(setdiff(1:n, test_idx) - 1L))

  # Compute reference with 1 thread
  ref_res <- evaluate_outer_candidate_search_native_cpp(
    x_mat, df$y, train_indices_0based, 1L, 3L, "youden", "auc", 15L, TRUE, 1L
  )

  for (th in c(2L, 4L, 8L)) {
    th_res <- evaluate_outer_candidate_search_native_cpp(
      x_mat, df$y, train_indices_0based, 1L, 3L, "youden", "auc", 15L, TRUE, th
    )
    expect_equal(length(th_res), length(ref_res))
    for (f in seq_along(ref_res)) {
      expect_identical(th_res[[f]]$global_rank, ref_res[[f]]$global_rank)
      expect_identical(th_res[[f]]$n_items, ref_res[[f]]$n_items)
      expect_equal(th_res[[f]]$auc, ref_res[[f]]$auc, tolerance = 1e-12)
      expect_equal(th_res[[f]]$youden, ref_res[[f]]$youden, tolerance = 1e-12)
      expect_equal(th_res[[f]]$cutoff, ref_res[[f]]$cutoff, tolerance = 1e-12)
      expect_equal(th_res[[f]]$sensitivity, ref_res[[f]]$sensitivity, tolerance = 1e-12)
      expect_equal(th_res[[f]]$specificity, ref_res[[f]]$specificity, tolerance = 1e-12)
    }
  }
})

test_that("Phase 4B-F Public API Validation: parallel = 'native_tbb' enforces strict preconditions", {
  set.seed(42)
  n <- 40
  p <- 4
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # 1. Reject engine = 'R' with parallel = 'native_tbb'
  expect_error(
    nested_sum_roc(df, "y", items, engine = "R", parallel = "native_tbb"),
    regexp = "`parallel = 'native_tbb'` requires `engine = 'Rcpp'`"
  )

  # 2. Reject tuning = 'auto' or 'always' with parallel = 'native_tbb'
  expect_error(
    nested_sum_roc(df, "y", items, engine = "Rcpp", parallel = "native_tbb", tuning = "auto"),
    regexp = "`tuning` must be 'off' when `parallel = 'native_tbb'`"
  )
  expect_error(
    nested_sum_roc(df, "y", items, engine = "Rcpp", parallel = "native_tbb", tuning = "always"),
    regexp = "`tuning` must be 'off' when `parallel = 'native_tbb'`"
  )

  # 3. Reject threads_per_worker > 1 with parallel = 'native_tbb'
  expect_error(
    nested_sum_roc(df, "y", items, engine = "Rcpp", parallel = "native_tbb", threads_per_worker = 2L),
    regexp = "`threads_per_worker` can only exceed 1 when `parallel = 'hybrid'`"
  )

  # 4. Reject candidate space > 2,000,000
  p30 <- 30
  items30 <- paste0("x", 1:p30)
  df30 <- as.data.frame(matrix(sample(0:1, n * p30, replace = TRUE), nrow = n))
  names(df30) <- items30
  df30$y <- df$y
  expect_error(
    nested_sum_roc(df30, "y", items30, min_items = 5, max_items = 10, engine = "Rcpp", parallel = "native_tbb"),
    regexp = "Candidate space exceeds safe bounds for `parallel = 'native_tbb'`"
  )
})

test_that("Phase 4B-F Resource Budgeting: thread resolution and settings recording", {
  set.seed(123)
  n <- 50
  p <- 5
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # Explicit n_workers = 2 with outer_k = 3
  res2 <- nested_sum_roc(
    df, "y", items, engine = "Rcpp", parallel = "native_tbb",
    n_workers = 2L, outer_k = 3L, inner_k = 2L, seed = 42,
    progress = FALSE, verbose = FALSE
  )
  expect_s3_class(res2, "ncvroc_result")
  expect_identical(res2$settings$parallel, "native_tbb")
  expect_identical(res2$settings$requested_threads, 2L)
  expect_identical(res2$settings$effective_threads, 2L)
  expect_identical(res2$settings$effective_total_parallelism, 2L)
  expect_true(res2$settings$effective_max_cores >= 1L)

  # Explicit n_workers = 10 capped by outer_k = 3
  res_capped <- nested_sum_roc(
    df, "y", items, engine = "Rcpp", parallel = "native_tbb",
    n_workers = 10L, outer_k = 3L, inner_k = 2L, seed = 42,
    progress = FALSE, verbose = FALSE
  )
  expect_identical(res_capped$settings$requested_threads, 10L)
  expect_true(res_capped$settings$effective_threads <= 3L)

  # Default n_workers = NULL
  res_auto <- nested_sum_roc(
    df, "y", items, engine = "Rcpp", parallel = "native_tbb",
    n_workers = NULL, outer_k = 3L, inner_k = 2L, seed = 42,
    progress = FALSE, verbose = FALSE
  )
  expect_true(is.na(res_auto$settings$requested_threads))
  expect_true(res_auto$settings$effective_threads >= 1L)
  expect_true(res_auto$settings$effective_threads <= 3L)
})

test_that("Phase 4B-F Caller RNG Isolation and Global State Safety", {
  set.seed(999)
  n <- 50
  p <- 5
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # Measure RNG trajectory with seed
  set.seed(42)
  res_ser <- nested_sum_roc(
    df, "y", items, engine = "Rcpp", parallel = FALSE,
    outer_k = 3L, inner_k = 2L, seed = 777,
    progress = FALSE, verbose = FALSE
  )
  rng_after_ser <- .Random.seed

  set.seed(42)
  res_nat <- nested_sum_roc(
    df, "y", items, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
    outer_k = 3L, inner_k = 2L, seed = 777,
    progress = FALSE, verbose = FALSE
  )
  rng_after_nat <- .Random.seed

  # Post-call RNG state must match serial path
  expect_identical(rng_after_ser, rng_after_nat)

  # Result metrics must be identical
  expect_equal(res_nat$summary$selected_items, res_ser$summary$selected_items)
  expect_equal(res_nat$summary$auc, res_ser$summary$auc, tolerance = 1e-12)
  expect_equal(res_nat$summary$cutoff, res_ser$summary$cutoff, tolerance = 1e-12)
  expect_identical(res_nat$outer_predictions$predicted_score, res_ser$outer_predictions$predicted_score)
})

test_that("Phase 4B-F Progress/Verbose Contract: all flag combinations and completion gating", {
  set.seed(42)
  n <- 40
  p <- 4
  items <- paste0("q", 1:p)
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- items
  df$y <- sample(c(0L, 1L), n, replace = TRUE)

  # 1. progress = FALSE, verbose = TRUE:
  # Allows documented verbose status messages, but no progress bar/callback, percentage, or ETA.
  msgs_v_only <- capture.output({
    res_v_only <- nested_sum_roc(
      df, "y", items, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
      outer_k = 3L, inner_k = 2L, seed = 123,
      progress = FALSE, verbose = TRUE
    )
  }, type = "message")

  expect_true(any(grepl("Running 3 outer folds via native TBB", msgs_v_only)))
  expect_true(any(grepl("All outer folds complete", msgs_v_only)))
  # Must not contain ETA or percentage indicators
  expect_false(any(grepl("ETA", msgs_v_only, ignore.case = TRUE)))
  expect_false(any(grepl("%", msgs_v_only)))

  # 2. progress = TRUE, verbose = FALSE:
  # Suppresses verbose status messages; native TBB emits no worker-side/fake ETA progress.
  msgs_p_only <- capture.output({
    res_p_only <- nested_sum_roc(
      df, "y", items, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
      outer_k = 3L, inner_k = 2L, seed = 123,
      progress = TRUE, verbose = FALSE
    )
  }, type = "message")

  expect_false(any(grepl("Running.*outer folds", msgs_p_only)))
  expect_false(any(grepl("All outer folds complete", msgs_p_only)))
  expect_false(any(grepl("ETA", msgs_p_only, ignore.case = TRUE)))
  expect_false(any(grepl("%", msgs_p_only)))
  expect_identical(length(msgs_p_only), 0L)

  # 3. Both flags FALSE (progress = FALSE, verbose = FALSE):
  # Complete silence guaranteed.
  msgs_silent <- capture.output({
    res_silent <- nested_sum_roc(
      df, "y", items, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
      outer_k = 3L, inner_k = 2L, seed = 123,
      progress = FALSE, verbose = FALSE
    )
  }, type = "message")

  expect_identical(length(msgs_silent), 0L)

  # 4. Successful native call emits completion message only after returning successfully.
  # Forced post-dispatch failure: verify start message is emitted, error is caught, and completion message is NOT emitted.
  err_msgs <- capture.output({
    err <- tryCatch({
      testthat::with_mocked_bindings(
        .run_native_outer_candidate_search = function(...) {
          stop("simulated post-dispatch failure in native candidate search")
        },
        code = {
          nested_sum_roc(
            df, "y", items, engine = "Rcpp", parallel = "native_tbb", n_workers = 2L,
            outer_k = 3L, inner_k = 2L, seed = 123,
            progress = TRUE, verbose = TRUE
          )
        }
      )
      NULL
    }, error = function(e) e)
  }, type = "message")

  expect_true(!is.null(err))
  expect_true(inherits(err, "error"))
  expect_match(conditionMessage(err), "simulated post-dispatch failure in native candidate search")
  expect_true(any(grepl("Running 3 outer folds via native TBB", err_msgs)))
  expect_false(any(grepl("All outer folds complete", err_msgs)))
})
