# test-cross-size-cv.R — Unit tests for cross_size_cv()

test_that("cross_size_cv basic execution with model_sizes = 1:3 and AUC selection", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 50, replace = TRUE),
    Q1 = sample(0:2, 50, replace = TRUE),
    Q2 = sample(0:2, 50, replace = TRUE),
    Q3 = sample(0:2, 50, replace = TRUE),
    Q4 = sample(0:2, 50, replace = TRUE)
  )

  # 4 predictors, sizes 1:3 -> choose(4,1) + choose(4,2) + choose(4,3) = 4 + 6 + 4 = 14 combos
  res <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, cv_method = "kfold", folds = 5, seed = 42)

  expect_s3_class(res, "cross_size_cv_result")
  expect_equal(res$model_sizes, 1:3)
  expect_equal(res$total_combinations, 14L)
  expect_equal(nrow(res$candidate_ranking), 14L)
  expect_equal(nrow(res$final_selected_model), 1L)
  expect_equal(nrow(res$oof_predictions), 50L)
  expect_length(res$cv_cutoff_distribution$per_fold_values, 5L)

  # Check that ranking is strictly ordered by AUC descending
  expect_true(all(diff(res$candidate_ranking$auc) <= 1e-10))

  # Mathematical identity: for fixed unweighted sum scores, pooled OOF AUC == full-data AUC
  ref_cv <- cv_sum_roc(d, y, res$final_selected_model$items, folds = 5, seed = 42)
  expect_equal(res$final_selected_model$cv_auc, ref_cv$summary$mean[ref_cv$summary$metric == "auc"])

  # Print / summary methods work without error
  expect_output(print(res), "Cross-Size Ordinary Cross-Validation")
  expect_output(summary(res), "Final Selected Model")
})

test_that("Mathematical identity: pooled OOF AUC == full-data AUC across k-fold, repeated k-fold, and LOOCV", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE)
  )

  items_test <- c("Q1", "Q2")

  # Full-data apparent AUC
  scores <- d$Q1 + d$Q2
  freq <- compute_score_frequencies(scores, d$y)
  full_auc <- compute_auc_from_table(freq$pos_counts, freq$neg_counts)

  # k-fold CV
  cv_kfold <- cv_sum_roc(d, y, items_test, cv_method = "kfold", folds = 5, seed = 123)
  expect_equal(cv_kfold$summary$mean[cv_kfold$summary$metric == "auc"], full_auc, tolerance = 1e-10)

  # Repeated k-fold CV (AUC SD across repeats is strictly 0)
  cv_rep <- cv_sum_roc(d, y, items_test, cv_method = "kfold", folds = 4, repeats = 3, seed = 123)
  expect_equal(cv_rep$summary$mean[cv_rep$summary$metric == "auc"], full_auc, tolerance = 1e-10)
  expect_equal(cv_rep$summary$sd[cv_rep$summary$metric == "auc"], 0.0, tolerance = 1e-10)

  # LOOCV
  cv_loo <- cv_sum_roc(d, y, items_test, cv_method = "loocv", seed = 123)
  expect_equal(cv_loo$summary$mean[cv_loo$summary$metric == "auc"], full_auc, tolerance = 1e-10)
})

test_that("All-candidate AUC identity & deterministic ranking test", {
  set.seed(999)
  d <- data.frame(
    y  = sample(0:1, 36, replace = TRUE),
    Q1 = sample(0:2, 36, replace = TRUE),
    Q2 = sample(0:2, 36, replace = TRUE),
    Q3 = sample(0:2, 36, replace = TRUE)
  )

  items_list <- list("Q1", "Q2", "Q3", c("Q1", "Q2"), c("Q1", "Q3"), c("Q2", "Q3"))
  individual_aucs <- numeric(length(items_list))
  for (i in seq_along(items_list)) {
    cv_i <- cv_sum_roc(d, y, items_list[[i]], folds = 3, seed = 999)
    individual_aucs[i] <- cv_i$summary$mean[cv_i$summary$metric == "auc"]
  }

  best_individual_idx <- which.max(individual_aucs)
  best_individual_items <- format_items(items_list[[best_individual_idx]])
  best_individual_auc <- individual_aucs[best_individual_idx]

  res_cross <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, folds = 3, seed = 999)

  expect_equal(res_cross$final_selected_model$items, best_individual_items)
  expect_equal(res_cross$final_selected_model$cv_auc, best_individual_auc, tolerance = 1e-10)

  for (i in seq_along(items_list)) {
    c_str <- format_items(items_list[[i]])
    c_auc <- res_cross$candidate_ranking$auc[res_cross$candidate_ranking$items == c_str]
    expect_equal(c_auc, individual_aucs[i], tolerance = 1e-10)
  }
})

test_that("Multi-pass hierarchical bounded fan-in merge exactness and bounded memory", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE),
    Q5 = sample(0:2, 40, replace = TRUE),
    Q6 = sample(0:2, 40, replace = TRUE)
  )

  # Size 3 from 6 items = choose(6, 3) = 20 candidates
  ref_full_s3 <- exhaustive_sum_roc(d, "y", paste0("Q", 1:6), min_items = 3, max_items = 3, rank_by = "auc")

  # Create initial block streams with tiny block_size = 2 -> generates 10 initial 1-block streams
  x_mat <- as.matrix(d[, paste0("Q", 1:6)])
  init_obj <- .create_initial_block_streams(
    x_mat              = x_mat,
    y                  = d$y,
    item_names         = paste0("Q", 1:6),
    size               = 3L,
    cutoff_method      = "youden",
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    parallel_mode      = "none",
    n_workers_res      = 1L,
    block_size         = 2L
  )
  on.exit(init_obj$cleanup(), add = TRUE)

  expect_equal(length(init_obj$initial_streams), 10L)

  # Hierarchical merge with bounded fan_in = 2 and block_size = 2
  final_stream <- .merge_block_streams_hierarchical(
    initial_streams    = init_obj$initial_streams,
    base_dir           = init_obj$base_dir,
    fan_in             = 2L,
    block_size         = 2L,
    prefer_fewer_items = TRUE
  )

  expect_equal(final_stream$total_rows, 20L)
  expect_equal(final_stream$n_blocks, 10L)

  for (bf in final_stream$block_files) {
    df_b <- readRDS(bf)
    expect_lte(nrow(df_b), 2L)
  }

  reader <- .open_block_stream_reader(final_stream, size_cum_offset = 0L)
  collected <- list()
  while (TRUE) {
    cand <- reader$pop()
    if (is.null(cand) || nrow(cand) == 0L) break
    collected[[length(collected) + 1L]] <- cand
  }
  reader$close()
  merged_df <- do.call(rbind, collected)

  expect_equal(nrow(merged_df), 20L)
  expect_identical(merged_df$items, ref_full_s3$items)
  expect_equal(merged_df$auc, ref_full_s3$auc, tolerance = 1e-10)
  expect_equal(merged_df$cutoff, ref_full_s3$cutoff)
  expect_false(anyDuplicated(merged_df$items) > 0)
})

test_that("Cross-size merge iterator matches full cross-size reference", {
  set.seed(456)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  ref_full_12 <- exhaustive_sum_roc(d, "y", paste0("Q", 1:4), min_items = 1, max_items = 2, rank_by = "auc")

  x_mat <- as.matrix(d[, paste0("Q", 1:4)])
  init_s1 <- .create_initial_block_streams(x_mat, d$y, paste0("Q", 1:4), size = 1L, cutoff_method = "youden",
                                          prefer_fewer_items = TRUE, engine = "Rcpp", parallel_mode = "none",
                                          n_workers_res = 1L, block_size = 2L)
  init_s2 <- .create_initial_block_streams(x_mat, d$y, paste0("Q", 1:4), size = 2L, cutoff_method = "youden",
                                          prefer_fewer_items = TRUE, engine = "Rcpp", parallel_mode = "none",
                                          n_workers_res = 1L, block_size = 2L)
  on.exit({ init_s1$cleanup(); init_s2$cleanup() }, add = TRUE)

  final_s1 <- .merge_block_streams_hierarchical(init_s1$initial_streams, init_s1$base_dir, fan_in = 2L, block_size = 2L)
  final_s2 <- .merge_block_streams_hierarchical(init_s2$initial_streams, init_s2$base_dir, fan_in = 2L, block_size = 2L)

  global_it <- .create_cross_size_block_stream_iterator(
    final_streams    = list(final_s1, final_s2),
    sizes            = c(1L, 2L),
    size_cum_offsets = c(0L, 4L),
    prefer_fewer_items = TRUE
  )

  collected <- list()
  while (TRUE) {
    cand <- global_it$pop()
    if (is.null(cand) || nrow(cand) == 0L) break
    collected[[length(collected) + 1L]] <- cand
  }
  global_it$cleanup()
  global_stream <- do.call(rbind, collected)

  expect_equal(nrow(global_stream), 10L)
  expect_identical(global_stream$items, ref_full_12$items)
  expect_equal(global_stream$auc, ref_full_12$auc, tolerance = 1e-10)
  expect_equal(global_stream$n_items, ref_full_12$n_items)
  expect_false(anyDuplicated(global_stream$items) > 0)
})

# =========================================================================
# COMPREHENSIVE PARALLEL TEST MATRIX FOR ORDINARY cross_size_cv()
# =========================================================================

test_that("ordinary cross_size_cv exactness matrix: (1) AUC unconstrained [none vs threads vs chunks]", {
  set.seed(777)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  .reset_routing_counters()
  res_none    <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", parallel = "none", seed = 777)
  res_threads <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", parallel = "threads", n_workers = 2, seed = 777)
  res_chunks  <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", parallel = "chunks", n_workers = 2, seed = 777)

  # Check settings
  expect_equal(res_none$settings$parallel, "none")
  expect_equal(res_threads$settings$parallel, "threads")
  expect_equal(res_chunks$settings$parallel, "chunks")

  # Exact matching
  expect_identical(res_none$final_selected_model$items, res_threads$final_selected_model$items)
  expect_identical(res_none$final_selected_model$items, res_chunks$final_selected_model$items)

  expect_equal(res_none$candidate_ranking$auc, res_threads$candidate_ranking$auc, tolerance = 1e-10)
  expect_equal(res_none$candidate_ranking$auc, res_chunks$candidate_ranking$auc, tolerance = 1e-10)

  expect_identical(res_none$candidate_ranking$items, res_threads$candidate_ranking$items)
  expect_identical(res_none$candidate_ranking$items, res_chunks$candidate_ranking$items)

  expect_equal(res_none$oof_predictions$predicted_score, res_threads$oof_predictions$predicted_score)
  expect_equal(res_none$oof_predictions$predicted_score, res_chunks$oof_predictions$predicted_score)
})

test_that("ordinary cross_size_cv exactness matrix: (2) AUC + OOF constraint [none vs threads vs chunks]", {
  set.seed(888)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  .reset_routing_counters()
  res_none    <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", sensitivity_min = 0.40, parallel = "none", seed = 888)
  res_threads <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", sensitivity_min = 0.40, parallel = "threads", n_workers = 2, seed = 888)
  res_chunks  <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", sensitivity_min = 0.40, parallel = "chunks", n_workers = 2, seed = 888)

  expect_identical(res_none$final_selected_model$items, res_threads$final_selected_model$items)
  expect_identical(res_none$final_selected_model$items, res_chunks$final_selected_model$items)

  expect_equal(res_none$final_selected_model$cv_sensitivity, res_threads$final_selected_model$cv_sensitivity, tolerance = 1e-10)
  expect_equal(res_none$final_selected_model$cv_sensitivity, res_chunks$final_selected_model$cv_sensitivity, tolerance = 1e-10)

  expect_identical(res_none$candidate_ranking$items, res_threads$candidate_ranking$items)
  expect_identical(res_none$candidate_ranking$items, res_chunks$candidate_ranking$items)

  # Check instrumentation
  expect_gt(.NCVROC_ROUTING_COUNTERS$threads_count, 0L)
})

test_that("ordinary cross_size_cv exactness matrix: (3) Cutoff-dependent (Youden & Accuracy) [none vs threads vs chunks]", {
  set.seed(999)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Youden
  .reset_routing_counters()
  res_y_none    <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "youden", parallel = "none", seed = 999)
  res_y_threads <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "youden", parallel = "threads", n_workers = 2, seed = 999)
  res_y_chunks  <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "youden", parallel = "chunks", n_workers = 2, seed = 999)

  expect_identical(res_y_none$final_selected_model$items, res_y_threads$final_selected_model$items)
  expect_identical(res_y_none$final_selected_model$items, res_y_chunks$final_selected_model$items)

  expect_equal(res_y_none$candidate_ranking$cv_youden, res_y_threads$candidate_ranking$cv_youden, tolerance = 1e-10)
  expect_equal(res_y_none$candidate_ranking$cv_youden, res_y_chunks$candidate_ranking$cv_youden, tolerance = 1e-10)

  expect_identical(res_y_none$candidate_ranking$items, res_y_threads$candidate_ranking$items)
  expect_identical(res_y_none$candidate_ranking$items, res_y_chunks$candidate_ranking$items)

  # Check instrumentation: chunk_psock_count was triggered
  expect_gt(.NCVROC_ROUTING_COUNTERS$chunk_psock_count, 0L)
  expect_gt(.NCVROC_ROUTING_COUNTERS$threads_count, 0L)
})

test_that("Boundary AUC tie test and prefer_fewer_items tie-breaking in constrained search", {
  y <- c(rep(1L, 10), rep(0L, 10))
  Q1 <- c(rep(2, 10), rep(0, 10))
  Q2 <- c(rep(1, 10), rep(0, 10))
  Q3 <- c(rep(1, 10), rep(0, 10))

  d <- data.frame(y = y, Q1 = Q1, Q2 = Q2, Q3 = Q3)

  res_fewer <- cross_size_cv(
    d, y, c("Q1", "Q2", "Q3"),
    model_sizes        = 1:2,
    selection_metric   = "auc",
    sensitivity_min    = 0.80,
    prefer_fewer_items = TRUE,
    folds              = 2,
    seed               = 42
  )
  expect_equal(res_fewer$final_selected_model$n_items, 1L)
  expect_equal(res_fewer$final_selected_model$items, "Q1")
})

test_that("Deterministic no-feasible-candidate test", {
  d <- data.frame(
    y  = c(1L, 1L, 0L, 0L),
    Q1 = c(1,  0,  1,  0),
    Q2 = c(0,  1,  1,  0)
  )

  expect_error(
    cross_size_cv(d, y, c("Q1", "Q2"), model_sizes = 1:2, sensitivity_min = 1.0, specificity_min = 1.0, folds = 2, seed = 42),
    "No candidate models satisfied"
  )
})

test_that(".update_running_top_n prunes correctly and retains only top_n elements", {
  buf <- NULL
  c1 <- data.frame(items = "Q1", n_items = 1L, cv_youden = 0.50, .global_combo_index = 1L, stringsAsFactors = FALSE)
  c2 <- data.frame(items = "Q2", n_items = 1L, cv_youden = 0.80, .global_combo_index = 2L, stringsAsFactors = FALSE)
  c3 <- data.frame(items = "Q3", n_items = 1L, cv_youden = 0.60, .global_combo_index = 3L, stringsAsFactors = FALSE)
  c4 <- data.frame(items = "Q4", n_items = 1L, cv_youden = 0.70, .global_combo_index = 4L, stringsAsFactors = FALSE)

  buf <- .update_running_top_n(buf, c1, "cv_youden", top_n = 2L)
  buf <- .update_running_top_n(buf, c2, "cv_youden", top_n = 2L)
  buf <- .update_running_top_n(buf, c3, "cv_youden", top_n = 2L)
  buf <- .update_running_top_n(buf, c4, "cv_youden", top_n = 2L)

  expect_equal(nrow(buf), 2L)
  expect_equal(buf$items, c("Q2", "Q4"))
  expect_equal(buf$cv_youden, c(0.80, 0.70))
})

# =========================================================================
# PHASE 1: GENERALIZED ORDINARY K-FOLD ENGINE TESTS
# =========================================================================

test_that("Phase 1: arbitrary K validation (folds = 2, 5, 7, 10)", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  # A. folds = 2 works
  res2 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold", folds = 2, seed = 123)
  expect_s3_class(res2, "cross_size_cv_result")
  expect_equal(res2$settings$effective_folds, 2L)
  expect_equal(nrow(res2$fold_results), 2L)

  # B. folds = 5 works
  res5 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold", folds = 5, seed = 123)
  expect_s3_class(res5, "cross_size_cv_result")
  expect_equal(res5$settings$effective_folds, 5L)
  expect_equal(nrow(res5$fold_results), 5L)

  # C. folds = 10 works
  res10 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold", folds = 10, seed = 123)
  expect_s3_class(res10, "cross_size_cv_result")
  expect_equal(res10$settings$effective_folds, 10L)
  expect_equal(nrow(res10$fold_results), 10L)

  # D. custom folds = 7 works
  res7 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, cv_method = "kfold", folds = 7, seed = 123)
  expect_s3_class(res7, "cross_size_cv_result")
  expect_equal(res7$settings$effective_folds, 7L)
  expect_equal(nrow(res7$fold_results), 7L)
})

test_that("Phase 1: invalid folds error handling (folds < 2, folds >= N, non-integer scalar)", {
  d <- data.frame(
    y  = rep(c(1L, 0L), 10), # n = 20
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )
  n <- nrow(d)

  # E. folds = 1 errors
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = 1), "folds must satisfy 2 <= folds < n")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = 0), "folds must satisfy 2 <= folds < n")

  # F. folds = N errors
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = n), "folds must satisfy 2 <= folds < n")

  # G. folds > N errors
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = n + 5), "folds must satisfy 2 <= folds < n")

  # H. non-integer and non-finite scalar errors
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = 5.5), "integer-valued scalar")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = c(2, 5)), "integer-valued scalar")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = NA), "integer-valued scalar")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = Inf), "integer-valued scalar")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = -Inf), "integer-valued scalar")
  expect_error(cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = NaN), "integer-valued scalar")
})

test_that("Phase 1: full outcome and label validation errors", {
  # I. single-class full outcome errors
  d_single <- data.frame(
    y  = rep(1L, 20),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )
  expect_error(cross_size_cv(d_single, y, Q1:Q2, model_sizes = 1:2, folds = 5), "must contain both positive and negative cases")

  # Unexpected third label in outcome
  d_tri <- data.frame(
    y  = c(rep(0L, 10), rep(1L, 10), 2L),
    Q1 = sample(0:2, 21, replace = TRUE),
    Q2 = sample(0:2, 21, replace = TRUE)
  )
  expect_error(cross_size_cv(d_tri, y, Q1:Q2, model_sizes = 1:2, positive_label = 1, negative_label = 0), "Outcome contains values other than positive_label and negative_label")

  # identical positive and negative label error
  d_ok <- data.frame(
    y  = rep(c(1L, 0L), 10),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )
  expect_error(cross_size_cv(d_ok, y, Q1:Q2, model_sizes = 1:2, positive_label = 1, negative_label = 1), "distinct")
})

test_that("Phase 1: K > minority class count produces one-class test folds with NA AUC (not 0.5)", {
  # 3 positives, 22 negatives (N = 25), folds = 5
  # minority count = 3 < K = 5 < N = 25
  set.seed(42)
  d <- data.frame(
    y  = c(rep(1L, 3), rep(0L, 22)),
    Q1 = c(2, 2, 1, sample(0:1, 22, replace = TRUE)),
    Q2 = c(1, 2, 2, sample(0:1, 22, replace = TRUE))
  )

  # Stratified CV: 3 positives partitioned across 5 folds -> 3 folds get 1 positive, 2 folds get 0 positives
  folds_obj <- .build_cv_folds(d$y, cv_method = "kfold", folds = 5, seed = 42)
  expect_length(folds_obj, 5)

  # Check test fold class counts
  test_pos_counts <- vapply(folds_obj, function(idx) sum(d$y[idx] == 1L), integer(1))
  expect_equal(sort(unname(test_pos_counts)), c(0L, 0L, 1L, 1L, 1L))

  # Check that each training fold contains both classes (2 or 3 positives, 17 or 18 negatives)
  for (f_name in names(folds_obj)) {
    train_y <- d$y[-folds_obj[[f_name]]]
    expect_gt(sum(train_y == 1L), 0L)
    expect_gt(sum(train_y == 0L), 0L)
  }

  # Run cross_size_cv with folds = 5
  res <- cross_size_cv(d, y, Q1:Q2, model_sizes = 1:2, folds = 5, seed = 42)
  expect_s3_class(res, "cross_size_cv_result")

  # One-class test folds: fold_results has test_n_pos == 0 for 2 folds
  expect_equal(sum(res$fold_results$test_n_pos == 0L), 2L)

  # Compute AUC on one-class test fold scores directly: must be NA_real_ (never 0.5)
  one_class_test_idx <- folds_obj[[which(test_pos_counts == 0L)[1]]]
  one_class_scores <- rowSums(d[one_class_test_idx, c("Q1", "Q2"), drop = FALSE])
  one_class_freq <- compute_score_frequencies(one_class_scores, d$y[one_class_test_idx])
  one_class_auc <- compute_auc_from_table(one_class_freq$pos_counts, one_class_freq$neg_counts)
  expect_true(is.na(one_class_auc))
  expect_false(identical(one_class_auc, 0.5))

  # OOF predictions exist for all N = 25 observations
  expect_equal(nrow(res$oof_predictions), 25L)
  expect_true(all(c("predicted_score", "predicted_class", "applied_cutoff") %in% names(res$oof_predictions)))
  expect_false(anyNA(res$oof_predictions$predicted_class))

  # Overall classification metrics are computed without error
  expect_true(is.numeric(res$final_selected_model$cv_sensitivity))
  expect_true(is.numeric(res$final_selected_model$cv_specificity))
  expect_true(is.numeric(res$final_selected_model$cv_accuracy))
})

test_that("Phase 1: seed determinism and fold variation with seed", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  # L. Same seed produces identical results
  res_seed1a <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, folds = 4, seed = 999)
  res_seed1b <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, folds = 4, seed = 999)
  expect_identical(res_seed1a$final_selected_model, res_seed1b$final_selected_model)
  expect_equal(res_seed1a$oof_predictions$applied_cutoff, res_seed1b$oof_predictions$applied_cutoff)

  # M. Different seed alters fold assignment
  res_seed2 <- cross_size_cv(d, y, Q1:Q3, model_sizes = 1:2, folds = 4, seed = 888)
  expect_false(identical(res_seed1a$oof_predictions$fold_id, res_seed2$oof_predictions$fold_id))
})

test_that("Phase 1: parallel exactness on generalized K (folds = 4)", {
  set.seed(333)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # N. serial vs threads exactness
  res_none    <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, folds = 4, parallel = "none", seed = 333)
  res_threads <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, folds = 4, parallel = "threads", n_workers = 2, seed = 333)
  expect_identical(res_none$final_selected_model$items, res_threads$final_selected_model$items)
  expect_equal(res_none$candidate_ranking$auc, res_threads$candidate_ranking$auc, tolerance = 1e-10)

  # O. serial vs chunks exactness
  res_chunks  <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, folds = 4, parallel = "chunks", n_workers = 2, seed = 333)
  expect_identical(res_none$final_selected_model$items, res_chunks$final_selected_model$items)
  expect_equal(res_none$candidate_ranking$auc, res_chunks$candidate_ranking$auc, tolerance = 1e-10)
})

test_that("Phase 1: existing AUC selected model is preserved for standard 5-fold case", {
  set.seed(555)
  d <- data.frame(
    y  = sample(0:1, 50, replace = TRUE),
    Q1 = sample(0:2, 50, replace = TRUE),
    Q2 = sample(0:2, 50, replace = TRUE),
    Q3 = sample(0:2, 50, replace = TRUE),
    Q4 = sample(0:2, 50, replace = TRUE)
  )

  res5 <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc", folds = 5, seed = 555)
  # Mathematical identity: selected AUC matches exhaustive search top winner
  exh <- exhaustive_sum_roc(d, "y", c("Q1", "Q2", "Q3", "Q4"), min_items = 1, max_items = 3, rank_by = "auc", top_n = 1)
  expect_identical(res5$final_selected_model$items, exh$items[1])
  expect_equal(res5$final_selected_model$cv_auc, exh$auc[1], tolerance = 1e-10)
})

# =========================================================================
# PHASE 2: CROSS-SIZE LOOCV CORE ENGINE TESTS
# =========================================================================

test_that("Phase 2: LOOCV fold validation and minority class guards", {
  # C. repeats = 2 errors for LOOCV
  d_ok <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 10)),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )
  expect_error(cross_size_cv(d_ok, y, Q1:Q2, model_sizes = 1:2, cv_method = "loocv", repeats = 2), "repeats > 1 is not supported")

  # D. minority class count <= 1 errors for LOOCV (1 positive, 19 negatives)
  d_min1 <- data.frame(
    y  = c(1L, rep(0L, 19)),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE)
  )
  expect_error(cross_size_cv(d_min1, y, Q1:Q2, model_sizes = 1:2, cv_method = "loocv"), "at least 2 positive and 2 negative cases")

  # E. >= 2 positives and >= 2 negatives succeeds
  d_min2 <- data.frame(
    y  = c(1L, 1L, rep(0L, 18)),
    Q1 = c(2, 1, sample(0:2, 18, replace = TRUE)),
    Q2 = c(1, 2, sample(0:2, 18, replace = TRUE))
  )
  res_min2 <- cross_size_cv(d_min2, y, Q1:Q2, model_sizes = 1:2, cv_method = "loocv")
  expect_s3_class(res_min2, "cross_size_cv_result")
  expect_equal(res_min2$settings$effective_folds, 20L)
})

test_that("Phase 2: LOOCV AUC exact selection, score identity, and wrapper equivalence", {
  set.seed(789)
  d <- data.frame(
    y  = sample(0:1, 30, replace = TRUE),
    Q1 = sample(0:2, 30, replace = TRUE),
    Q2 = sample(0:2, 30, replace = TRUE),
    Q3 = sample(0:2, 30, replace = TRUE),
    Q4 = sample(0:2, 30, replace = TRUE)
  )

  # F. AUC winner matches exhaustive search
  res_loocv <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:3, cv_method = "loocv", selection_metric = "auc")
  exh <- exhaustive_sum_roc(d, "y", paste0("Q", 1:4), min_items = 1, max_items = 3, rank_by = "auc", top_n = 1)
  expect_identical(res_loocv$final_selected_model$items, exh$items[1])
  expect_equal(res_loocv$final_selected_model$cv_auc, exh$auc[1], tolerance = 1e-10)

  # G. OOF score identity: selected candidate OOF scores sorted by row_index == full-data sum scores
  sel_items <- strsplit(res_loocv$final_selected_model$items, ",\\s*")[[1]]
  full_scores <- rowSums(d[, sel_items, drop = FALSE])
  oof_sorted <- res_loocv$oof_predictions[order(res_loocv$oof_predictions$row_index), ]
  expect_equal(oof_sorted$predicted_score, full_scores)

  # H. Pooled selected-candidate OOF AUC == full-data apparent AUC
  oof_freq <- compute_score_frequencies(oof_sorted$predicted_score, oof_sorted$true_outcome)
  oof_auc <- compute_auc_from_table(oof_freq$pos_counts, oof_freq$neg_counts)
  expect_equal(res_loocv$final_selected_model$cv_auc, oof_auc, tolerance = 1e-10)

  # J. N rows of OOF predictions exactly once each
  expect_equal(nrow(res_loocv$oof_predictions), 30L)
  expect_equal(sort(res_loocv$oof_predictions$row_index), seq_len(30))

  # L. Classification metrics calculated from all N OOF predictions
  expect_true(is.numeric(res_loocv$final_selected_model$cv_sensitivity))
  expect_true(is.numeric(res_loocv$final_selected_model$cv_specificity))
  expect_true(is.numeric(res_loocv$final_selected_model$cv_accuracy))

  # M. cross_size_loocv wrapper equivalence
  res_wrap <- cross_size_loocv(d, y, Q1:Q4, model_sizes = 1:3, selection_metric = "auc")
  expect_identical(res_loocv$final_selected_model, res_wrap$final_selected_model)
  expect_identical(res_loocv$candidate_ranking, res_wrap$candidate_ranking)
  expect_identical(res_loocv$model_size_summary, res_wrap$model_size_summary)
  expect_equal(res_loocv$oof_predictions$applied_cutoff, res_wrap$oof_predictions$applied_cutoff)

  # N. Discontinuous model_sizes = c(1, 3) works under LOOCV
  res_disc <- cross_size_loocv(d, y, Q1:Q4, model_sizes = c(1, 3), selection_metric = "auc")
  expect_equal(res_disc$model_sizes, c(1L, 3L))
  expect_equal(res_disc$model_size_summary$n_items, c(1L, 3L))

  # O. Same AUC selected model across serial / threads / chunks under LOOCV
  res_th <- cross_size_loocv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", parallel = "threads", n_workers = 2)
  res_ch <- cross_size_loocv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "auc", parallel = "chunks", n_workers = 2)
  expect_identical(res_loocv$final_selected_model$items, res_th$final_selected_model$items)
  expect_identical(res_loocv$final_selected_model$items, res_ch$final_selected_model$items)
})

test_that("Phase 2: training-derived cutoff leakage protection test in LOOCV", {
  # K. Training-derived cutoff leakage test:
  # In LOOCV fold for row 1, changing y[1] from 0 to 1 MUST NOT alter the training cutoff derived from rows 2:N
  set.seed(101)
  d1 <- data.frame(
    y  = c(0L, rep(1L, 10), rep(0L, 10)), # row 1 is 0
    Q1 = c(2, sample(0:2, 20, replace = TRUE)),
    Q2 = c(1, sample(0:2, 20, replace = TRUE))
  )
  d2 <- d1
  d2$y[1] <- 1L # row 1 inverted to 1, rows 2:21 unchanged

  folds1 <- .build_loocv_folds(nrow(d1))
  folds2 <- .build_loocv_folds(nrow(d2))

  # Run fixed model CV on fold 1
  res1 <- .run_fixed_model_cv(c("Q1", "Q2"), d1, d1$y, folds1[1], cutoff_method = "youden")
  res2 <- .run_fixed_model_cv(c("Q1", "Q2"), d2, d2$y, folds2[1], cutoff_method = "youden")

  # Training-derived cutoff for fold 1 MUST be strictly identical
  expect_equal(res1$fold_results$train_cutoff[1], res2$fold_results$train_cutoff[1])
  expect_equal(res1$oof_predictions$applied_cutoff[1], res2$oof_predictions$applied_cutoff[1])
})

# =========================================================================
# PHASE 2.1: CUTOFF-DEPENDENT LOOCV REFERENCE EQUALITY & CORRECTNESS TESTS
# =========================================================================

test_that("Phase 2.1: cutoff-dependent LOOCV matches explicit serial reference loop", {
  set.seed(42)
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 15)), # N = 25 (10 pos, 15 neg)
    Q1 = sample(0:2, 25, replace = TRUE),
    Q2 = sample(0:2, 25, replace = TRUE),
    Q3 = sample(0:2, 25, replace = TRUE),
    Q4 = sample(0:2, 25, replace = TRUE)
  )
  item_pool <- paste0("Q", 1:4)
  n <- nrow(d)
  sizes <- 1:2

  # Explicit Reference Loop for every candidate combination
  # Build all combinations for sizes 1:2
  all_combos <- list()
  for (s in sizes) {
    cmb_mat <- utils::combn(item_pool, s, simplify = FALSE)
    all_combos <- c(all_combos, cmb_mat)
  }

  calc_ref_candidate <- function(combo_items, selection_rule = "youden") {
    pred_cls <- integer(n)
    cutoffs  <- numeric(n)
    for (i in seq_len(n)) {
      train_d <- d[-i, ]
      test_d  <- d[i, ]

      tr_scores <- rowSums(train_d[, combo_items, drop = FALSE])
      tr_freq   <- compute_score_frequencies(tr_scores, train_d$y)
      tr_roc    <- compute_roc_metrics_from_table(tr_freq$pos_counts, tr_freq$neg_counts)
      tr_cut    <- find_optimal_cutoff(tr_roc, method = selection_rule)$cutoff
      cutoffs[i] <- tr_cut

      te_score  <- sum(test_d[1, combo_items])
      pred_cls[i] <- as.integer(te_score >= tr_cut)
    }

    # Aggregate all N binary predictions
    tp <- sum(pred_cls == 1L & d$y == 1L)
    tn <- sum(pred_cls == 0L & d$y == 0L)
    fp <- sum(pred_cls == 1L & d$y == 0L)
    fn <- sum(pred_cls == 0L & d$y == 1L)

    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    acc  <- (tp + tn) / n
    youd <- sens + spec - 1

    list(
      items       = paste(combo_items, collapse = ", "),
      n_items     = length(combo_items),
      sensitivity = sens,
      specificity = spec,
      youden      = youd,
      accuracy    = acc,
      cutoff_mean = mean(cutoffs),
      cutoff_sd   = stats::sd(cutoffs)
    )
  }

  ref_evals <- lapply(all_combos, calc_ref_candidate, selection_rule = "youden")
  ref_df <- do.call(rbind, lapply(ref_evals, as.data.frame))

  # Rank reference by Youden desc, n_items asc
  ref_df_ranked <- ref_df[order(-ref_df$youden, ref_df$n_items), ]
  rownames(ref_df_ranked) <- NULL

  # 1. Test LOOCV with selection_metric = "youden"
  res_youden <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv", selection_metric = "youden")
  expect_s3_class(res_youden, "cross_size_cv_result")

  # Selected model matches reference winner
  expect_identical(res_youden$final_selected_model$items, ref_df_ranked$items[1])
  expect_equal(res_youden$final_selected_model$cv_youden, ref_df_ranked$youden[1], tolerance = 1e-10)
  expect_equal(res_youden$final_selected_model$cv_sensitivity, ref_df_ranked$sensitivity[1], tolerance = 1e-10)
  expect_equal(res_youden$final_selected_model$cv_specificity, ref_df_ranked$specificity[1], tolerance = 1e-10)

  # Candidate ranking top matches reference ranking
  expect_identical(res_youden$candidate_ranking$items[1:5], ref_df_ranked$items[1:5])
  expect_equal(res_youden$candidate_ranking$cv_youden[1:5], ref_df_ranked$youden[1:5], tolerance = 1e-10)

  # 2. Test LOOCV with selection_metric = "accuracy"
  ref_evals_acc <- lapply(all_combos, calc_ref_candidate, selection_rule = "youden")
  ref_df_acc <- do.call(rbind, lapply(ref_evals_acc, as.data.frame))
  ref_acc_ranked <- ref_df_acc[order(-ref_df_acc$accuracy, ref_df_acc$n_items), ]
  rownames(ref_acc_ranked) <- NULL

  res_acc <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv", selection_metric = "accuracy")
  expect_identical(res_acc$final_selected_model$items, ref_acc_ranked$items[1])
  expect_equal(res_acc$final_selected_model$cv_accuracy, ref_acc_ranked$accuracy[1], tolerance = 1e-10)

  # 3. Test LOOCV with selection_metric = "sensitivity" and "specificity"
  res_sens <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv", selection_metric = "sensitivity")
  expect_s3_class(res_sens, "cross_size_cv_result")
  expect_true(is.numeric(res_sens$final_selected_model$cv_sensitivity))

  res_spec <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv", selection_metric = "specificity")
  expect_s3_class(res_spec, "cross_size_cv_result")
  expect_true(is.numeric(res_spec$final_selected_model$cv_specificity))

  # 4. Parallel consistency for cutoff-dependent LOOCV (chunks vs serial)
  res_ch <- cross_size_cv(d, y, Q1:Q4, model_sizes = 1:2, cv_method = "loocv", selection_metric = "youden",
                          parallel = "chunks", n_workers = 2)
  expect_identical(res_youden$final_selected_model$items, res_ch$final_selected_model$items)
  expect_equal(res_youden$candidate_ranking$cv_youden, res_ch$candidate_ranking$cv_youden, tolerance = 1e-10)

  # 5. Direct call vs cross_size_loocv wrapper settings consistency
  res_wrap_youd <- cross_size_loocv(d, y, Q1:Q4, model_sizes = 1:2, selection_metric = "youden")
  expect_identical(res_youden$settings$stratified, FALSE)
  expect_identical(res_wrap_youd$settings$stratified, FALSE)
  expect_identical(res_youden$final_selected_model, res_wrap_youd$final_selected_model)
})
