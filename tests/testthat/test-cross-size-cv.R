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
