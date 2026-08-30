# tests/testthat/test-nested-rank-bounded-evaluator.R
# Phase A: Rank-Bounded Nested Evaluation Primitive Verification

test_that("rank-bounded nested evaluator reproduces candidate-level evaluation work exactly", {
  set.seed(42)
  n <- 60
  p <- 6
  items <- paste0("Q", seq_len(p))
  dat <- data.frame(
    matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p),
    y = rbinom(n, 1, 0.5)
  )
  names(dat)[seq_len(p)] <- items

  outer_k <- 3
  inner_k <- 3
  seed <- 12345

  outer_folds <- make_stratified_folds(dat$y, k = outer_k, seed = seed)

  # Select contiguous and non-contiguous candidate ranks across model sizes 1 to 3
  # Size 1: choose(6, 1) = 6  (ranks 0..5)
  # Size 2: choose(6, 2) = 15 (ranks 0..14)
  # Size 3: choose(6, 3) = 20 (ranks 0..19)
  candidate_ranks <- data.frame(
    model_size = c(1L, 1L, 2L, 2L, 2L, 3L, 3L),
    combination_index_0based = c(0L, 5L, 0L, 7L, 14L, 0L, 19L)
  )

  # 1. Run rank-bounded nested evaluator
  rb_res <- .nested_evaluate_candidate_ranks(
    data            = dat,
    outcome         = dat$y,
    items           = items,
    candidate_ranks = candidate_ranks,
    outer_folds     = outer_folds,
    inner_k         = inner_k,
    cutoff_method   = "youden",
    seed            = seed
  )

  expect_equal(nrow(rb_res$candidate_metrics), nrow(candidate_ranks))
  expect_equal(length(rb_res$fold_metrics), outer_k)
  expect_equal(rb_res$n_candidates_evaluated, nrow(candidate_ranks) * outer_k)

  # 2. Directly evaluate each candidate on outer fold 1 and verify candidate-level equivalence
  f_out <- 1L
  test_idx <- outer_folds[[f_out]]
  train_idx <- setdiff(seq_len(n), test_idx)
  x_train <- as.matrix(dat[train_idx, items, drop = FALSE])
  y_train <- dat$y[train_idx]
  fold_seed <- seed + f_out

  direct_fold1_list <- lapply(seq_len(nrow(candidate_ranks)), function(ci) {
    msize <- candidate_ranks$model_size[ci]
    rank0 <- candidate_ranks$combination_index_0based[ci]
    cols_0based <- .combination_unrank(p, msize, rank0)
    cols_1based <- cols_0based + 1L

    in_folds <- make_stratified_folds(y_train, k = inner_k, seed = fold_seed)
    total_tp <- 0L
    total_tn <- 0L
    total_fp <- 0L
    total_fn <- 0L

    for (fi in seq_along(in_folds)) {
      in_test <- in_folds[[fi]]
      in_train <- setdiff(seq_along(y_train), in_test)

      # Optimal cutoff on inner train
      tr_scores <- rowSums(x_train[in_train, cols_1based, drop = FALSE])
      tr_freq <- compute_score_frequencies(tr_scores, y_train[in_train])
      tr_roc <- compute_roc_metrics_from_table(tr_freq$pos_counts, tr_freq$neg_counts)
      opt <- find_optimal_cutoff(tr_roc, method = "youden")
      cutoff_val <- opt$cutoff

      # Validation on inner test
      val_scores <- rowSums(x_train[in_test, cols_1based, drop = FALSE])
      val_y <- y_train[in_test]
      pred_c <- ifelse(val_scores >= cutoff_val, 1L, 0L)

      val_freq <- compute_score_frequencies(val_scores, val_y)
      val_auc <- compute_auc_from_table(val_freq$pos_counts, val_freq$neg_counts)

      total_tp <- total_tp + sum(pred_c == 1L & val_y == 1L)
      total_tn <- total_tn + sum(pred_c == 0L & val_y == 0L)
      total_fp <- total_fp + sum(pred_c == 1L & val_y == 0L)
      total_fn <- total_fn + sum(pred_c == 0L & val_y == 1L)
    }

    full_scores <- rowSums(x_train[, cols_1based, drop = FALSE])
    full_freq <- compute_score_frequencies(full_scores, y_train)
    full_auc <- compute_auc_from_table(full_freq$pos_counts, full_freq$neg_counts)

    s_val <- if (total_tp + total_fn > 0) total_tp / (total_tp + total_fn) else NA_real_
    sp_val <- if (total_tn + total_fp > 0) total_tn / (total_tn + total_fp) else NA_real_
    y_val <- if (is.na(s_val) || is.na(sp_val)) NA_real_ else s_val + sp_val - 1.0
    a_val <- if (total_tp + total_tn + total_fp + total_fn > 0) (total_tp + total_tn) / (total_tp + total_tn + total_fp + total_fn) else NA_real_

    data.frame(
      mean_auc = full_auc,
      mean_sensitivity = s_val,
      mean_specificity = sp_val,
      mean_youden = y_val,
      mean_accuracy = a_val
    )
  })
  direct_fold1_df <- do.call(rbind, direct_fold1_list)
  rb_fold1_df <- rb_res$fold_metrics[[f_out]]

  # Bitwise equivalence for all evaluated candidate metrics
  expect_equal(rb_fold1_df$mean_auc, direct_fold1_df$mean_auc)
  expect_equal(rb_fold1_df$mean_sensitivity, direct_fold1_df$mean_sensitivity)
  expect_equal(rb_fold1_df$mean_specificity, direct_fold1_df$mean_specificity)
  expect_equal(rb_fold1_df$mean_youden, direct_fold1_df$mean_youden)
  expect_equal(rb_fold1_df$mean_accuracy, direct_fold1_df$mean_accuracy)
})

test_that("rank-bounded evaluator preserves RNG state when wrapped with RNG preservation", {
  set.seed(999)
  n <- 50
  p <- 5
  dat <- data.frame(
    matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p),
    y = rbinom(n, 1, 0.5)
  )
  names(dat)[seq_len(p)] <- paste0("Q", seq_len(p))

  outer_folds <- make_stratified_folds(dat$y, k = 3, seed = 123)
  candidate_ranks <- data.frame(
    model_size = c(1L, 2L, 2L),
    combination_index_0based = c(0L, 0L, 9L)
  )

  rng_before <- .Random.seed

  .planner_with_preserved_rng({
    res <- .nested_evaluate_candidate_ranks(
      data            = dat,
      outcome         = dat$y,
      items           = names(dat)[seq_len(p)],
      candidate_ranks = candidate_ranks,
      outer_folds     = outer_folds,
      inner_k         = 3,
      seed            = 555
    )
  })

  rng_after <- .Random.seed
  expect_identical(rng_before, rng_after)
})

test_that("rank-bounded evaluator evaluates strictly requested candidate workload", {
  set.seed(123)
  n <- 50
  p <- 8
  dat <- data.frame(
    matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p),
    y = rbinom(n, 1, 0.5)
  )
  items <- paste0("Q", seq_len(p))
  names(dat)[seq_len(p)] <- items

  outer_folds <- make_stratified_folds(dat$y, k = 4, seed = 777)

  # Total candidate space for 1:4 items from 8 is choose(8,1)+choose(8,2)+choose(8,3)+choose(8,4) = 8+28+56+70 = 162
  # We request only 4 specific candidates
  candidate_ranks <- data.frame(
    model_size = c(1L, 2L, 3L, 4L),
    combination_index_0based = c(0L, 10L, 25L, 50L)
  )

  res <- .nested_evaluate_candidate_ranks(
    data            = dat,
    outcome         = dat$y,
    items           = items,
    candidate_ranks = candidate_ranks,
    outer_folds     = outer_folds,
    inner_k         = 3,
    seed            = 777
  )

  # Exactly 4 candidates * 4 outer folds = 16 candidate evaluations
  expect_equal(res$n_candidates_evaluated, 16L)
  expect_equal(nrow(res$candidate_metrics), 4L)
})

test_that("HIGH-4: Planner output connects directly to rank-bounded nested evaluator using combination_index_0based", {
  set.seed(42)
  n <- 50
  p <- 6
  dat <- data.frame(
    matrix(rbinom(n * p, 1, 0.4), nrow = n, ncol = p),
    y = rbinom(n, 1, 0.5)
  )
  items <- paste0("Q", seq_len(p))
  names(dat)[seq_len(p)] <- items

  outer_folds <- make_stratified_folds(dat$y, k = 3, seed = 42)

  # 1. Generate pilot candidates from execution planner
  pilot <- .planner_make_pilot_candidates(n_items = p, model_sizes = 1:3, max_candidates = 6L, item_names = items)

  expect_true("combination_index_0based" %in% names(pilot))
  expect_false("rank_within_size" %in% names(pilot))

  # 2. Directly feed pilot candidates into .nested_evaluate_candidate_ranks
  res <- .nested_evaluate_candidate_ranks(
    data            = dat,
    outcome         = dat$y,
    items           = items,
    candidate_ranks = pilot,
    outer_folds     = outer_folds,
    inner_k         = 3,
    seed            = 42
  )

  expect_equal(nrow(res$candidate_metrics), nrow(pilot))
  expect_true(all(c("model_size", "combination_index_0based", "mean_auc", "mean_youden") %in% names(res$candidate_metrics)))
  expect_equal(res$n_candidates_evaluated, nrow(pilot) * 3L)
})
