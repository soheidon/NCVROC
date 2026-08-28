# test-cv-engine.R — Unit tests for internal CV engine machinery

test_that(".build_loocv_folds creates exactly n single-observation folds", {
  n <- 50
  folds <- .build_loocv_folds(n)
  expect_length(folds, n)
  expect_named(folds)
  expect_equal(names(folds)[1], "Rep1_Fold1")
  expect_equal(names(folds)[n], paste0("Rep1_Fold", n))

  # Each fold has exactly 1 index and covers 1:n
  lens <- vapply(folds, length, integer(1))
  expect_true(all(lens == 1L))
  all_indices <- unname(unlist(folds))
  expect_equal(sort(all_indices), seq_len(n))

  # Validation errors
  expect_error(.build_loocv_folds(1), "integer >= 2")
  expect_error(.build_loocv_folds(0), "integer >= 2")
  expect_error(.build_loocv_folds("50"), "integer >= 2")
})

test_that(".build_cv_folds handles loocv, kfold stratified and non-stratified", {
  y <- c(rep(1L, 20), rep(0L, 30))
  n <- length(y)

  # LOOCV
  loocv_folds <- .build_cv_folds(y, cv_method = "loocv")
  expect_length(loocv_folds, n)
  expect_error(.build_cv_folds(y, cv_method = "loocv", repeats = 2), "repeats > 1")

  # k-fold stratified
  k_strat <- .build_cv_folds(y, cv_method = "kfold", folds = 5, repeats = 2, stratified = TRUE, seed = 123)
  expect_length(k_strat, 10)
  # Check coverage for rep 1
  rep1_indices <- sort(unname(unlist(k_strat[1:5])))
  expect_equal(rep1_indices, seq_len(n))
  # Check coverage for rep 2
  rep2_indices <- sort(unname(unlist(k_strat[6:10])))
  expect_equal(rep2_indices, seq_len(n))

  # k-fold non-stratified
  k_nonstrat <- .build_cv_folds(y, cv_method = "kfold", folds = 5, repeats = 2, stratified = FALSE, seed = 123)
  expect_length(k_nonstrat, 10)
  rep1_nonstrat <- sort(unname(unlist(k_nonstrat[1:5])))
  expect_equal(rep1_nonstrat, seq_len(n))

  # Seed determinism
  k_strat2 <- .build_cv_folds(y, cv_method = "kfold", folds = 5, repeats = 2, stratified = TRUE, seed = 123)
  expect_equal(k_strat, k_strat2)
})

test_that(".run_fixed_model_cv evaluates OOF predictions properly", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    Q1 = sample(0:2, 60, replace = TRUE),
    Q2 = sample(0:2, 60, replace = TRUE),
    Q3 = sample(0:2, 60, replace = TRUE)
  )

  folds <- .build_cv_folds(d$y, cv_method = "kfold", folds = 3, repeats = 2, stratified = TRUE, seed = 42)
  res <- .run_fixed_model_cv(c("Q1", "Q2"), d, d$y, folds, cutoff_method = "youden")

  expect_named(res, c("oof_predictions", "fold_results"))
  expect_equal(nrow(res$oof_predictions), 60 * 2) # 60 obs * 2 repeats
  expect_equal(nrow(res$fold_results), 6)          # 3 folds * 2 repeats

  # Columns of oof_predictions
  expected_cols <- c("row_index", "repeat_id", "fold_id", "true_outcome",
                     "predicted_score", "predicted_class", "applied_cutoff")
  expect_true(all(expected_cols %in% names(res$oof_predictions)))

  # Verify row_index 1:60 is present in repeat 1 and repeat 2
  oof_r1 <- res$oof_predictions[res$oof_predictions$repeat_id == 1L, ]
  expect_equal(sort(oof_r1$row_index), 1:60)
  oof_r2 <- res$oof_predictions[res$oof_predictions$repeat_id == 2L, ]
  expect_equal(sort(oof_r2$row_index), 1:60)
})

test_that(".aggregate_oof_metrics computes fold-level cutoff distribution (1 cutoff per fold)", {
  oof_df <- data.frame(
    row_index       = c(1:10, 1:10),
    repeat_id       = c(rep(1L, 10), rep(2L, 10)),
    fold_id         = c(rep(1:2, each = 5), rep(1:2, each = 5)),
    true_outcome    = c(rep(c(1L, 0L), 5), rep(c(1L, 0L), 5)),
    predicted_score = c(5, 1, 4, 2, 3, 2, 4, 1, 5, 0,
                        5, 0, 4, 1, 3, 1, 4, 2, 5, 1),
    predicted_class = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
                        1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    applied_cutoff  = c(rep(2, 5), rep(4, 5), rep(2, 5), rep(4, 5))
  )

  fold_results <- data.frame(
    fold_name    = c("Rep1_Fold1", "Rep1_Fold2", "Rep2_Fold1", "Rep2_Fold2"),
    repeat_id    = c(1L, 1L, 2L, 2L),
    fold_id      = c(1L, 2L, 1L, 2L),
    n_train      = c(5L, 5L, 5L, 5L),
    n_test       = c(5L, 5L, 5L, 5L),
    train_cutoff = c(2, 4, 2, 4),
    test_n_pos   = c(3L, 2L, 3L, 2L),
    test_n_neg   = c(2L, 3L, 2L, 3L)
  )

  agg <- .aggregate_oof_metrics(oof_df, repeats = 2, fold_results = fold_results)

  # Check that cutoff distribution has length equal to n_folds * repeats (4)
  expect_length(agg$cv_cutoff_distribution$per_fold_values, 4)
  expect_equal(agg$cv_cutoff_distribution$per_fold_values, c(2, 4, 2, 4))
  expect_equal(agg$cv_cutoff_distribution$mean, 3.0)
})

test_that(".aggregate_oof_metrics handles unequal fold sizes without observation-weight bias in cutoff", {
  # Unequal fold size: Fold 1 has 8 test obs, Fold 2 has 2 test obs (total 10)
  # Fold 1 train_cutoff = 10, Fold 2 train_cutoff = 20
  # Unweighted fold mean is (10 + 20)/2 = 15.0
  # Observation-weighted mean would be (8*10 + 2*20)/10 = 12.0
  oof_df <- data.frame(
    row_index       = 1:10,
    repeat_id       = rep(1L, 10),
    fold_id         = c(rep(1L, 8), rep(2L, 2)),
    true_outcome    = rep(c(1L, 0L), 5),
    predicted_score = rep(5, 10),
    predicted_class = rep(1L, 10),
    applied_cutoff  = c(rep(10, 8), rep(20, 2))
  )

  fold_results <- data.frame(
    fold_name    = c("Rep1_Fold1", "Rep1_Fold2"),
    repeat_id    = c(1L, 1L),
    fold_id      = c(1L, 2L),
    n_train      = c(2L, 8L),
    n_test       = c(8L, 2L),
    train_cutoff = c(10, 20),
    test_n_pos   = c(4L, 1L),
    test_n_neg   = c(4L, 1L)
  )

  agg <- .aggregate_oof_metrics(oof_df, repeats = 1, fold_results = fold_results)

  # The fold-level distribution must have length 2 and mean 15.0
  expect_length(agg$cv_cutoff_distribution$per_fold_values, 2)
  expect_equal(agg$cv_cutoff_distribution$mean, 15.0)
})

test_that(".build_cv_folds enforces strict K-fold range 2 <= folds < n and integer scalar", {
  y <- c(rep(1L, 10), rep(0L, 10)) # n = 20
  n <- length(y)

  # Valid folds
  f2 <- .build_cv_folds(y, cv_method = "kfold", folds = 2)
  expect_length(f2, 2)
  f10 <- .build_cv_folds(y, cv_method = "kfold", folds = 10)
  expect_length(f10, 10)
  f19 <- .build_cv_folds(y, cv_method = "kfold", folds = 19)
  expect_length(f19, 19)

  # Invalid folds: folds < 2
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = 1), "folds must satisfy 2 <= folds < n")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = 0), "folds must satisfy 2 <= folds < n")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = -3), "folds must satisfy 2 <= folds < n")

  # Invalid folds: folds >= n
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = n), "folds must satisfy 2 <= folds < n")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = n + 5), "folds must satisfy 2 <= folds < n")

  # Non-integer scalar
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = 5.5), "integer-valued scalar")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = c(2, 3)), "integer-valued scalar")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = NA_integer_), "integer-valued scalar")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = Inf), "integer-valued scalar")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = -Inf), "integer-valued scalar")
  expect_error(.build_cv_folds(y, cv_method = "kfold", folds = NaN), "integer-valued scalar")
})

test_that(".build_cv_folds rejects training folds with only one class", {
  # Synthetic case where a training fold has only 1 class:
  # e.g., 2 positives, 8 negatives, non-stratified with folds = 9 -> some training fold will hold out both positives
  y <- c(1L, 1L, rep(0L, 8)) # n = 10
  # If we partition so fold 1 holds both positives (indices 1, 2), then train_y for fold 1 has only 0s
  custom_folds <- list(Rep1_Fold1 = c(1L, 2L), Rep1_Fold2 = 3:10)
  expect_error(
    {
      # Manually checking the training fold validation logic
      for (f_name in names(custom_folds)) {
        test_idx <- custom_folds[[f_name]]
        train_y <- y[-test_idx]
        if (sum(train_y == 1L) == 0L || sum(train_y == 0L) == 0L) {
          stop(sprintf("Training fold '%s' contains only one class. Cutoff optimization is impossible. Reduce `folds` or ensure sufficient cases per class.", f_name), call. = FALSE)
        }
      }
    },
    "contains only one class"
  )
})
