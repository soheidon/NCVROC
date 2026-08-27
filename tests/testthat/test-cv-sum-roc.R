# test-cv-sum-roc.R — Unit tests for cv_sum_roc() and loocv_sum_roc()

test_that("cv_sum_roc basic k-fold execution works with NSE and standard names", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 50, replace = TRUE),
    Q1 = sample(0:2, 50, replace = TRUE),
    Q2 = sample(0:2, 50, replace = TRUE),
    Q3 = sample(0:2, 50, replace = TRUE),
    Q4 = sample(0:2, 50, replace = TRUE)
  )

  res <- cv_sum_roc(d, y, Q1:Q3, folds = 5, repeats = 1, seed = 42)

  expect_s3_class(res, "cv_sum_roc_result")
  expect_equal(res$n_items, 3L)
  expect_equal(res$items, c("Q1", "Q2", "Q3"))
  expect_equal(nrow(res$oof_predictions), 50)
  expect_equal(nrow(res$fold_results), 5)
  expect_true(is.numeric(res$final_full_data_cutoff))
  expect_true(is.list(res$cv_cutoff_distribution))
  expect_length(res$cv_cutoff_distribution$per_fold_values, 5)
  expect_true(is.data.frame(res$summary))
  expect_true(all(c("auc", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv") %in% res$summary$metric))

  # Print / summary methods work without error
  expect_output(print(res), "Fixed-Model Cross-Validation")
  expect_output(summary(res), "Out-of-Fold")
})

test_that("cv_sum_roc repeated k-fold produces repeat-level metrics and sd", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    Q1 = sample(0:2, 60, replace = TRUE),
    Q2 = sample(0:2, 60, replace = TRUE),
    Q3 = sample(0:2, 60, replace = TRUE)
  )

  res_rep <- cv_sum_roc(d, "y", c("Q1", "Q2"), folds = 3, repeats = 3, seed = 42)

  expect_equal(nrow(res_rep$oof_predictions), 60 * 3)
  expect_equal(nrow(res_rep$repeat_metrics), 3)
  expect_equal(nrow(res_rep$fold_results), 9)

  # Check that per_fold_values length is exactly 3 folds * 3 repeats = 9
  expect_length(res_rep$cv_cutoff_distribution$per_fold_values, 9)

  # Mathematical property of fixed sum-score models:
  # In each repeat, the pooled OOF score vector is identical to the full-data score vector,
  # so OOF AUC is IDENTICAL across all repeats, leading to AUC SD = 0!
  auc_sd <- res_rep$summary$sd[res_rep$summary$metric == "auc"]
  expect_equal(auc_sd, 0, tolerance = 1e-10)

  # Check oof_predictions schema
  expected_cols <- c("row_index", "repeat_id", "fold_id", "true_outcome",
                     "predicted_score", "predicted_class", "applied_cutoff")
  expect_true(all(expected_cols %in% names(res_rep$oof_predictions)))
})

test_that("loocv_sum_roc works and satisfies mathematical OOF AUC identity", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE)
  )

  # Run loocv_sum_roc
  res_loo <- loocv_sum_roc(d, y, Q1:Q2)

  expect_s3_class(res_loo, "cv_sum_roc_result")
  expect_equal(res_loo$cv_method, "loocv")
  expect_equal(nrow(res_loo$oof_predictions), 40)
  expect_equal(nrow(res_loo$fold_results), 40)
  expect_length(res_loo$cv_cutoff_distribution$per_fold_values, 40)

  # Verify every single observation was tested exactly once with test size = 1
  expect_equal(sort(res_loo$oof_predictions$row_index), 1:40)
  expect_true(all(res_loo$fold_results$n_test == 1L))

  # Mathematical identity check:
  # In fixed sum-score models, pooled OOF score equals full-data score vector,
  # so pooled OOF AUC MUST exactly equal full-data apparent AUC!
  oof_auc <- res_loo$summary$mean[res_loo$summary$metric == "auc"]
  full_auc <- res_loo$final_full_data_metrics$auc
  expect_equal(oof_auc, full_auc, tolerance = 1e-10)

  # Rejection of repeats > 1
  expect_error(cv_sum_roc(d, y, Q1:Q2, cv_method = "loocv", repeats = 2), "repeats > 1")
})

test_that("Strict deterministic single-fold isolation cutoff leakage regression test", {
  # Deterministic test setup without randomness:
  # 1. Training set: 4 observations
  #    - 2 controls: Q1 = 0, Q2 = 1 (sum score = 1, y = 0)
  #    - 2 cases:    Q1 = 2, Q2 = 2 (sum score = 4, y = 1)
  #    Optimal Youden cutoff is unambiguously 4 (Sens = 1.0, Spec = 1.0, Youden = 1.0).
  # 2. Test set A: 2 observations
  #    - obs 1: Q1 = 2, Q2 = 2 (sum score = 4, true y = 1) -> predicted_class = 1 (TP = 1)
  #    - obs 2: Q1 = 0, Q2 = 1 (sum score = 1, true y = 0) -> predicted_class = 0 (TN = 1)
  #    Sens = 1.0, Spec = 1.0.
  # 3. Test set B: identical predictors as A, but outcome labels INVERTED:
  #    - obs 1: sum score = 4, true y = 0 -> predicted_class = 1 (FP = 1)
  #    - obs 2: sum score = 1, true y = 1 -> predicted_class = 0 (FN = 1)
  #    Sens = 0.0, Spec = 0.0.

  d_train <- data.frame(
    y  = c(0L, 0L, 1L, 1L),
    Q1 = c(0, 0, 2, 2),
    Q2 = c(1, 1, 2, 2)
  )

  d_test_A <- data.frame(
    y  = c(1L, 0L),
    Q1 = c(2, 0),
    Q2 = c(2, 1)
  )

  d_test_B <- data.frame(
    y  = c(0L, 1L),  # inverted outcome labels
    Q1 = c(2, 0),
    Q2 = c(2, 1)
  )

  d_combined_A <- rbind(d_train, d_test_A)
  d_combined_B <- rbind(d_train, d_test_B)

  custom_folds <- list(Rep1_Fold1 = 5:6)

  res_A <- .run_fixed_model_cv(c("Q1", "Q2"), d_combined_A, d_combined_A$y, custom_folds, cutoff_method = "youden")
  res_B <- .run_fixed_model_cv(c("Q1", "Q2"), d_combined_B, d_combined_B$y, custom_folds, cutoff_method = "youden")

  # 1. Training-derived cutoff must be BIT-EXACT identical (4 in both)
  expect_identical(res_A$fold_results$train_cutoff, 4)
  expect_identical(res_B$fold_results$train_cutoff, 4)
  expect_identical(res_A$fold_results$train_cutoff, res_B$fold_results$train_cutoff)
  expect_identical(res_A$oof_predictions$applied_cutoff, res_B$oof_predictions$applied_cutoff)

  # 2. Predicted scores and predicted classes must be identical
  expect_equal(res_A$oof_predictions$predicted_score, c(4, 1))
  expect_equal(res_B$oof_predictions$predicted_score, c(4, 1))
  expect_equal(res_A$oof_predictions$predicted_class, c(1L, 0L))
  expect_equal(res_B$oof_predictions$predicted_class, c(1L, 0L))

  # 3. Test classification metrics must strictly reflect the inverted test outcomes
  agg_A <- .aggregate_oof_metrics(res_A$oof_predictions, repeats = 1, fold_results = res_A$fold_results)
  agg_B <- .aggregate_oof_metrics(res_B$oof_predictions, repeats = 1, fold_results = res_B$fold_results)

  sens_A <- agg_A$summary$mean[agg_A$summary$metric == "sensitivity"]
  spec_A <- agg_A$summary$mean[agg_A$summary$metric == "specificity"]
  sens_B <- agg_B$summary$mean[agg_B$summary$metric == "sensitivity"]
  spec_B <- agg_B$summary$mean[agg_B$summary$metric == "specificity"]

  expect_equal(sens_A, 1.0)
  expect_equal(spec_A, 1.0)
  expect_equal(sens_B, 0.0)
  expect_equal(spec_B, 0.0)
})

test_that("cv_sum_roc CI computation works when ci = TRUE", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 60, replace = TRUE),
    Q1 = sample(0:2, 60, replace = TRUE),
    Q2 = sample(0:2, 60, replace = TRUE)
  )

  res_ci <- cv_sum_roc(d, y, Q1:Q2, folds = 3, ci = TRUE, conf_level = 0.95, seed = 42)
  expect_true("auc_lower" %in% names(res_ci$final_full_data_metrics))
  expect_true("sensitivity_lower" %in% names(res_ci$final_full_data_metrics))
  expect_true(res_ci$final_full_data_metrics$auc_lower <= res_ci$final_full_data_metrics$auc)
  expect_true(res_ci$final_full_data_metrics$auc_upper >= res_ci$final_full_data_metrics$auc)
})
