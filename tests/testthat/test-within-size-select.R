# test-within-size-select.R — Tests for cv_select_sum_roc and loocv_select_sum_roc

test_that("cv_select_sum_roc matches cross_size_cv on k-fold selection (AUC and Youden)", {
  set.seed(123)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE),
    Q5 = sample(0:2, 40, replace = TRUE)
  )

  # 1. AUC Selection
  res_cv_sel <- cv_select_sum_roc(d, y, Q1:Q5, item_count = 3, folds = 4, selection_metric = "auc", seed = 777)
  res_direct <- cross_size_cv(d, y, Q1:Q5, model_sizes = 3, folds = 4, selection_metric = "auc", seed = 777)

  expect_s3_class(res_cv_sel, "cv_select_sum_roc_result")
  expect_s3_class(res_cv_sel, "cross_size_cv_result")
  expect_identical(res_cv_sel$final_selected_model$items, res_direct$final_selected_model$items)
  expect_equal(res_cv_sel$final_selected_model$cv_auc, res_direct$final_selected_model$cv_auc, tolerance = 1e-10)
  expect_identical(res_cv_sel$candidate_ranking$items, res_direct$candidate_ranking$items)
  expect_equal(res_cv_sel$candidate_ranking$cv_auc, res_direct$candidate_ranking$cv_auc, tolerance = 1e-10)
  expect_equal(res_cv_sel$oof_predictions$applied_cutoff, res_direct$oof_predictions$applied_cutoff)
  expect_equal(res_cv_sel$settings$selection_scope, "within_size")
  expect_equal(res_cv_sel$settings$item_count, 3L)

  # 2. Youden Selection
  res_cv_youd <- cv_select_sum_roc(d, y, Q1:Q5, item_count = 2, folds = 4, selection_metric = "youden", seed = 888)
  res_dir_youd <- cross_size_cv(d, y, Q1:Q5, model_sizes = 2, folds = 4, selection_metric = "youden", seed = 888)

  expect_identical(res_cv_youd$final_selected_model$items, res_dir_youd$final_selected_model$items)
  expect_equal(res_cv_youd$final_selected_model$cv_youden, res_dir_youd$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_equal(res_cv_youd$candidate_ranking$cv_youden, res_dir_youd$candidate_ranking$cv_youden, tolerance = 1e-10)

  # 3. Accuracy Selection
  res_cv_acc <- cv_select_sum_roc(d, y, Q1:Q5, item_count = 2, folds = 4, selection_metric = "accuracy", seed = 999)
  res_dir_acc <- cross_size_cv(d, y, Q1:Q5, model_sizes = 2, folds = 4, selection_metric = "accuracy", seed = 999)
  expect_identical(res_cv_acc$final_selected_model$items, res_dir_acc$final_selected_model$items)
  expect_equal(res_cv_acc$final_selected_model$cv_accuracy, res_dir_acc$final_selected_model$cv_accuracy, tolerance = 1e-10)
})

test_that("loocv_select_sum_roc matches cross_size_cv on LOOCV selection (AUC, Youden, and Accuracy)", {
  set.seed(456)
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 15)), # N = 25
    Q1 = sample(0:2, 25, replace = TRUE),
    Q2 = sample(0:2, 25, replace = TRUE),
    Q3 = sample(0:2, 25, replace = TRUE),
    Q4 = sample(0:2, 25, replace = TRUE)
  )

  # 1. LOOCV AUC Selection
  res_loo_sel <- loocv_select_sum_roc(d, y, Q1:Q4, item_count = 2, selection_metric = "auc")
  res_loo_dir <- cross_size_cv(d, y, Q1:Q4, model_sizes = 2, cv_method = "loocv", selection_metric = "auc")

  expect_s3_class(res_loo_sel, "loocv_select_sum_roc_result")
  expect_s3_class(res_loo_sel, "cv_select_sum_roc_result")
  expect_s3_class(res_loo_sel, "cross_size_cv_result")
  expect_identical(res_loo_sel$final_selected_model$items, res_loo_dir$final_selected_model$items)
  expect_equal(res_loo_sel$final_selected_model$cv_auc, res_loo_dir$final_selected_model$cv_auc, tolerance = 1e-10)
  expect_identical(res_loo_sel$candidate_ranking$items, res_loo_dir$candidate_ranking$items)
  expect_equal(res_loo_sel$candidate_ranking$cv_auc, res_loo_dir$candidate_ranking$cv_auc, tolerance = 1e-10)
  expect_equal(res_loo_sel$settings$selection_scope, "within_size")
  expect_equal(res_loo_sel$settings$item_count, 2L)
  expect_equal(res_loo_sel$settings$cv_method, "loocv")

  # 2. LOOCV Youden Selection
  res_loo_youd <- loocv_select_sum_roc(d, y, Q1:Q4, item_count = 2, selection_metric = "youden")
  res_dir_youd <- cross_size_cv(d, y, Q1:Q4, model_sizes = 2, cv_method = "loocv", selection_metric = "youden")

  expect_identical(res_loo_youd$final_selected_model$items, res_dir_youd$final_selected_model$items)
  expect_equal(res_loo_youd$final_selected_model$cv_youden, res_dir_youd$final_selected_model$cv_youden, tolerance = 1e-10)
  expect_equal(res_loo_youd$candidate_ranking$cv_youden, res_dir_youd$candidate_ranking$cv_youden, tolerance = 1e-10)
  expect_equal(res_loo_youd$oof_predictions$applied_cutoff, res_dir_youd$oof_predictions$applied_cutoff)
  expect_equal(res_loo_youd$cv_cutoff_distribution, res_dir_youd$cv_cutoff_distribution)

  # 3. LOOCV Accuracy Selection
  res_loo_acc <- loocv_select_sum_roc(d, y, Q1:Q4, item_count = 2, selection_metric = "accuracy")
  res_dir_acc <- cross_size_cv(d, y, Q1:Q4, model_sizes = 2, cv_method = "loocv", selection_metric = "accuracy")

  expect_identical(res_loo_acc$final_selected_model$items, res_dir_acc$final_selected_model$items)
  expect_equal(res_loo_acc$final_selected_model$cv_accuracy, res_dir_acc$final_selected_model$cv_accuracy, tolerance = 1e-10)
  expect_equal(res_loo_acc$candidate_ranking$cv_accuracy, res_dir_acc$candidate_ranking$cv_accuracy, tolerance = 1e-10)
  expect_equal(res_loo_acc$oof_predictions$applied_cutoff, res_dir_acc$oof_predictions$applied_cutoff)
  expect_equal(res_loo_acc$cv_cutoff_distribution, res_dir_acc$cv_cutoff_distribution)
})

test_that("item_count validation in within-size wrappers", {
  d <- data.frame(
    y  = c(rep(1L, 10), rep(0L, 10)),
    Q1 = sample(0:2, 20, replace = TRUE),
    Q2 = sample(0:2, 20, replace = TRUE),
    Q3 = sample(0:2, 20, replace = TRUE)
  )

  # Valid item_counts: 1, 2, 3
  res1 <- cv_select_sum_roc(d, y, Q1:Q3, item_count = 1, folds = 4)
  expect_equal(res1$settings$item_count, 1L)
  res3 <- cv_select_sum_roc(d, y, Q1:Q3, item_count = 3, folds = 4)
  expect_equal(res3$settings$item_count, 3L)

  # Invalid item_counts
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = 0), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = -1), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = 1.5), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = NA), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = Inf), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = -Inf), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = NaN), "positive integer-valued scalar")
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = c(1, 2)), "positive integer-valued scalar")

  # Exceeding available items
  expect_error(cv_select_sum_roc(d, y, Q1:Q3, item_count = 4), "cannot exceed the number of available items")
  expect_error(loocv_select_sum_roc(d, y, Q1:Q3, item_count = 5), "cannot exceed the number of available items")
})

test_that("within-size wrappers parallel consistency and print methods", {
  set.seed(654)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )

  # Parallel exactness in cv_select_sum_roc
  res_none <- cv_select_sum_roc(d, y, Q1:Q4, item_count = 2, folds = 4, parallel = "none", seed = 555)
  res_th   <- cv_select_sum_roc(d, y, Q1:Q4, item_count = 2, folds = 4, parallel = "threads", n_workers = 2, seed = 555)
  res_ch   <- cv_select_sum_roc(d, y, Q1:Q4, item_count = 2, folds = 4, parallel = "chunks", n_workers = 2, seed = 555)

  expect_identical(res_none$final_selected_model$items, res_th$final_selected_model$items)
  expect_identical(res_none$final_selected_model$items, res_ch$final_selected_model$items)
  expect_equal(res_none$candidate_ranking$cv_auc, res_th$candidate_ranking$cv_auc, tolerance = 1e-10)
  expect_equal(res_none$candidate_ranking$cv_auc, res_ch$candidate_ranking$cv_auc, tolerance = 1e-10)

  # Print and summary methods execute without error
  out_cv <- capture.output(print(res_none))
  expect_true(any(grepl("Within-Size CV Sum-Score Selection", out_cv)))
  expect_true(any(grepl("Within-size \\(2 items\\)", out_cv)))

  res_loo <- loocv_select_sum_roc(d, y, Q1:Q4, item_count = 2)
  out_loo <- capture.output(summary(res_loo))
  expect_true(any(grepl("Leave-One-Out \\(LOOCV\\)", out_loo)))
})
