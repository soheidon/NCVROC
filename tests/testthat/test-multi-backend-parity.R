# test-multi-backend-parity.R — Phase 6 Multi-Backend Parity Sweep

test_that("Phase 6: cross_size_cv backend parity for sensitivity_target and clinical_constraint", {
  df <- make_test_data()

  # 1. sensitivity_target across none, threads, chunks
  res_none_sens <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "none",
    seed = 42,
    progress = FALSE
  )

  res_threads_sens <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "threads",
    n_workers = 2,
    seed = 42,
    progress = FALSE
  )

  res_chunks_sens <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "chunks",
    n_workers = 2,
    seed = 42,
    progress = FALSE
  )

  expect_identical(res_none_sens$final_selected_model$items, res_threads_sens$final_selected_model$items)
  expect_identical(res_none_sens$final_selected_model$items, res_chunks_sens$final_selected_model$items)
  expect_equal(res_none_sens$final_full_data_cutoff, res_threads_sens$final_full_data_cutoff, tolerance = 1e-8)
  expect_equal(res_none_sens$final_full_data_cutoff, res_chunks_sens$final_full_data_cutoff, tolerance = 1e-8)
  expect_equal(res_none_sens$candidate_ranking$cv_youden, res_threads_sens$candidate_ranking$cv_youden, tolerance = 1e-8)
  expect_equal(res_none_sens$candidate_ranking$cv_youden, res_chunks_sens$candidate_ranking$cv_youden, tolerance = 1e-8)

  # 2. clinical_constraint across none, threads, chunks
  res_none_clin <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "clinical_constraint",
    sensitivity_min = 0.6,
    specificity_min = 0.6,
    parallel = "none",
    seed = 42,
    progress = FALSE
  )

  res_threads_clin <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "clinical_constraint",
    sensitivity_min = 0.6,
    specificity_min = 0.6,
    parallel = "threads",
    n_workers = 2,
    seed = 42,
    progress = FALSE
  )

  res_chunks_clin <- cross_size_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    cutoff_method = "clinical_constraint",
    sensitivity_min = 0.6,
    specificity_min = 0.6,
    parallel = "chunks",
    n_workers = 2,
    seed = 42,
    progress = FALSE
  )

  expect_identical(res_none_clin$final_selected_model$items, res_threads_clin$final_selected_model$items)
  expect_identical(res_none_clin$final_selected_model$items, res_chunks_clin$final_selected_model$items)
  expect_equal(res_none_clin$final_full_data_cutoff, res_threads_clin$final_full_data_cutoff, tolerance = 1e-8)
  expect_equal(res_none_clin$final_full_data_cutoff, res_chunks_clin$final_full_data_cutoff, tolerance = 1e-8)
})

test_that("Phase 6: cross_size_nested_cv backend parity across none, outer, threads, chunks, hybrid", {
  df <- make_test_data()

  # Use 3 outer folds, 2 inner folds
  res_none <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "none",
    seed = 123,
    progress = FALSE
  )

  res_outer <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "outer",
    n_workers = 2,
    seed = 123,
    progress = FALSE
  )

  res_threads <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "threads",
    n_workers = 2,
    seed = 123,
    progress = FALSE
  )

  res_chunks <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "chunks",
    n_workers = 2,
    seed = 123,
    progress = FALSE
  )

  res_hybrid <- cross_size_nested_cv(
    df, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_folds = 3, inner_folds = 2, outer_repeats = 1,
    cutoff_method = "sensitivity_target",
    sensitivity_min = 0.7,
    parallel = "hybrid",
    n_workers = 2,
    threads_per_worker = 1,
    seed = 123,
    progress = FALSE
  )

  # Check outer_fold_results parity
  expect_identical(res_none$outer_fold_results$selected_items, res_outer$outer_fold_results$selected_items)
  expect_identical(res_none$outer_fold_results$selected_items, res_threads$outer_fold_results$selected_items)
  expect_identical(res_none$outer_fold_results$selected_items, res_chunks$outer_fold_results$selected_items)
  expect_identical(res_none$outer_fold_results$selected_items, res_hybrid$outer_fold_results$selected_items)

  expect_equal(res_none$outer_fold_results$outer_auc, res_outer$outer_fold_results$outer_auc, tolerance = 1e-8)
  expect_equal(res_none$outer_fold_results$outer_auc, res_threads$outer_fold_results$outer_auc, tolerance = 1e-8)
  expect_equal(res_none$outer_fold_results$outer_auc, res_chunks$outer_fold_results$outer_auc, tolerance = 1e-8)
  expect_equal(res_none$outer_fold_results$outer_auc, res_hybrid$outer_fold_results$outer_auc, tolerance = 1e-8)
})
