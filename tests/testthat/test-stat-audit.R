test_that("Nested CV data leakage metamorphic test: flipping Fold 1 outer-test labels leaves Fold 1 selection invariant", {
  set.seed(501)
  n <- 36L
  p <- 5L
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- c(rep(0L, 18L), rep(1L, 18L))
  X[19:36, 1] <- X[19:36, 1] + 1.2
  colnames(X) <- paste0("q", seq_len(p))
  df <- as.data.frame(X)
  df$y <- y

  # Obtain exact Fold 1 test indices using make_stratified_folds with same seed
  folds_ref <- NCVROC::make_stratified_folds(y, k = 3L, repeats = 1L, seed = 88L)
  fold1_test <- folds_ref[[1L]]

  # Flip predictor signs & spike q5 ONLY on Fold 1 outer-test rows so stratified
  # fold assignment remains identical while outer-test performance & preselect signal change
  df_flipped <- df
  df_flipped[fold1_test, colnames(X)] <- -df_flipped[fold1_test, colnames(X)]
  df_flipped$q5[fold1_test] <- ifelse(df_flipped$y[fold1_test] == 1L, 10, -10)

  for (cfg in list(
    list(parallel = "none", engine = "R", n_workers = 1L),
    list(parallel = "native_outer_fold", engine = "Rcpp", n_workers = 2L),
    list(parallel = "native_streaming", engine = "Rcpp", n_workers = 2L)
  )) {
    res_orig <- nested_sum_roc(
      data = df, outcome = "y", items = colnames(X),
      min_items = 1L, max_items = 2L,
      outer_k = 3L, inner_k = 2L, preselect_top_n = 2L,
      parallel = cfg$parallel, engine = cfg$engine, n_workers = cfg$n_workers,
      seed = 88L, progress = FALSE, verbose = FALSE
    )
    res_flip <- nested_sum_roc(
      data = df_flipped, outcome = "y", items = colnames(X),
      min_items = 1L, max_items = 2L,
      outer_k = 3L, inner_k = 2L, preselect_top_n = 2L,
      parallel = cfg$parallel, engine = cfg$engine, n_workers = cfg$n_workers,
      seed = 88L, progress = FALSE, verbose = FALSE
    )
    f1_orig <- res_orig$outer_results[[1L]]
    f1_flip <- res_flip$outer_results[[1L]]
    expect_identical(f1_flip$predictions$row_index, f1_orig$predictions$row_index)
    expect_identical(f1_flip$selected_items, f1_orig$selected_items)
    expect_equal(f1_flip$cutoff, f1_orig$cutoff, tolerance = 1e-12)
    expect_equal(f1_flip$inner_mean_youden, f1_orig$inner_mean_youden, tolerance = 1e-12)
    expect_false(isTRUE(all.equal(f1_flip$auc, f1_orig$auc)))
  }

  # Also construct a direct unstratified deterministic outer split via cross_size_nested_cv (stratified = FALSE)
  cs_orig <- cross_size_nested_cv(
    data = df, outcome = "y", items = colnames(X)[1:4],
    model_sizes = 1:2, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
    stratified = FALSE, engine = "Rcpp", parallel = "none",
    seed = 99L, progress = FALSE, verbose = FALSE
  )
  f1_test_cs <- cs_orig$outer_predictions$row_index[cs_orig$outer_predictions$repeat_id == 1L & cs_orig$outer_predictions$fold_id == 1L]
  df_flip_cs <- df
  df_flip_cs$y[f1_test_cs] <- 1L - df_flip_cs$y[f1_test_cs]
  cs_flip <- cross_size_nested_cv(
    data = df_flip_cs, outcome = "y", items = colnames(X)[1:4],
    model_sizes = 1:2, outer_k = 3L, inner_k = 2L, outer_repeats = 1L,
    stratified = FALSE, engine = "Rcpp", parallel = "none",
    seed = 99L, progress = FALSE, verbose = FALSE
  )
  expect_identical(cs_flip$outer_fold_results$selected_items[1L], cs_orig$outer_fold_results$selected_items[1L])
  expect_equal(cs_flip$outer_fold_results$selected_cutoff[1L], cs_orig$outer_fold_results$selected_cutoff[1L], tolerance = 1e-12)
  expect_false(isTRUE(all.equal(cs_flip$outer_fold_results$outer_auc[1L], cs_orig$outer_fold_results$outer_auc[1L])))
})

test_that("cv_performance_audit matches independent TP/FP/TN/FN, macro, pooled OOF, and handles zero-class folds & repeats", {
  # Synthetic ncvroc_result with known confusion counts, including a fold with zero positives
  preds <- data.frame(
    row_index    = 1:9,
    true_outcome = c(1L, 1L, 0L,   1L, 0L, 0L,   0L, 0L, 0L),
    score        = c(3,  1,  2,    4,  2,  1,    3,  2,  1),
    predicted    = c(1L, 0L, 1L,   1L, 0L, 0L,   1L, 0L, 0L),
    outer_fold   = c(rep("Fold1", 3L), rep("Fold2", 3L), rep("Fold3", 3L)),
    stringsAsFactors = FALSE
  )
  fake_res <- structure(
    list(
      summary           = data.frame(outer_fold = c("Fold1", "Fold2", "Fold3"), auc = c(0.5, 1.0, NA_real_), sensitivity = c(0.5, 1.0, NA_real_), specificity = c(0.0, 1.0, 2/3), stringsAsFactors = FALSE),
      outer_predictions = preds,
      outer_results     = list(),
      settings          = list(cutoff_method = "youden")
    ),
    class = "ncvroc_result"
  )

  aud <- cv_performance_audit(fake_res)
  expect_equal(aud$status, "verified")
  expect_equal(aud$fold_metrics$tp, c(1L, 1L, 0L))
  expect_equal(aud$fold_metrics$fp, c(1L, 0L, 1L))
  expect_equal(aud$fold_metrics$tn, c(0L, 2L, 2L))
  expect_equal(aud$fold_metrics$fn, c(1L, 0L, 0L))
  expect_equal(aud$fold_metrics$n_pos, c(2L, 1L, 0L))
  expect_equal(aud$fold_metrics$n_neg, c(1L, 2L, 3L))
  expect_equal(aud$fold_metrics$na_reason[3L], "zero_positives")
  expect_equal(aud$fold_macro$auc$n_valid, 2L)
  expect_equal(aud$fold_macro$auc$n_excluded, 1L)
  expect_equal(aud$fold_macro$auc$mean, mean(c(0.5, 1.0)), tolerance = 1e-12)
  # Count-pooled sensitivity = (1 + 1 + 0) / (2 + 1 + 0) = 2/3 (distinct from fold-macro mean (0.5+1)/2 = 0.75!)
  expect_equal(aud$fold_macro$count_pooled_sensitivity, 2 / 3, tolerance = 1e-12)
  expect_equal(aud$fold_macro$sensitivity$mean, 0.75, tolerance = 1e-12)
  # Pooled OOF
  expect_equal(aud$pooled_oof$n_total, 9L)
  expect_equal(aud$pooled_oof$tp, 2L)

  # Repeated CV: verify no subject double-counting in pooled_oof and independent repeat_metrics
  set.seed(502)
  df2 <- as.data.frame(matrix(rnorm(30 * 4), 30, 4))
  df2$y <- rep(0:1, each = 15L)
  cv_rep <- cv_sum_roc(df2, "y", paste0("V", 1:3), folds = 3L, repeats = 2L, seed = 12L)
  aud_rep <- cv_performance_audit(cv_rep)
  expect_equal(aud_rep$pooled_oof$n_repeats, 2L)
  expect_false(aud_rep$pooled_oof$repeated_double_counting)
  expect_equal(nrow(aud_rep$repeat_metrics), 2L)
  rep1_folds <- aud_rep$fold_metrics[aud_rep$fold_metrics$repeat_id == 1L, ]
  expect_equal(aud_rep$repeat_metrics$fold_macro_auc[1L], mean(rep1_folds$auc, na.rm = TRUE), tolerance = 1e-12)
  expect_equal(aud_rep$repeat_metrics$fold_macro_sens[1L], mean(rep1_folds$sensitivity, na.rm = TRUE), tolerance = 1e-12)

  # Summary-only fallback -> explicit na_reason
  sum_only <- fake_res
  sum_only$outer_predictions <- NULL
  aud_sum <- cv_performance_audit(sum_only)
  expect_true(all(!is.na(aud_sum$fold_metrics$na_reason)))
  expect_equal(aud_sum$fold_metrics$na_reason[3L], "undefined_metric_predictions_absent")

  # Missing predictions & summary -> not_verifiable
  partial <- structure(list(settings = list()), class = "ncvroc_result")
  expect_equal(cv_performance_audit(partial)$status, "not_verifiable")
})

test_that("clinical_constraint_audit classifies all states, uses total_folds, and avoids false feasibility without constraint", {
  fake_cs <- structure(
    list(
      outer_fold_results = data.frame(
        fold_id           = 1:6,
        repeat_id         = rep(1L, 6L),
        outer_auc         = c(0.8, 0.7, NA_real_, NA_real_, NA_real_, NA_real_),
        outer_sensitivity = c(0.9, 0.6, NA_real_, NA_real_, NA_real_, NA_real_),
        outer_specificity = c(0.85, 0.7, NA_real_, NA_real_, NA_real_, NA_real_),
        selection_status  = c("selected", "fallback_youden", "no_feasible_candidate", "invalid_evaluation", "cancelled", "not_evaluated"),
        stringsAsFactors  = FALSE
      ),
      settings = list(cutoff_method = "clinical_constraint", sensitivity_min = 0.8, specificity_min = 0.8)
    ),
    class = "cross_size_nested_cv_result"
  )

  ca <- clinical_constraint_audit(fake_cs)
  expect_equal(ca$status, "verified")
  expect_equal(ca$total_folds, 6L)
  expect_equal(ca$feasible_count, 1L)
  expect_equal(ca$feasibility_rate, 1 / 6, tolerance = 1e-12)
  expect_equal(ca$fallback_count, 1L)
  expect_equal(unname(ca$state_counts[c("feasible", "infeasible", "undefined_metric", "cancelled", "not_evaluated")]),
               c(1L, 2L, 1L, 1L, 1L))

  # Without a clinical constraint, status must be not_applicable
  no_const <- fake_cs
  no_const$settings <- list(cutoff_method = "youden")
  ca_na <- clinical_constraint_audit(no_const)
  expect_equal(ca_na$status, "not_applicable")
  expect_false(ca_na$constraint_active)

  # ncvroc_result with clinical_constraint but no selection_status -> not_verifiable
  ncv_const <- structure(
    list(summary = data.frame(outer_fold = 1:2, auc = c(0.8, 0.8), sensitivity = c(0.8, 0.8), specificity = c(0.8, 0.8)),
         settings = list(cutoff_method = "clinical_constraint", sensitivity_min = 0.8)),
    class = "ncvroc_result"
  )
  expect_equal(clinical_constraint_audit(ncv_const)$status, "not_verifiable")
})

test_that("selection_strategy_audit detects comparable, incompatible, partially_verifiable, and ncvroc_audit integrates all scopes", {
  set.seed(503)
  df <- as.data.frame(matrix(rnorm(36 * 5), 36, 5))
  df$y <- rep(0:1, each = 18L)
  items <- paste0("V", 1:5)

  r_youd <- nested_sum_roc(df, "y", items, 1L, 2L, outer_k = 3L, inner_k = 2L,
                           cutoff_method = "youden", parallel = "native_streaming",
                           engine = "Rcpp", seed = 77L, progress = FALSE, verbose = FALSE)
  r_tl <- nested_sum_roc(df, "y", items, 1L, 2L, outer_k = 3L, inner_k = 2L,
                         cutoff_method = "closest_topleft", parallel = "native_streaming",
                         engine = "Rcpp", seed = 77L, progress = FALSE, verbose = FALSE)
  r_diff_seed <- nested_sum_roc(df, "y", items, 1L, 2L, outer_k = 3L, inner_k = 2L,
                                cutoff_method = "closest_topleft", parallel = "native_streaming",
                                engine = "Rcpp", seed = 78L, progress = FALSE, verbose = FALSE)
  r_diff_pre <- nested_sum_roc(df, "y", items, 1L, 2L, outer_k = 3L, inner_k = 2L,
                               preselect_top_n = 3L, cutoff_method = "closest_topleft",
                               parallel = "native_streaming", engine = "Rcpp",
                               seed = 77L, progress = FALSE, verbose = FALSE)

  cmp_ok <- selection_strategy_audit(list(youden = r_youd, topleft = r_tl))
  expect_true(cmp_ok$comparable)
  expect_equal(cmp_ok$status, "comparable")

  cmp_bad <- selection_strategy_audit(list(youden = r_youd, diff_seed = r_diff_seed))
  expect_false(cmp_bad$comparable)
  expect_equal(cmp_bad$status, "incompatible")

  cmp_pre <- selection_strategy_audit(list(youden = r_youd, diff_pre = r_diff_pre))
  expect_equal(cmp_pre$status, "incompatible")

  # Missing seed/split metadata -> partially_verifiable (never comparable)
  r_missing <- r_tl
  r_missing$settings$seed <- NULL
  cmp_part <- selection_strategy_audit(list(youden = r_youd, missing_seed = r_missing))
  expect_false(cmp_part$comparable)
  expect_equal(cmp_part$status, "partially_verifiable")

  aud_ncv <- ncvroc_audit(r_youd, compare_to = list(topleft = r_tl))
  expect_s3_class(aud_ncv, "ncvroc_audit")
  expect_equal(aud_ncv$verifiability_summary$leakage, "verified")
  expect_equal(aud_ncv$verifiability_summary$performance, "verified")
  expect_equal(aud_ncv$verifiability_summary$constraints, "not_applicable")
  expect_equal(aud_ncv$verifiability_summary$strategy, "comparable")
  expect_output(print(aud_ncv), "NCVROC Statistical Audit")

  # Stripped outer fold row_index -> leakage must be not_verifiable (never verified)
  r_no_idx <- r_youd
  r_no_idx$outer_results[[1L]]$predictions$row_index <- NULL
  expect_equal(ncvroc_audit(r_no_idx)$verifiability_summary$leakage, "not_verifiable")

  # Overlapping outer fold row_index -> leakage must be violation
  r_overlap <- r_youd
  r_overlap$outer_results[[2L]]$predictions$row_index[1L] <- r_overlap$outer_results[[1L]]$predictions$row_index[1L]
  expect_equal(ncvroc_audit(r_overlap)$verifiability_summary$leakage, "violation")

  # Ordinary CV -> leakage is not_applicable
  cv_ord <- cv_sum_roc(df, "y", items[1:3], folds = 3L, repeats = 1L, seed = 12L)
  expect_equal(ncvroc_audit(cv_ord)$verifiability_summary$leakage, "not_applicable")

  # Candidate stability audit
  cands <- list(M1 = "V1", M2 = c("V1", "V2"))
  stab <- candidate_stability_roc(df, y, cands, resampling = "bootstrap", bootstrap_reps = 15L, seed = 11L, progress = FALSE)
  aud_stab <- ncvroc_audit(stab)
  expect_equal(aud_stab$verifiability_summary$stability, "verified")
  expect_equal(aud_stab$stability_audit$denominator_all, 15L)
  expect_true(all(c("fixed_candidate_performance_stability", "candidate_rank_stability", "candidate_selection_frequency") %in%
                    aud_stab$stability_audit$estimands_included))

  stab_missing <- stab
  stab_missing$candidate_summary <- NULL
  expect_equal(ncvroc_audit(stab_missing)$verifiability_summary$stability, "not_verifiable")

  stab_no_rmeta <- stab
  stab_no_rmeta$replicate_metadata <- NULL
  expect_equal(ncvroc_audit(stab_no_rmeta)$verifiability_summary$stability, "not_verifiable")
})
