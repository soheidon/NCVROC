# test-checkpoint-canonical-interop.R
# Tests for v0.28.0 backend-neutral canonical outer-fold checkpointing and
# R serial <-> Scope B Native bidirectional interoperability.

.make_interop_test_data <- function(n = 80L, p = 5L, seed = 202L) {
  set.seed(seed)
  y <- rep(c(0L, 1L), length.out = n)
  x <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  x[y == 1L, 1L] <- x[y == 1L, 1L] + 1.0
  x[y == 1L, 2L] <- x[y == 1L, 2L] + 0.6
  df <- as.data.frame(x)
  items <- paste0("q", seq_len(p))
  colnames(df) <- items
  df$y <- y
  list(data = df, outcome = "y", items = items)
}

.expect_interop_parity <- function(res_a, res_b, tol = 1e-12) {
  expect_s3_class(res_a, "ncvroc_result")
  expect_s3_class(res_b, "ncvroc_result")
  expect_identical(res_a$summary$outer_fold, res_b$summary$outer_fold)
  expect_identical(res_a$summary$selected_items, res_b$summary$selected_items)
  expect_identical(res_a$summary$n_items, res_b$summary$n_items)
  for (col in c("auc", "cutoff", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv")) {
    expect_equal(res_a$summary[[col]], res_b$summary[[col]], tolerance = tol)
  }
  expect_identical(res_a$selected_models, res_b$selected_models)
  expect_identical(res_a$selected_model_frequency$items, res_b$selected_model_frequency$items)
  expect_identical(res_a$selected_model_frequency$n_selections, res_b$selected_model_frequency$n_selections)
  expect_identical(res_a$outer_predictions$row_index, res_b$outer_predictions$row_index)
  expect_identical(res_a$outer_predictions$true_outcome, res_b$outer_predictions$true_outcome)
  expect_identical(res_a$outer_predictions$predicted_class, res_b$outer_predictions$predicted_class)
  expect_equal(res_a$outer_predictions$predicted_score, res_b$outer_predictions$predicted_score, tolerance = tol)
  expect_identical(res_a$outer_predictions$outer_fold, res_b$outer_predictions$outer_fold)
}

test_that("R1/R4: Canonical schema, backend-neutral identity, and legacy v0.27 rejection", {
  td <- .make_interop_test_data()
  cp_r <- tempfile("cp_id_r_")
  cp_n <- tempfile("cp_id_n_")
  on.exit(unlink(c(cp_r, cp_n), recursive = TRUE, force = TRUE), add = TRUE)

  res_r <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_r, progress = FALSE, verbose = FALSE
  )
  res_n <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_n, progress = FALSE, verbose = FALSE
  )

  id_r <- readRDS(file.path(cp_r, "identity.rds"))
  id_n <- readRDS(file.path(cp_n, "identity.rds"))
  expect_identical(id_r$schema_version, 2L)
  expect_identical(id_n$schema_version, 2L)
  expect_identical(id_r$analysis_identity_hash, id_n$analysis_identity_hash)
  expect_identical(id_r$core$fold_identities, id_n$core$fold_identities)
  expect_null(id_r$core$backend)
  expect_null(id_r$core$package_version)
  expect_identical(id_r$provenance$producer_backend, "r_serial")
  expect_identical(id_n$provenance$producer_backend, "scope_b_native_outer_fold")

  # Changing package_version in provenance must NOT invalidate compatibility
  id_r_pkg <- id_r
  id_r_pkg$provenance$package_version <- "0.99.0"
  saveRDS(id_r_pkg, file.path(cp_r, "identity.rds"))
  env1 <- readRDS(file.path(cp_r, "folds", "repeat_001_fold_001.rds"))
  env1_pkg <- env1
  env1_pkg$producer$package_version <- "0.99.0"
  saveRDS(env1_pkg, file.path(cp_r, "folds", "repeat_001_fold_001.rds"))

  res_r_resume <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_r, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(res_r_resume$settings$checkpoint_provenance$n_reused, 3L)

  # Restore env1 and test legacy v0.27 rejection (identity and envelope)
  saveRDS(env1, file.path(cp_r, "folds", "repeat_001_fold_001.rds"))
  cp_leg <- tempfile("cp_legacy_")
  on.exit(unlink(cp_leg, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(cp_leg, "folds"), recursive = TRUE)
  dir.create(file.path(cp_leg, "pending"), recursive = TRUE)
  saveRDS(
    list(
      format_id = "NCVROC_nested_sum_roc_checkpoint_identity",
      schema_version = 1L,
      core = list(backend = "scope_b_native_outer_fold")
    ),
    file.path(cp_leg, "identity.rds")
  )
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "R", parallel = "none",
      checkpoint_dir = cp_leg, progress = FALSE, verbose = FALSE
    ),
    "Unsupported legacy v0.27 checkpoint schema"
  )

  # Legacy v0.27 fold envelope inside v2 directory must also fail closed
  env_leg <- env1
  env_leg$format_id <- "NCVROC_nested_sum_roc_outer_fold_checkpoint"
  env_leg$schema_version <- 1L
  saveRDS(env_leg, file.path(cp_r, "folds", "repeat_001_fold_001.rds"))
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "R", parallel = "none",
      checkpoint_dir = cp_r, progress = FALSE, verbose = FALSE
    ),
    "legacy v0.27"
  )
})

test_that("R2/R3: Strict canonical payload type, NaN/Inf/NA, index, and producer validation", {
  td <- .make_interop_test_data()
  cp_val <- tempfile("cp_val_")
  on.exit(unlink(cp_val, recursive = TRUE, force = TRUE), add = TRUE)

  nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 11L,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_val, progress = FALSE, verbose = FALSE
  )

  id_rec <- readRDS(file.path(cp_val, "identity.rds"))
  f1_path <- file.path(cp_val, "folds", "repeat_001_fold_001.rds")
  env_ok <- readRDS(f1_path)

  # Build a dummy fold_spec matching f1
  spec1 <- list(
    repeat_id = env_ok$repeat_id,
    outer_fold_id = env_ok$outer_fold_id,
    fold_name = env_ok$outer_fold,
    test_indices = env_ok$payload$predictions$row_index - 1L
  )

  # Fractional ID/n_items rejected
  p_frac <- env_ok$payload
  p_frac$n_items <- 1.5
  expect_match(NCVROC:::.cp_validate_canonical_payload(p_frac, spec1, td$items), "n_items")

  # NaN in cutoff rejected (must NOT normalize NaN to NA)
  p_nan <- env_ok$payload
  p_nan$cutoff <- NaN
  expect_match(NCVROC:::.cp_validate_canonical_payload(p_nan, spec1, td$items), "cutoff")

  # Inf in predicted_score rejected
  p_inf <- env_ok$payload
  p_inf$predictions$predicted_score[1L] <- Inf
  expect_match(NCVROC:::.cp_validate_canonical_payload(p_inf, spec1, td$items), "predicted_score")

  # Allowed NA in ppv/npv accepted, but NA in accuracy rejected
  p_ppv_na <- env_ok$payload
  p_ppv_na$ppv <- NA_real_
  p_ppv_na$npv <- NA_real_
  expect_null(NCVROC:::.cp_validate_canonical_payload(p_ppv_na, spec1, td$items))
  p_acc_na <- env_ok$payload
  p_acc_na$accuracy <- NA_real_
  expect_match(NCVROC:::.cp_validate_canonical_payload(p_acc_na, spec1, td$items), "accuracy")

  # Unknown producer rejected
  env_unk <- env_ok
  env_unk$producer$backend <- "unknown_future_backend"
  expect_match(
    NCVROC:::.cp_validate_envelope(env_unk, id_rec, spec1, env_ok$split_hash, td$items),
    "producer"
  )

  # Wrong selected_items label rejected
  p_lbl <- env_ok$payload
  p_lbl$selected_items_label <- "wrong_label"
  expect_match(NCVROC:::.cp_validate_canonical_payload(p_lbl, spec1, td$items), "selected_items_label")

  # Tampered true_outcome WITH recomputed payload_hash fails closed against y_vec
  env_tamper_y <- env_ok
  env_tamper_y$payload$predictions$true_outcome[1L] <- 1L - env_tamper_y$payload$predictions$true_outcome[1L]
  env_tamper_y$payload_hash <- NCVROC:::.cp_hash_object(env_tamper_y$payload)
  saveRDS(env_tamper_y, f1_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 11L,
      engine = "R", parallel = "none",
      checkpoint_dir = cp_val, resume = TRUE, progress = FALSE, verbose = FALSE
    ),
    "true_outcome does not match analysis outcome labels"
  )
  saveRDS(env_ok, f1_path)

  # Adapters must reject fractional/NaN/Inf inputs BEFORE coercion
  r_fold_ok <- list(
    outer_fold        = spec1$fold_name,
    selected_items    = env_ok$payload$selected_items_label,
    n_items           = env_ok$payload$n_items,
    inner_mean_auc    = env_ok$payload$inner_mean_auc,
    inner_mean_youden = env_ok$payload$inner_mean_youden,
    auc               = env_ok$payload$auc,
    sensitivity       = env_ok$payload$sensitivity,
    specificity       = env_ok$payload$specificity,
    youden            = env_ok$payload$youden,
    accuracy          = env_ok$payload$accuracy,
    ppv               = env_ok$payload$ppv,
    npv               = env_ok$payload$npv,
    cutoff            = env_ok$payload$cutoff,
    predictions       = env_ok$payload$predictions
  )
  r_fold_frac_row <- r_fold_ok
  r_fold_frac_row$predictions$row_index[1L] <- r_fold_frac_row$predictions$row_index[1L] + 0.5
  expect_error(NCVROC:::.r_fold_to_canonical(r_fold_frac_row, spec1, td$items, td$data$y), "row_index")

  r_fold_frac_cls <- r_fold_ok
  r_fold_frac_cls$predictions$predicted_class[1L] <- 0.5
  expect_error(NCVROC:::.r_fold_to_canonical(r_fold_frac_cls, spec1, td$items, td$data$y), "predicted_class")

  nat_ok <- list(
    completed              = TRUE,
    status_code            = 0L,
    repeat_id              = spec1$repeat_id,
    outer_fold_id          = spec1$outer_fold_id,
    outer_fold             = spec1$fold_name,
    selected_col_indices   = env_ok$payload$selected_col_indices,
    n_items                = env_ok$payload$n_items,
    n_candidates_evaluated = 5L,
    selected_global_rank   = 0L,
    inner_mean_auc         = env_ok$payload$inner_mean_auc,
    inner_mean_sensitivity = env_ok$payload$inner_mean_sensitivity,
    inner_mean_specificity = env_ok$payload$inner_mean_specificity,
    inner_mean_youden      = env_ok$payload$inner_mean_youden,
    auc                    = env_ok$payload$auc,
    cutoff                 = env_ok$payload$cutoff,
    sensitivity            = env_ok$payload$sensitivity,
    specificity            = env_ok$payload$specificity,
    youden                 = env_ok$payload$youden,
    accuracy               = env_ok$payload$accuracy,
    ppv                    = env_ok$payload$ppv,
    npv                    = env_ok$payload$npv,
    predictions            = env_ok$payload$predictions
  )
  nat_frac_idx <- nat_ok
  nat_frac_idx$selected_col_indices <- as.numeric(nat_frac_idx$selected_col_indices) + 0.5
  expect_error(NCVROC:::.native_fold_to_canonical(nat_frac_idx, spec1, td$items, td$data$y), "selected_col_indices")

  nat_frac_cand <- nat_ok
  nat_frac_cand$n_candidates_evaluated <- 5.5
  expect_error(NCVROC:::.native_fold_to_canonical(nat_frac_cand, spec1, td$items, td$data$y), "n_candidates_evaluated")

  # Tuning != 'off' with checkpoint_dir fails closed
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 11L,
      engine = "R", parallel = "none", tuning = "auto",
      checkpoint_dir = cp_val, progress = FALSE, verbose = FALSE
    ),
    "tuning"
  )
})

test_that("R5/R6/R7: Bidirectional R<->Native matrix, partial/full resume, mixed producers, and zero recomputation", {
  td <- .make_interop_test_data()
  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  reset_seams()
  on.exit(reset_seams(), add = TRUE)

  # Ground truth fresh R serial (checkpoint_dir = NULL)
  ref_r <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "R", parallel = "none",
    checkpoint_dir = NULL, progress = FALSE, verbose = FALSE
  )
  # Ground truth fresh Scope B Native (checkpoint_dir = NULL)
  ref_n <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = NULL, progress = FALSE, verbose = FALSE
  )
  .expect_interop_parity(ref_r, ref_n)

  # 1. R serial fresh -> Native full resume (0 native folds executed)
  cp_rn <- tempfile("cp_rn_")
  on.exit(unlink(cp_rn, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  r_fresh <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_rn, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$r_folds_executed, 4L)
  expect_identical(r_fresh$settings$checkpoint_provenance$n_computed, 4L)
  expect_identical(r_fresh$settings$checkpoint_provenance$n_reused, 0L)
  .expect_interop_parity(ref_r, r_fresh)

  reset_seams()
  n_from_r <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_rn, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$native_folds_executed, 0L)
  expect_identical(seams$native_call_count, 0L)
  expect_identical(n_from_r$settings$checkpoint_provenance$n_reused, 4L)
  expect_identical(n_from_r$settings$checkpoint_provenance$reused_from_r, 4L)
  expect_identical(n_from_r$settings$checkpoint_provenance$reused_from_native, 0L)
  .expect_interop_parity(ref_r, n_from_r)

  # 2. Native fresh -> R serial full resume (0 R folds executed)
  cp_nr <- tempfile("cp_nr_")
  on.exit(unlink(cp_nr, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  n_fresh <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_nr, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$native_folds_executed, 4L)
  .expect_interop_parity(ref_n, n_fresh)

  reset_seams()
  r_from_n <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_nr, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$r_folds_executed, 0L)
  expect_identical(r_from_n$settings$checkpoint_provenance$n_reused, 4L)
  expect_identical(r_from_n$settings$checkpoint_provenance$reused_from_r, 0L)
  expect_identical(r_from_n$settings$checkpoint_provenance$reused_from_native, 4L)
  .expect_interop_parity(ref_n, r_from_n)

  # 3. Partial R -> Native (2 folds) -> Mixed R/Native -> R serial & Native TBB
  cp_mix <- tempfile("cp_mix_")
  on.exit(unlink(cp_mix, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  seams$fail_during_compute_fold <- "Rep1_Fold3"
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 4, inner_k = 2, seed = 77L,
      engine = "R", parallel = "none",
      checkpoint_dir = cp_mix, progress = FALSE, verbose = FALSE
    ),
    "Rep1_Fold3"
  )
  # Fold1 and Fold2 were persisted by R serial; now resume Fold3 & Fold4 with Native (fail Fold4 to leave 3 folds: 2 R + 1 Native)
  reset_seams()
  seams$fail_during_compute_fold <- "Rep1_Fold4"
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 4, inner_k = 2, seed = 77L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
      checkpoint_dir = cp_mix, resume = TRUE, progress = FALSE, verbose = FALSE
    ),
    "Rep1_Fold4"
  )
  expect_identical(seams$native_folds_executed, 2L)

  # Now cp_mix has Fold1 (r_serial), Fold2 (r_serial), Fold3 (scope_b_native_outer_fold), Fold4 (absent)
  # Resume with R serial -> reuses 3 (2 R + 1 Native) and computes 1 (Fold4)
  reset_seams()
  mix_r <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_mix, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$r_folds_executed, 1L)
  expect_identical(mix_r$settings$checkpoint_provenance$reused_from_r, 2L)
  expect_identical(mix_r$settings$checkpoint_provenance$reused_from_native, 1L)
  expect_identical(mix_r$settings$checkpoint_provenance$computed_folds, "Rep1_Fold4")
  expect_identical(
    unname(mix_r$settings$checkpoint_provenance$fold_producers),
    c("r_serial", "r_serial", "scope_b_native_outer_fold", "r_serial")
  )
  .expect_interop_parity(ref_r, mix_r)

  # Full resume of mixed directory with Scope B Native TBB (2 threads) -> 0 computed
  reset_seams()
  mix_n <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_mix, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$native_folds_executed, 0L)
  expect_identical(mix_n$settings$checkpoint_provenance$reused_from_r, 3L)
  expect_identical(mix_n$settings$checkpoint_provenance$reused_from_native, 1L)
  .expect_interop_parity(ref_r, mix_n)
})

test_that("R7: RNG invariance with seed = NULL and force_recompute cross-backend replacement", {
  td <- .make_interop_test_data()
  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  reset_seams()
  on.exit(reset_seams(), add = TRUE)

  # Capture fresh R serial with seed = NULL from a known RNG state
  set.seed(999L)
  fresh_r <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "R", parallel = "none",
    checkpoint_dir = NULL, progress = FALSE, verbose = FALSE
  )
  rng_after_fresh <- .Random.seed

  # Checkpointed R serial from the exact same RNG state, interrupted at Fold2, then resumed by Native
  cp_rng <- tempfile("cp_rng_")
  on.exit(unlink(cp_rng, recursive = TRUE, force = TRUE), add = TRUE)
  set.seed(999L)
  seams$fail_during_compute_fold <- "Rep1_Fold2"
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = NULL,
      engine = "R", parallel = "none",
      checkpoint_dir = cp_rng, progress = FALSE, verbose = FALSE
    ),
    "Rep1_Fold2"
  )
  rng_after_partial <- .Random.seed
  expect_identical(rng_after_partial, rng_after_fresh)

  # Resume remaining folds with Scope B Native from same starting RNG state -> exact parity with fresh_r
  reset_seams()
  set.seed(999L)
  res_rng <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_rng, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(.Random.seed, rng_after_fresh)
  expect_identical(res_rng$settings$checkpoint_provenance$n_reused, 1L)
  expect_identical(res_rng$settings$checkpoint_provenance$n_computed, 2L)
  .expect_interop_parity(fresh_r, res_rng)

  # force_recompute = TRUE with R serial replaces Native-produced folds with R-produced folds
  reset_seams()
  set.seed(999L)
  res_forced <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "R", parallel = "none",
    checkpoint_dir = cp_rng, resume = FALSE, force_recompute = TRUE,
    progress = FALSE, verbose = FALSE
  )
  expect_identical(seams$r_folds_executed, 3L)
  expect_identical(res_forced$settings$checkpoint_provenance$n_reused, 0L)
  expect_identical(res_forced$settings$checkpoint_provenance$n_computed, 3L)
  expect_identical(
    unname(res_forced$settings$checkpoint_provenance$fold_producers),
    rep("r_serial", 3L)
  )
  .expect_interop_parity(fresh_r, res_forced)
})
