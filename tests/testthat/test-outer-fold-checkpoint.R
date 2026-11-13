# test-outer-fold-checkpoint.R
# Tests for Scope B completed outer-fold checkpoint persistence and resume (v0.27.0)

.make_cp_test_data <- function(n = 80L, p = 5L, seed = 101L) {
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

.expect_ncvroc_parity <- function(res_a, res_b, tol = 1e-12) {
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

test_that("R1: API validation, backend restriction, and checkpoint_dir = NULL legacy parity", {
  td <- .make_cp_test_data()

  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   checkpoint_dir = "", progress = FALSE, verbose = FALSE),
    "checkpoint_dir"
  )
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   checkpoint_dir = c("a", "b"), progress = FALSE, verbose = FALSE),
    "checkpoint_dir"
  )
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   resume = NA, progress = FALSE, verbose = FALSE),
    "resume"
  )
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   force_recompute = "yes", progress = FALSE, verbose = FALSE),
    "force_recompute"
  )
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   checkpoint_dir = NULL, force_recompute = TRUE,
                   progress = FALSE, verbose = FALSE),
    "force_recompute = TRUE"
  )

  cp_unused <- tempfile("cp_unused_")
  on.exit(unlink(cp_unused, recursive = TRUE, force = TRUE), add = TRUE)
  for (pm in c("outer", "chunks", "threads", "hybrid", "native_tbb")) {
    eng <- if (pm %in% c("hybrid", "native_tbb")) "Rcpp" else "R"
    expect_error(
      nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                     engine = eng, parallel = pm,
                     checkpoint_dir = cp_unused,
                     progress = FALSE, verbose = FALSE),
      "native_outer_fold"
    )
    expect_false(dir.exists(cp_unused))
  }

  # Existing regular file as checkpoint_dir must fail closed
  reg_file <- tempfile("cp_reg_")
  writeLines("not a dir", reg_file)
  on.exit(unlink(reg_file, force = TRUE), add = TRUE)
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   engine = "Rcpp", parallel = "native_outer_fold",
                   checkpoint_dir = reg_file, progress = FALSE, verbose = FALSE),
    "not a directory"
  )

  # checkpoint_dir = NULL preserves exact legacy structure and RNG parity
  set.seed(777L)
  r1 <- nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                       outer_k = 3, inner_k = 2, seed = 42L,
                       engine = "Rcpp", parallel = "native_outer_fold",
                       n_workers = 1L, progress = FALSE, verbose = FALSE)
  seed_after_1 <- .Random.seed

  set.seed(777L)
  r2 <- nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                       outer_k = 3, inner_k = 2, seed = 42L,
                       engine = "Rcpp", parallel = "native_outer_fold",
                       n_workers = 1L, checkpoint_dir = NULL, resume = FALSE,
                       progress = FALSE, verbose = FALSE)
  seed_after_2 <- .Random.seed

  expect_identical(seed_after_1, seed_after_2)
  expect_null(r1$settings$checkpoint_provenance)
  expect_null(r2$settings$checkpoint_provenance)
  .expect_ncvroc_parity(r1, r2)
})

test_that("R1: Identity mismatch, corrupt artifacts, orphan/pending files fail closed", {
  td <- .make_cp_test_data()
  cp_dir <- tempfile("cp_id_")
  on.exit(unlink(cp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res_base <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
  )

  id_path <- file.path(cp_dir, "identity.rds")
  f1_path <- file.path(cp_dir, "folds", "repeat_001_fold_001.rds")
  expect_true(file.exists(id_path))
  expect_true(file.exists(f1_path))
  id_rec <- readRDS(id_path)
  f1_env <- readRDS(f1_path)
  expect_identical(id_rec$core$hash_algorithm, "sha256")
  expect_match(id_rec$analysis_identity_hash, "^[0-9a-f]{64}$")
  expect_identical(f1_env$hash_algorithm, "sha256")
  expect_match(f1_env$payload_hash, "^[0-9a-f]{64}$")

  # Unavailable SHA-256 must fail closed and never silently downgrade
  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  reset_seams()
  on.exit(reset_seams(), add = TRUE)
  seams$simulate_sha256_unavailable <- TRUE
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "SHA-256"
  )
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()

  # Stored weaker hash_algorithm (e.g. md5) must be rejected
  f1_md5 <- f1_env
  f1_md5$hash_algorithm <- "md5"
  saveRDS(f1_md5, f1_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "hash_algorithm mismatch"
  )
  saveRDS(f1_env, f1_path)

  id_hash_before <- NCVROC:::.cp_hash_file(id_path)
  f1_hash_before <- NCVROC:::.cp_hash_file(f1_path)

  # Parameter mismatch fails closed even with force_recompute = TRUE
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 99L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, force_recompute = TRUE,
      progress = FALSE, verbose = FALSE
    ),
    "identity mismatch"
  )
  # Data mismatch fails closed
  td2 <- td
  td2$data$q1[1L] <- td2$data$q1[1L] + 1.0
  expect_error(
    nested_sum_roc(
      td2$data, td2$outcome, td2$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "identity mismatch"
  )
  expect_identical(NCVROC:::.cp_hash_file(id_path), id_hash_before)
  expect_identical(NCVROC:::.cp_hash_file(f1_path), f1_hash_before)

  # Orphan file in folds/ fails closed
  orphan_path <- file.path(cp_dir, "folds", "unexpected.rds")
  saveRDS(list(a = 1), orphan_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "Unexpected or orphan"
  )
  unlink(orphan_path)

  # Leftover file in pending/ fails closed
  pend_leftover <- file.path(cp_dir, "pending", "stale.rds.tmp")
  saveRDS(list(a = 1), pend_leftover)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "Unresolved pending"
  )
  unlink(pend_leftover)

  # Tampered payload hash fails closed and preserves file
  env_f1 <- readRDS(f1_path)
  env_tampered <- env_f1
  env_tampered$payload$auc <- env_tampered$payload$auc + 0.01
  saveRDS(env_tampered, f1_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "payload_hash mismatch"
  )

  # Incomplete status in envelope fails closed
  env_inc <- env_f1
  env_inc$completion_status <- "in_progress"
  saveRDS(env_inc, f1_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "completion_status"
  )

  # Corrupt RDS fold file fails closed
  writeBin(as.raw(c(0x00, 0xff, 0x13)), f1_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "Corrupt checkpoint file"
  )

  # Restore valid f1, remove identity.rds -> missing identity while folds exist fails closed
  saveRDS(env_f1, f1_path)
  unlink(id_path)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "missing identity.rds"
  )
})

test_that("R1: Path canonicalization, prefix-collision, traversal, and locking contract", {
  td <- .make_cp_test_data()
  base_dir <- tempfile("cp_path_")
  dir.create(base_dir, recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cp_a <- file.path(base_dir, "run_a")
  cp_a_alias <- file.path(base_dir, "sub", "..", "run_a", ".")
  cp_prefix <- file.path(base_dir, "run_a_extra")
  dir.create(file.path(base_dir, "sub"))

  r_a <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_a, progress = FALSE, verbose = FALSE
  )

  # Alias path resolves to exact same canonical root and reuses all folds
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  r_alias <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_a_alias, progress = FALSE, verbose = FALSE
  )
  expect_identical(r_a$settings$checkpoint_provenance$canonical_checkpoint_dir,
                   r_alias$settings$checkpoint_provenance$canonical_checkpoint_dir)
  expect_identical(r_alias$settings$checkpoint_provenance$n_reused, 3L)
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_folds_executed, 0L)

  # Prefix-collision check in .cp_assert_contained
  dir.create(file.path(cp_prefix, "folds"), recursive = TRUE)
  expect_error(
    NCVROC:::.cp_assert_contained(cp_a, file.path(cp_prefix, "folds", "repeat_001_fold_001.rds")),
    "escapes canonical checkpoint root"
  )
  expect_error(
    NCVROC:::.cp_assert_contained(cp_a, file.path(cp_a, "..", "escape.rds")),
    "escapes canonical checkpoint root"
  )

  # Exclusive directory lock: collision, corrupt owner, release-by-owner only
  paths <- NCVROC:::.cp_resolve_root(cp_a)
  tok1 <- NCVROC:::.cp_acquire_lock(paths)
  expect_true(dir.exists(paths$lock_dir))

  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_a_alias, progress = FALSE, verbose = FALSE
    ),
    "Checkpoint directory is locked"
  )

  # Release with wrong nonce must NOT delete the lock
  fake_tok <- tok1
  fake_tok$nonce <- "wrong_nonce"
  expect_false(NCVROC:::.cp_release_lock(fake_tok))
  expect_true(dir.exists(paths$lock_dir))

  # Owner releases cleanly
  expect_true(NCVROC:::.cp_release_lock(tok1))
  expect_false(dir.exists(paths$lock_dir))

  # Corrupt owner.rds in .lock is preserved and fails closed
  dir.create(paths$lock_dir)
  writeBin(as.raw(c(0x01, 0x02)), file.path(paths$lock_dir, "owner.rds"))
  expect_error(NCVROC:::.cp_acquire_lock(paths), "corrupt owner.rds")
  expect_true(dir.exists(paths$lock_dir))
  unlink(paths$lock_dir, recursive = TRUE, force = TRUE)
})

test_that("R2/R3: Staging, fault injection, old-checkpoint preservation, and serial partial resume", {
  td <- .make_cp_test_data()
  cp_dir <- tempfile("cp_r23_")
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  on.exit({
    reset_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  fresh_ref <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 123L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    progress = FALSE, verbose = FALSE
  )

  seams <- NCVROC:::.ncvroc_checkpoint_test_seams

  # Interrupt serial run during fold 3: folds 1 and 2 must survive on disk
  reset_seams()
  seams$fail_during_compute_fold <- "Rep1_Fold3"
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 4, inner_k = 2, seed = 123L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "Rep1_Fold3"
  )
  expect_true(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_001.rds")))
  expect_true(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_002.rds")))
  expect_false(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_003.rds")))
  expect_false(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_004.rds")))
  expect_false(dir.exists(file.path(cp_dir, ".lock")))

  # Partial resume computes only folds 3 and 4
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  res_partial <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 123L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_dir, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_folds_executed, 2L)
  expect_identical(res_partial$settings$checkpoint_provenance$n_reused, 2L)
  expect_identical(res_partial$settings$checkpoint_provenance$n_computed, 2L)
  .expect_ncvroc_parity(fresh_ref, res_partial)

  # Full resume executes 0 native folds
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  res_full <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 123L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_dir, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_folds_executed, 0L)
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_call_count, 0L)
  expect_identical(res_full$settings$checkpoint_provenance$n_reused, 4L)
  .expect_ncvroc_parity(fresh_ref, res_full)

  # resume = FALSE, force_recompute = FALSE refuses to overwrite
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 4, inner_k = 2, seed = 123L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, resume = FALSE, force_recompute = FALSE,
      progress = FALSE, verbose = FALSE
    ),
    "force_recompute = TRUE"
  )

  # Failed replacement during force_recompute = TRUE preserves old valid checkpoint by hash
  f2_path <- file.path(cp_dir, "folds", "repeat_001_fold_002.rds")
  f2_hash_orig <- NCVROC:::.cp_hash_file(f2_path)
  reset_seams()
  seams$capture_artifact_hashes <- TRUE
  seams$fail_during_replace <- "Rep1_Fold2"

  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 4, inner_k = 2, seed = 123L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_dir, resume = TRUE, force_recompute = TRUE,
      progress = FALSE, verbose = FALSE
    ),
    "prior valid checkpoint restored"
  )
  expect_identical(NCVROC:::.cp_hash_file(f2_path), f2_hash_orig)
  pend_file <- NCVROC:::.ncvroc_checkpoint_test_seams$last_pending_file
  expect_true(file.exists(pend_file))
  expect_identical(NCVROC:::.cp_hash_file(pend_file),
                   NCVROC:::.ncvroc_checkpoint_test_seams$last_pending_hash)
  unlink(pend_file)

  # Force recompute succeeds and recomputes all 4 folds
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  res_forced <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 4, inner_k = 2, seed = 123L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_dir, resume = FALSE, force_recompute = TRUE,
    progress = FALSE, verbose = FALSE
  )
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_folds_executed, 4L)
  expect_identical(res_forced$settings$checkpoint_provenance$n_reused, 0L)
  expect_identical(res_forced$settings$checkpoint_provenance$n_computed, 4L)
  .expect_ncvroc_parity(fresh_ref, res_forced)
})

test_that("R4: Parallel TBB status transport persists completed siblings before deterministic error", {
  td <- .make_cp_test_data()
  cp_dir <- tempfile("cp_r4_")
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  on.exit({
    reset_seams()
    unlink(cp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Direct C++ throw_on_error = FALSE test: returns per-fold statuses without throwing
  y_vec <- td$data[[td$outcome]]
  x_mat <- as.matrix(td$data[, td$items, drop = FALSE])
  outer_folds <- make_stratified_folds(y_vec, k = 3, repeats = 1, seed = 42L)
  specs <- NCVROC:::.build_native_outer_fold_specs(
    outer_folds = outer_folds, y = y_vec, n_total = length(y_vec),
    inner_k = 2L, inner_repeats = 1L, n_candidates = 15L, seed = 42L
  )
  # Corrupt inner_folds of fold 2 to all-same class so worker fails inside C++
  pos_idx_0 <- which(y_vec[specs[[2L]]$train_indices + 1L] == 1L) - 1L
  neg_idx_0 <- which(y_vec[specs[[2L]]$train_indices + 1L] == 0L) - 1L
  specs[[2L]]$inner_folds[[1L]] <- list(pos_idx_0, neg_idx_0)

  cpp_res <- NCVROC:::evaluate_outer_folds_native_cpp(
    x = x_mat, y = y_vec, fold_specs = specs,
    min_items = 1L, max_items = 2L,
    cutoff_method = "youden", preselect_by = "auc", selection_criterion = "auc",
    preselect_top_n = 20L, prefer_fewer_items = TRUE,
    num_threads = 2L, schema_version = 1L, throw_on_error = FALSE
  )
  expect_length(cpp_res, 3L)
  expect_true(isTRUE(cpp_res[[1L]]$completed))
  expect_false(isTRUE(cpp_res[[2L]]$completed))
  expect_true(isTRUE(cpp_res[[3L]]$completed))

  # Bounded-wave parallel checkpointed run (wave_size = n_workers = 2):
  # Wave 1 runs Fold1 & Fold2; Fold1 is persisted before Fold2's deterministic error,
  # and Wave 2 (Fold3) is not dispatched after Wave 1 fails.
  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams()
  seams$fail_during_compute_fold <- "Rep1_Fold2"
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = 42L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
      checkpoint_dir = cp_dir, progress = FALSE, verbose = FALSE
    ),
    "Rep1_Fold2"
  )
  expect_true(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_001.rds")))
  expect_false(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_002.rds")))
  expect_false(file.exists(file.path(cp_dir, "folds", "repeat_001_fold_003.rds")))

  # Resume with 2 threads computes remaining Rep1_Fold2 and Rep1_Fold3 and matches fresh run
  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  res_resumed <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    checkpoint_dir = cp_dir, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(NCVROC:::.ncvroc_checkpoint_test_seams$native_folds_executed, 2L)
  expect_identical(res_resumed$settings$checkpoint_provenance$reused_folds, "Rep1_Fold1")
  expect_identical(res_resumed$settings$checkpoint_provenance$computed_folds, c("Rep1_Fold2", "Rep1_Fold3"))

  fresh_2t <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
    progress = FALSE, verbose = FALSE
  )
  .expect_ncvroc_parity(fresh_2t, res_resumed)
})

test_that("R5: Repeated outer CV, RNG parity (fixed & NULL seed), 1/N thread invariance, and progress accounting", {
  td <- .make_cp_test_data(n = 90L, p = 5L, seed = 202L)
  cp_rep <- tempfile("cp_rep_")
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  on.exit({
    reset_seams()
    unlink(cp_rep, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Repeated outer CV (2 repeats x 3 folds = 6 folds)
  fresh_rep <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, outer_repeats = 2, inner_k = 2, seed = 55L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    progress = FALSE, verbose = FALSE
  )

  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams()
  seams$fail_before_aggregate <- TRUE
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, outer_repeats = 2, inner_k = 2, seed = 55L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 2L,
      checkpoint_dir = cp_rep, progress = FALSE, verbose = FALSE
    ),
    "before final aggregation"
  )
  # Remove 2 folds to simulate partial progress across repeats, then resume with 1 thread
  unlink(file.path(cp_rep, "folds", "repeat_001_fold_002.rds"))
  unlink(file.path(cp_rep, "folds", "repeat_002_fold_003.rds"))

  NCVROC:::.reset_ncvroc_checkpoint_test_seams()
  msgs <- testthat::capture_messages(
    res_rep <- nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, outer_repeats = 2, inner_k = 2, seed = 55L,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_rep, resume = TRUE, progress = TRUE, verbose = FALSE
    )
  )
  expect_true(any(grepl("4/6 outer fold\\(s\\) reused; 2/6 outer fold\\(s\\) to compute", msgs)))
  expect_true(any(grepl("2 newly computed outer fold\\(s\\) persisted", msgs)))
  expect_false(any(grepl("ETA", msgs, fixed = TRUE)))
  .expect_ncvroc_parity(fresh_rep, res_rep)

  # NULL seed RNG parity when caller reproduces RNG state
  cp_null <- tempfile("cp_null_")
  on.exit(unlink(cp_null, recursive = TRUE, force = TRUE), add = TRUE)

  set.seed(888L)
  fresh_null <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    progress = FALSE, verbose = FALSE
  )
  seed_fresh_null <- .Random.seed

  set.seed(888L)
  cp_null_res1 <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_null, progress = FALSE, verbose = FALSE
  )
  seed_cp_null <- .Random.seed
  expect_identical(seed_fresh_null, seed_cp_null)
  .expect_ncvroc_parity(fresh_null, cp_null_res1)

  # Reproducing same RNG state resumes cleanly; different RNG state fails closed
  unlink(file.path(cp_null, "folds", "repeat_001_fold_003.rds"))
  set.seed(888L)
  cp_null_res2 <- nested_sum_roc(
    td$data, td$outcome, td$items, max_items = 2,
    outer_k = 3, inner_k = 2, seed = NULL,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    checkpoint_dir = cp_null, resume = TRUE, progress = FALSE, verbose = FALSE
  )
  expect_identical(.Random.seed, seed_fresh_null)
  .expect_ncvroc_parity(fresh_null, cp_null_res2)

  set.seed(999L)
  expect_error(
    nested_sum_roc(
      td$data, td$outcome, td$items, max_items = 2,
      outer_k = 3, inner_k = 2, seed = NULL,
      engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
      checkpoint_dir = cp_null, resume = TRUE, progress = FALSE, verbose = FALSE
    ),
    "identity mismatch"
  )
})

test_that("R5: Artificial interruptions at every lifecycle stage preserve valid state", {
  td <- .make_cp_test_data()
  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  reset_seams <- NCVROC:::.reset_ncvroc_checkpoint_test_seams
  on.exit(reset_seams(), add = TRUE)

  # 1. fail_before_compute: 0 folds persisted
  cp1 <- tempfile("cp_int1_")
  on.exit(unlink(cp1, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  seams$fail_before_compute <- TRUE
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   outer_k = 3, inner_k = 2, seed = 42L,
                   engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                   checkpoint_dir = cp1, progress = FALSE, verbose = FALSE),
    "before computing"
  )
  expect_length(list.files(file.path(cp1, "folds")), 0L)

  # 2. fail_between_complete_and_write at Fold2: Fold1 persisted, Fold2 absent
  cp2 <- tempfile("cp_int2_")
  on.exit(unlink(cp2, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  seams$fail_between_complete_and_write <- "Rep1_Fold2"
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   outer_k = 3, inner_k = 2, seed = 42L,
                   engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                   checkpoint_dir = cp2, progress = FALSE, verbose = FALSE),
    "between fold completion and checkpoint write"
  )
  expect_true(file.exists(file.path(cp2, "folds", "repeat_001_fold_001.rds")))
  expect_false(file.exists(file.path(cp2, "folds", "repeat_001_fold_002.rds")))

  # 3. fail_during_pending_write at Fold2: Fold1 intact, partial pending file preserved
  cp3 <- tempfile("cp_int3_")
  on.exit(unlink(cp3, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  seams$capture_artifact_hashes <- TRUE
  seams$fail_during_pending_write <- "Rep1_Fold2"
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   outer_k = 3, inner_k = 2, seed = 42L,
                   engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                   checkpoint_dir = cp3, progress = FALSE, verbose = FALSE),
    "during pending checkpoint write"
  )
  expect_true(file.exists(file.path(cp3, "folds", "repeat_001_fold_001.rds")))
  expect_false(file.exists(file.path(cp3, "folds", "repeat_001_fold_002.rds")))
  expect_true(file.exists(seams$last_pending_file))
  expect_identical(NCVROC:::.cp_hash_file(seams$last_pending_file), seams$last_pending_hash)

  # 4. fail_during_readback and fail_before_publish at Fold2
  for (seam_name in c("fail_during_readback", "fail_before_publish")) {
    cp_s <- tempfile("cp_ints_")
    on.exit(unlink(cp_s, recursive = TRUE, force = TRUE), add = TRUE)
    reset_seams()
    seams[[seam_name]] <- "Rep1_Fold2"
    expect_error(
      nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                     outer_k = 3, inner_k = 2, seed = 42L,
                     engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                     checkpoint_dir = cp_s, progress = FALSE, verbose = FALSE)
    )
    expect_true(file.exists(file.path(cp_s, "folds", "repeat_001_fold_001.rds")))
    expect_false(file.exists(file.path(cp_s, "folds", "repeat_001_fold_002.rds")))
    expect_length(list.files(file.path(cp_s, "pending")), 1L)
  }

  # 5. fail_after_publish at Fold2: Fold1 and Fold2 both persisted; resume computes only Fold3
  cp5 <- tempfile("cp_int5_")
  on.exit(unlink(cp5, recursive = TRUE, force = TRUE), add = TRUE)
  reset_seams()
  seams$fail_after_publish <- "Rep1_Fold2"
  expect_error(
    nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                   outer_k = 3, inner_k = 2, seed = 42L,
                   engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                   checkpoint_dir = cp5, progress = FALSE, verbose = FALSE),
    "after checkpoint publication"
  )
  expect_true(file.exists(file.path(cp5, "folds", "repeat_001_fold_001.rds")))
  expect_true(file.exists(file.path(cp5, "folds", "repeat_001_fold_002.rds")))
  expect_false(file.exists(file.path(cp5, "folds", "repeat_001_fold_003.rds")))

  reset_seams()
  res_r5 <- nested_sum_roc(td$data, td$outcome, td$items, max_items = 2,
                           outer_k = 3, inner_k = 2, seed = 42L,
                           engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
                           checkpoint_dir = cp5, resume = TRUE, progress = FALSE, verbose = FALSE)
  expect_identical(res_r5$settings$checkpoint_provenance$n_reused, 2L)
  expect_identical(res_r5$settings$checkpoint_provenance$n_computed, 1L)
  expect_identical(seams$native_folds_executed, 1L)
})

