# test-nested-result-cache.R — Unit tests for cross_size_nested_cv() result-RDS reuse
# Comprehensive coverage for canonical locks, nonces, transaction namespaces, crash recovery, and identity/provenance.

# Helper to construct valid transaction directory structure for testing recovery
.create_test_tx_dir <- function(target_path,
                                tx_nonce             = "tx_testnonce1234",
                                orig_owner_nonce     = "owner_orig1234",
                                stage                = "prepared",
                                had_prior_target     = FALSE,
                                prior_target_hash    = NULL,
                                backup_obj           = NULL,
                                new_result_obj       = NULL,
                                identity_key         = "test_key",
                                custom_temp_path     = NULL,
                                custom_backup_path   = NULL,
                                custom_dir_nonce     = NULL,
                                custom_state_nonce   = NULL,
                                custom_state_owner   = NULL,
                                custom_id_schema     = NULL,
                                custom_state_schema  = NULL,
                                custom_s1_schema     = NULL,
                                custom_s2_schema     = NULL,
                                custom_s3_schema     = NULL,
                                custom_s1_seq        = NULL,
                                custom_s2_seq        = NULL,
                                custom_s3_seq        = NULL,
                                custom_s2_pred_hash  = NULL,
                                custom_s3_pred_hash  = NULL,
                                omit_identity        = FALSE,
                                omit_s1              = FALSE,
                                omit_s2              = FALSE,
                                extra_files          = NULL) {
  canonical_target <- .canonicalize_result_path(target_path)
  parent_dir <- dirname(canonical_target)
  base_name <- basename(canonical_target)
  dir_nonce <- if (!is.null(custom_dir_nonce)) custom_dir_nonce else tx_nonce
  tx_dir <- file.path(parent_dir, paste0(".", base_name, ".tx-", dir_nonce))
  dir.create(tx_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Identity
  if (!omit_identity) {
    id_schema <- if (!is.null(custom_id_schema)) custom_id_schema else 1L
    id_rec <- list(
      transaction_schema   = id_schema,
      format_version       = "NCVROC_tx_identity_v1",
      canonical_target     = canonical_target,
      transaction_nonce    = tx_nonce,
      original_owner_nonce = orig_owner_nonce,
      created_at           = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
      pid                  = Sys.getpid(),
      hostname             = Sys.info()[["nodename"]]
    )
    saveRDS(id_rec, file.path(tx_dir, "identity.rds"))
  }

  # 2. new_result.rds
  new_res_path <- if (!is.null(custom_temp_path)) custom_temp_path else file.path(tx_dir, "new_result.rds")
  if (!is.null(new_result_obj)) {
    saveRDS(new_result_obj, new_res_path)
  }
  new_res_hash <- if (file.exists(new_res_path)) unname(tools::md5sum(new_res_path)) else "no_hash"

  # 3. prior_backup.rds
  bak_path <- NULL
  bak_hash <- NULL
  if (had_prior_target) {
    bak_path <- if (!is.null(custom_backup_path)) custom_backup_path else file.path(tx_dir, "prior_backup.rds")
    if (!is.null(backup_obj)) {
      saveRDS(backup_obj, bak_path)
    }
    bak_hash <- if (file.exists(bak_path)) unname(tools::md5sum(bak_path)) else NULL
  }

  state_nonce <- if (!is.null(custom_state_nonce)) custom_state_nonce else tx_nonce
  state_owner <- if (!is.null(custom_state_owner)) custom_state_owner else orig_owner_nonce
  state_schema <- if (!is.null(custom_state_schema)) custom_state_schema else 1L

  # 4. State 1 (prepared)
  s1_file <- file.path(tx_dir, "state_1_prepared.rds")
  s1_hash <- NULL
  if (!omit_s1) {
    s1_schema <- if (!is.null(custom_s1_schema)) custom_s1_schema else state_schema
    s1_seq <- if (!is.null(custom_s1_seq)) custom_s1_seq else 1L
    s1_rec <- list(
      transaction_schema   = s1_schema,
      canonical_target     = canonical_target,
      transaction_nonce    = state_nonce,
      original_owner_nonce = state_owner,
      stage                = "prepared",
      stage_seq            = s1_seq,
      predecessor_hash     = NULL,
      had_prior_target     = had_prior_target,
      prior_target_hash    = prior_target_hash,
      backup_path          = bak_path,
      backup_hash          = bak_hash,
      temp_path            = new_res_path,
      new_result_hash      = new_res_hash,
      identity_key         = identity_key,
      cache_schema         = 1L,
      pid                  = Sys.getpid(),
      time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    saveRDS(s1_rec, s1_file)
    s1_hash <- unname(tools::md5sum(s1_file))
  }

  # 5. State 2 (publishing)
  s2_file <- file.path(tx_dir, "state_2_publishing.rds")
  s2_hash <- NULL
  if (stage %in% c("publishing", "committed") && !omit_s2) {
    s2_schema <- if (!is.null(custom_s2_schema)) custom_s2_schema else state_schema
    s2_seq <- if (!is.null(custom_s2_seq)) custom_s2_seq else 2L
    pred_h <- if (!is.null(custom_s2_pred_hash)) custom_s2_pred_hash else s1_hash
    s2_rec <- list(
      transaction_schema   = s2_schema,
      canonical_target     = canonical_target,
      transaction_nonce    = state_nonce,
      original_owner_nonce = state_owner,
      stage                = "publishing",
      stage_seq            = s2_seq,
      predecessor_hash     = pred_h,
      had_prior_target     = had_prior_target,
      prior_target_hash    = prior_target_hash,
      backup_path          = bak_path,
      backup_hash          = bak_hash,
      temp_path            = new_res_path,
      new_result_hash      = new_res_hash,
      identity_key         = identity_key,
      cache_schema         = 1L,
      pid                  = Sys.getpid(),
      time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    saveRDS(s2_rec, s2_file)
    s2_hash <- unname(tools::md5sum(s2_file))
  }

  # 6. State 3 (committed)
  s3_file <- file.path(tx_dir, "state_3_committed.rds")
  if (stage == "committed") {
    s3_schema <- if (!is.null(custom_s3_schema)) custom_s3_schema else state_schema
    s3_seq <- if (!is.null(custom_s3_seq)) custom_s3_seq else 3L
    pred_h3 <- if (!is.null(custom_s3_pred_hash)) custom_s3_pred_hash else s2_hash
    s3_rec <- list(
      transaction_schema   = s3_schema,
      canonical_target     = canonical_target,
      transaction_nonce    = state_nonce,
      original_owner_nonce = state_owner,
      stage                = "committed",
      stage_seq            = s3_seq,
      predecessor_hash     = pred_h3,
      had_prior_target     = had_prior_target,
      prior_target_hash    = prior_target_hash,
      backup_path          = bak_path,
      backup_hash          = bak_hash,
      temp_path            = new_res_path,
      new_result_hash      = new_res_hash,
      identity_key         = identity_key,
      cache_schema         = 1L,
      pid                  = Sys.getpid(),
      time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    saveRDS(s3_rec, s3_file)
  }

  if (!is.null(extra_files)) {
    for (fn in names(extra_files)) {
      writeLines(extra_files[[fn]], file.path(tx_dir, fn))
    }
  }

  tx_dir
}

# Standard test toy dataset
.make_toy_data <- function() {
  data.frame(
    y  = c(1, 1, 0, 0, 1, 0, 1, 0),
    q1 = c(2, 1, 0, 0, 2, 1, 2, 0),
    q2 = c(2, 2, 1, 0, 2, 0, 1, 0),
    q3 = c(1, 2, 1, 0, 2, 1, 1, 0)
  )
}

test_that("Legacy behavior without result_file preserves output class, structure, and RNG", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  set.seed(123)
  res_legacy <- cross_size_nested_cv(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2", "q3"),
    model_sizes   = 1:2,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    seed          = 999,
    progress      = FALSE,
    result_file   = NULL
  )

  expect_s3_class(res_legacy, "cross_size_nested_cv_result")
  expect_named(res_legacy, c(
    "summary", "outer_fold_results", "selected_models_by_outer_fold",
    "model_size_selection_frequency", "item_combination_selection_frequency",
    "cutoff_distribution", "outer_predictions", "model_sizes", "settings"
  ))
  expect_null(res_legacy$loaded_from_cache)
})

test_that("Input validation rejects malformed result_file and force_recompute", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  # force_recompute = TRUE with result_file = NULL
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, force_recompute = TRUE),
    "force_recompute = TRUE` was specified, but `result_file` is NULL"
  )

  # Invalid force_recompute types
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, force_recompute = "TRUE", result_file = "a.rds"),
    "force_recompute` must be a single logical value"
  )
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, force_recompute = c(TRUE, FALSE), result_file = "a.rds"),
    "force_recompute` must be a single logical value"
  )
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, force_recompute = NA, result_file = "a.rds"),
    "force_recompute` must be a single logical value"
  )

  # Invalid result_file types
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, result_file = 123),
    "result_file` must be a single non-empty character string"
  )
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, result_file = ""),
    "result_file` must be a single non-empty character string"
  )
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, result_file = c("a.rds", "b.rds")),
    "result_file` must be a single non-empty character string"
  )

  # result_file is existing directory
  tmp_dir <- tempfile("test_dir_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, result_file = tmp_dir),
    "points to an existing directory"
  )
})

test_that("1. Ownership binding: mismatched owner_nonce rejected; new recovery lock succeeds", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("own_bind_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Generate a valid baseline result object and envelope
  res_base <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_env <- readRDS(rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # 1a. Mismatched original_owner_nonce in state record vs identity.rds fails closed
  tx_dir_forged <- .create_test_tx_dir(
    target_path        = rds_path,
    tx_nonce           = "tx_forged_owner",
    orig_owner_nonce   = "owner_genuine",
    stage              = "prepared",
    had_prior_target   = TRUE,
    prior_target_hash  = base_hash,
    backup_obj         = base_env,
    new_result_obj     = base_env,
    identity_key       = base_env$identity_key,
    custom_state_owner = "owner_forged"
  )
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "corrupt, forged, or mismatched with identity"
  )
  expect_true(dir.exists(tx_dir_forged)) # preserved
  expect_identical(unname(tools::md5sum(rds_path)), base_hash) # untouched
  unlink(tx_dir_forged, recursive = TRUE, force = TRUE)
  unlink(.nested_cv_lock_dir(canonical_target), recursive = TRUE, force = TRUE)

  # 1b. Valid transaction with different creator nonce successfully recovers under new recovery lock
  tx_dir_valid <- .create_test_tx_dir(
    target_path        = rds_path,
    tx_nonce           = "tx_valid_prior",
    orig_owner_nonce   = "owner_old_session_12345",
    stage              = "prepared",
    had_prior_target   = TRUE,
    prior_target_hash  = base_hash,
    backup_obj         = base_env,
    new_result_obj     = base_env,
    identity_key       = base_env$identity_key
  )
  # When recovered, new lock has a freshly generated owner nonce, but must not require equality with original owner nonce
  res_rec <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_s3_class(res_rec, "cross_size_nested_cv_result")
  expect_false(dir.exists(tx_dir_valid)) # safely cleaned up
})

test_that("2. Transaction nonce binding: directory suffix mismatch fails closed", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("tx_nonce_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_env <- readRDS(rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # Directory suffix tx-dir_mismatch vs recorded nonce tx_nonce_recorded
  tx_dir <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "tx_recorded",
    custom_dir_nonce  = "tx_mismatched_dir",
    stage             = "prepared",
    had_prior_target  = TRUE,
    prior_target_hash = base_hash,
    backup_obj        = base_env,
    new_result_obj    = base_env,
    identity_key      = base_env$identity_key
  )

  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Transaction identity at .* is corrupt, invalid, or mismatched with directory name"
  )
  expect_true(dir.exists(tx_dir))
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)
})

test_that("3. Exact sibling-prefix path regression: common-prefix different-parent rejected without touching sibling or target", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("exact_sibling_prefix_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_env <- readRDS(rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # Construct exact sibling prefix relationship specified by Codex:
  # tx_dir = <parent>/.target.rds.tx-abc
  # bad artifact parent = <parent>/.target.rds.tx-abc_sibling
  # bad artifact = <parent>/.target.rds.tx-abc_sibling/prior_backup.rds
  tx_dir <- file.path(tmp_dir, ".target.rds.tx-abc")
  bad_parent <- file.path(tmp_dir, ".target.rds.tx-abc_sibling")
  dir.create(bad_parent)
  bad_artifact <- file.path(bad_parent, "prior_backup.rds")
  saveRDS(base_env, bad_artifact)
  bad_artifact_hash <- unname(tools::md5sum(bad_artifact))

  # Create valid-looking tx_dir pointing backup_path at bad_artifact
  .create_test_tx_dir(
    target_path        = rds_path,
    tx_nonce           = "abc",
    stage              = "prepared",
    had_prior_target   = TRUE,
    prior_target_hash  = base_hash,
    backup_obj         = base_env,
    new_result_obj     = base_env,
    identity_key       = base_env$identity_key,
    custom_backup_path = bad_artifact
  )

  # Recovery must reject bad artifact path without deleting/modifying bad_artifact or target.rds
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Invalid or out-of-directory backup_path in transaction"
  )

  # Assert byte preservation
  expect_true(file.exists(bad_artifact))
  expect_identical(unname(tools::md5sum(bad_artifact)), bad_artifact_hash)
  expect_true(dir.exists(bad_parent))
  expect_true(dir.exists(tx_dir))
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  unlink(tx_dir, recursive = TRUE, force = TRUE)
  unlink(bad_parent, recursive = TRUE, force = TRUE)
  unlink(.nested_cv_lock_dir(canonical_target), recursive = TRUE, force = TRUE)

  # Positive control: genuine tx_dir/prior_backup.rds is accepted
  tx_dir_pos <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "pos_control",
    stage             = "prepared",
    had_prior_target  = TRUE,
    prior_target_hash = base_hash,
    backup_obj        = base_env,
    new_result_obj    = base_env,
    identity_key      = base_env$identity_key
  )
  res_pos <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_s3_class(res_pos, "cross_size_nested_cv_result")
  expect_false(dir.exists(tx_dir_pos))
})

test_that("4. Strict pre-coercion metadata validation: table-driven matrix covering schema and sequence across all state positions", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("strict_meta_matrix_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  lock_dir <- .nested_cv_lock_dir(canonical_target)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_env <- readRDS(rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # Define table of positions across identity and all state records
  positions <- list(
    list(
      name         = "identity_schema",
      stage        = "prepared",
      param        = "custom_id_schema",
      expected_err = "Transaction identity at .* is corrupt, invalid, or mismatched",
      frac_val     = 1.5
    ),
    list(
      name         = "state_1_schema",
      stage        = "prepared",
      param        = "custom_s1_schema",
      expected_err = "state_1_prepared.rds at .* is corrupt, forged",
      frac_val     = 1.5
    ),
    list(
      name         = "state_2_schema",
      stage        = "publishing",
      param        = "custom_s2_schema",
      expected_err = "state_2_publishing.rds at .* is corrupt, forged",
      frac_val     = 1.5
    ),
    list(
      name         = "state_3_schema",
      stage        = "committed",
      param        = "custom_s3_schema",
      expected_err = "state_3_committed.rds at .* is corrupt, forged",
      frac_val     = 1.5
    ),
    list(
      name         = "state_1_stage_seq",
      stage        = "prepared",
      param        = "custom_s1_seq",
      expected_err = "state_1_prepared.rds at .* is corrupt, forged",
      frac_val     = 1.5
    ),
    list(
      name         = "state_2_stage_seq",
      stage        = "publishing",
      param        = "custom_s2_seq",
      expected_err = "state_2_publishing.rds at .* is corrupt, forged",
      frac_val     = 2.5
    ),
    list(
      name         = "state_3_stage_seq",
      stage        = "committed",
      param        = "custom_s3_seq",
      expected_err = "state_3_committed.rds at .* is corrupt, forged",
      frac_val     = 3.5
    )
  )

  for (pos in positions) {
    # 6 invalid value variations
    invalid_cases <- list(
      fractional = pos$frac_val,
      na         = NA_integer_,
      inf        = Inf,
      empty      = integer(0),
      vector     = c(1L, 2L),
      character  = "1"
    )

    for (case_name in names(invalid_cases)) {
      invalid_val <- invalid_cases[[case_name]]

      default_args <- list(
        target_path       = rds_path,
        tx_nonce          = paste0("meta_", pos$name, "_", case_name),
        stage             = pos$stage,
        had_prior_target  = TRUE,
        prior_target_hash = base_hash,
        backup_obj        = base_env,
        new_result_obj    = base_env,
        identity_key      = base_env$identity_key
      )
      default_args[[pos$param]] <- invalid_val

      # 1. Create fixture
      tx_dir <- do.call(.create_test_tx_dir, default_args)

      # 2. Snapshot all fixture files and computation count before
      tx_files_before <- sort(list.files(tx_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE))
      tx_hashes_before <- sapply(tx_files_before, tools::md5sum)
      target_hash_before <- unname(tools::md5sum(rds_path))
      comp_before <- .ncvroc_test_seams$computation_count

      # 3. Assert recovery fails closed
      expect_error(
        cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
        pos$expected_err,
        info = sprintf("Testing position: %s, case: %s", pos$name, case_name)
      )

      # 4. Assert computation count unchanged (rejection before computation)
      expect_identical(.ncvroc_test_seams$computation_count, comp_before)

      # 5. Assert lock and tx directory preserved
      expect_true(dir.exists(tx_dir))
      expect_true(dir.exists(lock_dir))

      # 6. Assert all fixture files remain byte-identical without modification or deletion
      tx_files_after <- sort(list.files(tx_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE))
      expect_identical(tx_files_after, tx_files_before)
      tx_hashes_after <- sapply(tx_files_after, tools::md5sum)
      expect_identical(tx_hashes_after, tx_hashes_before)
      expect_identical(unname(tools::md5sum(rds_path)), target_hash_before)

      # 7. Clean up this case
      unlink(tx_dir, recursive = TRUE, force = TRUE)
      unlink(lock_dir, recursive = TRUE, force = TRUE)
    }
  }

  # Positive controls for exact valid integer metadata at every state position
  pos_controls <- list(
    list(stage = "prepared", args = list(custom_id_schema = 1L, custom_s1_schema = 1L, custom_s1_seq = 1L)),
    list(stage = "publishing", args = list(custom_id_schema = 1L, custom_s1_schema = 1L, custom_s1_seq = 1L, custom_s2_schema = 1L, custom_s2_seq = 2L)),
    list(stage = "committed", args = list(custom_id_schema = 1L, custom_s1_schema = 1L, custom_s1_seq = 1L, custom_s2_schema = 1L, custom_s2_seq = 2L, custom_s3_schema = 1L, custom_s3_seq = 3L))
  )

  for (pc in pos_controls) {
    pc_args <- list(
      target_path       = rds_path,
      tx_nonce          = paste0("pos_ctrl_", pc$stage),
      stage             = pc$stage,
      had_prior_target  = TRUE,
      prior_target_hash = base_hash,
      backup_obj        = base_env,
      new_result_obj    = base_env,
      identity_key      = base_env$identity_key
    )
    pc_args <- utils::modifyList(pc_args, pc$args)
    tx_dir_pos <- do.call(.create_test_tx_dir, pc_args)

    res_pos <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
    expect_s3_class(res_pos, "cross_size_nested_cv_result")
    expect_false(dir.exists(tx_dir_pos))
    expect_identical(unname(tools::md5sum(rds_path)), base_hash)
  }
})

test_that("5. Valid recovery matrix: prior target preservation, new-target completion, backup restore, no-prior interruption", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("rec_matrix_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Base target
  res_base <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_env <- readRDS(rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # 5a. Prepared stage with valid intact prior target -> preserves target and cleans tx
  tx_dir_prep <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "preprecovery",
    stage             = "prepared",
    had_prior_target  = TRUE,
    prior_target_hash = base_hash,
    backup_obj        = base_env,
    new_result_obj    = base_env,
    identity_key      = base_env$identity_key
  )
  res_rec_prep <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_false(dir.exists(tx_dir_prep))
  expect_true(file.exists(rds_path))
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  # 5b. Publishing stage with missing target -> restores verified backup
  tx_dir_pub <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "pubrecovery",
    stage             = "publishing",
    had_prior_target  = TRUE,
    prior_target_hash = base_hash,
    backup_obj        = base_env,
    new_result_obj    = base_env,
    identity_key      = base_env$identity_key
  )
  unlink(rds_path, force = TRUE)
  res_rec_pub <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_true(file.exists(rds_path))
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)
  expect_false(dir.exists(tx_dir_pub))

  # 5c. Prepared stage without prior target and missing target -> cleans tx
  unlink(rds_path, force = TRUE)
  tx_dir_noprior <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "nopriorrecovery",
    stage             = "prepared",
    had_prior_target  = FALSE,
    prior_target_hash = NULL,
    backup_obj        = NULL,
    new_result_obj    = base_env,
    identity_key      = base_env$identity_key
  )
  res_rec_noprior <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_false(dir.exists(tx_dir_noprior))
  expect_true(file.exists(rds_path))

  # 5d. Committed stage with valid new target -> cleans tx
  tx_dir_com <- .create_test_tx_dir(
    target_path       = rds_path,
    tx_nonce          = "comrecovery",
    stage             = "committed",
    had_prior_target  = TRUE,
    prior_target_hash = base_hash,
    backup_obj        = base_env,
    new_result_obj    = readRDS(rds_path),
    identity_key      = base_env$identity_key
  )
  res_rec_com <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_false(dir.exists(tx_dir_com))
  expect_true(file.exists(rds_path))
})

test_that("6. State update failure seams: pending write, pending retention, read-back validate, final publish, committed failure with direct byte/hash assertions", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("seams_test_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "seam_target.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  lock_dir <- .nested_cv_lock_dir(canonical_target)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Initial baseline computation
  cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  base_hash <- unname(tools::md5sum(rds_path))

  # 6a. Injected failure before writing pending state (publishing)
  # Assert: state_1 preserved, no seq-2 pending file, prior target byte-identical, tx & lock preserved, error explains manual inspection
  .reset_ncvroc_test_seams()
  .ncvroc_test_seams$capture_artifact_hashes <- TRUE
  .ncvroc_test_seams$fail_state_write_pending <- "publishing"
  err_6a <- tryCatch(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err_6a, "Failed to transition to 'publishing' state", fixed = TRUE)
  expect_match(err_6a, "manual inspection", fixed = TRUE)
  expect_match(err_6a, "resolve lock explicitly", fixed = TRUE)

  tx_dirs_6a <- .find_nested_cv_tx_dirs(canonical_target)
  expect_equal(length(tx_dirs_6a), 1L)
  expect_match(err_6a, tx_dirs_6a[1], fixed = TRUE)

  # Target byte-identical
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  # state_1 byte-identical to recorded hash
  s1_file_6a <- .ncvroc_test_seams$last_state_1_file
  expect_true(file.exists(s1_file_6a))
  expect_identical(unname(tools::md5sum(s1_file_6a)), .ncvroc_test_seams$last_state_1_hash)

  # No pending seq-2 file exists
  pend_6a <- list.files(tx_dirs_6a[1], pattern = "^pending_state_2_", full.names = TRUE)
  expect_equal(length(pend_6a), 0L)

  # Lock preserved
  expect_true(dir.exists(lock_dir))

  unlink(tx_dirs_6a, recursive = TRUE, force = TRUE)
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()

  # 6b. Injected failure immediately after writing pending state (publishing)
  # Assert: pending sequence-2 file captured immediately after write and remains byte-identical after unwinding, state_1 byte-identical, target unmutated, lock/tx preserved
  .ncvroc_test_seams$capture_artifact_hashes <- TRUE
  .ncvroc_test_seams$fail_state_after_write_pending <- "publishing"
  err_6b <- tryCatch(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err_6b, "Injected failure immediately after writing pending state 'publishing'", fixed = TRUE)
  expect_match(err_6b, "manual inspection", fixed = TRUE)

  tx_dirs_6b <- .find_nested_cv_tx_dirs(canonical_target)
  expect_equal(length(tx_dirs_6b), 1L)
  expect_match(err_6b, tx_dirs_6b[1], fixed = TRUE)

  # Target unmutated
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  # state_1 byte-identical
  s1_file_6b <- .ncvroc_test_seams$last_state_1_file
  expect_true(file.exists(s1_file_6b))
  expect_identical(unname(tools::md5sum(s1_file_6b)), .ncvroc_test_seams$last_state_1_hash)

  # Pending seq-2 file remains and is byte-identical
  pend_file_6b <- .ncvroc_test_seams$last_pending_file
  expect_true(file.exists(pend_file_6b))
  expect_identical(unname(tools::md5sum(pend_file_6b)), .ncvroc_test_seams$last_pending_hash)
  pend_files_6b <- list.files(tx_dirs_6b[1], pattern = "^pending_state_2_publishing_", full.names = TRUE)
  expect_equal(length(pend_files_6b), 1L)
  expect_identical(pend_files_6b[1], pend_file_6b)

  # Lock preserved
  expect_true(dir.exists(lock_dir))

  unlink(tx_dirs_6b, recursive = TRUE, force = TRUE)
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()

  # 6c. Injected failure during read-back validation of pending state (publishing)
  # Assert: corrupted pending sequence-2 file captured after corruption seam remains byte-identical after unwinding, state_1 byte-identical, target unmutated, lock/tx preserved
  .ncvroc_test_seams$capture_artifact_hashes <- TRUE
  .ncvroc_test_seams$fail_state_validate <- "publishing"
  err_6c <- tryCatch(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err_6c, "Failed to read-back validate pending state record 'publishing'", fixed = TRUE)
  expect_match(err_6c, "manual inspection", fixed = TRUE)

  tx_dirs_6c <- .find_nested_cv_tx_dirs(canonical_target)
  expect_equal(length(tx_dirs_6c), 1L)
  expect_match(err_6c, tx_dirs_6c[1], fixed = TRUE)

  # Target unmutated
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  # state_1 byte-identical
  s1_file_6c <- .ncvroc_test_seams$last_state_1_file
  expect_true(file.exists(s1_file_6c))
  expect_identical(unname(tools::md5sum(s1_file_6c)), .ncvroc_test_seams$last_state_1_hash)

  # Corrupted pending file remains byte-identical
  pend_file_6c <- .ncvroc_test_seams$last_pending_file
  expect_true(file.exists(pend_file_6c))
  expect_identical(unname(tools::md5sum(pend_file_6c)), .ncvroc_test_seams$last_pending_hash)
  expect_identical(readLines(pend_file_6c), "corrupted")
  pend_files_6c <- list.files(tx_dirs_6c[1], pattern = "^pending_state_2_publishing_", full.names = TRUE)
  expect_equal(length(pend_files_6c), 1L)
  expect_identical(pend_files_6c[1], pend_file_6c)

  # Lock preserved
  expect_true(dir.exists(lock_dir))

  unlink(tx_dirs_6c, recursive = TRUE, force = TRUE)
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()

  # 6d. Injected failure during final publish of state (publishing)
  # Assert: pending sequence-2 file remains byte-identical after unwinding, state_1 byte-identical, target unmutated, lock/tx preserved
  .ncvroc_test_seams$capture_artifact_hashes <- TRUE
  .ncvroc_test_seams$fail_state_publish <- "publishing"
  err_6d <- tryCatch(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err_6d, "Injected failure at publishing final state 'publishing'", fixed = TRUE)
  expect_match(err_6d, "manual inspection", fixed = TRUE)

  tx_dirs_6d <- .find_nested_cv_tx_dirs(canonical_target)
  expect_equal(length(tx_dirs_6d), 1L)
  expect_match(err_6d, tx_dirs_6d[1], fixed = TRUE)

  # Target unmutated
  expect_identical(unname(tools::md5sum(rds_path)), base_hash)

  # state_1 byte-identical
  s1_file_6d <- .ncvroc_test_seams$last_state_1_file
  expect_true(file.exists(s1_file_6d))
  expect_identical(unname(tools::md5sum(s1_file_6d)), .ncvroc_test_seams$last_state_1_hash)

  # Pending seq-2 file remains byte-identical
  pend_file_6d <- .ncvroc_test_seams$last_pending_file
  expect_true(file.exists(pend_file_6d))
  expect_identical(unname(tools::md5sum(pend_file_6d)), .ncvroc_test_seams$last_pending_hash)
  pend_files_6d <- list.files(tx_dirs_6d[1], pattern = "^pending_state_2_publishing_", full.names = TRUE)
  expect_equal(length(pend_files_6d), 1L)
  expect_identical(pend_files_6d[1], pend_file_6d)

  # Lock preserved
  expect_true(dir.exists(lock_dir))

  unlink(tx_dirs_6d, recursive = TRUE, force = TRUE)
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()

  # 6e. Injected failure during committed state update (after successful verified target publication)
  # Assert: verified new target hash identical, state_1 and state_2 hashes identical, pending seq-3 hash identical, new target envelope readable/valid, lock/tx preserved
  .ncvroc_test_seams$capture_artifact_hashes <- TRUE
  .ncvroc_test_seams$fail_state_after_write_pending <- "committed"
  err_6e <- tryCatch(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err_6e, "failed to record final 'committed' state", fixed = TRUE)
  expect_match(err_6e, "manual inspection", fixed = TRUE)

  tx_dirs_6e <- .find_nested_cv_tx_dirs(canonical_target)
  expect_equal(length(tx_dirs_6e), 1L)
  expect_match(err_6e, tx_dirs_6e[1], fixed = TRUE)
  expect_match(err_6e, canonical_target, fixed = TRUE)

  # Verified new target hash
  new_target_hash <- .ncvroc_test_seams$last_target_published_hash
  expect_true(!is.null(new_target_hash))
  expect_identical(unname(tools::md5sum(rds_path)), new_target_hash)

  # Target envelope readable and valid
  target_new_env <- readRDS(rds_path)
  expect_s3_class(target_new_env$result, "cross_size_nested_cv_result")
  expect_identical(target_new_env$format, "NCVROC_cross_size_nested_cv_result")
  expect_true(target_new_env$complete)

  # state_1 and state_2 byte-identical
  s1_file_6e <- .ncvroc_test_seams$last_state_1_file
  expect_true(file.exists(s1_file_6e))
  expect_identical(unname(tools::md5sum(s1_file_6e)), .ncvroc_test_seams$last_state_1_hash)

  s2_file_6e <- .ncvroc_test_seams$last_state_2_file
  expect_true(file.exists(s2_file_6e))
  expect_identical(unname(tools::md5sum(s2_file_6e)), .ncvroc_test_seams$last_state_2_hash)

  # Pending seq-3 file remains byte-identical
  pend_file_6e <- .ncvroc_test_seams$last_pending_file
  expect_true(file.exists(pend_file_6e))
  expect_identical(unname(tools::md5sum(pend_file_6e)), .ncvroc_test_seams$last_pending_hash)
  pend_files_6e <- list.files(tx_dirs_6e[1], pattern = "^pending_state_3_committed_", full.names = TRUE)
  expect_equal(length(pend_files_6e), 1L)
  expect_identical(pend_files_6e[1], pend_file_6e)

  # Lock preserved
  expect_true(dir.exists(lock_dir))

  unlink(tx_dirs_6e, recursive = TRUE, force = TRUE)
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()
})

test_that("6f. Disabled capture regression: normal transaction leaves all last_* capture fields NULL", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("capture_disabled_test_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "prod_target.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_false(isTRUE(.ncvroc_test_seams$capture_artifact_hashes))
  expect_null(.ncvroc_test_seams$last_pending_file)
  expect_null(.ncvroc_test_seams$last_pending_hash)
  expect_null(.ncvroc_test_seams$last_state_1_file)
  expect_null(.ncvroc_test_seams$last_state_1_hash)
  expect_null(.ncvroc_test_seams$last_state_2_file)
  expect_null(.ncvroc_test_seams$last_state_2_hash)
  expect_null(.ncvroc_test_seams$last_target_published_hash)

  res <- cross_size_nested_cv(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2"),
    model_sizes   = 1,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    seed          = 123,
    progress      = FALSE,
    result_file   = rds_path
  )

  expect_s3_class(res, "cross_size_nested_cv_result")
  expect_true(file.exists(rds_path))

  # Verify all capture fields remain NULL after successful transaction
  expect_null(.ncvroc_test_seams$last_pending_file)
  expect_null(.ncvroc_test_seams$last_pending_hash)
  expect_null(.ncvroc_test_seams$last_state_1_file)
  expect_null(.ncvroc_test_seams$last_state_1_hash)
  expect_null(.ncvroc_test_seams$last_state_2_file)
  expect_null(.ncvroc_test_seams$last_state_2_hash)
  expect_null(.ncvroc_test_seams$last_target_published_hash)

  .reset_ncvroc_test_seams()
})

test_that("7. Real relative-path alias: setwd to temporary directory and resolve canonical locks", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  old_wd <- getwd()
  tmp_workspace <- tempfile("rel_alias_test_")
  dir.create(tmp_workspace)
  dir.create(file.path(tmp_workspace, "subdir"))

  # Strictly ensure working directory restoration
  on.exit({
    setwd(old_wd)
    unlink(tmp_workspace, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  setwd(tmp_workspace)

  rel_path <- "subdir/result.rds"
  dot_rel_path <- "./subdir/result.rds"
  abs_path <- normalizePath(file.path(tmp_workspace, rel_path), winslash = "/", mustWork = FALSE)

  # Acquire lock using relative path
  lock_token <- .acquire_nested_cv_lock(.canonicalize_result_path(rel_path))
  expect_true(dir.exists(lock_token$lock_dir))

  # Calling cross_size_nested_cv with ./subdir/result.rds must be rejected by the same lock
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = dot_rel_path),
    "Cannot acquire exclusive lock"
  )

  # Calling cross_size_nested_cv with absolute spelling must be rejected by the same lock
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = abs_path),
    "Cannot acquire exclusive lock"
  )

  .release_nested_cv_lock(lock_token)
  expect_false(dir.exists(lock_token$lock_dir))

  # Successfully compute via relative path
  res <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rel_path)
  expect_s3_class(res, "cross_size_nested_cv_result")
  expect_true(file.exists(abs_path))
})

test_that("8. Lock replacement / ownership prevents deleting other callers' locks", {
  .reset_ncvroc_test_seams()
  tmp_dir <- tempfile("lock_own_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "test.rds")
  canonical_target <- .canonicalize_result_path(rds_path)
  lock_dir <- .nested_cv_lock_dir(canonical_target)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  token1 <- .acquire_nested_cv_lock(canonical_target)
  expect_true(dir.exists(lock_dir))

  owner_file <- file.path(lock_dir, "owner.rds")
  forged_owner <- readRDS(owner_file)
  forged_owner$ownership_nonce <- "forged_nonce_9999"
  saveRDS(forged_owner, owner_file)

  # Caller 1 attempts release: must NOT delete replaced lock!
  .release_nested_cv_lock(token1)
  expect_true(dir.exists(lock_dir))

  # Restore genuine owner and verify matching release deletes lock
  forged_owner$ownership_nonce <- token1$ownership_nonce
  saveRDS(forged_owner, owner_file)
  .release_nested_cv_lock(token1)
  expect_false(dir.exists(lock_dir))
})

test_that("9. Owner-record failure cleans reservation and prevents unlocked analysis", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("owner_fail_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "owner_fail.rds")
  lock_dir <- .nested_cv_lock_dir(.canonicalize_result_path(rds_path))
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  .ncvroc_test_seams$owner_write_fail <- TRUE
  comp_count <- .ncvroc_test_seams$computation_count

  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path),
    "Failed to write and verify lock owner metadata"
  )

  # No computation was performed, and lock reservation was cleaned up
  expect_identical(.ncvroc_test_seams$computation_count, comp_count)
  expect_false(dir.exists(lock_dir))
  .reset_ncvroc_test_seams()
})

test_that("10. Pre-existing sidecars and candidate collision names are never overwritten or deleted", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("sidecar_test_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "target.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  bak_file <- file.path(tmp_dir, "target.rds.bak")
  journal_file <- file.path(tmp_dir, "target.rds.journal")
  writeLines("important user backup", bak_file)
  writeLines("important user journal", journal_file)

  res <- cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  expect_s3_class(res, "cross_size_nested_cv_result")
  expect_true(file.exists(rds_path))

  expect_identical(readLines(bak_file), "important user backup")
  expect_identical(readLines(journal_file), "important user journal")
})

test_that("11. Read-only cache hit succeeds with write probe failure; miss fails before computation", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("ro_suite_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "ro_result.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Miss: compute initial result
  res1 <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)

  # Read-only hit with write_probe_fail = TRUE
  .ncvroc_test_seams$write_probe_fail <- TRUE
  comp_count <- .ncvroc_test_seams$computation_count
  res2 <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)

  expect_identical(.ncvroc_test_seams$computation_count, comp_count)
  expect_identical(res1$summary, res2$summary)

  # Miss with write_probe_fail = TRUE fails before computation
  non_existent <- file.path(tmp_dir, "non_existent.rds")
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = non_existent),
    "Parent directory for `result_file` is not writable"
  )
  expect_identical(.ncvroc_test_seams$computation_count, comp_count)
  .reset_ncvroc_test_seams()
})

test_that("12. Publish failure matrix: fail before publish, during replace, during verify, during restore, and during cleanup", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("matrix_test_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "matrix.rds")
  canonical_matrix_path <- .canonicalize_result_path(rds_path)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path)
  md5_orig <- tools::md5sum(rds_path)

  # 12a. Fail before publish
  .ncvroc_test_seams$fail_before_publish <- TRUE
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Injected failure before publish"
  )
  expect_identical(unname(tools::md5sum(rds_path)), unname(md5_orig))
  .reset_ncvroc_test_seams()

  # 12b. Fail during replace -> restored from backup
  .ncvroc_test_seams$fail_during_replace <- TRUE
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Restored prior result file from verified backup"
  )
  expect_identical(unname(tools::md5sum(rds_path)), unname(md5_orig))
  .reset_ncvroc_test_seams()

  # 12c. Fail during verify -> restored from backup
  .ncvroc_test_seams$fail_during_verify <- TRUE
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "successfully restored previous result file"
  )
  expect_identical(unname(tools::md5sum(rds_path)), unname(md5_orig))
  .reset_ncvroc_test_seams()

  # 12d. Fail during verify and restore -> preserves transaction directory
  .ncvroc_test_seams$fail_during_verify <- TRUE
  .ncvroc_test_seams$fail_during_restore <- TRUE
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Preserved transaction at"
  )
  tx_dirs <- .find_nested_cv_tx_dirs(canonical_matrix_path)
  expect_equal(length(tx_dirs), 1L)
  unlink(tx_dirs, recursive = TRUE, force = TRUE)
  unlink(.nested_cv_lock_dir(canonical_matrix_path), recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()

  # 12e. Fail during cleanup -> reports exact leftovers
  .ncvroc_test_seams$fail_during_cleanup <- TRUE
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = rds_path, force_recompute = TRUE),
    "Remaining leftovers at"
  )
  tx_dirs <- .find_nested_cv_tx_dirs(canonical_matrix_path)
  expect_equal(length(tx_dirs), 1L)
  unlink(tx_dirs, recursive = TRUE, force = TRUE)
  unlink(.nested_cv_lock_dir(canonical_matrix_path), recursive = TRUE, force = TRUE)
  .reset_ncvroc_test_seams()
})

test_that("13. Identity vs Provenance: requested execution settings affect identity; original execution provenance preserved on hit", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("ident_prov_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "prov_test.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Compute with tuning = "auto"
  res_orig <- cross_size_nested_cv(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2", "q3"),
    model_sizes   = 1,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    tuning        = "auto",
    seed          = 123,
    progress      = FALSE,
    result_file   = rds_path
  )

  expect_true(!is.null(res_orig$settings$execution_plan))
  orig_plan <- res_orig$settings$execution_plan

  # Changing requested tuning to "off" produces identity mismatch
  expect_error(
    cross_size_nested_cv(
      data          = d,
      outcome       = "y",
      items         = c("q1", "q2", "q3"),
      model_sizes   = 1,
      outer_folds   = 2,
      inner_folds   = 2,
      outer_repeats = 1,
      tuning        = "off",
      seed          = 123,
      progress      = FALSE,
      result_file   = rds_path
    ),
    "analysis identity does not match"
  )

  # Matching call returns cached result with original execution_plan provenance intact
  comp_before <- .ncvroc_test_seams$computation_count
  res_hit <- cross_size_nested_cv(
    data          = d,
    outcome       = "y",
    items         = c("q1", "q2", "q3"),
    model_sizes   = 1,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    tuning        = "auto",
    seed          = 123,
    progress      = FALSE,
    result_file   = rds_path
  )

  expect_identical(.ncvroc_test_seams$computation_count, comp_before)
  expect_identical(res_hit$settings$execution_plan, orig_plan)
})

test_that("14. Multi-analysis path independence and RNG invariance bit-for-bit", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("multi_path_")
  dir.create(tmp_dir)
  path_a <- file.path(tmp_dir, "analysis_a.rds")
  path_b <- file.path(tmp_dir, "analysis_b.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Compute A & B
  res_a1 <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = path_a)
  res_b1 <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 2, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = path_b)

  # Cache hits preserve RNG bit-for-bit
  set.seed(9999)
  rng_before <- .Random.seed
  res_a2 <- cross_size_nested_cv(d, "y", c("q1", "q2", "q3"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, seed = 123, progress = FALSE, result_file = path_a)
  rng_after <- .Random.seed

  expect_identical(rng_before, rng_after)
  expect_identical(res_a1$summary, res_a2$summary)
  expect_identical(res_b1$summary, res_b1$summary)
  expect_false(identical(res_a1$summary, res_b1$summary))
})

test_that("15. Corrupt RDS, invalid format, wrong schema, and incomplete markers fail closed", {
  .reset_ncvroc_test_seams()
  d <- .make_toy_data()

  tmp_dir <- tempfile("corrupt_test_")
  dir.create(tmp_dir)
  rds_path <- file.path(tmp_dir, "corrupt.rds")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # 1. Corrupt byte stream
  writeBin(as.raw(c(1, 2, 3, 4)), rds_path)
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, result_file = rds_path),
    "could not be read"
  )

  # 2. Legacy / plain RDS without envelope
  saveRDS(list(a = 1, b = 2), rds_path)
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, result_file = rds_path),
    "not a valid NCVROC result envelope"
  )

  # 3. Unsupported schema version
  saveRDS(list(format = "NCVROC_cross_size_nested_cv_result", cache_schema = 999L, complete = TRUE), rds_path)
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, result_file = rds_path),
    "unsupported cache schema version"
  )

  # 4. Incomplete marker
  saveRDS(list(format = "NCVROC_cross_size_nested_cv_result", cache_schema = 1L, complete = FALSE), rds_path)
  expect_error(
    cross_size_nested_cv(d, "y", c("q1", "q2"), model_sizes = 1, outer_folds = 2, inner_folds = 2, outer_repeats = 1, result_file = rds_path),
    "is marked incomplete"
  )
})
