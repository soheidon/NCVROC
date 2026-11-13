# checkpoint.R -- Backend-neutral completed outer-fold checkpoint persistence and resume
#
# Internal functions (all @keywords internal):
#   Opt-in, outer-fold-level disk persistence and resume for
#   nested_sum_roc(..., parallel = "none") and
#   nested_sum_roc(..., parallel = "native_outer_fold").
#   Independent from v0.23.2 R/cache.R completed-result cache.

# Legacy v0.27 (schema_version = 1) identifiers for explicit rejection
.CHECKPOINT_LEGACY_FORMAT_ID                <- "NCVROC_nested_sum_roc_outer_fold_checkpoint"
.CHECKPOINT_LEGACY_IDENTITY_FORMAT_ID       <- "NCVROC_nested_sum_roc_checkpoint_identity"
.CHECKPOINT_LEGACY_SCHEMA_VERSION           <- 1L

# Canonical v0.28 (schema_version = 2) identifiers
.CHECKPOINT_CANONICAL_FORMAT_ID                <- "NCVROC_nested_sum_roc_canonical_outer_fold_checkpoint"
.CHECKPOINT_CANONICAL_IDENTITY_FORMAT_ID       <- "NCVROC_nested_sum_roc_canonical_checkpoint_identity"
.CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION  <- 2L
.CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION  <- 2L
.CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION   <- 2L
.CHECKPOINT_STATISTICAL_CONTRACT_ID            <- "NCVROC_nested_sum_roc_stat_contract_v1"
.CHECKPOINT_LOCK_SCHEMA_VERSION                <- 1L
.CHECKPOINT_NATIVE_SCHEMA_VERSION              <- 1L
.CHECKPOINT_HASH_ALGORITHM                     <- "sha256"
.CHECKPOINT_SUPPORTED_PRODUCERS                <- c("r_serial", "scope_b_native_outer_fold")

# Opt-in internal test seams (default OFF; zero I/O when checkpointing is disabled)
.ncvroc_checkpoint_test_seams <- new.env(parent = emptyenv())
.reset_ncvroc_checkpoint_test_seams <- function() {
  .ncvroc_checkpoint_test_seams$nonce_counter                  <- 0L
  .ncvroc_checkpoint_test_seams$lock_owner_write_fail          <- FALSE
  .ncvroc_checkpoint_test_seams$simulate_sha256_unavailable    <- FALSE
  .ncvroc_checkpoint_test_seams$fail_before_compute            <- FALSE
  .ncvroc_checkpoint_test_seams$fail_during_compute_fold       <- NULL
  .ncvroc_checkpoint_test_seams$fail_between_complete_and_write <- NULL
  .ncvroc_checkpoint_test_seams$fail_during_pending_write      <- NULL
  .ncvroc_checkpoint_test_seams$fail_during_readback           <- NULL
  .ncvroc_checkpoint_test_seams$fail_before_publish            <- NULL
  .ncvroc_checkpoint_test_seams$fail_during_replace            <- NULL
  .ncvroc_checkpoint_test_seams$fail_after_publish             <- NULL
  .ncvroc_checkpoint_test_seams$fail_before_aggregate          <- FALSE
  .ncvroc_checkpoint_test_seams$capture_artifact_hashes        <- FALSE
  .ncvroc_checkpoint_test_seams$last_pending_file              <- NULL
  .ncvroc_checkpoint_test_seams$last_pending_hash              <- NULL
  .ncvroc_checkpoint_test_seams$last_backup_file               <- NULL
  .ncvroc_checkpoint_test_seams$last_backup_hash               <- NULL
  .ncvroc_checkpoint_test_seams$last_published_file            <- NULL
  .ncvroc_checkpoint_test_seams$last_published_hash            <- NULL
  .ncvroc_checkpoint_test_seams$native_call_count              <- 0L
  .ncvroc_checkpoint_test_seams$native_folds_executed          <- 0L
  .ncvroc_checkpoint_test_seams$r_folds_executed               <- 0L
  invisible(NULL)
}
.reset_ncvroc_checkpoint_test_seams()

# ---- Hashing helpers (no RNG mutation; SHA-256 required, no downgrade) ----

.cp_require_sha256 <- function() {
  avail <- !isTRUE(.ncvroc_checkpoint_test_seams$simulate_sha256_unavailable) &&
    exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)
  if (!isTRUE(avail)) {
    stop(
      "Outer-fold checkpointing requires SHA-256 support (`tools::sha256sum`, available in R >= 4.5.0); refusing to downgrade to a weaker hash algorithm.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.cp_hash_bytes <- function(raw_bytes) {
  .cp_require_sha256()
  tolower(unname(tools::sha256sum(bytes = raw_bytes)))
}

.cp_hash_object <- function(obj) {
  raw_bytes <- serialize(obj, NULL, version = 3L)
  .cp_hash_bytes(raw_bytes)
}

.cp_hash_file <- function(path) {
  .cp_require_sha256()
  if (!file.exists(path)) {
    stop("Cannot hash non-existent file: ", path, call. = FALSE)
  }
  tolower(unname(tools::sha256sum(files = path)))
}

# ---- Argument & path validation ----

.cp_validate_checkpoint_dir_arg <- function(checkpoint_dir) {
  if (is.null(checkpoint_dir)) {
    return(NULL)
  }
  if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L ||
      is.na(checkpoint_dir) || !nzchar(trimws(checkpoint_dir))) {
    stop("`checkpoint_dir` must be NULL or a non-empty single character path.", call. = FALSE)
  }
  checkpoint_dir
}

.cp_validate_scalar_logical <- function(val, arg_name) {
  if (!is.logical(val) || length(val) != 1L || is.na(val)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg_name), call. = FALSE)
  }
  isTRUE(val)
}

.cp_is_symlink_or_reparse <- function(path) {
  rl <- suppressWarnings(tryCatch(Sys.readlink(path), error = function(e) ""))
  is.character(rl) && length(rl) == 1L && !is.na(rl) && nzchar(rl)
}

.cp_assert_contained <- function(root_canonical, target_path, must_exist = FALSE) {
  root_clean <- sub("/+$", "", normalizePath(root_canonical, winslash = "/", mustWork = TRUE))
  if (must_exist && !file.exists(target_path) && !dir.exists(target_path)) {
    stop("Expected checkpoint path does not exist: ", target_path, call. = FALSE)
  }
  if ((file.exists(target_path) || dir.exists(target_path)) && .cp_is_symlink_or_reparse(target_path)) {
    stop("Checkpoint artifact must not be a symbolic link or reparse point: ", target_path, call. = FALSE)
  }
  parent_dir <- dirname(target_path)
  if (!dir.exists(parent_dir)) {
    stop("Checkpoint parent directory does not exist: ", parent_dir, call. = FALSE)
  }
  if (.cp_is_symlink_or_reparse(parent_dir)) {
    stop("Checkpoint parent directory must not be a symbolic link or reparse point: ", parent_dir, call. = FALSE)
  }
  parent_norm <- sub("/+$", "", normalizePath(parent_dir, winslash = "/", mustWork = TRUE))
  target_norm <- paste0(parent_norm, "/", basename(target_path))
  if (file.exists(target_path) || dir.exists(target_path)) {
    target_norm <- sub("/+$", "", normalizePath(target_path, winslash = "/", mustWork = TRUE))
  }
  prefix <- paste0(root_clean, "/")
  if (!identical(substr(target_norm, 1L, nchar(prefix)), prefix)) {
    stop("Checkpoint path escapes canonical checkpoint root: ", target_path, call. = FALSE)
  }
  target_norm
}

.cp_resolve_root <- function(checkpoint_dir) {
  if (file.exists(checkpoint_dir) && !dir.exists(checkpoint_dir)) {
    stop("`checkpoint_dir` exists and is not a directory: ", checkpoint_dir, call. = FALSE)
  }
  if (dir.exists(checkpoint_dir) && .cp_is_symlink_or_reparse(checkpoint_dir)) {
    stop("`checkpoint_dir` must not be a symbolic link or reparse point: ", checkpoint_dir, call. = FALSE)
  }
  if (!dir.exists(checkpoint_dir)) {
    ok <- dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(checkpoint_dir)) {
      stop("Failed to create checkpoint directory: ", checkpoint_dir, call. = FALSE)
    }
  }
  root_canonical <- sub("/+$", "", normalizePath(checkpoint_dir, winslash = "/", mustWork = TRUE))
  if (.cp_is_symlink_or_reparse(root_canonical)) {
    stop("Canonical `checkpoint_dir` must not be a symbolic link or reparse point: ", root_canonical, call. = FALSE)
  }

  folds_dir   <- file.path(root_canonical, "folds")
  pending_dir <- file.path(root_canonical, "pending")
  for (subd in c(folds_dir, pending_dir)) {
    if (file.exists(subd) && !dir.exists(subd)) {
      stop("Checkpoint subdirectory path exists as a non-directory file: ", subd, call. = FALSE)
    }
    if (dir.exists(subd) && .cp_is_symlink_or_reparse(subd)) {
      stop("Checkpoint subdirectory must not be a symbolic link or reparse point: ", subd, call. = FALSE)
    }
    if (!dir.exists(subd)) {
      ok <- dir.create(subd, recursive = FALSE, showWarnings = FALSE)
      if (!ok && !dir.exists(subd)) {
        stop("Failed to create checkpoint subdirectory: ", subd, call. = FALSE)
      }
    }
    .cp_assert_contained(root_canonical, subd, must_exist = TRUE)
  }

  list(
    root_canonical = root_canonical,
    identity_file  = file.path(root_canonical, "identity.rds"),
    folds_dir      = folds_dir,
    pending_dir    = pending_dir,
    lock_dir       = file.path(root_canonical, ".lock")
  )
}

.cp_fold_filename <- function(repeat_id, outer_fold_id) {
  r_id <- as.integer(repeat_id)
  f_id <- as.integer(outer_fold_id)
  if (is.na(r_id) || r_id < 1L || is.na(f_id) || f_id < 1L) {
    stop("Invalid repeat_id or outer_fold_id for checkpoint path.", call. = FALSE)
  }
  sprintf("repeat_%03d_fold_%03d.rds", r_id, f_id)
}

# ---- Exclusive directory lock ----

.cp_next_nonce <- function() {
  .ncvroc_checkpoint_test_seams$nonce_counter <- as.integer(.ncvroc_checkpoint_test_seams$nonce_counter) + 1L
  sprintf(
    "pid%d_ctr%d_t%.6f",
    Sys.getpid(),
    .ncvroc_checkpoint_test_seams$nonce_counter,
    as.numeric(Sys.time())
  )
}

.cp_acquire_lock <- function(paths) {
  lock_dir <- paths$lock_dir
  .cp_assert_contained(paths$root_canonical, lock_dir, must_exist = FALSE)
  owner_file <- file.path(lock_dir, "owner.rds")

  created <- dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE)
  if (!isTRUE(created)) {
    owner_desc <- "owner record missing or unreadable"
    if (file.exists(owner_file)) {
      owner <- tryCatch(readRDS(owner_file), error = function(e) NULL)
      if (is.list(owner)) {
        owner_desc <- sprintf(
          "pid=%s, hostname=%s, acquired_at=%s, nonce=%s",
          as.character(owner$pid %||% "unknown"),
          as.character(owner$hostname %||% "unknown"),
          as.character(owner$acquired_at %||% "unknown"),
          as.character(owner$nonce %||% "unknown")
        )
      } else {
        owner_desc <- "corrupt owner.rds"
      }
    }
    stop(
      sprintf(
        "Checkpoint directory is locked (%s). Target: %s. Owner: %s. Remove the lock directory manually only after verifying no run is active.",
        lock_dir, paths$root_canonical, owner_desc
      ),
      call. = FALSE
    )
  }

  nonce <- .cp_next_nonce()
  owner_record <- list(
    schema_version = .CHECKPOINT_LOCK_SCHEMA_VERSION,
    canonical_root = paths$root_canonical,
    nonce          = nonce,
    pid            = Sys.getpid(),
    hostname       = unname(Sys.info()[["nodename"]]),
    acquired_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  )

  write_ok <- tryCatch({
    if (isTRUE(.ncvroc_checkpoint_test_seams$lock_owner_write_fail)) {
      stop("Simulated lock owner write failure.", call. = FALSE)
    }
    saveRDS(owner_record, owner_file, compress = FALSE)
    rb <- readRDS(owner_file)
    is.list(rb) && identical(rb$nonce, nonce) && identical(rb$canonical_root, paths$root_canonical)
  }, error = function(e) {
    FALSE
  })

  if (!isTRUE(write_ok)) {
    unlink(lock_dir, recursive = TRUE, force = TRUE)
    stop("Failed to write and verify checkpoint lock owner record at: ", owner_file, call. = FALSE)
  }

  list(
    acquired       = TRUE,
    canonical_root = paths$root_canonical,
    lock_dir       = lock_dir,
    owner_file     = owner_file,
    nonce          = nonce
  )
}

.cp_release_lock <- function(lock_token) {
  if (is.null(lock_token) || !isTRUE(lock_token$acquired)) {
    return(invisible(FALSE))
  }
  if (!dir.exists(lock_token$lock_dir) || !file.exists(lock_token$owner_file)) {
    return(invisible(FALSE))
  }
  owner <- tryCatch(readRDS(lock_token$owner_file), error = function(e) NULL)
  if (!is.list(owner) ||
      !identical(owner$nonce, lock_token$nonce) ||
      !identical(owner$canonical_root, lock_token$canonical_root)) {
    return(invisible(FALSE))
  }
  unlink(lock_token$lock_dir, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

# ---- Analysis & split identity ----

.cp_build_split_descriptor <- function(spec) {
  list(
    repeat_id       = as.integer(spec$repeat_id),
    outer_fold_id   = as.integer(spec$outer_fold_id),
    fold_name       = as.character(spec$fold_name),
    train_indices   = as.integer(spec$train_indices),
    test_indices    = as.integer(spec$test_indices),
    inner_folds     = spec$inner_folds,
    has_seed        = isTRUE(spec$has_seed),
    inner_seed_base = as.integer(spec$inner_seed_base)
  )
}

.cp_order_fold_specs <- function(fold_specs) {
  r_ids <- vapply(fold_specs, function(s) as.integer(s$repeat_id), integer(1L))
  f_ids <- vapply(fold_specs, function(s) as.integer(s$outer_fold_id), integer(1L))
  ord <- order(r_ids, f_ids)
  fold_specs[ord]
}

.cp_build_analysis_identity <- function(x_mat,
                                        y,
                                        items,
                                        outcome_col,
                                        positive_label,
                                        negative_label,
                                        fold_specs_ordered,
                                        min_items,
                                        max_items,
                                        cutoff_method,
                                        preselect_top_n,
                                        preselect_by,
                                        selection_criterion,
                                        outer_k,
                                        inner_k,
                                        outer_repeats,
                                        inner_repeats,
                                        stratified,
                                        seed) {
  data_repr <- list(
    x_mat          = unname(x_mat),
    colnames       = colnames(x_mat),
    y              = as.integer(y),
    items          = as.character(items),
    outcome_col    = as.character(outcome_col),
    positive_label = positive_label,
    negative_label = negative_label
  )
  data_digest <- .cp_hash_object(data_repr)

  fold_identities <- lapply(fold_specs_ordered, function(spec) {
    desc <- .cp_build_split_descriptor(spec)
    list(
      repeat_id     = desc$repeat_id,
      outer_fold_id = desc$outer_fold_id,
      fold_name     = desc$fold_name,
      split_hash    = .cp_hash_object(desc)
    )
  })

  outer_folds_digest <- .cp_hash_object(lapply(fold_specs_ordered, .cp_build_split_descriptor))

  options_repr <- list(
    min_items           = as.integer(min_items),
    max_items           = as.integer(max_items),
    cutoff_method       = as.character(cutoff_method),
    preselect_top_n     = as.integer(preselect_top_n),
    preselect_by        = as.character(preselect_by),
    selection_criterion = as.character(selection_criterion),
    prefer_fewer_items  = TRUE,
    outer_k             = as.integer(outer_k),
    inner_k             = as.integer(inner_k),
    outer_repeats       = as.integer(outer_repeats),
    inner_repeats       = as.integer(inner_repeats),
    stratified          = isTRUE(stratified),
    seed                = seed
  )

  pkg_ver <- as.character(utils::packageVersion("NCVROC"))

  core <- list(
    format_id                    = .CHECKPOINT_IDENTITY_FORMAT_ID,
    schema_version               = .CHECKPOINT_IDENTITY_SCHEMA_VERSION,
    hash_algorithm               = .CHECKPOINT_HASH_ALGORITHM,
    native_result_schema_version = .CHECKPOINT_NATIVE_SCHEMA_VERSION,
    envelope_schema_version      = .CHECKPOINT_ENVELOPE_SCHEMA_VERSION,
    package_version              = pkg_ver,
    backend                      = .CHECKPOINT_BACKEND_ID,
    data_digest                  = data_digest,
    outer_folds_digest           = outer_folds_digest,
    fold_identities              = fold_identities,
    options                      = options_repr
  )

  list(
    format_id              = .CHECKPOINT_IDENTITY_FORMAT_ID,
    schema_version         = .CHECKPOINT_IDENTITY_SCHEMA_VERSION,
    analysis_identity_hash = .cp_hash_object(core),
    core                   = core,
    provenance             = list(
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      r_version  = paste(R.version$major, R.version$minor, sep = "."),
      pid        = Sys.getpid(),
      hostname   = unname(Sys.info()[["nodename"]])
    )
  )
}

.cp_validate_or_create_identity <- function(paths, identity_record) {
  id_file <- paths$identity_file
  .cp_assert_contained(paths$root_canonical, id_file, must_exist = FALSE)

  if (!file.exists(id_file)) {
    existing_folds   <- list.files(paths$folds_dir, all.files = TRUE, no.. = TRUE)
    existing_pending <- list.files(paths$pending_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing_folds) > 0L || length(existing_pending) > 0L) {
      stop(
        "Checkpoint directory is missing identity.rds while fold or pending artifacts exist. Refusing to overwrite or adopt unverified artifacts.",
        call. = FALSE
      )
    }
    pending_id <- file.path(paths$pending_dir, paste0("identity_", .cp_next_nonce(), ".rds.tmp"))
    .cp_assert_contained(paths$root_canonical, pending_id, must_exist = FALSE)
    saveRDS(identity_record, pending_id, compress = FALSE)
    rb <- tryCatch(readRDS(pending_id), error = function(e) NULL)
    if (!is.list(rb) || !identical(rb$analysis_identity_hash, identity_record$analysis_identity_hash)) {
      stop("Failed read-back validation for checkpoint identity.rds.", call. = FALSE)
    }
    ok <- file.rename(pending_id, id_file)
    if (!isTRUE(ok) || !file.exists(id_file)) {
      stop("Failed to publish checkpoint identity.rds.", call. = FALSE)
    }
    return(invisible(identity_record))
  }

  stored <- tryCatch(readRDS(id_file), error = function(e) {
    stop("Corrupt checkpoint identity.rds at ", id_file, ": ", conditionMessage(e), call. = FALSE)
  })
  if (!is.list(stored) ||
      !identical(stored$format_id, .CHECKPOINT_IDENTITY_FORMAT_ID) ||
      !identical(as.integer(stored$schema_version), .CHECKPOINT_IDENTITY_SCHEMA_VERSION) ||
      !is.list(stored$core)) {
    stop("Unsupported or malformed checkpoint identity.rds at: ", id_file, call. = FALSE)
  }
  if (!identical(stored$core$hash_algorithm, identity_record$core$hash_algorithm)) {
    stop(sprintf("Checkpoint hash algorithm mismatch: stored '%s', current '%s'.",
                 as.character(stored$core$hash_algorithm),
                 as.character(identity_record$core$hash_algorithm)),
         call. = FALSE)
  }
  if (!identical(stored$core$backend, identity_record$core$backend)) {
    stop(sprintf("Checkpoint backend mismatch: stored '%s', current '%s'.",
                 as.character(stored$core$backend),
                 as.character(identity_record$core$backend)),
         call. = FALSE)
  }
  if (!identical(stored$core$package_version, identity_record$core$package_version)) {
    stop(sprintf("Checkpoint package version mismatch: stored '%s', current '%s'.",
                 as.character(stored$core$package_version),
                 as.character(identity_record$core$package_version)),
         call. = FALSE)
  }
  if (!identical(as.integer(stored$core$native_result_schema_version),
                 as.integer(identity_record$core$native_result_schema_version)) ||
      !identical(as.integer(stored$core$envelope_schema_version),
                 as.integer(identity_record$core$envelope_schema_version))) {
    stop("Checkpoint schema version mismatch with existing directory.", call. = FALSE)
  }
  if (!identical(stored$analysis_identity_hash, identity_record$analysis_identity_hash)) {
    stop(
      sprintf(
        "Checkpoint analysis identity mismatch (stored %s != current %s). Use a new checkpoint_dir for a different dataset, parameters, or fold split.",
        substr(as.character(stored$analysis_identity_hash), 1L, 12L),
        substr(as.character(identity_record$analysis_identity_hash), 1L, 12L)
      ),
      call. = FALSE
    )
  }
  invisible(stored)
}

# ---- Envelope & payload validation ----

.cp_validate_native_payload <- function(payload, fold_spec, n_items_total) {
  if (!is.list(payload)) {
    return("payload is not a list")
  }
  if (!identical(as.integer(payload$schema_version), .CHECKPOINT_NATIVE_SCHEMA_VERSION)) {
    return("payload schema_version mismatch")
  }
  if (!isTRUE(payload$completed) || !identical(as.integer(payload$status_code), 0L)) {
    return("payload is not marked completed with status_code == 0")
  }
  if (!identical(as.integer(payload$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(payload$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(as.character(payload$outer_fold), as.character(fold_spec$fold_name))) {
    return("payload fold identity does not match expected fold")
  }
  sel_idx <- payload$selected_col_indices
  if (!is.integer(sel_idx) || length(sel_idx) < 1L || anyNA(sel_idx) ||
      any(sel_idx < 1L) || any(sel_idx > n_items_total)) {
    return("payload selected_col_indices invalid")
  }
  if (!identical(as.integer(payload$n_items), length(sel_idx))) {
    return("payload n_items mismatch")
  }
  preds <- payload$predictions
  req_cols <- c("row_index", "true_outcome", "predicted_score", "predicted_class")
  if (!is.data.frame(preds) || !all(req_cols %in% colnames(preds))) {
    return("payload predictions data.frame missing required columns")
  }
  expected_rows <- as.integer(fold_spec$test_indices + 1L)
  if (!identical(as.integer(preds$row_index), expected_rows)) {
    return("payload predictions row_index does not match outer test split")
  }
  req_scalars <- c("n_candidates_evaluated", "selected_global_rank",
                   "inner_mean_auc", "inner_mean_sensitivity",
                   "inner_mean_specificity", "inner_mean_youden",
                   "auc", "cutoff", "sensitivity", "specificity",
                   "youden", "accuracy", "ppv", "npv")
  for (nm in req_scalars) {
    val <- payload[[nm]]
    if (is.null(val) || !is.numeric(val) || length(val) != 1L) {
      return(sprintf("payload scalar '%s' missing or invalid", nm))
    }
  }
  NULL
}

.cp_validate_envelope <- function(env, identity_record, fold_spec, split_hash, n_items_total) {
  if (!is.list(env)) {
    return("envelope is not a list")
  }
  if (!identical(env$format_id, .CHECKPOINT_FORMAT_ID)) {
    return("envelope format_id mismatch")
  }
  if (!identical(as.integer(env$schema_version), .CHECKPOINT_ENVELOPE_SCHEMA_VERSION)) {
    return("envelope schema_version mismatch")
  }
  if (!identical(env$hash_algorithm, .CHECKPOINT_HASH_ALGORITHM)) {
    return("envelope hash_algorithm mismatch")
  }
  if (!identical(env$analysis_identity_hash, identity_record$analysis_identity_hash)) {
    return("envelope analysis_identity_hash mismatch")
  }
  if (!identical(as.integer(env$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(env$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(as.character(env$outer_fold), as.character(fold_spec$fold_name))) {
    return("envelope fold identity mismatch")
  }
  if (!identical(env$split_hash, split_hash)) {
    return("envelope split_hash mismatch")
  }
  if (!identical(env$completion_status, "complete")) {
    return("envelope completion_status is not 'complete'")
  }
  prod <- env$producer
  if (!is.list(prod) ||
      !identical(prod$package_version, identity_record$core$package_version) ||
      !identical(prod$backend, .CHECKPOINT_BACKEND_ID) ||
      !identical(as.integer(prod$native_result_schema_version), .CHECKPOINT_NATIVE_SCHEMA_VERSION)) {
    return("envelope producer metadata mismatch")
  }
  payload_err <- .cp_validate_native_payload(env$payload, fold_spec, n_items_total)
  if (!is.null(payload_err)) {
    return(payload_err)
  }
  computed_hash <- .cp_hash_object(env$payload)
  if (!identical(env$payload_hash, computed_hash)) {
    return("envelope payload_hash mismatch")
  }
  NULL
}

.cp_build_fold_envelope <- function(identity_record, fold_spec, split_hash, nat_res) {
  list(
    format_id              = .CHECKPOINT_FORMAT_ID,
    schema_version         = .CHECKPOINT_ENVELOPE_SCHEMA_VERSION,
    hash_algorithm         = .CHECKPOINT_HASH_ALGORITHM,
    analysis_identity_hash = identity_record$analysis_identity_hash,
    repeat_id              = as.integer(fold_spec$repeat_id),
    outer_fold_id          = as.integer(fold_spec$outer_fold_id),
    outer_fold             = as.character(fold_spec$fold_name),
    split_hash             = split_hash,
    completion_status      = "complete",
    payload                = nat_res,
    payload_hash           = .cp_hash_object(nat_res),
    producer               = list(
      package_version              = identity_record$core$package_version,
      backend                      = .CHECKPOINT_BACKEND_ID,
      native_result_schema_version = .CHECKPOINT_NATIVE_SCHEMA_VERSION,
      backend_provenance           = as.character(nat_res$backend_provenance %||% .CHECKPOINT_BACKEND_ID)
    ),
    provenance             = list(
      saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      pid      = Sys.getpid(),
      hostname = unname(Sys.info()[["nodename"]])
    ),
    validation             = list(
      validated_on_write = TRUE,
      read_back_verified = TRUE
    )
  )
}

# ---- Inspection & classification ----

.cp_inspect_directory <- function(paths, identity_record, fold_specs_ordered, n_items_total) {
  expected_files <- vapply(fold_specs_ordered, function(s) {
    .cp_fold_filename(s$repeat_id, s$outer_fold_id)
  }, character(1L))

  # Check for unexpected/orphan files in folds/
  actual_folds_entries <- list.files(paths$folds_dir, all.files = TRUE, no.. = TRUE)
  unexpected_folds <- setdiff(actual_folds_entries, expected_files)
  if (length(unexpected_folds) > 0L) {
    stop(
      "Unexpected or orphan checkpoint artifact(s) in folds/: ",
      paste(unexpected_folds, collapse = ", "),
      ". Resolve or use a new checkpoint_dir.",
      call. = FALSE
    )
  }

  # Check for leftover pending artifacts
  actual_pending <- list.files(paths$pending_dir, all.files = TRUE, no.. = TRUE)
  if (length(actual_pending) > 0L) {
    stop(
      "Unresolved pending checkpoint artifact(s) in pending/: ",
      paste(actual_pending, collapse = ", "),
      ". Previous write may have been interrupted; inspect and remove stale pending artifacts or use a new checkpoint_dir.",
      call. = FALSE
    )
  }

  split_hashes <- vapply(identity_record$core$fold_identities, `[[`, character(1L), "split_hash")
  states <- vector("list", length(fold_specs_ordered))

  for (i in seq_along(fold_specs_ordered)) {
    spec <- fold_specs_ordered[[i]]
    fname <- expected_files[[i]]
    fpath <- file.path(paths$folds_dir, fname)
    .cp_assert_contained(paths$root_canonical, fpath, must_exist = FALSE)

    if (!file.exists(fpath)) {
      states[[i]] <- list(
        status     = "ABSENT",
        fold_name  = spec$fold_name,
        file_path  = fpath,
        split_hash = split_hashes[[i]],
        payload    = NULL
      )
      next
    }

    if (dir.exists(fpath)) {
      stop("Expected fold checkpoint file is a directory: ", fpath, call. = FALSE)
    }

    env <- tryCatch(readRDS(fpath), error = function(e) {
      stop(
        sprintf("Corrupt checkpoint file for %s (%s): %s",
                spec$fold_name, fpath, conditionMessage(e)),
        call. = FALSE
      )
    })
    err <- .cp_validate_envelope(env, identity_record, spec, split_hashes[[i]], n_items_total)
    if (!is.null(err)) {
      stop(
        sprintf("Invalid checkpoint envelope for %s (%s): %s",
                spec$fold_name, fpath, err),
        call. = FALSE
      )
    }
    states[[i]] <- list(
      status     = "COMPLETE_VALID",
      fold_name  = spec$fold_name,
      file_path  = fpath,
      split_hash = split_hashes[[i]],
      payload    = env$payload
    )
  }

  states
}

# ---- Safe staging, read-back validation, publication & replacement ----

.cp_publish_fold <- function(paths,
                             identity_record,
                             fold_spec,
                             split_hash,
                             nat_res,
                             n_items_total,
                             allow_replace = FALSE) {
  fold_name <- fold_spec$fold_name
  if (identical(.ncvroc_checkpoint_test_seams$fail_between_complete_and_write, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_between_complete_and_write)) {
    stop("Simulated failure between fold completion and checkpoint write for ", fold_name, call. = FALSE)
  }

  payload_err <- .cp_validate_native_payload(nat_res, fold_spec, n_items_total)
  if (!is.null(payload_err)) {
    stop("Cannot persist invalid fold result for ", fold_name, ": ", payload_err, call. = FALSE)
  }

  env <- .cp_build_fold_envelope(identity_record, fold_spec, split_hash, nat_res)
  nonce <- .cp_next_nonce()
  pending_name <- sprintf("repeat_%03d_fold_%03d_%s.rds.tmp",
                          as.integer(fold_spec$repeat_id),
                          as.integer(fold_spec$outer_fold_id),
                          nonce)
  pending_path <- file.path(paths$pending_dir, pending_name)
  .cp_assert_contained(paths$root_canonical, pending_path, must_exist = FALSE)

  if (identical(.ncvroc_checkpoint_test_seams$fail_during_pending_write, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_during_pending_write)) {
    # Write truncated/partial file to simulate interrupted pending write
    writeBin(as.raw(c(0x01, 0x02, 0x03)), pending_path)
    if (isTRUE(.ncvroc_checkpoint_test_seams$capture_artifact_hashes)) {
      .ncvroc_checkpoint_test_seams$last_pending_file <- pending_path
      .ncvroc_checkpoint_test_seams$last_pending_hash <- .cp_hash_file(pending_path)
    }
    stop("Simulated failure during pending checkpoint write for ", fold_name, call. = FALSE)
  }

  saveRDS(env, pending_path, compress = FALSE)
  if (isTRUE(.ncvroc_checkpoint_test_seams$capture_artifact_hashes)) {
    .ncvroc_checkpoint_test_seams$last_pending_file <- pending_path
    .ncvroc_checkpoint_test_seams$last_pending_hash <- .cp_hash_file(pending_path)
  }

  if (identical(.ncvroc_checkpoint_test_seams$fail_during_readback, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_during_readback)) {
    stop("Simulated failure during checkpoint read-back validation for ", fold_name, call. = FALSE)
  }

  rb_env <- tryCatch(readRDS(pending_path), error = function(e) {
    stop("Read-back failed for staged checkpoint ", pending_path, ": ", conditionMessage(e), call. = FALSE)
  })
  rb_err <- .cp_validate_envelope(rb_env, identity_record, fold_spec, split_hash, n_items_total)
  if (!is.null(rb_err)) {
    stop("Read-back validation failed for staged checkpoint ", pending_path, ": ", rb_err, call. = FALSE)
  }

  if (identical(.ncvroc_checkpoint_test_seams$fail_before_publish, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_before_publish)) {
    stop("Simulated failure before checkpoint publication for ", fold_name, call. = FALSE)
  }

  target_path <- file.path(paths$folds_dir, .cp_fold_filename(fold_spec$repeat_id, fold_spec$outer_fold_id))
  .cp_assert_contained(paths$root_canonical, target_path, must_exist = FALSE)

  if (file.exists(target_path)) {
    if (!isTRUE(allow_replace)) {
      stop(
        sprintf(
          "Refusing to overwrite existing fold checkpoint for %s (%s) without force_recompute = TRUE.",
          fold_name, target_path
        ),
        call. = FALSE
      )
    }

    backup_name <- sprintf("backup_repeat_%03d_fold_%03d_%s.rds",
                           as.integer(fold_spec$repeat_id),
                           as.integer(fold_spec$outer_fold_id),
                           nonce)
    backup_path <- file.path(paths$pending_dir, backup_name)
    .cp_assert_contained(paths$root_canonical, backup_path, must_exist = FALSE)

    moved_old <- file.rename(target_path, backup_path)
    if (!isTRUE(moved_old) || !file.exists(backup_path)) {
      stop("Failed to stage backup of prior checkpoint before replacement: ", target_path, call. = FALSE)
    }
    if (isTRUE(.ncvroc_checkpoint_test_seams$capture_artifact_hashes)) {
      .ncvroc_checkpoint_test_seams$last_backup_file <- backup_path
      .ncvroc_checkpoint_test_seams$last_backup_hash <- .cp_hash_file(backup_path)
    }

    pub_ok <- FALSE
    if (!(identical(.ncvroc_checkpoint_test_seams$fail_during_replace, fold_name) ||
          isTRUE(.ncvroc_checkpoint_test_seams$fail_during_replace))) {
      pub_ok <- isTRUE(file.rename(pending_path, target_path)) && file.exists(target_path)
    }

    if (!pub_ok) {
      # Restore prior valid checkpoint
      restored <- file.rename(backup_path, target_path)
      if (!isTRUE(restored) || !file.exists(target_path)) {
        stop(
          sprintf(
            "Replacement failed for %s AND automatic restore failed; backup preserved at %s, pending at %s.",
            fold_name, backup_path, pending_path
          ),
          call. = FALSE
        )
      }
      stop(
        sprintf(
          "Checkpoint replacement failed for %s; prior valid checkpoint restored at %s (pending artifact preserved at %s).",
          fold_name, target_path, pending_path
        ),
        call. = FALSE
      )
    }

    # Verify newly published target before removing backup
    pub_rb <- tryCatch(readRDS(target_path), error = function(e) NULL)
    pub_err <- if (is.null(pub_rb)) "readRDS failed" else .cp_validate_envelope(pub_rb, identity_record, fold_spec, split_hash, n_items_total)
    if (!is.null(pub_err)) {
      unlink(target_path, force = TRUE)
      file.rename(backup_path, target_path)
      stop("Published replacement verification failed for ", fold_name, ": ", pub_err, call. = FALSE)
    }

    unlink(backup_path, force = TRUE)
  } else {
    pub_ok <- isTRUE(file.rename(pending_path, target_path)) && file.exists(target_path)
    if (!pub_ok) {
      stop(
        sprintf(
          "Failed to publish fold checkpoint for %s to %s (pending artifact preserved at %s).",
          fold_name, target_path, pending_path
        ),
        call. = FALSE
      )
    }
  }

  if (isTRUE(.ncvroc_checkpoint_test_seams$capture_artifact_hashes)) {
    .ncvroc_checkpoint_test_seams$last_published_file <- target_path
    .ncvroc_checkpoint_test_seams$last_published_hash <- .cp_hash_file(target_path)
  }

  if (identical(.ncvroc_checkpoint_test_seams$fail_after_publish, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_after_publish)) {
    stop("Simulated failure immediately after checkpoint publication for ", fold_name, call. = FALSE)
  }

  invisible(target_path)
}

# ---- Scope B checkpointed execution driver ----

.run_native_outer_folds_checkpointed <- function(x_mat,
                                                 y_vec,
                                                 outer_folds,
                                                 items,
                                                 outcome_col,
                                                 positive_label,
                                                 negative_label,
                                                 min_items,
                                                 max_items,
                                                 cutoff_method,
                                                 preselect_by,
                                                 selection_criterion,
                                                 preselect_top_n,
                                                 outer_k,
                                                 inner_k,
                                                 outer_repeats,
                                                 inner_repeats,
                                                 stratified,
                                                 seed,
                                                 num_threads,
                                                 checkpoint_dir,
                                                 resume,
                                                 force_recompute,
                                                 progress,
                                                 verbose,
                                                 schema_version = 1L) {
  total_combos <- .count_total_combos(length(items), min_items, max_items)
  n_candidates <- as.integer(min(preselect_top_n, total_combos))

  # Precompute all outer and inner fold assignments before any skips (RNG invariance)
  fold_specs_raw <- .build_native_outer_fold_specs(
    outer_folds   = outer_folds,
    y             = y_vec,
    n_total       = length(y_vec),
    inner_k       = inner_k,
    inner_repeats = as.integer(inner_repeats),
    n_candidates  = n_candidates,
    seed          = seed
  )
  fold_specs <- .cp_order_fold_specs(fold_specs_raw)
  n_folds <- length(fold_specs)
  n_items_total <- length(items)

  paths <- .cp_resolve_root(checkpoint_dir)
  lock_token <- .cp_acquire_lock(paths)
  on.exit(.cp_release_lock(lock_token), add = TRUE)

  identity_record <- .cp_build_analysis_identity(
    x_mat               = x_mat,
    y                   = y_vec,
    items               = items,
    outcome_col         = outcome_col,
    positive_label      = positive_label,
    negative_label      = negative_label,
    fold_specs_ordered  = fold_specs,
    min_items           = min_items,
    max_items           = max_items,
    cutoff_method       = cutoff_method,
    preselect_top_n     = preselect_top_n,
    preselect_by        = preselect_by,
    selection_criterion = selection_criterion,
    outer_k             = outer_k,
    inner_k             = inner_k,
    outer_repeats       = outer_repeats,
    inner_repeats       = inner_repeats,
    stratified          = stratified,
    seed                = seed
  )
  .cp_validate_or_create_identity(paths, identity_record)

  states <- .cp_inspect_directory(paths, identity_record, fold_specs, n_items_total)

  # Enforce resume = FALSE non-overwrite rule
  if (!isTRUE(resume) && !isTRUE(force_recompute)) {
    existing_valid <- vapply(states, function(st) identical(st$status, "COMPLETE_VALID"), logical(1L))
    if (any(existing_valid)) {
      existing_names <- vapply(states[existing_valid], `[[`, character(1L), "fold_name")
      stop(
        sprintf(
          "Existing fold checkpoint(s) found (%s) with `resume = FALSE` and `force_recompute = FALSE`. Use a new `checkpoint_dir`, set `resume = TRUE` to reuse, or set `force_recompute = TRUE` to recompute and replace.",
          paste(existing_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  reused_idx   <- integer(0L)
  compute_idx  <- integer(0L)
  final_raw    <- vector("list", n_folds)

  for (i in seq_len(n_folds)) {
    if (identical(states[[i]]$status, "COMPLETE_VALID") && isTRUE(resume) && !isTRUE(force_recompute)) {
      reused_idx <- c(reused_idx, i)
      final_raw[[i]] <- states[[i]]$payload
    } else {
      compute_idx <- c(compute_idx, i)
    }
  }

  reused_names  <- vapply(fold_specs[reused_idx], `[[`, character(1L), "fold_name")
  compute_names <- vapply(fold_specs[compute_idx], `[[`, character(1L), "fold_name")

  if (isTRUE(progress) || isTRUE(verbose)) {
    message(sprintf(
      "Checkpoint: %d/%d outer fold(s) reused; %d/%d outer fold(s) to compute.",
      length(reused_idx), n_folds, length(compute_idx), n_folds
    ))
  }

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_compute)) {
    stop("Simulated failure before computing remaining folds.", call. = FALSE)
  }

  if (length(compute_idx) > 0L) {
    split_hashes <- vapply(states, `[[`, character(1L), "split_hash")
    allow_rep <- isTRUE(force_recompute)

    if (as.integer(num_threads) == 1L) {
      # Serial execution: run and persist one fold at a time so each completed fold
      # is durably saved before the next fold begins.
      for (idx in compute_idx) {
        spec <- fold_specs[[idx]]
        if (!is.null(.ncvroc_checkpoint_test_seams$fail_during_compute_fold) &&
            identical(.ncvroc_checkpoint_test_seams$fail_during_compute_fold, spec$fold_name)) {
          stop("Simulated failure during native computation of ", spec$fold_name, call. = FALSE)
        }
        .ncvroc_checkpoint_test_seams$native_call_count <- as.integer(.ncvroc_checkpoint_test_seams$native_call_count) + 1L
        .ncvroc_checkpoint_test_seams$native_folds_executed <- as.integer(.ncvroc_checkpoint_test_seams$native_folds_executed) + 1L
        res_list <- evaluate_outer_folds_native_cpp(
          x                   = x_mat,
          y                   = y_vec,
          fold_specs          = list(spec),
          min_items           = as.integer(min_items),
          max_items           = as.integer(max_items),
          cutoff_method       = cutoff_method,
          preselect_by        = preselect_by,
          selection_criterion = selection_criterion,
          preselect_top_n     = as.integer(preselect_top_n),
          prefer_fewer_items  = TRUE,
          num_threads         = 1L,
          schema_version      = as.integer(schema_version),
          throw_on_error      = FALSE
        )
        nat_res <- res_list[[1L]]
        if (!isTRUE(nat_res$completed) || !identical(as.integer(nat_res$status_code), 0L)) {
          stop(
            sprintf(
              "Native outer fold failed at %s (repeat_id=%d, outer_fold_id=%d, status=%d): %s",
              as.character(nat_res$outer_fold),
              as.integer(nat_res$repeat_id),
              as.integer(nat_res$outer_fold_id),
              as.integer(nat_res$status_code),
              as.character(nat_res$status_message)
            ),
            call. = FALSE
          )
        }
        .cp_publish_fold(
          paths           = paths,
          identity_record = identity_record,
          fold_spec       = spec,
          split_hash      = split_hashes[[idx]],
          nat_res         = nat_res,
          n_items_total   = n_items_total,
          allow_replace   = allow_rep
        )
        final_raw[[idx]] <- nat_res
      }
    } else {
      # Parallel TBB execution: receive per-fold success/failure records without
      # throwing across worker boundaries; persist every completed fold on the main
      # R control thread before reporting any deterministic failure.
      specs_to_run <- fold_specs[compute_idx]
      .ncvroc_checkpoint_test_seams$native_call_count <- as.integer(.ncvroc_checkpoint_test_seams$native_call_count) + 1L
      .ncvroc_checkpoint_test_seams$native_folds_executed <- as.integer(.ncvroc_checkpoint_test_seams$native_folds_executed) + length(specs_to_run)
      res_list <- evaluate_outer_folds_native_cpp(
        x                   = x_mat,
        y                   = y_vec,
        fold_specs          = specs_to_run,
        min_items           = as.integer(min_items),
        max_items           = as.integer(max_items),
        cutoff_method       = cutoff_method,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        preselect_top_n     = as.integer(preselect_top_n),
        prefer_fewer_items  = TRUE,
        num_threads         = as.integer(num_threads),
        schema_version      = as.integer(schema_version),
        throw_on_error      = FALSE
      )

      first_failure <- NULL
      for (k in seq_along(compute_idx)) {
        idx <- compute_idx[[k]]
        spec <- fold_specs[[idx]]
        nat_res <- res_list[[k]]
        if (!identical(as.integer(nat_res$repeat_id), as.integer(spec$repeat_id)) ||
            !identical(as.integer(nat_res$outer_fold_id), as.integer(spec$outer_fold_id))) {
          stop("Native outer fold result order mismatch at ", spec$fold_name, call. = FALSE)
        }
        if (!is.null(.ncvroc_checkpoint_test_seams$fail_during_compute_fold) &&
            identical(.ncvroc_checkpoint_test_seams$fail_during_compute_fold, spec$fold_name)) {
          nat_res$completed <- FALSE
          nat_res$status_code <- 99L
          nat_res$status_message <- paste0("Simulated worker failure at ", spec$fold_name)
        }
        if (isTRUE(nat_res$completed) && identical(as.integer(nat_res$status_code), 0L)) {
          .cp_publish_fold(
            paths           = paths,
            identity_record = identity_record,
            fold_spec       = spec,
            split_hash      = split_hashes[[idx]],
            nat_res         = nat_res,
            n_items_total   = n_items_total,
            allow_replace   = allow_rep
          )
          final_raw[[idx]] <- nat_res
        } else if (is.null(first_failure)) {
          first_failure <- nat_res
        }
      }

      if (!is.null(first_failure)) {
        stop(
          sprintf(
            "Native outer fold failed at %s (repeat_id=%d, outer_fold_id=%d, status=%d): %s",
            as.character(first_failure$outer_fold),
            as.integer(first_failure$repeat_id),
            as.integer(first_failure$outer_fold_id),
            as.integer(first_failure$status_code),
            as.character(first_failure$status_message)
          ),
          call. = FALSE
        )
      }
    }

    if (isTRUE(progress) || isTRUE(verbose)) {
      message(sprintf(
        "Checkpoint: %d newly computed outer fold(s) persisted (%d/%d complete).",
        length(compute_idx), n_folds, n_folds
      ))
    }
  }

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_aggregate)) {
    stop("Simulated failure before final aggregation.", call. = FALSE)
  }

  list(
    raw_results = final_raw,
    per_fold    = lapply(final_raw, .convert_native_outer_fold_result_to_r, items = items),
    provenance  = list(
      checkpoint_enabled       = TRUE,
      canonical_checkpoint_dir = paths$root_canonical,
      analysis_identity_hash   = identity_record$analysis_identity_hash,
      reused_folds             = reused_names,
      computed_folds           = compute_names,
      n_reused                 = length(reused_idx),
      n_computed               = length(compute_idx),
      resume                   = isTRUE(resume),
      force_recompute          = isTRUE(force_recompute)
    )
  )
}
