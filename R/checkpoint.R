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
  .ncvroc_checkpoint_test_seams$batch_size_override            <- NULL
  .ncvroc_checkpoint_test_seams$fail_during_batch_write        <- NULL
  .ncvroc_checkpoint_test_seams$fail_during_model_size_write   <- NULL
  .ncvroc_checkpoint_test_seams$fail_before_outer_promotion    <- NULL
  .ncvroc_checkpoint_test_seams$r_batches_executed             <- 0L
  .ncvroc_checkpoint_test_seams$native_batches_executed        <- 0L
  .ncvroc_checkpoint_test_seams$model_sizes_executed           <- 0L
  .ncvroc_checkpoint_test_seams$simulate_interrupt_at          <- NULL
  .ncvroc_checkpoint_test_seams$interrupt_waves_completed      <- 0L
  .ncvroc_checkpoint_test_seams$r_interrupt_hook               <- NULL
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
  inner_1based <- lapply(spec$inner_folds, function(cand_folds) {
    lapply(cand_folds, function(f_idx) as.integer(f_idx) + 1L)
  })
  list(
    repeat_id       = as.integer(spec$repeat_id),
    outer_fold_id   = as.integer(spec$outer_fold_id),
    fold_name       = as.character(spec$fold_name),
    train_indices   = as.integer(spec$train_indices) + 1L,
    test_indices    = as.integer(spec$test_indices) + 1L,
    inner_folds     = inner_1based,
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
                                        seed,
                                        producer_backend = NULL) {
  x_canon <- unname(as.matrix(x_mat))
  storage.mode(x_canon) <- "double"

  data_repr <- list(
    x_mat          = x_canon,
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
    seed                = if (is.null(seed)) NULL else as.integer(seed)
  )

  pkg_ver <- as.character(utils::packageVersion("NCVROC"))

  core <- list(
    format_id               = .CHECKPOINT_CANONICAL_IDENTITY_FORMAT_ID,
    schema_version          = .CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION,
    envelope_schema_version = .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION,
    payload_schema_version  = .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION,
    statistical_contract_id = .CHECKPOINT_STATISTICAL_CONTRACT_ID,
    hash_algorithm          = .CHECKPOINT_HASH_ALGORITHM,
    data_digest             = data_digest,
    outer_folds_digest      = outer_folds_digest,
    fold_identities         = fold_identities,
    options                 = options_repr
  )

  list(
    format_id              = .CHECKPOINT_CANONICAL_IDENTITY_FORMAT_ID,
    schema_version         = .CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION,
    analysis_identity_hash = .cp_hash_object(core),
    core                   = core,
    provenance             = list(
      created_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      package_version  = pkg_ver,
      producer_backend = if (is.null(producer_backend)) NA_character_ else as.character(producer_backend),
      r_version        = paste(R.version$major, R.version$minor, sep = "."),
      pid              = Sys.getpid(),
      hostname         = unname(Sys.info()[["nodename"]])
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
  if (!is.list(stored) || !is.list(stored$core)) {
    stop("Unsupported or malformed checkpoint identity.rds at: ", id_file, call. = FALSE)
  }
  if (identical(stored$format_id, .CHECKPOINT_LEGACY_IDENTITY_FORMAT_ID) ||
      identical(suppressWarnings(as.integer(stored$schema_version)), .CHECKPOINT_LEGACY_SCHEMA_VERSION)) {
    stop(
      sprintf(
        "Unsupported legacy v0.27 checkpoint schema (schema_version = %s) at %s; use a new checkpoint_dir (canonical schema_version = %d required).",
        as.character(stored$schema_version), id_file, .CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION
      ),
      call. = FALSE
    )
  }
  if (!identical(stored$format_id, .CHECKPOINT_CANONICAL_IDENTITY_FORMAT_ID) ||
      !identical(suppressWarnings(as.integer(stored$schema_version)), .CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION)) {
    stop("Unsupported or malformed checkpoint identity.rds at: ", id_file, call. = FALSE)
  }
  if (!identical(stored$core$hash_algorithm, identity_record$core$hash_algorithm)) {
    stop(sprintf("Checkpoint hash algorithm mismatch: stored '%s', current '%s'.",
                 as.character(stored$core$hash_algorithm),
                 as.character(identity_record$core$hash_algorithm)),
         call. = FALSE)
  }
  if (!identical(stored$core$statistical_contract_id, identity_record$core$statistical_contract_id)) {
    stop(sprintf("Checkpoint statistical contract mismatch: stored '%s', current '%s'.",
                 as.character(stored$core$statistical_contract_id),
                 as.character(identity_record$core$statistical_contract_id)),
         call. = FALSE)
  }
  if (!identical(suppressWarnings(as.integer(stored$core$payload_schema_version)),
                 as.integer(identity_record$core$payload_schema_version)) ||
      !identical(suppressWarnings(as.integer(stored$core$envelope_schema_version)),
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

# ---- Scalar & vector type validation helpers ----

.cp_is_valid_int_scalar <- function(v, min_val = -Inf, max_val = Inf, allow_na = FALSE) {
  if (!is.numeric(v) || is.logical(v) || length(v) != 1L) {
    return(FALSE)
  }
  if (is.nan(v)) {
    return(FALSE)
  }
  if (is.na(v)) {
    return(isTRUE(allow_na))
  }
  is.finite(v) && (v == floor(v)) && (v >= min_val) && (v <= max_val)
}

.cp_is_valid_int_vec <- function(v, min_val = -Inf, max_val = Inf, allow_na = FALSE) {
  if (!is.numeric(v) || is.logical(v)) {
    return(FALSE)
  }
  if (any(is.nan(v))) {
    return(FALSE)
  }
  if (!isTRUE(allow_na) && anyNA(v)) {
    return(FALSE)
  }
  non_na <- v[!is.na(v)]
  if (length(non_na) == 0L) {
    return(TRUE)
  }
  all(is.finite(non_na)) && all(non_na == floor(non_na)) &&
    all(non_na >= min_val) && all(non_na <= max_val)
}

.cp_is_valid_num_scalar <- function(v, allow_na = FALSE) {
  if (!is.numeric(v) || is.logical(v) || length(v) != 1L) {
    return(FALSE)
  }
  if (is.nan(v)) {
    return(FALSE)
  }
  if (is.na(v)) {
    return(isTRUE(allow_na))
  }
  is.finite(v)
}

.cp_is_valid_num_vec <- function(v, allow_na = FALSE) {
  if (!is.numeric(v) || is.logical(v)) {
    return(FALSE)
  }
  if (any(is.nan(v))) {
    return(FALSE)
  }
  if (!isTRUE(allow_na) && anyNA(v)) {
    return(FALSE)
  }
  non_na <- v[!is.na(v)]
  all(is.finite(non_na))
}

# ---- Canonical conversion adapters ----

.cp_validate_source_predictions <- function(preds, fold_spec, y_vec = NULL) {
  req_cols <- c("row_index", "true_outcome", "predicted_score", "predicted_class")
  if (!is.data.frame(preds) || !all(req_cols %in% colnames(preds))) {
    stop("Fold predictions must be a data.frame with required columns.", call. = FALSE)
  }
  expected_rows <- as.integer(fold_spec$test_indices + 1L)
  if (!.cp_is_valid_int_vec(preds$row_index, min_val = 1, allow_na = FALSE) ||
      !identical(as.integer(preds$row_index), expected_rows)) {
    stop("Fold predictions row_index is invalid or does not match outer test split.", call. = FALSE)
  }
  if (!.cp_is_valid_int_vec(preds$true_outcome, min_val = 0, max_val = 1, allow_na = FALSE)) {
    stop("Fold predictions true_outcome must be integral 0/1 values without NA/NaN.", call. = FALSE)
  }
  if (!is.null(y_vec) && !identical(as.integer(preds$true_outcome), as.integer(y_vec[expected_rows]))) {
    stop("Fold predictions true_outcome does not match analysis outcome labels.", call. = FALSE)
  }
  if (!.cp_is_valid_num_vec(preds$predicted_score, allow_na = FALSE)) {
    stop("Fold predictions predicted_score must be finite numeric values.", call. = FALSE)
  }
  if (!.cp_is_valid_int_vec(preds$predicted_class, min_val = 0, max_val = 1, allow_na = FALSE)) {
    stop("Fold predictions predicted_class must be integral 0/1 values without NA/NaN.", call. = FALSE)
  }
  data.frame(
    row_index       = as.integer(preds$row_index),
    true_outcome    = as.integer(preds$true_outcome),
    predicted_score = as.numeric(preds$predicted_score),
    predicted_class = as.integer(preds$predicted_class),
    stringsAsFactors = FALSE
  )
}

.cp_validate_source_metrics <- function(src, context_label) {
  req_finite <- c("inner_mean_auc", "cutoff", "accuracy")
  for (nm in req_finite) {
    if (!.cp_is_valid_num_scalar(src[[nm]], allow_na = FALSE)) {
      stop(sprintf("%s scalar '%s' must be a finite numeric scalar.", context_label, nm), call. = FALSE)
    }
  }
  opt_na <- c("inner_mean_youden", "auc", "sensitivity", "specificity", "youden", "ppv", "npv")
  for (nm in opt_na) {
    if (!.cp_is_valid_num_scalar(src[[nm]], allow_na = TRUE)) {
      stop(sprintf("%s scalar '%s' must be a numeric scalar without NaN/Inf.", context_label, nm), call. = FALSE)
    }
  }
}

.r_fold_to_canonical <- function(r_fold, fold_spec, items, y_vec = NULL) {
  if (!is.list(r_fold)) {
    stop("R fold result must be a list.", call. = FALSE)
  }
  if (!.cp_is_valid_int_scalar(fold_spec$repeat_id, min_val = 1) ||
      !.cp_is_valid_int_scalar(fold_spec$outer_fold_id, min_val = 1) ||
      !is.character(fold_spec$fold_name) || length(fold_spec$fold_name) != 1L || is.na(fold_spec$fold_name)) {
    stop("Invalid fold_spec identity for R fold canonicalization.", call. = FALSE)
  }
  diag <- attr(r_fold, "ncvroc_inner_diag", exact = TRUE)
  sel_items_vec <- .parse_itemset(r_fold$selected_items)
  sel_idx <- match(sel_items_vec, items)
  if (length(sel_idx) < 1L || anyNA(sel_idx) || anyDuplicated(sel_idx)) {
    stop("Cannot map R selected_items to canonical item indices.", call. = FALSE)
  }
  if (!is.null(r_fold$n_items) &&
      (!.cp_is_valid_int_scalar(r_fold$n_items, min_val = 1, max_val = length(items)) ||
       !identical(as.integer(r_fold$n_items), length(sel_idx)))) {
    stop("R fold n_items is invalid or does not match selected_items.", call. = FALSE)
  }
  sel_idx <- as.integer(sel_idx)
  sel_items_canon <- as.character(items[sel_idx])
  sel_label <- format_items(sel_items_canon)

  preds_canon <- .cp_validate_source_predictions(r_fold$predictions, fold_spec, y_vec = y_vec)
  .cp_validate_source_metrics(r_fold, "R fold")

  n_cand <- 1L
  if (is.list(diag) && !is.null(diag$n_candidates_evaluated)) {
    if (!.cp_is_valid_int_scalar(diag$n_candidates_evaluated, min_val = 1)) {
      stop("R fold n_candidates_evaluated must be a positive integer scalar.", call. = FALSE)
    }
    n_cand <- as.integer(diag$n_candidates_evaluated)
  }
  glob_rank <- NA_integer_
  if (is.list(diag) && !is.null(diag$selected_global_rank)) {
    if (!.cp_is_valid_int_scalar(diag$selected_global_rank, min_val = 0, allow_na = TRUE)) {
      stop("R fold selected_global_rank must be a non-negative integer scalar or NA.", call. = FALSE)
    }
    glob_rank <- as.integer(diag$selected_global_rank)
  }
  inner_sens <- NA_real_
  if (is.list(diag) && !is.null(diag$inner_mean_sensitivity)) {
    if (!.cp_is_valid_num_scalar(diag$inner_mean_sensitivity, allow_na = TRUE)) {
      stop("R fold inner_mean_sensitivity must be a numeric scalar without NaN/Inf.", call. = FALSE)
    }
    inner_sens <- as.numeric(diag$inner_mean_sensitivity)
  }
  inner_spec <- NA_real_
  if (is.list(diag) && !is.null(diag$inner_mean_specificity)) {
    if (!.cp_is_valid_num_scalar(diag$inner_mean_specificity, allow_na = TRUE)) {
      stop("R fold inner_mean_specificity must be a numeric scalar without NaN/Inf.", call. = FALSE)
    }
    inner_spec <- as.numeric(diag$inner_mean_specificity)
  }

  list(
    schema_version          = .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION,
    statistical_contract_id = .CHECKPOINT_STATISTICAL_CONTRACT_ID,
    repeat_id               = as.integer(fold_spec$repeat_id),
    outer_fold_id           = as.integer(fold_spec$outer_fold_id),
    outer_fold              = as.character(fold_spec$fold_name),
    completion_status       = "complete",
    selected_col_indices    = sel_idx,
    selected_items          = sel_items_canon,
    selected_items_label    = sel_label,
    n_items                 = as.integer(length(sel_idx)),
    n_candidates_evaluated  = n_cand,
    selected_global_rank    = glob_rank,
    inner_mean_auc          = as.numeric(r_fold$inner_mean_auc),
    inner_mean_sensitivity  = inner_sens,
    inner_mean_specificity  = inner_spec,
    inner_mean_youden       = as.numeric(r_fold$inner_mean_youden),
    auc                     = as.numeric(r_fold$auc),
    cutoff                  = as.numeric(r_fold$cutoff),
    sensitivity             = as.numeric(r_fold$sensitivity),
    specificity             = as.numeric(r_fold$specificity),
    youden                  = as.numeric(r_fold$youden),
    accuracy                = as.numeric(r_fold$accuracy),
    ppv                     = as.numeric(r_fold$ppv),
    npv                     = as.numeric(r_fold$npv),
    predictions             = preds_canon
  )
}

.native_fold_to_canonical <- function(nat_res, fold_spec, items, y_vec = NULL) {
  if (!is.list(nat_res) || !isTRUE(nat_res$completed) ||
      !.cp_is_valid_int_scalar(nat_res$status_code, min_val = 0, max_val = 0)) {
    stop("Cannot normalize incomplete NativeOuterFoldResult.", call. = FALSE)
  }
  if (!.cp_is_valid_int_scalar(nat_res$repeat_id, min_val = 1) ||
      !.cp_is_valid_int_scalar(nat_res$outer_fold_id, min_val = 1) ||
      !identical(as.integer(nat_res$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(nat_res$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(nat_res$outer_fold, as.character(fold_spec$fold_name))) {
    stop("Native fold identity is invalid or does not match expected fold.", call. = FALSE)
  }
  if (!.cp_is_valid_int_vec(nat_res$selected_col_indices, min_val = 1, max_val = length(items), allow_na = FALSE) ||
      length(nat_res$selected_col_indices) < 1L || anyDuplicated(as.integer(nat_res$selected_col_indices))) {
    stop("Native selected_col_indices invalid, out of bounds, or duplicated.", call. = FALSE)
  }
  sel_idx <- as.integer(nat_res$selected_col_indices)
  if (!.cp_is_valid_int_scalar(nat_res$n_items, min_val = 1, max_val = length(items)) ||
      !identical(as.integer(nat_res$n_items), length(sel_idx))) {
    stop("Native n_items invalid or does not match selected_col_indices.", call. = FALSE)
  }
  if (!.cp_is_valid_int_scalar(nat_res$n_candidates_evaluated, min_val = 1)) {
    stop("Native n_candidates_evaluated must be a positive integer scalar.", call. = FALSE)
  }
  if (!.cp_is_valid_int_scalar(nat_res$selected_global_rank, min_val = 0, allow_na = TRUE)) {
    stop("Native selected_global_rank must be a non-negative integer scalar or NA.", call. = FALSE)
  }
  sel_items_canon <- as.character(items[sel_idx])
  sel_label <- format_items(sel_items_canon)

  preds_canon <- .cp_validate_source_predictions(nat_res$predictions, fold_spec, y_vec = y_vec)
  .cp_validate_source_metrics(nat_res, "Native fold")
  if (!.cp_is_valid_num_scalar(nat_res$inner_mean_sensitivity, allow_na = TRUE)) {
    stop("Native fold scalar 'inner_mean_sensitivity' must be a numeric scalar without NaN/Inf.", call. = FALSE)
  }
  if (!.cp_is_valid_num_scalar(nat_res$inner_mean_specificity, allow_na = TRUE)) {
    stop("Native fold scalar 'inner_mean_specificity' must be a numeric scalar without NaN/Inf.", call. = FALSE)
  }

  list(
    schema_version          = .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION,
    statistical_contract_id = .CHECKPOINT_STATISTICAL_CONTRACT_ID,
    repeat_id               = as.integer(nat_res$repeat_id),
    outer_fold_id           = as.integer(nat_res$outer_fold_id),
    outer_fold              = as.character(nat_res$outer_fold),
    completion_status       = "complete",
    selected_col_indices    = sel_idx,
    selected_items          = sel_items_canon,
    selected_items_label    = sel_label,
    n_items                 = as.integer(length(sel_idx)),
    n_candidates_evaluated  = as.integer(nat_res$n_candidates_evaluated),
    selected_global_rank    = as.integer(nat_res$selected_global_rank),
    inner_mean_auc          = as.numeric(nat_res$inner_mean_auc),
    inner_mean_sensitivity  = as.numeric(nat_res$inner_mean_sensitivity),
    inner_mean_specificity  = as.numeric(nat_res$inner_mean_specificity),
    inner_mean_youden       = as.numeric(nat_res$inner_mean_youden),
    auc                     = as.numeric(nat_res$auc),
    cutoff                  = as.numeric(nat_res$cutoff),
    sensitivity             = as.numeric(nat_res$sensitivity),
    specificity             = as.numeric(nat_res$specificity),
    youden                  = as.numeric(nat_res$youden),
    accuracy                = as.numeric(nat_res$accuracy),
    ppv                     = as.numeric(nat_res$ppv),
    npv                     = as.numeric(nat_res$npv),
    predictions             = preds_canon
  )
}

.canonical_to_r_aggregation_record <- function(canon) {
  preds <- data.frame(
    row_index       = as.integer(canon$predictions$row_index),
    true_outcome    = as.integer(canon$predictions$true_outcome),
    predicted_score = as.numeric(canon$predictions$predicted_score),
    predicted_class = as.integer(canon$predictions$predicted_class),
    stringsAsFactors = FALSE
  )
  list(
    outer_fold        = as.character(canon$outer_fold),
    selected_items    = as.character(canon$selected_items_label),
    n_items           = as.integer(canon$n_items),
    inner_mean_auc    = as.numeric(canon$inner_mean_auc),
    inner_mean_youden = as.numeric(canon$inner_mean_youden),
    auc               = as.numeric(canon$auc),
    sensitivity       = as.numeric(canon$sensitivity),
    specificity       = as.numeric(canon$specificity),
    youden            = as.numeric(canon$youden),
    accuracy          = as.numeric(canon$accuracy),
    ppv               = as.numeric(canon$ppv),
    npv               = as.numeric(canon$npv),
    cutoff            = as.numeric(canon$cutoff),
    predictions       = preds
  )
}

# ---- Envelope & canonical payload validation ----

.cp_validate_canonical_payload <- function(payload, fold_spec, items, y_vec = NULL) {
  if (!is.list(payload)) {
    return("payload is not a list")
  }
  if (!.cp_is_valid_int_scalar(payload$schema_version, min_val = 1, max_val = 1000) ||
      !identical(as.integer(payload$schema_version), .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION)) {
    return("payload schema_version mismatch")
  }
  if (!identical(payload$statistical_contract_id, .CHECKPOINT_STATISTICAL_CONTRACT_ID)) {
    return("payload statistical_contract_id mismatch")
  }
  if (!identical(payload$completion_status, "complete")) {
    return("payload completion_status is not 'complete'")
  }
  if (!.cp_is_valid_int_scalar(payload$repeat_id, min_val = 1) ||
      !.cp_is_valid_int_scalar(payload$outer_fold_id, min_val = 1) ||
      !identical(as.integer(payload$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(payload$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(payload$outer_fold, as.character(fold_spec$fold_name))) {
    return("payload fold identity does not match expected fold")
  }

  n_items_total <- length(items)
  sel_idx <- payload$selected_col_indices
  if (!.cp_is_valid_int_vec(sel_idx, min_val = 1, max_val = n_items_total, allow_na = FALSE) ||
      length(sel_idx) < 1L || anyDuplicated(as.integer(sel_idx))) {
    return("payload selected_col_indices invalid")
  }
  sel_idx_int <- as.integer(sel_idx)
  if (!.cp_is_valid_int_scalar(payload$n_items, min_val = 1, max_val = n_items_total) ||
      !identical(as.integer(payload$n_items), length(sel_idx_int))) {
    return("payload n_items mismatch")
  }
  if (!is.character(payload$selected_items) || anyNA(payload$selected_items) ||
      !identical(payload$selected_items, as.character(items[sel_idx_int]))) {
    return("payload selected_items mismatch")
  }
  if (!is.character(payload$selected_items_label) || length(payload$selected_items_label) != 1L ||
      is.na(payload$selected_items_label) ||
      !identical(payload$selected_items_label, format_items(items[sel_idx_int]))) {
    return("payload selected_items_label mismatch")
  }
  if (!.cp_is_valid_int_scalar(payload$n_candidates_evaluated, min_val = 1)) {
    return("payload n_candidates_evaluated invalid")
  }
  if (!.cp_is_valid_int_scalar(payload$selected_global_rank, min_val = 0, allow_na = TRUE)) {
    return("payload selected_global_rank invalid")
  }

  req_finite <- c("inner_mean_auc", "cutoff", "accuracy")
  for (nm in req_finite) {
    if (!.cp_is_valid_num_scalar(payload[[nm]], allow_na = FALSE)) {
      return(sprintf("payload scalar '%s' missing, non-finite, or invalid", nm))
    }
  }
  opt_na <- c("inner_mean_sensitivity", "inner_mean_specificity", "inner_mean_youden",
              "auc", "sensitivity", "specificity", "youden", "ppv", "npv")
  for (nm in opt_na) {
    if (!.cp_is_valid_num_scalar(payload[[nm]], allow_na = TRUE)) {
      return(sprintf("payload scalar '%s' missing, NaN/Inf, or invalid", nm))
    }
  }

  preds <- payload$predictions
  req_cols <- c("row_index", "true_outcome", "predicted_score", "predicted_class")
  if (!is.data.frame(preds) || !identical(colnames(preds), req_cols)) {
    return("payload predictions data.frame missing or misordered required columns")
  }
  expected_rows <- as.integer(fold_spec$test_indices + 1L)
  if (!.cp_is_valid_int_vec(preds$row_index, min_val = 1, allow_na = FALSE) ||
      !identical(as.integer(preds$row_index), expected_rows)) {
    return("payload predictions row_index does not match outer test split")
  }
  if (!.cp_is_valid_int_vec(preds$true_outcome, min_val = 0, max_val = 1, allow_na = FALSE)) {
    return("payload predictions true_outcome invalid")
  }
  if (!is.null(y_vec) && !identical(as.integer(preds$true_outcome), as.integer(y_vec[expected_rows]))) {
    return("payload predictions true_outcome does not match analysis outcome labels")
  }
  if (!.cp_is_valid_num_vec(preds$predicted_score, allow_na = FALSE)) {
    return("payload predictions predicted_score invalid")
  }
  if (!.cp_is_valid_int_vec(preds$predicted_class, min_val = 0, max_val = 1, allow_na = FALSE)) {
    return("payload predictions predicted_class invalid")
  }

  NULL
}

.cp_validate_envelope <- function(env, identity_record, fold_spec, split_hash, items, y_vec = NULL) {
  if (!is.list(env)) {
    return("envelope is not a list")
  }
  if (identical(env$format_id, .CHECKPOINT_LEGACY_FORMAT_ID) ||
      identical(suppressWarnings(as.integer(env$schema_version)), .CHECKPOINT_LEGACY_SCHEMA_VERSION)) {
    return("unsupported legacy v0.27 checkpoint schema (schema_version = 1); use a new checkpoint_dir")
  }
  if (!identical(env$format_id, .CHECKPOINT_CANONICAL_FORMAT_ID)) {
    return("envelope format_id mismatch")
  }
  if (!.cp_is_valid_int_scalar(env$schema_version, min_val = 1) ||
      !identical(as.integer(env$schema_version), .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION)) {
    return("envelope schema_version mismatch")
  }
  if (!identical(env$statistical_contract_id, .CHECKPOINT_STATISTICAL_CONTRACT_ID)) {
    return("envelope statistical_contract_id mismatch")
  }
  if (!identical(env$hash_algorithm, .CHECKPOINT_HASH_ALGORITHM)) {
    return("envelope hash_algorithm mismatch")
  }
  if (!identical(env$analysis_identity_hash, identity_record$analysis_identity_hash)) {
    return("envelope analysis_identity_hash mismatch")
  }
  if (!.cp_is_valid_int_scalar(env$repeat_id, min_val = 1) ||
      !.cp_is_valid_int_scalar(env$outer_fold_id, min_val = 1) ||
      !identical(as.integer(env$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(env$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(env$outer_fold, as.character(fold_spec$fold_name))) {
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
      !is.character(prod$backend) || length(prod$backend) != 1L || is.na(prod$backend) ||
      !(prod$backend %in% .CHECKPOINT_SUPPORTED_PRODUCERS) ||
      !is.character(prod$backend_provenance) || length(prod$backend_provenance) != 1L ||
      is.na(prod$backend_provenance) || !nzchar(prod$backend_provenance) ||
      !is.character(prod$package_version) || length(prod$package_version) != 1L ||
      is.na(prod$package_version) || !nzchar(prod$package_version) ||
      !identical(suppressWarnings(as.integer(prod$payload_schema_version)), .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION) ||
      !identical(suppressWarnings(as.integer(prod$envelope_schema_version)), .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION) ||
      !identical(prod$statistical_contract_id, .CHECKPOINT_STATISTICAL_CONTRACT_ID)) {
    return("envelope producer metadata invalid or unsupported")
  }
  payload_err <- .cp_validate_canonical_payload(env$payload, fold_spec, items, y_vec = y_vec)
  if (!is.null(payload_err)) {
    return(payload_err)
  }
  computed_hash <- .cp_hash_object(env$payload)
  if (!identical(env$payload_hash, computed_hash)) {
    return("envelope payload_hash mismatch")
  }
  NULL
}

.cp_build_fold_envelope <- function(identity_record,
                                    fold_spec,
                                    split_hash,
                                    canon_payload,
                                    producer_backend,
                                    backend_provenance = producer_backend) {
  list(
    format_id               = .CHECKPOINT_CANONICAL_FORMAT_ID,
    schema_version          = .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION,
    statistical_contract_id = .CHECKPOINT_STATISTICAL_CONTRACT_ID,
    hash_algorithm          = .CHECKPOINT_HASH_ALGORITHM,
    analysis_identity_hash  = identity_record$analysis_identity_hash,
    repeat_id               = as.integer(fold_spec$repeat_id),
    outer_fold_id           = as.integer(fold_spec$outer_fold_id),
    outer_fold              = as.character(fold_spec$fold_name),
    split_hash              = split_hash,
    completion_status       = "complete",
    payload                 = canon_payload,
    payload_hash            = .cp_hash_object(canon_payload),
    producer                = list(
      backend                 = as.character(producer_backend),
      backend_provenance      = as.character(backend_provenance),
      package_version         = as.character(utils::packageVersion("NCVROC")),
      payload_schema_version  = .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION,
      envelope_schema_version = .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION,
      statistical_contract_id = .CHECKPOINT_STATISTICAL_CONTRACT_ID
    ),
    provenance              = list(
      saved_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      pid       = Sys.getpid(),
      hostname  = unname(Sys.info()[["nodename"]])
    ),
    validation              = list(
      validated_on_write = TRUE,
      read_back_verified = TRUE
    )
  )
}

# ---- Inspection & classification ----

.cp_inspect_directory <- function(paths, identity_record, fold_specs_ordered, items, y_vec = NULL) {
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
        status           = "ABSENT",
        fold_name        = spec$fold_name,
        file_path        = fpath,
        split_hash       = split_hashes[[i]],
        producer_backend = NA_character_,
        payload          = NULL
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
    err <- .cp_validate_envelope(env, identity_record, spec, split_hashes[[i]], items, y_vec = y_vec)
    if (!is.null(err)) {
      stop(
        sprintf("Invalid checkpoint envelope for %s (%s): %s",
                spec$fold_name, fpath, err),
        call. = FALSE
      )
    }
    states[[i]] <- list(
      status           = "COMPLETE_VALID",
      fold_name        = spec$fold_name,
      file_path        = fpath,
      split_hash       = split_hashes[[i]],
      producer_backend = as.character(env$producer$backend),
      payload          = env$payload
    )
  }

  states
}

# ---- Safe staging, read-back validation, publication & replacement ----

.cp_publish_fold <- function(paths,
                             identity_record,
                             fold_spec,
                             split_hash,
                             canon_payload,
                             items,
                             producer_backend,
                             backend_provenance = producer_backend,
                             allow_replace = FALSE,
                             y_vec = NULL) {
  fold_name <- fold_spec$fold_name
  if (identical(.ncvroc_checkpoint_test_seams$fail_between_complete_and_write, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_between_complete_and_write)) {
    stop("Simulated failure between fold completion and checkpoint write for ", fold_name, call. = FALSE)
  }

  payload_err <- .cp_validate_canonical_payload(canon_payload, fold_spec, items, y_vec = y_vec)
  if (!is.null(payload_err)) {
    stop("Cannot persist invalid fold result for ", fold_name, ": ", payload_err, call. = FALSE)
  }

  env <- .cp_build_fold_envelope(
    identity_record    = identity_record,
    fold_spec          = fold_spec,
    split_hash         = split_hash,
    canon_payload      = canon_payload,
    producer_backend   = producer_backend,
    backend_provenance = backend_provenance
  )
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
  rb_err <- .cp_validate_envelope(rb_env, identity_record, fold_spec, split_hash, items, y_vec = y_vec)
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
    pub_err <- if (is.null(pub_rb)) "readRDS failed" else .cp_validate_envelope(pub_rb, identity_record, fold_spec, split_hash, items, y_vec = y_vec)
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

# ---- Shared checkpoint preparation & provenance assembly ----

.cp_prepare_run <- function(x_mat,
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
                            checkpoint_dir,
                            resume,
                            force_recompute,
                            consumer_backend,
                            progress,
                            verbose) {
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

  paths <- .cp_resolve_root(checkpoint_dir)
  lock_token <- .cp_acquire_lock(paths)

  identity_record <- tryCatch({
    id_rec <- .cp_build_analysis_identity(
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
      seed                = seed,
      producer_backend    = consumer_backend
    )
    .cp_validate_or_create_identity(paths, id_rec)
    id_rec
  }, error = function(e) {
    .cp_release_lock(lock_token)
    stop(e)
  })

  states <- tryCatch(
    .cp_inspect_directory(paths, identity_record, fold_specs, items, y_vec = y_vec),
    error = function(e) {
      .cp_release_lock(lock_token)
      stop(e)
    }
  )

  # Enforce resume = FALSE non-overwrite rule
  if (!isTRUE(resume) && !isTRUE(force_recompute)) {
    existing_valid <- vapply(states, function(st) identical(st$status, "COMPLETE_VALID"), logical(1L))
    if (any(existing_valid)) {
      .cp_release_lock(lock_token)
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

  reused_idx       <- integer(0L)
  compute_idx      <- integer(0L)
  final_canon      <- vector("list", n_folds)
  reused_producers <- character(0L)
  fold_producers   <- stats::setNames(rep(NA_character_, n_folds), vapply(fold_specs, `[[`, character(1L), "fold_name"))

  for (i in seq_len(n_folds)) {
    if (identical(states[[i]]$status, "COMPLETE_VALID") && isTRUE(resume) && !isTRUE(force_recompute)) {
      reused_idx <- c(reused_idx, i)
      final_canon[[i]] <- states[[i]]$payload
      prod_b <- as.character(states[[i]]$producer_backend)
      reused_producers <- c(reused_producers, prod_b)
      fold_producers[[i]] <- prod_b
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

  list(
    paths            = paths,
    lock_token       = lock_token,
    identity_record  = identity_record,
    fold_specs       = fold_specs,
    states           = states,
    n_folds          = n_folds,
    reused_idx       = reused_idx,
    compute_idx      = compute_idx,
    reused_names     = reused_names,
    compute_names    = compute_names,
    reused_producers = reused_producers,
    fold_producers   = fold_producers,
    final_canon      = final_canon
  )
}

.cp_build_provenance <- function(prep, consumer_backend, resume, force_recompute) {
  list(
    checkpoint_enabled       = TRUE,
    consumer_backend         = as.character(consumer_backend),
    canonical_checkpoint_dir = prep$paths$root_canonical,
    analysis_identity_hash   = prep$identity_record$analysis_identity_hash,
    reused_folds             = prep$reused_names,
    computed_folds           = prep$compute_names,
    n_reused                 = length(prep$reused_idx),
    n_computed               = length(prep$compute_idx),
    reused_from_r            = as.integer(sum(prep$reused_producers == "r_serial")),
    reused_from_native       = as.integer(sum(prep$reused_producers == "scope_b_native_outer_fold")),
    fold_producers           = prep$fold_producers,
    resume                   = isTRUE(resume),
    force_recompute          = isTRUE(force_recompute)
  )
}

# ---- User cancellation and cooperative interrupt helpers ----

.cp_resolve_cancel_dir <- function(checkpoint_dir, create_if_missing = FALSE) {
  if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L ||
      is.na(checkpoint_dir) || !nzchar(trimws(checkpoint_dir))) {
    stop("`checkpoint_dir` must be a single non-empty character path.", call. = FALSE)
  }
  if (!dir.exists(checkpoint_dir)) {
    if (file.exists(checkpoint_dir)) {
      stop("`checkpoint_dir` exists as a non-directory file: ", checkpoint_dir, call. = FALSE)
    }
    if (!isTRUE(create_if_missing)) {
      return(NULL)
    }
    ok <- dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(checkpoint_dir)) {
      stop("Failed to create `checkpoint_dir`: ", checkpoint_dir, call. = FALSE)
    }
  }
  root_canonical <- normalizePath(checkpoint_dir, winslash = "/", mustWork = TRUE)
  if (.cp_is_symlink_or_reparse(root_canonical)) {
    stop("Canonical `checkpoint_dir` must not be a symbolic link or reparse point: ", root_canonical, call. = FALSE)
  }
  root_canonical
}

#' Request Safe Cancellation of a Checkpointed NCVROC Run
#'
#' Creates a `.cancel` marker file inside `checkpoint_dir`. A running
#' checkpointed `ncvroc()` or `nested_sum_roc()` execution polls this marker at
#' safe unit/wave boundaries, persists all already-completed units, discards
#' any in-flight work, releases the checkpoint lock, and exits cleanly with an
#' `ncvroc_interrupted` condition so the run can be resumed later with
#' `resume = TRUE`.
#'
#' @param checkpoint_dir Character(1). Path to the checkpoint directory.
#'
#' @return Invisibly returns the normalized path to the `.cancel` marker file.
#' @export
ncvroc_request_cancel <- function(checkpoint_dir) {
  root_canonical <- .cp_resolve_cancel_dir(checkpoint_dir, create_if_missing = TRUE)
  cancel_path <- file.path(root_canonical, ".cancel")
  .cp_assert_contained(root_canonical, cancel_path, must_exist = FALSE)
  if (.cp_is_symlink_or_reparse(cancel_path)) {
    stop("Cancel marker path must not be a symbolic link or reparse point: ", cancel_path, call. = FALSE)
  }
  writeLines(
    c(
      sprintf("requested_at=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
      sprintf("requested_by_pid=%d", Sys.getpid())
    ),
    con = cancel_path
  )
  invisible(cancel_path)
}

#' Clear a Pending Cancellation Marker in a Checkpoint Directory
#'
#' Removes any `.cancel` marker file inside `checkpoint_dir` if present.
#'
#' @param checkpoint_dir Character(1). Path to the checkpoint directory.
#'
#' @return Invisibly returns `TRUE` if a `.cancel` marker existed and was
#'   removed, or `FALSE` if none was present.
#' @export
ncvroc_clear_cancel <- function(checkpoint_dir) {
  root_canonical <- .cp_resolve_cancel_dir(checkpoint_dir, create_if_missing = FALSE)
  if (is.null(root_canonical)) {
    return(invisible(FALSE))
  }
  cancel_path <- file.path(root_canonical, ".cancel")
  .cp_assert_contained(root_canonical, cancel_path, must_exist = FALSE)
  if (file.exists(cancel_path)) {
    unlink(cancel_path, force = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

.cp_cancel_file_path <- function(paths) {
  file.path(paths$root_canonical, ".cancel")
}

.cp_clear_cancel_file <- function(paths) {
  cancel_path <- .cp_cancel_file_path(paths)
  if (file.exists(cancel_path)) {
    unlink(cancel_path, force = TRUE)
  }
  invisible(NULL)
}

.cp_poll_interrupt <- function(paths, stage_label = NULL, unit_index = NULL) {
  sim_at <- .ncvroc_checkpoint_test_seams$simulate_interrupt_at
  if (!is.null(sim_at)) {
    matched <- FALSE
    if (is.numeric(sim_at) && !is.null(unit_index) &&
        identical(as.integer(sim_at), as.integer(unit_index))) {
      matched <- TRUE
    } else if (is.character(sim_at) && !is.null(stage_label) &&
               identical(as.character(sim_at), as.character(stage_label))) {
      matched <- TRUE
    }
    if (matched) {
      return("simulated_interrupt")
    }
  }

  cancel_path <- .cp_cancel_file_path(paths)
  if (file.exists(cancel_path)) {
    return("cancel_file")
  }

  hook <- .ncvroc_checkpoint_test_seams$r_interrupt_hook
  if (is.function(hook)) {
    res <- tryCatch(
      {
        hook(stage_label = stage_label, unit_index = unit_index)
        NULL
      },
      interrupt = function(e) "r_user_interrupt"
    )
    if (!is.null(res)) {
      return(res)
    }
  }

  tryCatch(
    {
      check_user_interrupt_cpp()
      NULL
    },
    interrupt = function(e) "r_user_interrupt"
  )
}

.cp_signal_interrupted <- function(paths,
                                  reason,
                                  completed_units = 0L,
                                  total_units = 0L,
                                  unit_label = "outer fold(s)",
                                  granularity = "outer_fold") {
  .cp_clear_cancel_file(paths)
  msg <- sprintf(
    paste0(
      "NCVROC checkpointed execution INTERRUPTED (%s). ",
      "Saved %d/%d COMPLETE %s under checkpoint_dir = \"%s\" ",
      "(granularity = \"%s\"); any in-flight work was discarded without partial persistence. ",
      "Re-run the same call with `resume = TRUE` and `checkpoint_dir = \"%s\"` to resume."
    ),
    as.character(reason),
    as.integer(completed_units),
    as.integer(total_units),
    as.character(unit_label),
    paths$root_canonical,
    as.character(granularity),
    paths$root_canonical
  )
  cond <- structure(
    list(
      message         = msg,
      status          = "INTERRUPTED",
      reason          = as.character(reason),
      checkpoint_dir  = paths$root_canonical,
      granularity     = as.character(granularity),
      completed_units = as.integer(completed_units),
      total_units     = as.integer(total_units),
      unit_label      = as.character(unit_label)
    ),
    class = c("ncvroc_interrupted", "error", "condition")
  )
  stop(cond)
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
  prep <- .cp_prepare_run(
    x_mat               = x_mat,
    y_vec               = y_vec,
    outer_folds         = outer_folds,
    items               = items,
    outcome_col         = outcome_col,
    positive_label      = positive_label,
    negative_label      = negative_label,
    min_items           = min_items,
    max_items           = max_items,
    cutoff_method       = cutoff_method,
    preselect_by        = preselect_by,
    selection_criterion = selection_criterion,
    preselect_top_n     = preselect_top_n,
    outer_k             = outer_k,
    inner_k             = inner_k,
    outer_repeats       = outer_repeats,
    inner_repeats       = inner_repeats,
    stratified          = stratified,
    seed                = seed,
    checkpoint_dir      = checkpoint_dir,
    resume              = resume,
    force_recompute     = force_recompute,
    consumer_backend    = "scope_b_native_outer_fold",
    progress            = progress,
    verbose             = verbose
  )
  on.exit(.cp_release_lock(prep$lock_token), add = TRUE)

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_compute)) {
    stop("Simulated failure before computing remaining folds.", call. = FALSE)
  }

  if (length(prep$compute_idx) > 0L) {
    split_hashes <- vapply(prep$states, `[[`, character(1L), "split_hash")
    allow_rep <- isTRUE(force_recompute)

    if (as.integer(num_threads) == 1L) {
      # Serial execution: run and persist one fold at a time so each completed fold
      # is durably saved before the next fold begins.
      for (wave_i in seq_along(prep$compute_idx)) {
        idx <- prep$compute_idx[[wave_i]]
        spec <- prep$fold_specs[[idx]]
        intr_reason <- .cp_poll_interrupt(
          prep$paths,
          stage_label = spec$fold_name,
          unit_index = wave_i
        )
        if (!is.null(intr_reason)) {
          n_saved <- sum(!vapply(prep$final_canon, is.null, logical(1L)))
          .cp_signal_interrupted(
            paths           = prep$paths,
            reason          = intr_reason,
            completed_units = n_saved,
            total_units     = prep$n_folds,
            unit_label      = "outer fold(s)",
            granularity     = "outer_fold"
          )
        }
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
        canon_res <- .native_fold_to_canonical(nat_res, spec, items, y_vec = y_vec)
        .cp_publish_fold(
          paths              = prep$paths,
          identity_record    = prep$identity_record,
          fold_spec          = spec,
          split_hash         = split_hashes[[idx]],
          canon_payload      = canon_res,
          items              = items,
          producer_backend   = "scope_b_native_outer_fold",
          backend_provenance = as.character(nat_res$backend_provenance %||% "scope_b_native_serial"),
          allow_replace      = allow_rep,
          y_vec              = y_vec
        )
        prep$final_canon[[idx]] <- canon_res
        prep$fold_producers[[idx]] <- "scope_b_native_outer_fold"
        .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
          as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L
      }
    } else {
      # Bounded-wave parallel TBB execution: dispatch at most `num_threads` folds per
      # .Call(), persist every completed fold on the R main control thread, and poll
      # for user interrupt / cancel marker between waves.
      wave_size <- max(1L, as.integer(num_threads))
      n_compute <- length(prep$compute_idx)
      wave_starts <- seq.int(1L, n_compute, by = wave_size)

      for (w_idx in seq_along(wave_starts)) {
        w_start <- wave_starts[[w_idx]]
        w_end <- min(n_compute, w_start + wave_size - 1L)
        wave_compute_idx <- prep$compute_idx[w_start:w_end]

        intr_reason <- .cp_poll_interrupt(
          prep$paths,
          stage_label = sprintf("wave_%d", w_idx),
          unit_index = w_idx
        )
        if (!is.null(intr_reason)) {
          n_saved <- sum(!vapply(prep$final_canon, is.null, logical(1L)))
          .cp_signal_interrupted(
            paths           = prep$paths,
            reason          = intr_reason,
            completed_units = n_saved,
            total_units     = prep$n_folds,
            unit_label      = "outer fold(s)",
            granularity     = "outer_fold"
          )
        }

        specs_to_run <- prep$fold_specs[wave_compute_idx]
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
        for (k in seq_along(wave_compute_idx)) {
          idx <- wave_compute_idx[[k]]
          spec <- prep$fold_specs[[idx]]
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
            canon_res <- .native_fold_to_canonical(nat_res, spec, items, y_vec = y_vec)
            .cp_publish_fold(
              paths              = prep$paths,
              identity_record    = prep$identity_record,
              fold_spec          = spec,
              split_hash         = split_hashes[[idx]],
              canon_payload      = canon_res,
              items              = items,
              producer_backend   = "scope_b_native_outer_fold",
              backend_provenance = as.character(nat_res$backend_provenance %||% "scope_b_native_tbb"),
              allow_replace      = allow_rep,
              y_vec              = y_vec
            )
            prep$final_canon[[idx]] <- canon_res
            prep$fold_producers[[idx]] <- "scope_b_native_outer_fold"
          } else if (is.null(first_failure)) {
            first_failure <- nat_res
          }
        }

        .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
          as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L

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
    }

    if (isTRUE(progress) || isTRUE(verbose)) {
      message(sprintf(
        "Checkpoint: %d newly computed outer fold(s) persisted (%d/%d complete).",
        length(prep$compute_idx), prep$n_folds, prep$n_folds
      ))
    }
  }

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_aggregate)) {
    stop("Simulated failure before final aggregation.", call. = FALSE)
  }

  list(
    canonical_results = prep$final_canon,
    per_fold          = lapply(prep$final_canon, .canonical_to_r_aggregation_record),
    provenance        = .cp_build_provenance(prep, "scope_b_native_outer_fold", resume, force_recompute)
  )
}

# ---- R serial checkpointed execution driver ----

.run_r_serial_outer_folds_checkpointed <- function(full_data,
                                                   x_mat,
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
                                                   use_streaming_ncv,
                                                   engine,
                                                   seed,
                                                   checkpoint_dir,
                                                   resume,
                                                   force_recompute,
                                                   progress,
                                                   verbose) {
  prep <- .cp_prepare_run(
    x_mat               = x_mat,
    y_vec               = y_vec,
    outer_folds         = outer_folds,
    items               = items,
    outcome_col         = outcome_col,
    positive_label      = positive_label,
    negative_label      = negative_label,
    min_items           = min_items,
    max_items           = max_items,
    cutoff_method       = cutoff_method,
    preselect_by        = preselect_by,
    selection_criterion = selection_criterion,
    preselect_top_n     = preselect_top_n,
    outer_k             = outer_k,
    inner_k             = inner_k,
    outer_repeats       = outer_repeats,
    inner_repeats       = inner_repeats,
    stratified          = stratified,
    seed                = seed,
    checkpoint_dir      = checkpoint_dir,
    resume              = resume,
    force_recompute     = force_recompute,
    consumer_backend    = "r_serial",
    progress            = progress,
    verbose             = verbose
  )
  on.exit(.cp_release_lock(prep$lock_token), add = TRUE)

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_compute)) {
    stop("Simulated failure before computing remaining folds.", call. = FALSE)
  }

  if (length(prep$compute_idx) > 0L) {
    split_hashes <- vapply(prep$states, `[[`, character(1L), "split_hash")
    allow_rep <- isTRUE(force_recompute)
    total_combos <- .count_total_combos(length(items), min_items, max_items)
    n_total <- length(y_vec)

    for (wave_i in seq_along(prep$compute_idx)) {
      idx <- prep$compute_idx[[wave_i]]
      spec <- prep$fold_specs[[idx]]
      intr_reason <- .cp_poll_interrupt(
        prep$paths,
        stage_label = spec$fold_name,
        unit_index = wave_i
      )
      if (!is.null(intr_reason)) {
        n_saved <- sum(!vapply(prep$final_canon, is.null, logical(1L)))
        .cp_signal_interrupted(
          paths           = prep$paths,
          reason          = intr_reason,
          completed_units = n_saved,
          total_units     = prep$n_folds,
          unit_label      = "outer fold(s)",
          granularity     = "outer_fold"
        )
      }
      if (!is.null(.ncvroc_checkpoint_test_seams$fail_during_compute_fold) &&
          identical(.ncvroc_checkpoint_test_seams$fail_during_compute_fold, spec$fold_name)) {
        stop("Simulated failure during R serial computation of ", spec$fold_name, call. = FALSE)
      }

      .ncvroc_checkpoint_test_seams$r_folds_executed <- as.integer(.ncvroc_checkpoint_test_seams$r_folds_executed) + 1L

      orig_i <- match(spec$fold_name, names(outer_folds))
      if (is.na(orig_i)) {
        stop("Fold name mismatch during R serial checkpointed execution: ", spec$fold_name, call. = FALSE)
      }

      precomp_inner <- lapply(spec$inner_folds, function(cand_folds) {
        lapply(cand_folds, function(f_idx) as.integer(f_idx) + 1L)
      })

      prg_i <- .progress_make(
        total_combos,
        label = sprintf("NCVROC [Fold %d/%d]", idx, prep$n_folds),
        enabled = progress,
        progress_mode = "exact",
        unit = "candidates"
      )
      cb_i <- if (progress) function(n_done) prg_i$tick(n_done) else NULL

      interrupted_mid_fold <- FALSE
      r_fold <- tryCatch(
        .evaluate_single_outer_fold(
          i                       = orig_i,
          outer_folds             = outer_folds,
          full_data               = full_data,
          y                       = y_vec,
          n_total                 = n_total,
          items                   = items,
          outcome_col             = outcome_col,
          min_items               = min_items,
          max_items               = max_items,
          positive_label          = positive_label,
          negative_label          = negative_label,
          cutoff_method           = cutoff_method,
          preselect_top_n         = preselect_top_n,
          preselect_by            = preselect_by,
          selection_criterion     = selection_criterion,
          inner_k                 = inner_k,
          inner_repeats           = inner_repeats,
          use_streaming_ncv       = use_streaming_ncv,
          engine                  = engine,
          seed                    = seed,
          progress                = FALSE,
          verbose                 = verbose,
          cl_chunk                = NULL,
          parallel_inner          = "none",
          n_workers_inner         = 1L,
          progress_callback       = cb_i,
          precomputed_inner_folds = precomp_inner,
          return_canonical_diag   = TRUE
        ),
        interrupt = function(e) {
          interrupted_mid_fold <<- TRUE
          NULL
        }
      )
      if (isTRUE(interrupted_mid_fold)) {
        n_saved <- sum(!vapply(prep$final_canon, is.null, logical(1L)))
        .cp_signal_interrupted(
          paths           = prep$paths,
          reason          = "r_user_interrupt",
          completed_units = n_saved,
          total_units     = prep$n_folds,
          unit_label      = "outer fold(s)",
          granularity     = "outer_fold"
        )
      }
      prg_i$finish()

      canon_res <- .r_fold_to_canonical(r_fold, spec, items, y_vec = y_vec)
      .cp_publish_fold(
        paths              = prep$paths,
        identity_record    = prep$identity_record,
        fold_spec          = spec,
        split_hash         = split_hashes[[idx]],
        canon_payload      = canon_res,
        items              = items,
        producer_backend   = "r_serial",
        backend_provenance = paste0("r_serial_", as.character(engine)),
        allow_replace      = allow_rep,
        y_vec              = y_vec
      )
      prep$final_canon[[idx]] <- canon_res
      prep$fold_producers[[idx]] <- "r_serial"
      .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
        as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L
    }

    if (isTRUE(progress) || isTRUE(verbose)) {
      message(sprintf(
        "Checkpoint: %d newly computed outer fold(s) persisted (%d/%d complete).",
        length(prep$compute_idx), prep$n_folds, prep$n_folds
      ))
    }
  }

  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_aggregate)) {
    stop("Simulated failure before final aggregation.", call. = FALSE)
  }

  list(
    canonical_results = prep$final_canon,
    per_fold          = lapply(prep$final_canon, .canonical_to_r_aggregation_record),
    provenance        = .cp_build_provenance(prep, "r_serial", resume, force_recompute)
  )
}
