# cache.R — Result caching with atomic writes
#
# Internal functions (all @keywords internal):
#   .compute_cache_key()
#   .load_cache()
#   .save_cache()
#
# Cache key is computed from normalized internal data + all analysis parameters
# using serialize() + writeBin() + tools::md5sum() (base R only, no dependencies).
#
# Atomic writes: build in <key>.building-<pid>/, write complete=TRUE metadata,
# then rename to <key>/. On collision, old <key>/ → <key>.old-<pid>/ first.

# ---- Cache key computation ----

#' Compute a deterministic cache key from analysis inputs
#'
#' Hashes the normalized analysis data (after .prepare_ncvroc_data()) plus all
#' parameters that affect results. Uses serialize(version=3) + writeBin +
#' tools::md5sum() so the cache depends only on data values, not on RDS header
#' or compression artifacts.
#'
#' @param cache_data data.frame, the normalized analysis data.
#' @param cache_outcome Character, outcome column name.
#' @param cache_items Character vector, item column names.
#' @param min_items Integer.
#' @param max_items Integer.
#' @param mode Character or NULL (NULL for roc_bruteforce).
#' @param outer_k Integer or NULL.
#' @param inner_k Integer or NULL.
#' @param outer_repeats Integer or NULL.
#' @param inner_repeats Integer or NULL.
#' @param cutoff_method Character.
#' @param selection_criterion Character or NULL.
#' @param preselect_top_n Integer or NULL.
#' @param preselect_by Character or NULL.
#' @param final_search Logical or NULL.
#' @param final_rank_by Character or NULL.
#' @param engine Character.
#' @param seed Integer or NULL.
#' @param positive_label Scalar.
#' @param negative_label Scalar.
#' @param stratified Logical or NULL.
#' @param chunk_size Integer.
#'
#' @return A 32-character hex string (MD5 hash), or NULL if cache_data is NULL.
#' @keywords internal
.compute_cache_key <- function(cache_data,
                               cache_outcome,
                               cache_items,
                               min_items,
                               max_items,
                               mode              = NULL,
                               outer_k           = NULL,
                               inner_k           = NULL,
                               outer_repeats     = NULL,
                               inner_repeats     = NULL,
                               cutoff_method,
                               selection_criterion = NULL,
                               preselect_top_n   = NULL,
                               preselect_by      = NULL,
                               final_search      = NULL,
                               final_rank_by     = NULL,
                               engine,
                               seed              = NULL,
                               positive_label,
                               negative_label,
                               stratified        = NULL,
                               chunk_size) {
  if (is.null(cache_data)) return(NULL)

  cache_input <- list(
    data               = cache_data,
    outcome            = cache_outcome,
    items              = cache_items,
    min_items          = min_items,
    max_items          = max_items,
    mode               = mode,
    outer_k            = outer_k,
    inner_k            = inner_k,
    outer_repeats      = outer_repeats,
    inner_repeats      = inner_repeats,
    cutoff_method      = cutoff_method,
    selection_criterion = selection_criterion,
    preselect_top_n    = preselect_top_n,
    preselect_by       = preselect_by,
    final_search       = final_search,
    final_rank_by      = final_rank_by,
    engine             = engine,
    seed               = seed,
    positive_label     = positive_label,
    negative_label     = negative_label,
    stratified         = stratified,
    chunk_size         = chunk_size,
    pkg_version        = as.character(utils::packageVersion("NCVROC")),
    r_version          = paste(R.version$major, R.version$minor, sep = "."),
    cache_fmt          = CACHE_FORMAT_VERSION
  )

  raw <- serialize(cache_input, NULL, version = 3)
  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeBin(raw, tmp)
  unname(tools::md5sum(tmp))
}

# ---- Cache load ----

#' Load a cached analysis result
#'
#' Checks for a completed cache entry directory at `cache_dir/<key>/`. Reads
#' metadata.rds to verify `complete == TRUE`, then loads result.rds and resolves
#' relative paths against the cache entry directory.
#'
#' @param cache_dir Character, root cache directory.
#' @param cache_key Character, 32-char hex key.
#'
#' @return The cached result object (with resolved paths), or NULL if no
#'   valid cache entry exists.
#' @keywords internal
.load_cache <- function(cache_dir, cache_key) {
  if (is.null(cache_dir) || is.null(cache_key)) return(NULL)

  entry_dir <- file.path(cache_dir, cache_key)

  if (!dir.exists(entry_dir)) return(NULL)

  meta_file <- file.path(entry_dir, "metadata.rds")
  if (!file.exists(meta_file)) return(NULL)

  meta <- tryCatch(readRDS(meta_file), error = function(e) NULL)
  if (is.null(meta)) return(NULL)

  if (!isTRUE(meta$complete)) return(NULL)

  result_file <- file.path(entry_dir, "result.rds")
  if (!file.exists(result_file)) return(NULL)

  result <- tryCatch(readRDS(result_file), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  # Resolve relative paths against cache entry directory
  if (!is.null(result$chunk_dir) && !is.null(result$chunk_prefix)) {
    result$chunk_dir <- normalizePath(
      file.path(entry_dir, result$chunk_dir),
      winslash = "/", mustWork = FALSE
    )
  }

  if (!is.null(result$results_file)) {
    result$results_file <- normalizePath(
      file.path(entry_dir, result$results_file),
      winslash = "/", mustWork = FALSE
    )
  }

  # If storage_backend is memory, full table is already embedded
  # If storage_backend is single_rds, results_file is now resolved
  # If storage_backend is chunked_rds, chunk_dir is now resolved
  # If storage_backend is none, no full table to load

  result$cache_entry_dir <- normalizePath(entry_dir, winslash = "/", mustWork = FALSE)
  result$loaded_from_cache <- TRUE

  result
}

# ---- Cache save ----

#' Save an analysis result to cache (atomic write)
#'
#' Writes the result to `<cache_dir>/<key>.building-<pid>/`, marks
#' `complete = TRUE` in metadata.rds, then atomically renames to `<key>/`.
#' If an existing complete `<key>/` already exists, it is first renamed to
#' `<key>.old-<pid>/` then removed (best-effort).
#'
#' @param result The analysis result object.
#' @param full_results data.frame or NULL — the full candidate table
#'   (for single_rds backend).
#' @param cache_dir Character, root cache directory.
#' @param cache_key Character, 32-char hex key.
#' @param metadata_list Named list of metadata (from the calling function).
#' @param storage_backend Character: "memory", "single_rds", "chunked_rds", or "none".
#'
#' @return The result object (possibly modified with relative paths).
#' @keywords internal
.save_cache <- function(result, full_results, cache_dir, cache_key,
                         metadata_list, storage_backend) {
  if (is.null(cache_dir) || is.null(cache_key)) return(result)

  pid <- Sys.getpid()
  building_dir <- file.path(cache_dir, paste0(cache_key, ".building-", pid))

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(building_dir)) {
    unlink(building_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(building_dir, recursive = TRUE, showWarnings = FALSE)

  entry_dir <- file.path(cache_dir, cache_key)

  # Store cached paths relative to the cache entry directory, so the cache
  # stays self-contained and relocatable.
  if (storage_backend == "single_rds" && !is.null(full_results)) {
    rds_file <- file.path(building_dir, "full_results.rds")
    saveRDS(full_results, rds_file)
    result$results_file <- "full_results.rds"
    result$storage_backend <- "single_rds"
  } else if (storage_backend == "chunked_rds") {
    # Chunks are already written directly into building_dir/chunks/
    # by the chunked evaluation path. Just store the relative path.
    result$chunk_dir <- "chunks"
    result$storage_backend <- "chunked_rds"
  } else if (storage_backend == "memory") {
    result$storage_backend <- "memory"
  } else {
    result$storage_backend <- "none"
  }

  # Write atomic metadata LAST (marks build as complete)
  meta <- c(metadata_list, list(complete = TRUE))
  saveRDS(meta, file.path(building_dir, "metadata.rds"))

  # Write the (lightweight) result object
  saveRDS(result, file.path(building_dir, "result.rds"))

  result$cache_entry_dir <- normalizePath(
    building_dir, winslash = "/", mustWork = FALSE
  )
  result$loaded_from_cache <- FALSE

  # Atomically commit: rename old → .old-<pid>, building → <key>, remove .old-<pid>
  if (dir.exists(entry_dir)) {
    old_dir <- file.path(cache_dir, paste0(cache_key, ".old-", pid))
    if (dir.exists(old_dir)) {
      unlink(old_dir, recursive = TRUE, force = TRUE)
    }
    file.rename(entry_dir, old_dir)
    file.rename(building_dir, entry_dir)
    unlink(old_dir, recursive = TRUE, force = TRUE)
  } else {
    file.rename(building_dir, entry_dir)
  }

  # Resolve paths for the current session
  result$chunk_dir <- normalizePath(
    file.path(entry_dir, "chunks"),
    winslash = "/", mustWork = FALSE
  )
  if (!is.null(result$results_file)) {
    result$results_file <- normalizePath(
      file.path(entry_dir, result$results_file),
      winslash = "/", mustWork = FALSE
    )
  }
  result$cache_entry_dir <- normalizePath(
    entry_dir, winslash = "/", mustWork = FALSE
  )

  result
}

#' Clean up a building directory left by an interrupted cache write
#'
#' Called when cache save fails. Removes the .building-<pid>/ directory if
#' it exists and has no `complete = TRUE` metadata.
#'
#' @param building_dir Character, path to the building directory.
#' @return Invisible NULL.
#' @keywords internal
.cleanup_building_cache <- function(building_dir) {
  if (dir.exists(building_dir)) {
    meta_file <- file.path(building_dir, "metadata.rds")
    if (!file.exists(meta_file)) {
      unlink(building_dir, recursive = TRUE, force = TRUE)
      return(invisible(NULL))
    }
    meta <- tryCatch(readRDS(meta_file), error = function(e) NULL)
    if (!isTRUE(meta$complete)) {
      unlink(building_dir, recursive = TRUE, force = TRUE)
    }
  }
  invisible(NULL)
}

# ---- Completed-result RDS caching (v0.23.2-R1) ----

.RESULT_CACHE_FORMAT <- "NCVROC_cross_size_nested_cv_result"
.RESULT_CACHE_SCHEMA_VERSION <- 1L
.RESULT_CACHE_LOCK_SCHEMA_VERSION <- 1L
.RESULT_CACHE_TX_SCHEMA_VERSION <- 1L

# Test seams for deterministic error injection and tracking during testthat runs
.ncvroc_test_seams <- new.env(parent = emptyenv())
.ncvroc_test_seams$write_probe_fail <- FALSE
.ncvroc_test_seams$owner_write_fail <- FALSE
.ncvroc_test_seams$fail_state_write_pending <- FALSE
.ncvroc_test_seams$fail_state_after_write_pending <- FALSE
.ncvroc_test_seams$fail_state_validate <- FALSE
.ncvroc_test_seams$fail_state_publish <- FALSE
.ncvroc_test_seams$fail_before_publish <- FALSE
.ncvroc_test_seams$fail_during_replace <- FALSE
.ncvroc_test_seams$fail_during_verify <- FALSE
.ncvroc_test_seams$fail_during_restore <- FALSE
.ncvroc_test_seams$fail_during_cleanup <- FALSE
.ncvroc_test_seams$capture_artifact_hashes <- FALSE
.ncvroc_test_seams$last_pending_file <- NULL
.ncvroc_test_seams$last_pending_hash <- NULL
.ncvroc_test_seams$last_state_1_file <- NULL
.ncvroc_test_seams$last_state_1_hash <- NULL
.ncvroc_test_seams$last_state_2_file <- NULL
.ncvroc_test_seams$last_state_2_hash <- NULL
.ncvroc_test_seams$last_target_published_hash <- NULL
.ncvroc_test_seams$computation_count <- 0L
.ncvroc_test_seams$nonce_counter <- 0L

.reset_ncvroc_test_seams <- function() {
  .ncvroc_test_seams$write_probe_fail <- FALSE
  .ncvroc_test_seams$owner_write_fail <- FALSE
  .ncvroc_test_seams$fail_state_write_pending <- FALSE
  .ncvroc_test_seams$fail_state_after_write_pending <- FALSE
  .ncvroc_test_seams$fail_state_validate <- FALSE
  .ncvroc_test_seams$fail_state_publish <- FALSE
  .ncvroc_test_seams$fail_before_publish <- FALSE
  .ncvroc_test_seams$fail_during_replace <- FALSE
  .ncvroc_test_seams$fail_during_verify <- FALSE
  .ncvroc_test_seams$fail_during_restore <- FALSE
  .ncvroc_test_seams$fail_during_cleanup <- FALSE
  .ncvroc_test_seams$capture_artifact_hashes <- FALSE
  .ncvroc_test_seams$last_pending_file <- NULL
  .ncvroc_test_seams$last_pending_hash <- NULL
  .ncvroc_test_seams$last_state_1_file <- NULL
  .ncvroc_test_seams$last_state_1_hash <- NULL
  .ncvroc_test_seams$last_state_2_file <- NULL
  .ncvroc_test_seams$last_state_2_hash <- NULL
  .ncvroc_test_seams$last_target_published_hash <- NULL
  .ncvroc_test_seams$computation_count <- 0L
  .ncvroc_test_seams$nonce_counter <- 0L
}

#' Validate strictly that a metadata value is a finite scalar integer-valued number without truncation
#' @noRd
.is_strict_scalar_integer <- function(x, expected_val = NULL) {
  if (is.null(x) || !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    return(FALSE)
  }
  if (x != floor(x)) {
    return(FALSE)
  }
  if (x < -.Machine$integer.max || x > .Machine$integer.max) {
    return(FALSE)
  }
  if (!is.null(expected_val)) {
    if (as.integer(x) != as.integer(expected_val)) {
      return(FALSE)
    }
  }
  TRUE
}

#' Generate unique ownership or transaction nonce without altering .Random.seed
#' @noRd
.generate_nonce <- function(prefix = "nonce") {
  .ncvroc_test_seams$nonce_counter <- .ncvroc_test_seams$nonce_counter + 1L
  payload <- list(
    prefix    = prefix,
    time      = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
    pid       = Sys.getpid(),
    counter   = .ncvroc_test_seams$nonce_counter,
    node      = Sys.info()[["nodename"]]
  )
  raw_bytes <- serialize(payload, NULL, version = 3)
  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeBin(raw_bytes, tmp)
  paste0(prefix, "_", substr(unname(tools::md5sum(tmp)), 1, 16))
}

#' Canonicalize result file path for consistent locking, cache checks, and transactions
#' @noRd
.canonicalize_result_path <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !is.character(path)) {
    return(NULL)
  }
  p <- gsub("\\\\", "/", path)
  if (file.exists(p)) {
    norm <- normalizePath(p, winslash = "/", mustWork = TRUE)
  } else {
    parent <- dirname(p)
    base <- basename(p)
    if (dir.exists(parent)) {
      norm_parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
      norm <- file.path(norm_parent, base)
    } else {
      norm <- normalizePath(p, winslash = "/", mustWork = FALSE)
    }
  }
  norm
}

#' Sibling lock directory path derived from canonical path (case-insensitive on Windows)
#' @noRd
.nested_cv_lock_dir <- function(canonical_path) {
  lock_path_key <- if (.Platform$OS.type == "windows") {
    tolower(canonical_path)
  } else {
    canonical_path
  }
  paste0(lock_path_key, ".lock")
}

#' Probe whether parent directory is writable
#' @noRd
.probe_parent_writable <- function(parent_dir) {
  if (isTRUE(.ncvroc_test_seams$write_probe_fail)) {
    return(FALSE)
  }
  if (parent_dir == "") parent_dir <- "."
  probe_tmp <- tempfile(pattern = ".write_probe_", tmpdir = parent_dir)
  tryCatch({
    writeLines("probe", probe_tmp)
    unlink(probe_tmp, force = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

#' Acquire exclusive per-path lock with verified owner record
#' @noRd
.acquire_nested_cv_lock <- function(canonical_target) {
  lock_dir <- .nested_cv_lock_dir(canonical_target)

  # Atomic lock directory creation (fails if already exists)
  acquired <- suppressWarnings(dir.create(lock_dir, recursive = FALSE))
  if (!acquired) {
    owner_file <- file.path(lock_dir, "owner.rds")
    owner_diag <- "Lock owner diagnostics unavailable."
    if (file.exists(owner_file)) {
      owner_info <- tryCatch(readRDS(owner_file), error = function(e) NULL)
      if (is.list(owner_info)) {
        owner_diag <- sprintf(
          "Held by PID %s on host '%s' since %s (nonce: %s).",
          as.character(owner_info$pid),
          as.character(owner_info$hostname),
          as.character(owner_info$acquired_at),
          as.character(owner_info$ownership_nonce)
        )
      }
    }
    stop(sprintf(
      "Cannot acquire exclusive lock for result file '%s'. Lock directory exists at '%s' (%s). Code never automatically deletes locks to prevent data corruption. If no process is running, verify that no writer is active before manually removing the lock directory.",
      canonical_target, lock_dir, owner_diag
    ), call. = FALSE)
  }

  ownership_nonce <- .generate_nonce("owner")
  owner_record <- list(
    lock_schema      = .RESULT_CACHE_LOCK_SCHEMA_VERSION,
    canonical_target = canonical_target,
    ownership_nonce  = ownership_nonce,
    pid              = Sys.getpid(),
    hostname         = Sys.info()[["nodename"]],
    acquired_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  # Write owner record with read-back verification and test seam
  owner_file <- file.path(lock_dir, "owner.rds")
  write_ok <- FALSE
  if (!isTRUE(.ncvroc_test_seams$owner_write_fail)) {
    saveRDS(owner_record, owner_file)
    read_back <- tryCatch(readRDS(owner_file), error = function(e) NULL)
    if (is.list(read_back) &&
        identical(read_back$ownership_nonce, ownership_nonce) &&
        identical(read_back$canonical_target, canonical_target)) {
      write_ok <- TRUE
    }
  }

  if (!write_ok) {
    # Clean up only the lock directory we just created
    unlink(lock_dir, recursive = TRUE, force = TRUE)
    stop(sprintf(
      "Failed to write and verify lock owner metadata for '%s'. Cleaned reservation; no analysis will run unlocked.",
      canonical_target
    ), call. = FALSE)
  }

  token <- new.env(parent = emptyenv())
  token$acquired <- TRUE
  token$lock_dir <- lock_dir
  token$canonical_target <- canonical_target
  token$ownership_nonce <- ownership_nonce
  token$preserve <- FALSE
  token
}

#' Release exclusive per-path lock only if matching caller's token
#' @noRd
.release_nested_cv_lock <- function(lock_token) {
  if (is.environment(lock_token) && isTRUE(lock_token$acquired) && !isTRUE(lock_token$preserve)) {
    lock_dir <- lock_token$lock_dir
    if (!is.null(lock_dir) && dir.exists(lock_dir)) {
      owner_file <- file.path(lock_dir, "owner.rds")
      if (file.exists(owner_file)) {
        owner_info <- tryCatch(readRDS(owner_file), error = function(e) NULL)
        if (is.list(owner_info) &&
            identical(owner_info$ownership_nonce, lock_token$ownership_nonce) &&
            identical(owner_info$canonical_target, lock_token$canonical_target)) {
          unlink(lock_dir, recursive = TRUE, force = TRUE)
        }
      }
    }
  }
}

#' Compute normalized analysis identity and deterministic key for cross_size_nested_cv
#'
#' @noRd
.compute_nested_cv_result_identity <- function(data,
                                               outcome_name,
                                               item_names,
                                               sizes,
                                               outer_folds,
                                               inner_folds,
                                               outer_repeats,
                                               inner_repeats,
                                               selection_metric,
                                               cutoff_method,
                                               sensitivity_min,
                                               specificity_min,
                                               prefer_fewer_items,
                                               stratified,
                                               positive_label,
                                               negative_label,
                                               engine,
                                               parallel_mode,
                                               n_workers,
                                               threads_per_worker,
                                               tuning,
                                               seed) {
  norm_data <- data[, c(outcome_name, item_names), drop = FALSE]
  rownames(norm_data) <- NULL

  identity_list <- list(
    data               = norm_data,
    outcome_name       = as.character(outcome_name),
    item_names         = as.character(item_names),
    model_sizes        = as.integer(sizes),
    outer_folds        = as.integer(outer_folds),
    inner_folds        = as.integer(inner_folds),
    outer_repeats      = as.integer(outer_repeats),
    inner_repeats      = as.integer(inner_repeats),
    selection_metric   = as.character(selection_metric),
    cutoff_method      = as.character(cutoff_method),
    sensitivity_min    = if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else NULL,
    specificity_min    = if (!is.null(specificity_min)) as.numeric(specificity_min) else NULL,
    prefer_fewer_items = isTRUE(prefer_fewer_items),
    stratified         = isTRUE(stratified),
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = as.character(engine),
    parallel           = as.character(parallel_mode),
    n_workers          = if (!is.null(n_workers)) as.integer(n_workers) else NULL,
    threads_per_worker = as.integer(threads_per_worker),
    tuning             = as.character(tuning),
    seed               = if (!is.null(seed)) as.integer(seed) else NULL,
    pkg_version        = as.character(utils::packageVersion("NCVROC")),
    cache_schema       = .RESULT_CACHE_SCHEMA_VERSION,
    r_version          = paste(R.version$major, R.version$minor, sep = ".")
  )

  raw_bytes <- serialize(identity_list, NULL, version = 3)
  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeBin(raw_bytes, tmp)
  key <- unname(tools::md5sum(tmp))

  list(
    identity = identity_list,
    key      = key
  )
}

#' Validate a loaded cache envelope object
#' @noRd
.validate_nested_cv_envelope <- function(cached_env, identity_obj, result_file) {
  if (inherits(cached_env, "error")) {
    stop(sprintf(
      "Result file '%s' exists but could not be read (file may be corrupted). Set `force_recompute = TRUE` to recompute and overwrite it. Error: %s",
      result_file, conditionMessage(cached_env)
    ), call. = FALSE)
  }

  if (!is.list(cached_env) ||
      !identical(cached_env$format, .RESULT_CACHE_FORMAT)) {
    stop(sprintf(
      "Result file '%s' is not a valid NCVROC result envelope (unrecognized format or legacy RDS). Set `force_recompute = TRUE` to recompute and overwrite it.",
      result_file
    ), call. = FALSE)
  }

  if (!.is_strict_scalar_integer(cached_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION)) {
    stop(sprintf(
      "Result file '%s' has unsupported cache schema version (%s). Expected version %d. Set `force_recompute = TRUE` to recompute and overwrite it.",
      result_file, as.character(cached_env$cache_schema), .RESULT_CACHE_SCHEMA_VERSION
    ), call. = FALSE)
  }

  if (!isTRUE(cached_env$complete)) {
    stop(sprintf(
      "Result file '%s' is marked incomplete. Set `force_recompute = TRUE` to recompute and overwrite it.",
      result_file
    ), call. = FALSE)
  }

  if (!identical(cached_env$identity_key, identity_obj$key)) {
    stop(sprintf(
      "Result file '%s' contains a completed result, but its analysis identity does not match the current call parameters. Set `force_recompute = TRUE` to recompute and overwrite it, or specify a different `result_file` path.",
      result_file
    ), call. = FALSE)
  }

  if (!inherits(cached_env$result, "cross_size_nested_cv_result")) {
    stop(sprintf(
      "Result file '%s' contains an invalid result payload (not of class 'cross_size_nested_cv_result'). Set `force_recompute = TRUE` to recompute and overwrite it.",
      result_file
    ), call. = FALSE)
  }

  TRUE
}

#' Escape regex special characters
#' @noRd
.escape_regex <- function(string) {
  gsub("([.\\^$*+?()[{\\\\|])", "\\\\\\1", string)
}

#' Find transaction directories associated with canonical target
#' @noRd
.find_nested_cv_tx_dirs <- function(canonical_target) {
  parent_dir <- dirname(canonical_target)
  base_name <- basename(canonical_target)
  if (!dir.exists(parent_dir)) return(character(0))

  pattern <- paste0("^\\.", .escape_regex(base_name), "\\.tx-")
  all_entries <- list.files(parent_dir, pattern = pattern, full.names = TRUE, all.files = TRUE)
  dirs <- all_entries[dir.exists(all_entries)]
  dirs[file.exists(file.path(dirs, "identity.rds")) | file.exists(file.path(dirs, "state_1_prepared.rds"))]
}

#' Check for a read-only cache hit before acquiring locks or creating directories
#' @noRd
.check_readonly_nested_cv_hit <- function(canonical_target, identity_obj, verbose) {
  if (!file.exists(canonical_target)) {
    return(NULL)
  }

  lock_dir <- .nested_cv_lock_dir(canonical_target)

  # Check 1: lock present? If lock is held, fail busy-path promptly.
  if (dir.exists(lock_dir)) {
    owner_file <- file.path(lock_dir, "owner.rds")
    owner_diag <- "Lock owner diagnostics unavailable."
    if (file.exists(owner_file)) {
      owner_info <- tryCatch(readRDS(owner_file), error = function(e) NULL)
      if (is.list(owner_info)) {
        owner_diag <- sprintf(
          "Held by PID %s on host '%s' since %s (nonce: %s).",
          as.character(owner_info$pid),
          as.character(owner_info$hostname),
          as.character(owner_info$acquired_at),
          as.character(owner_info$ownership_nonce)
        )
      }
    }
    stop(sprintf(
      "Cannot read result file '%s' because an active write lock is present at '%s' (%s). Another process may be writing. Wait for completion or manually resolve if stale.",
      canonical_target, lock_dir, owner_diag
    ), call. = FALSE)
  }

  # If a transaction directory exists without an active lock, a previous crash occurred.
  # Return NULL so caller can acquire lock and execute recovery.
  tx_dirs <- .find_nested_cv_tx_dirs(canonical_target)
  if (length(tx_dirs) > 0L) {
    return(NULL)
  }

  # Read and validate target file
  cached_env <- tryCatch(readRDS(canonical_target), error = function(e) e)
  .validate_nested_cv_envelope(cached_env, identity_obj, canonical_target)

  # Check 2: lock or transaction directory appeared during read?
  if (dir.exists(lock_dir) || length(.find_nested_cv_tx_dirs(canonical_target)) > 0L) {
    stop(sprintf(
      "Detected concurrent write lock or transaction during read of result file '%s'. Failing read to prevent returning partially modified data.",
      canonical_target
    ), call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf("Loaded cached cross_size_nested_cv result from: %s", canonical_target))
  }

  cached_env$result
}

#' Write and validate durable sequenced state record with unique pending name
#' @noRd
.write_durable_tx_state <- function(tx_dir, state_record, stage_name, seq_num) {
  if (identical(.ncvroc_test_seams$fail_state_write_pending, stage_name) ||
      isTRUE(.ncvroc_test_seams$fail_state_write_pending)) {
    stop(sprintf("Injected failure before writing pending state '%s'.", stage_name), call. = FALSE)
  }

  pending_nonce <- .generate_nonce("pend")
  pending_file <- file.path(tx_dir, sprintf("pending_state_%d_%s_%s.rds", as.integer(seq_num), stage_name, pending_nonce))
  saveRDS(state_record, pending_file)
  if (isTRUE(.ncvroc_test_seams$capture_artifact_hashes)) {
    .ncvroc_test_seams$last_pending_file <- pending_file
    .ncvroc_test_seams$last_pending_hash <- unname(tools::md5sum(pending_file))
  }

  # Test seam hook: fail immediately after writing pending file
  if (identical(.ncvroc_test_seams$fail_state_after_write_pending, stage_name) ||
      isTRUE(.ncvroc_test_seams$fail_state_after_write_pending)) {
    stop(sprintf("Injected failure immediately after writing pending state '%s'.", stage_name), call. = FALSE)
  }

  if (identical(.ncvroc_test_seams$fail_state_validate, stage_name) ||
      isTRUE(.ncvroc_test_seams$fail_state_validate)) {
    writeLines("corrupted", pending_file)
    if (isTRUE(.ncvroc_test_seams$capture_artifact_hashes)) {
      .ncvroc_test_seams$last_pending_hash <- unname(tools::md5sum(pending_file))
    }
  }

  read_back <- tryCatch(readRDS(pending_file), error = function(e) NULL)
  if (!is.list(read_back) ||
      !identical(read_back$stage, stage_name) ||
      !.is_strict_scalar_integer(read_back$stage_seq, seq_num) ||
      !identical(read_back$transaction_nonce, state_record$transaction_nonce) ||
      !identical(read_back$original_owner_nonce, state_record$original_owner_nonce)) {
    stop(sprintf("Failed to read-back validate pending state record '%s'.", stage_name), call. = FALSE)
  }

  if (identical(.ncvroc_test_seams$fail_state_publish, stage_name) ||
      isTRUE(.ncvroc_test_seams$fail_state_publish)) {
    stop(sprintf("Injected failure at publishing final state '%s'.", stage_name), call. = FALSE)
  }

  final_file <- file.path(tx_dir, sprintf("state_%d_%s.rds", as.integer(seq_num), stage_name))
  if (file.exists(final_file)) {
    stop(sprintf("State record '%s' already exists; monotonically sequenced states cannot be overwritten.", final_file), call. = FALSE)
  }

  ren_ok <- file.rename(pending_file, final_file)
  if (!ren_ok) {
    ren_ok <- file.copy(pending_file, final_file, overwrite = FALSE) && unlink(pending_file, force = TRUE)
  }
  if (!ren_ok || !file.exists(final_file)) {
    stop(sprintf("Failed to publish final state record '%s'.", final_file), call. = FALSE)
  }

  list(
    file = final_file,
    hash = unname(tools::md5sum(final_file))
  )
}

#' Recover transaction state under exclusive lock before computation
#' @noRd
.recover_nested_cv_transactions <- function(canonical_target, lock_token) {
  # 1. Validate recovery lock
  if (!is.environment(lock_token) || !isTRUE(lock_token$acquired)) {
    stop("Cannot recover transactions without a valid acquired lock token.", call. = FALSE)
  }
  lock_dir <- lock_token$lock_dir
  if (!dir.exists(lock_dir)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Recovery lock directory '%s' does not exist.", lock_dir), call. = FALSE)
  }
  owner_file <- file.path(lock_dir, "owner.rds")
  if (!file.exists(owner_file)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Recovery lock owner record '%s' missing.", owner_file), call. = FALSE)
  }
  owner_rec <- tryCatch(readRDS(owner_file), error = function(e) NULL)
  if (!is.list(owner_rec) ||
      !identical(owner_rec$canonical_target, canonical_target) ||
      !identical(owner_rec$ownership_nonce, lock_token$ownership_nonce)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Recovery lock owner record in '%s' is invalid or does not match current lock token.", lock_dir), call. = FALSE)
  }

  # 2. Find transaction directories
  tx_dirs <- .find_nested_cv_tx_dirs(canonical_target)
  if (length(tx_dirs) == 0L) {
    return(invisible(NULL))
  }

  if (length(tx_dirs) > 1L) {
    lock_token$preserve <- TRUE
    stop(sprintf(
      "Multiple unresolved transaction directories found for '%s': %s. Preserving all evidence and lock for manual inspection.",
      canonical_target, paste(tx_dirs, collapse = ", ")
    ), call. = FALSE)
  }

  tx_dir <- tx_dirs[1]

  # 3. Transaction directory name validation and nonce extraction
  base_name <- basename(canonical_target)
  expected_prefix <- paste0(".", base_name, ".tx-")
  tx_dir_base <- basename(tx_dir)
  if (!startsWith(tx_dir_base, expected_prefix)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Transaction directory '%s' does not match expected prefix '%s'.", tx_dir, expected_prefix), call. = FALSE)
  }
  tx_nonce_from_dir <- substring(tx_dir_base, nchar(expected_prefix) + 1L)
  if (nchar(tx_nonce_from_dir) == 0L) {
    lock_token$preserve <- TRUE
    stop(sprintf("Transaction directory '%s' has empty transaction nonce.", tx_dir), call. = FALSE)
  }

  # 4. Read and validate immutable identity record with strict pre-coercion checks
  identity_file <- file.path(tx_dir, "identity.rds")
  if (!file.exists(identity_file)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Transaction directory at '%s' is missing immutable identity.rds. Preserving all evidence.", tx_dir), call. = FALSE)
  }
  tx_identity <- tryCatch(readRDS(identity_file), error = function(e) NULL)
  if (!is.list(tx_identity) ||
      !.is_strict_scalar_integer(tx_identity$transaction_schema, .RESULT_CACHE_TX_SCHEMA_VERSION) ||
      !identical(tx_identity$canonical_target, canonical_target) ||
      !identical(tx_identity$transaction_nonce, tx_nonce_from_dir) ||
      is.null(tx_identity$original_owner_nonce) ||
      !is.character(tx_identity$original_owner_nonce) ||
      length(tx_identity$original_owner_nonce) != 1L ||
      nchar(tx_identity$original_owner_nonce) == 0L) {
    lock_token$preserve <- TRUE
    stop(sprintf("Transaction identity at '%s' is corrupt, invalid, or mismatched with directory name. Preserving all evidence.", identity_file), call. = FALSE)
  }
  orig_owner_nonce <- tx_identity$original_owner_nonce

  # 5. Check for unexpected or torn artifacts in tx_dir
  all_files <- list.files(tx_dir, all.files = TRUE, no.. = TRUE)
  allowed_exact_names <- c("identity.rds", "state_1_prepared.rds", "state_2_publishing.rds", "state_3_committed.rds", "new_result.rds", "prior_backup.rds")
  unexpected_files <- setdiff(all_files, allowed_exact_names)
  if (length(unexpected_files) > 0L) {
    lock_token$preserve <- TRUE
    stop(sprintf("Unexpected or uncommitted pending artifacts found in transaction directory '%s': %s. Preserving evidence.",
                 tx_dir, paste(unexpected_files, collapse = ", ")), call. = FALSE)
  }

  # 6. Validate state sequence
  state_files <- c(
    prepared   = file.path(tx_dir, "state_1_prepared.rds"),
    publishing = file.path(tx_dir, "state_2_publishing.rds"),
    committed  = file.path(tx_dir, "state_3_committed.rds")
  )
  has_s1 <- file.exists(state_files[["prepared"]])
  has_s2 <- file.exists(state_files[["publishing"]])
  has_s3 <- file.exists(state_files[["committed"]])

  if (!has_s1) {
    lock_token$preserve <- TRUE
    stop(sprintf("Transaction directory at '%s' is missing initial state_1_prepared.rds.", tx_dir), call. = FALSE)
  }
  if (has_s3 && !has_s2) {
    lock_token$preserve <- TRUE
    stop(sprintf("Invalid state sequence at '%s': state_3_committed exists but state_2_publishing is missing.", tx_dir), call. = FALSE)
  }

  s1 <- tryCatch(readRDS(state_files[["prepared"]]), error = function(e) NULL)
  s2 <- if (has_s2) tryCatch(readRDS(state_files[["publishing"]]), error = function(e) NULL) else NULL
  s3 <- if (has_s3) tryCatch(readRDS(state_files[["committed"]]), error = function(e) NULL) else NULL

  .validate_state_record <- function(rec, expected_stage, expected_seq, prev_file = NULL) {
    if (!is.list(rec) ||
        !.is_strict_scalar_integer(rec$transaction_schema, .RESULT_CACHE_TX_SCHEMA_VERSION) ||
        !identical(rec$canonical_target, canonical_target) ||
        !identical(rec$transaction_nonce, tx_nonce_from_dir) ||
        !identical(rec$original_owner_nonce, orig_owner_nonce) ||
        !identical(rec$stage, expected_stage) ||
        !.is_strict_scalar_integer(rec$stage_seq, expected_seq)) {
      return(FALSE)
    }
    if (expected_seq == 1L) {
      if (!is.null(rec$predecessor_hash)) return(FALSE)
    } else {
      if (is.null(prev_file) || !file.exists(prev_file)) return(FALSE)
      prev_hash <- unname(tools::md5sum(prev_file))
      if (!identical(as.character(rec$predecessor_hash), as.character(prev_hash))) return(FALSE)
    }
    TRUE
  }

  if (!.validate_state_record(s1, "prepared", 1L)) {
    lock_token$preserve <- TRUE
    stop(sprintf("state_1_prepared.rds at '%s' is corrupt, forged, or mismatched with identity.", tx_dir), call. = FALSE)
  }
  if (has_s2 && !.validate_state_record(s2, "publishing", 2L, state_files[["prepared"]])) {
    lock_token$preserve <- TRUE
    stop(sprintf("state_2_publishing.rds at '%s' is corrupt, forged, or broken predecessor hash chain.", tx_dir), call. = FALSE)
  }
  if (has_s3 && !.validate_state_record(s3, "committed", 3L, state_files[["publishing"]])) {
    lock_token$preserve <- TRUE
    stop(sprintf("state_3_committed.rds at '%s' is corrupt, forged, or broken predecessor hash chain.", tx_dir), call. = FALSE)
  }

  latest_state <- if (has_s3) s3 else if (has_s2) s2 else s1

  # 7. Exact artifact path validation
  .is_valid_tx_artifact_path <- function(path, expected_base) {
    if (is.null(path) || !is.character(path) || length(path) != 1L || is.na(path)) return(FALSE)
    norm_tx <- normalizePath(tx_dir, winslash = "/", mustWork = FALSE)
    norm_p  <- normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
    identical(norm_tx, norm_p) && identical(basename(path), expected_base)
  }

  if (!.is_valid_tx_artifact_path(latest_state$temp_path, "new_result.rds")) {
    lock_token$preserve <- TRUE
    stop(sprintf("Invalid or out-of-directory temp_path detected in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
  }
  if (!file.exists(latest_state$temp_path)) {
    lock_token$preserve <- TRUE
    stop(sprintf("new_result.rds missing in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
  }
  if (!identical(as.character(unname(tools::md5sum(latest_state$temp_path))), as.character(latest_state$new_result_hash))) {
    lock_token$preserve <- TRUE
    stop(sprintf("MD5 mismatch for new_result.rds in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
  }

  temp_env <- tryCatch(readRDS(latest_state$temp_path), error = function(e) NULL)
  if (!is.list(temp_env) ||
      !identical(temp_env$format, .RESULT_CACHE_FORMAT) ||
      !.is_strict_scalar_integer(temp_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) ||
      !isTRUE(temp_env$complete) ||
      !identical(temp_env$identity_key, latest_state$identity_key) ||
      !inherits(temp_env$result, "cross_size_nested_cv_result")) {
    lock_token$preserve <- TRUE
    stop(sprintf("new_result.rds envelope at '%s' is invalid or corrupt. Preserving evidence.", latest_state$temp_path), call. = FALSE)
  }

  if (isTRUE(latest_state$had_prior_target)) {
    if (!.is_valid_tx_artifact_path(latest_state$backup_path, "prior_backup.rds")) {
      lock_token$preserve <- TRUE
      stop(sprintf("Invalid or out-of-directory backup_path in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
    }
    if (!file.exists(latest_state$backup_path)) {
      lock_token$preserve <- TRUE
      stop(sprintf("prior_backup.rds missing in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
    }
    bak_hash <- unname(tools::md5sum(latest_state$backup_path))
    if (!identical(as.character(bak_hash), as.character(latest_state$backup_hash)) ||
        !identical(as.character(bak_hash), as.character(latest_state$prior_target_hash))) {
      lock_token$preserve <- TRUE
      stop(sprintf("prior_backup.rds hash mismatch in transaction at '%s'. Preserving evidence.", tx_dir), call. = FALSE)
    }
    bak_env <- tryCatch(readRDS(latest_state$backup_path), error = function(e) NULL)
    if (!is.list(bak_env) ||
        !identical(bak_env$format, .RESULT_CACHE_FORMAT) ||
        !.is_strict_scalar_integer(bak_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) ||
        !isTRUE(bak_env$complete) ||
        !inherits(bak_env$result, "cross_size_nested_cv_result")) {
      lock_token$preserve <- TRUE
      stop(sprintf("prior_backup.rds envelope at '%s' is invalid or corrupt. Preserving evidence.", latest_state$backup_path), call. = FALSE)
    }
  } else {
    if (!is.null(latest_state$backup_path) || !is.null(latest_state$prior_target_hash) || !is.null(latest_state$backup_hash)) {
      lock_token$preserve <- TRUE
      stop(sprintf("Transaction at '%s' has unexpected backup metadata when had_prior_target is FALSE. Preserving evidence.", tx_dir), call. = FALSE)
    }
  }

  # 8. Target evaluation vs recovery decision
  target_exists <- file.exists(canonical_target)
  target_hash <- if (target_exists) unname(tools::md5sum(canonical_target)) else NULL

  # Case 1: Committed stage
  if (identical(latest_state$stage, "committed")) {
    if (target_exists && identical(as.character(target_hash), as.character(latest_state$new_result_hash))) {
      target_env <- tryCatch(readRDS(canonical_target), error = function(e) NULL)
      if (is.list(target_env) &&
          identical(target_env$format, .RESULT_CACHE_FORMAT) &&
          .is_strict_scalar_integer(target_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) &&
          isTRUE(target_env$complete) &&
          identical(target_env$identity_key, latest_state$identity_key) &&
          inherits(target_env$result, "cross_size_nested_cv_result")) {
        unlink(tx_dir, recursive = TRUE, force = TRUE)
        return(invisible(NULL))
      }
    }
    lock_token$preserve <- TRUE
    stop(sprintf("Committed transaction at '%s' target mismatch or corruption. Preserving evidence.", tx_dir), call. = FALSE)
  }

  # Case 2: Publishing stage
  if (identical(latest_state$stage, "publishing")) {
    # 2A: target replaced and verified
    if (target_exists && identical(as.character(target_hash), as.character(latest_state$new_result_hash))) {
      target_env <- tryCatch(readRDS(canonical_target), error = function(e) NULL)
      if (is.list(target_env) &&
          identical(target_env$format, .RESULT_CACHE_FORMAT) &&
          .is_strict_scalar_integer(target_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) &&
          isTRUE(target_env$complete) &&
          identical(target_env$identity_key, latest_state$identity_key) &&
          inherits(target_env$result, "cross_size_nested_cv_result")) {
        unlink(tx_dir, recursive = TRUE, force = TRUE)
        return(invisible(NULL))
      }
    }

    # 2B: had prior target, and target matches prior target (not replaced yet)
    if (isTRUE(latest_state$had_prior_target) &&
        target_exists &&
        identical(as.character(target_hash), as.character(latest_state$prior_target_hash))) {
      target_env <- tryCatch(readRDS(canonical_target), error = function(e) NULL)
      if (is.list(target_env) &&
          identical(target_env$format, .RESULT_CACHE_FORMAT) &&
          .is_strict_scalar_integer(target_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) &&
          isTRUE(target_env$complete) &&
          inherits(target_env$result, "cross_size_nested_cv_result")) {
        unlink(tx_dir, recursive = TRUE, force = TRUE)
        return(invisible(NULL))
      }
    }

    # 2C: had prior target, but target is missing or corrupted -> restore from verified backup
    if (isTRUE(latest_state$had_prior_target)) {
      if (target_exists) unlink(canonical_target, force = TRUE)
      file.copy(latest_state$backup_path, canonical_target, overwrite = TRUE)
      restored_hash <- unname(tools::md5sum(canonical_target))
      if (identical(as.character(restored_hash), as.character(latest_state$prior_target_hash))) {
        unlink(tx_dir, recursive = TRUE, force = TRUE)
        return(invisible(NULL))
      } else {
        lock_token$preserve <- TRUE
        stop(sprintf("Failed to restore verified backup for '%s' during publishing recovery.", canonical_target), call. = FALSE)
      }
    }

    # 2D: had no prior target and target does not exist
    if (!isTRUE(latest_state$had_prior_target) && !target_exists) {
      unlink(tx_dir, recursive = TRUE, force = TRUE)
      return(invisible(NULL))
    }
  }

  # Case 3: Prepared stage
  if (identical(latest_state$stage, "prepared")) {
    if (isTRUE(latest_state$had_prior_target)) {
      if (target_exists && identical(as.character(target_hash), as.character(latest_state$prior_target_hash))) {
        target_env <- tryCatch(readRDS(canonical_target), error = function(e) NULL)
        if (is.list(target_env) &&
            identical(target_env$format, .RESULT_CACHE_FORMAT) &&
            .is_strict_scalar_integer(target_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) &&
            isTRUE(target_env$complete) &&
            inherits(target_env$result, "cross_size_nested_cv_result")) {
          unlink(tx_dir, recursive = TRUE, force = TRUE)
          return(invisible(NULL))
        }
      }
      lock_token$preserve <- TRUE
      stop(sprintf("Prepared stage transaction at '%s' has corrupted or missing prior target.", tx_dir), call. = FALSE)
    } else {
      if (!target_exists) {
        unlink(tx_dir, recursive = TRUE, force = TRUE)
        return(invisible(NULL))
      }
      lock_token$preserve <- TRUE
      stop(sprintf("Prepared stage transaction at '%s' unexpectedly found target file '%s'.", tx_dir, canonical_target), call. = FALSE)
    }
  }

  # Ambiguous / unresolvable state
  lock_token$preserve <- TRUE
  stop(sprintf(
    "Unresolvable transaction recovery state for '%s' (transaction directory at '%s', stage: '%s'). Preserving all evidence and lock for manual inspection.",
    canonical_target, tx_dir, latest_state$stage
  ), call. = FALSE)
}

#' Publish completed result using transaction directory protocol
#' @noRd
.publish_nested_cv_result_transaction <- function(result_obj, identity_obj, canonical_target, lock_token) {
  if (is.null(canonical_target)) return(invisible(NULL))

  parent_dir <- dirname(canonical_target)
  base_name <- basename(canonical_target)
  tx_nonce <- .generate_nonce("tx")
  tx_dir <- file.path(parent_dir, paste0(".", base_name, ".tx-", tx_nonce))

  # Pre-existing sidecar check: dir.create must be exclusive
  if (dir.exists(tx_dir) || file.exists(tx_dir)) {
    stop(sprintf("Collision on transaction directory reservation: '%s' already exists.", tx_dir), call. = FALSE)
  }
  created <- suppressWarnings(dir.create(tx_dir, recursive = FALSE))
  if (!created) {
    stop(sprintf("Failed to create transaction directory: '%s'.", tx_dir), call. = FALSE)
  }

  on.exit({
    if (dir.exists(tx_dir) && !isTRUE(lock_token$preserve) && !isTRUE(.ncvroc_test_seams$fail_during_cleanup)) {
      unlink(tx_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  # 1. Write immutable transaction identity record
  identity_record <- list(
    transaction_schema   = .RESULT_CACHE_TX_SCHEMA_VERSION,
    format_version       = "NCVROC_tx_identity_v1",
    canonical_target     = canonical_target,
    transaction_nonce    = tx_nonce,
    original_owner_nonce = lock_token$ownership_nonce,
    created_at           = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
    pid                  = Sys.getpid(),
    hostname             = Sys.info()[["nodename"]]
  )
  identity_file <- file.path(tx_dir, "identity.rds")
  saveRDS(identity_record, identity_file)
  id_readback <- tryCatch(readRDS(identity_file), error = function(e) NULL)
  if (!is.list(id_readback) ||
      !.is_strict_scalar_integer(id_readback$transaction_schema, .RESULT_CACHE_TX_SCHEMA_VERSION) ||
      !identical(id_readback$transaction_nonce, tx_nonce) ||
      !identical(id_readback$original_owner_nonce, lock_token$ownership_nonce)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Failed to write and verify immutable transaction identity record in '%s'. Preserving evidence.", tx_dir), call. = FALSE)
  }

  # 2. Write new result envelope to temp_path
  envelope <- list(
    format       = .RESULT_CACHE_FORMAT,
    cache_schema = .RESULT_CACHE_SCHEMA_VERSION,
    complete     = TRUE,
    identity     = identity_obj$identity,
    identity_key = identity_obj$key,
    result       = result_obj
  )

  temp_path <- file.path(tx_dir, "new_result.rds")
  saveRDS(envelope, temp_path)

  # Test seam hook: fail_before_publish
  if (isTRUE(.ncvroc_test_seams$fail_before_publish)) {
    stop("Injected failure before publish.", call. = FALSE)
  }

  # Verify temp envelope
  temp_env <- tryCatch(readRDS(temp_path), error = function(e) e)
  .validate_nested_cv_envelope(temp_env, identity_obj, temp_path)
  new_result_hash <- unname(tools::md5sum(temp_path))

  had_prior_target <- file.exists(canonical_target)
  prior_target_hash <- NULL
  backup_path <- NULL
  backup_hash <- NULL

  if (had_prior_target) {
    prior_target_hash <- unname(tools::md5sum(canonical_target))
    backup_path <- file.path(tx_dir, "prior_backup.rds")
    file.copy(canonical_target, backup_path, overwrite = TRUE)
    backup_hash <- unname(tools::md5sum(backup_path))
    if (!identical(as.character(backup_hash), as.character(prior_target_hash))) {
      stop(sprintf("Backup verification failed (MD5 mismatch) before replacing '%s'.", canonical_target), call. = FALSE)
    }
  }

  # State 1: prepared
  state_1 <- list(
    transaction_schema   = .RESULT_CACHE_TX_SCHEMA_VERSION,
    canonical_target     = canonical_target,
    transaction_nonce    = tx_nonce,
    original_owner_nonce = lock_token$ownership_nonce,
    stage                = "prepared",
    stage_seq            = 1L,
    predecessor_hash     = NULL,
    had_prior_target     = had_prior_target,
    prior_target_hash    = prior_target_hash,
    backup_path          = backup_path,
    backup_hash          = backup_hash,
    temp_path            = temp_path,
    new_result_hash      = new_result_hash,
    identity_key         = identity_obj$key,
    cache_schema         = .RESULT_CACHE_SCHEMA_VERSION,
    pid                  = Sys.getpid(),
    time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  res_state_1 <- tryCatch(
    .write_durable_tx_state(tx_dir, state_1, "prepared", 1L),
    error = function(e) {
      lock_token$preserve <- TRUE
      stop(sprintf(
        "Failed to write initial 'prepared' state record in transaction directory '%s'. Preserving all evidence and lock for manual inspection. Error: %s",
        tx_dir, conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (isTRUE(.ncvroc_test_seams$capture_artifact_hashes)) {
    .ncvroc_test_seams$last_state_1_file <- res_state_1$file
    .ncvroc_test_seams$last_state_1_hash <- res_state_1$hash
  }

  # State 2: publishing
  state_2 <- list(
    transaction_schema   = .RESULT_CACHE_TX_SCHEMA_VERSION,
    canonical_target     = canonical_target,
    transaction_nonce    = tx_nonce,
    original_owner_nonce = lock_token$ownership_nonce,
    stage                = "publishing",
    stage_seq            = 2L,
    predecessor_hash     = res_state_1$hash,
    had_prior_target     = had_prior_target,
    prior_target_hash    = prior_target_hash,
    backup_path          = backup_path,
    backup_hash          = backup_hash,
    temp_path            = temp_path,
    new_result_hash      = new_result_hash,
    identity_key         = identity_obj$key,
    cache_schema         = .RESULT_CACHE_SCHEMA_VERSION,
    pid                  = Sys.getpid(),
    time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  res_state_2 <- tryCatch(
    .write_durable_tx_state(tx_dir, state_2, "publishing", 2L),
    error = function(e) {
      lock_token$preserve <- TRUE
      stop(sprintf(
        "Failed to transition to 'publishing' state in transaction directory '%s'. Target was not modified. Preserving prior valid state (state_1_prepared.rds), pending artifacts, and lock for manual inspection. Operator must verify no writer is active, resolve lock explicitly, and inspect preserved evidence. Error: %s",
        tx_dir, conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (isTRUE(.ncvroc_test_seams$capture_artifact_hashes)) {
    .ncvroc_test_seams$last_state_2_file <- res_state_2$file
    .ncvroc_test_seams$last_state_2_hash <- res_state_2$hash
  }

  # Test seam hook: fail_during_replace
  if (isTRUE(.ncvroc_test_seams$fail_during_replace)) {
    if (had_prior_target) {
      unlink(canonical_target, force = TRUE)
      file.copy(backup_path, canonical_target, overwrite = TRUE)
      restored_hash <- unname(tools::md5sum(canonical_target))
      if (identical(as.character(restored_hash), as.character(prior_target_hash))) {
        stop(sprintf("Failed to replace result file at '%s'. Restored prior result file from verified backup.", canonical_target), call. = FALSE)
      } else {
        lock_token$preserve <- TRUE
        stop(sprintf("Failed to replace result file at '%s' AND restore from backup failed. Preserving transaction at '%s'.", canonical_target, tx_dir), call. = FALSE)
      }
    } else {
      stop(sprintf("Failed to replace result file at '%s'.", canonical_target), call. = FALSE)
    }
  }

  # Replacement
  if (had_prior_target) {
    unlink(canonical_target, force = TRUE)
  }
  ren_ok <- file.rename(temp_path, canonical_target)
  if (!ren_ok) {
    ren_ok <- file.copy(temp_path, canonical_target, overwrite = TRUE)
  }
  if (!ren_ok) {
    if (had_prior_target) {
      file.copy(backup_path, canonical_target, overwrite = TRUE)
      restored_hash <- unname(tools::md5sum(canonical_target))
      if (identical(as.character(restored_hash), as.character(prior_target_hash))) {
        stop(sprintf("Failed to replace result file at '%s'. Restored prior result file from verified backup.", canonical_target), call. = FALSE)
      } else {
        lock_token$preserve <- TRUE
        stop(sprintf("Failed to replace result file at '%s' and restore from backup failed. Preserving transaction at '%s'.", canonical_target, tx_dir), call. = FALSE)
      }
    } else {
      stop(sprintf("Failed to publish result file to '%s'.", canonical_target), call. = FALSE)
    }
  }

  # Test seam hook: fail_during_verify
  if (isTRUE(.ncvroc_test_seams$fail_during_verify)) {
    writeLines("corrupted", canonical_target)
  }

  # Verification of newly published target
  pub_env <- tryCatch(readRDS(canonical_target), error = function(e) e)
  verify_pub_ok <- !inherits(pub_env, "error") &&
    is.list(pub_env) &&
    identical(pub_env$format, .RESULT_CACHE_FORMAT) &&
    .is_strict_scalar_integer(pub_env$cache_schema, .RESULT_CACHE_SCHEMA_VERSION) &&
    isTRUE(pub_env$complete) &&
    identical(pub_env$identity_key, identity_obj$key) &&
    inherits(pub_env$result, "cross_size_nested_cv_result") &&
    identical(as.character(unname(tools::md5sum(canonical_target))), as.character(new_result_hash))

  if (!verify_pub_ok) {
    if (isTRUE(.ncvroc_test_seams$fail_during_restore)) {
      unlink(canonical_target, force = TRUE)
      lock_token$preserve <- TRUE
      stop(sprintf("Published result file failed verification and restoration could not be verified. Preserved transaction at '%s'.", tx_dir), call. = FALSE)
    }
    if (had_prior_target) {
      unlink(canonical_target, force = TRUE)
      file.copy(backup_path, canonical_target, overwrite = TRUE)
      restored_hash <- unname(tools::md5sum(canonical_target))
      if (identical(as.character(restored_hash), as.character(prior_target_hash))) {
        stop(sprintf("Published result file failed verification; successfully restored previous result file at '%s'.", canonical_target), call. = FALSE)
      } else {
        lock_token$preserve <- TRUE
        stop(sprintf("Published result file failed verification and restoration could not be verified. Preserved transaction at '%s'.", tx_dir), call. = FALSE)
      }
    } else {
      unlink(canonical_target, force = TRUE)
      stop(sprintf("Published result file at '%s' failed verification.", canonical_target), call. = FALSE)
    }
  }

  if (isTRUE(.ncvroc_test_seams$capture_artifact_hashes)) {
    .ncvroc_test_seams$last_target_published_hash <- unname(tools::md5sum(canonical_target))
  }

  # State 3: committed
  state_3 <- list(
    transaction_schema   = .RESULT_CACHE_TX_SCHEMA_VERSION,
    canonical_target     = canonical_target,
    transaction_nonce    = tx_nonce,
    original_owner_nonce = lock_token$ownership_nonce,
    stage                = "committed",
    stage_seq            = 3L,
    predecessor_hash     = res_state_2$hash,
    had_prior_target     = had_prior_target,
    prior_target_hash    = prior_target_hash,
    backup_path          = backup_path,
    backup_hash          = backup_hash,
    temp_path            = temp_path,
    new_result_hash      = new_result_hash,
    identity_key         = identity_obj$key,
    cache_schema         = .RESULT_CACHE_SCHEMA_VERSION,
    pid                  = Sys.getpid(),
    time                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  target_curr_hash <- if (file.exists(canonical_target)) unname(tools::md5sum(canonical_target)) else "absent"
  res_state_3 <- tryCatch(
    .write_durable_tx_state(tx_dir, state_3, "committed", 3L),
    error = function(e) {
      lock_token$preserve <- TRUE
      stop(sprintf(
        "Published new target at '%s' (observed MD5: %s), but failed to record final 'committed' state in transaction directory '%s'. Preserving prior valid state records, all transaction artifacts, and lock for manual inspection. Transaction is not marked committed until state is durable. Error: %s",
        canonical_target, target_curr_hash, tx_dir, conditionMessage(e)
      ), call. = FALSE)
    }
  )

  # Test seam hook: fail_during_cleanup
  if (isTRUE(.ncvroc_test_seams$fail_during_cleanup)) {
    lock_token$preserve <- TRUE
    stop(sprintf("Target verified successfully, but transaction cleanup failed. Remaining leftovers at: '%s'.", tx_dir), call. = FALSE)
  }

  # Cleanup transaction directory
  unlink(tx_dir, recursive = TRUE, force = TRUE)

  invisible(NULL)
}
