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
