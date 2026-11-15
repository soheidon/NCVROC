# ==============================================================================
# NCVROC Artifact Layout Resolver & Manifest Management (v0.34.0 §18)
# ==============================================================================

.ARTIFACT_CATEGORIES <- c(
  "reports",
  "results",
  "candidate_chunks",
  "checkpoints",
  "diagnostics"
)

.ARTIFACT_STATUSES <- c(
  "complete",
  "in_progress",
  "skipped",
  "failed"
)

.ARTIFACT_MANIFEST_FILENAME <- "artifact_manifest.rds"
.ARTIFACT_MANIFEST_SCHEMA_VERSION <- 1L

.artifact_norm_path <- function(path, must_work = FALSE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.artifact_path_key <- function(path) {
  p <- sub("/+$", "", .artifact_norm_path(path, must_work = FALSE))
  if (.Platform$OS.type == "windows") tolower(p) else p
}

.artifact_is_subpath <- function(path, parent) {
  p_key <- .artifact_path_key(path)
  parent_key <- .artifact_path_key(parent)
  identical(p_key, parent_key) || startsWith(p_key, paste0(parent_key, "/"))
}

.artifact_validate_dir_arg <- function(artifact_dir, arg_name = "artifact_dir") {
  if (!is.character(artifact_dir) || length(artifact_dir) != 1L ||
      is.na(artifact_dir) || !nzchar(trimws(artifact_dir))) {
    stop("`", arg_name, "` must be a single non-empty character path.", call. = FALSE)
  }
  artifact_dir
}

.artifact_validate_rel_segment <- function(x, arg_name) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", arg_name, "` must be NULL or a single non-empty character string.", call. = FALSE)
  }
  x_norm <- gsub("\\\\", "/", x)
  if (startsWith(x_norm, "/") || grepl("^[A-Za-z]:", x_norm)) {
    stop("`", arg_name, "` must be a relative path within the artifact category: ", x, call. = FALSE)
  }
  parts <- strsplit(x_norm, "/", fixed = TRUE)[[1L]]
  if (any(parts %in% c("", ".", ".."))) {
    stop("`", arg_name, "` must not contain empty, '.', or '..' path segments: ", x, call. = FALSE)
  }
  x_norm
}

.artifact_is_symlink_or_reparse <- function(path) {
  if (.cp_is_symlink_or_reparse(path)) {
    return(TRUE)
  }
  if (file.exists(path) || dir.exists(path)) {
    parent_dir <- dirname(path)
    if (dir.exists(parent_dir) && !identical(.artifact_path_key(parent_dir), .artifact_path_key(path))) {
      norm_parent <- sub("/+$", "", .artifact_norm_path(parent_dir, must_work = TRUE))
      norm_self <- sub("/+$", "", .artifact_norm_path(path, must_work = TRUE))
      expected <- paste0(norm_parent, "/", basename(path))
      if (.Platform$OS.type == "windows") {
        if (!identical(tolower(norm_self), tolower(expected))) {
          return(TRUE)
        }
      } else if (!identical(norm_self, expected)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

.artifact_ensure_dir <- function(dir_path, label = "directory") {
  if (.artifact_is_symlink_or_reparse(dir_path)) {
    stop("`", label, "` must not be a symbolic link or reparse point: ", dir_path, call. = FALSE)
  }
  if (file.exists(dir_path) && !dir.exists(dir_path)) {
    stop("`", label, "` exists and is not a directory: ", dir_path, call. = FALSE)
  }
  if (!dir.exists(dir_path)) {
    ok <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(dir_path)) {
      stop("Failed to create ", label, ": ", dir_path, call. = FALSE)
    }
  }
  canon <- sub("/+$", "", .artifact_norm_path(dir_path, must_work = TRUE))
  if (.artifact_is_symlink_or_reparse(canon)) {
    stop("Canonical `", label, "` must not be a symbolic link or reparse point: ", canon, call. = FALSE)
  }
  canon
}

#' Resolve NCVROC 5-category artifact layout
#'
#' Resolves the canonical paths for the five role-based artifact categories
#' (`reports`, `results`, `candidate_chunks`, `checkpoints`, `diagnostics`) and
#' the manifest file (`artifact_manifest.rds`) under `artifact_dir`.
#'
#' @param artifact_dir Character(1). Root directory for organized analysis artifacts.
#' @param create Logical(1). If `TRUE`, create `artifact_dir` and the five
#'   category subdirectories. Default `FALSE`.
#'
#' @return A list of class `"ncvroc_artifact_layout"` containing `artifact_dir`,
#'   `manifest_file`, and `categories` (named character vector of the five
#'   category directories).
#' @export
ncvroc_artifact_layout <- function(artifact_dir, create = FALSE) {
  .artifact_validate_dir_arg(artifact_dir, "artifact_dir")
  if (!is.logical(create) || length(create) != 1L || is.na(create)) {
    stop("`create` must be TRUE or FALSE.", call. = FALSE)
  }

  if (.artifact_is_symlink_or_reparse(artifact_dir)) {
    stop("`artifact_dir` must not be a symbolic link or reparse point: ", artifact_dir, call. = FALSE)
  }
  if (file.exists(artifact_dir) && !dir.exists(artifact_dir)) {
    stop("`artifact_dir` exists and is not a directory: ", artifact_dir, call. = FALSE)
  }

  root_path <- if (isTRUE(create)) {
    .artifact_ensure_dir(artifact_dir, label = "artifact_dir")
  } else {
    sub("/+$", "", .artifact_norm_path(artifact_dir, must_work = FALSE))
  }

  cats <- stats::setNames(
    vapply(.ARTIFACT_CATEGORIES, function(cat_name) {
      p <- file.path(root_path, cat_name)
      if (isTRUE(create)) {
        .artifact_ensure_dir(p, label = cat_name)
      } else {
        if (.artifact_is_symlink_or_reparse(p)) {
          stop("`", cat_name, "` must not be a symbolic link or reparse point: ", p, call. = FALSE)
        }
        if (file.exists(p) && !dir.exists(p)) {
          stop("`", cat_name, "` exists and is not a directory: ", p, call. = FALSE)
        }
        .artifact_norm_path(p, must_work = FALSE)
      }
    }, character(1L)),
    .ARTIFACT_CATEGORIES
  )

  out <- list(
    artifact_dir  = root_path,
    manifest_file = .artifact_norm_path(file.path(root_path, .ARTIFACT_MANIFEST_FILENAME), must_work = FALSE),
    categories    = cats
  )
  class(out) <- "ncvroc_artifact_layout"
  out
}

.artifact_walk_subpath <- function(base_dir,
                                  rel_path,
                                  label_prefix,
                                  create = FALSE,
                                  leaf_is_dir = TRUE) {
  parts <- strsplit(rel_path, "/", fixed = TRUE)[[1L]]
  cur <- base_dir
  n_parts <- length(parts)
  for (i in seq_len(n_parts)) {
    seg <- parts[[i]]
    cur <- file.path(cur, seg)
    is_dir_seg <- (i < n_parts) || isTRUE(leaf_is_dir)
    seg_label <- paste0(label_prefix, "/", paste(parts[seq_len(i)], collapse = "/"))

    if (.artifact_is_symlink_or_reparse(cur)) {
      stop("`", seg_label, "` must not be a symbolic link or reparse point: ", cur, call. = FALSE)
    }
    if (is_dir_seg) {
      if (file.exists(cur) && !dir.exists(cur)) {
        stop("`", seg_label, "` exists and is not a directory: ", cur, call. = FALSE)
      }
      if (isTRUE(create) && !dir.exists(cur)) {
        ok <- dir.create(cur, recursive = FALSE, showWarnings = FALSE)
        if (!ok && !dir.exists(cur)) {
          stop("Failed to create ", seg_label, ": ", cur, call. = FALSE)
        }
        if (.artifact_is_symlink_or_reparse(cur)) {
          stop("`", seg_label, "` must not be a symbolic link or reparse point: ", cur, call. = FALSE)
        }
      }
    }
  }
  cur
}

#' Resolve a role-based artifact path inside an NCVROC artifact root
#'
#' @param artifact_dir Character(1). Root directory for organized analysis artifacts.
#' @param category Character(1). One of `"reports"`, `"results"`,
#'   `"candidate_chunks"`, `"checkpoints"`, or `"diagnostics"`.
#' @param filename Optional character(1) relative file or directory name inside
#'   the category (or inside `subdir`).
#' @param subdir Optional character(1) relative subdirectory inside the category.
#' @param create Logical(1). If `TRUE`, ensure the parent directory (or category
#'   directory when `filename` is `NULL`) exists. Default `FALSE`.
#'
#' @return Character(1) normalized path.
#' @export
ncvroc_artifact_path <- function(artifact_dir,
                                 category,
                                 filename = NULL,
                                 subdir = NULL,
                                 create = FALSE) {
  if (!is.character(category) || length(category) != 1L || is.na(category) ||
      !(category %in% .ARTIFACT_CATEGORIES)) {
    stop(
      "`category` must be one of: ",
      paste(shQuote(.ARTIFACT_CATEGORIES), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  subdir_clean <- .artifact_validate_rel_segment(subdir, "subdir")
  filename_clean <- .artifact_validate_rel_segment(filename, "filename")

  layout <- ncvroc_artifact_layout(artifact_dir, create = create)
  target <- layout$categories[[category]]
  if (!is.null(subdir_clean)) {
    target <- .artifact_walk_subpath(
      target,
      subdir_clean,
      label_prefix = category,
      create = create,
      leaf_is_dir = TRUE
    )
  }
  if (!is.null(filename_clean)) {
    parent_label <- if (!is.null(subdir_clean)) paste0(category, "/", subdir_clean) else category
    target <- .artifact_walk_subpath(
      target,
      filename_clean,
      label_prefix = parent_label,
      create = create,
      leaf_is_dir = FALSE
    )
  }
  .artifact_norm_path(target, must_work = FALSE)
}

.artifact_pkg_version <- function() {
  ns <- tryCatch(asNamespace("NCVROC"), error = function(e) NULL)
  if (!is.null(ns)) {
    ver <- tryCatch(getNamespaceVersion(ns), error = function(e) NULL)
    if (length(ver) && !is.na(ver[[1L]]) && nzchar(ver[[1L]])) {
      return(as.character(ver[[1L]]))
    }
  }
  as.character(utils::packageVersion("NCVROC"))
}

.artifact_empty_manifest <- function(root_path) {
  list(
    schema_version  = .ARTIFACT_MANIFEST_SCHEMA_VERSION,
    package_version = .artifact_pkg_version(),
    artifact_dir    = root_path,
    updated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    artifacts       = list()
  )
}

.artifact_validate_manifest_obj <- function(obj, manifest_file) {
  if (!is.list(obj) ||
      is.null(obj$schema_version) ||
      !is.numeric(obj$schema_version) ||
      length(obj$schema_version) != 1L ||
      !identical(as.integer(obj$schema_version), .ARTIFACT_MANIFEST_SCHEMA_VERSION) ||
      !is.list(obj$artifacts)) {
    stop("Corrupted or unsupported artifact manifest at: ", manifest_file, call. = FALSE)
  }
  obj
}

#' Read an NCVROC artifact manifest
#'
#' @param artifact_dir Character(1). Root directory for organized analysis artifacts.
#'
#' @return Parsed manifest list, or an empty manifest structure if
#'   `artifact_manifest.rds` does not yet exist. Stops with an error if the
#'   manifest file exists but cannot be read or is malformed.
#' @export
ncvroc_read_manifest <- function(artifact_dir) {
  layout <- ncvroc_artifact_layout(artifact_dir, create = FALSE)
  manifest_file <- layout$manifest_file
  if (!file.exists(manifest_file)) {
    return(.artifact_empty_manifest(layout$artifact_dir))
  }
  obj <- tryCatch(
    readRDS(manifest_file),
    error = function(e) {
      stop("Failed to read artifact manifest at ", manifest_file, ": ",
           conditionMessage(e), call. = FALSE)
    }
  )
  .artifact_validate_manifest_obj(obj, manifest_file)
}

.artifact_write_manifest_atomic <- function(manifest, artifact_dir) {
  root_path <- .artifact_ensure_dir(artifact_dir, label = "artifact_dir")
  manifest_file <- file.path(root_path, .ARTIFACT_MANIFEST_FILENAME)
  manifest$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  manifest$artifact_dir <- root_path

  tmp <- tempfile(pattern = ".artifact_manifest_", tmpdir = root_path, fileext = ".rds")
  ok_write <- tryCatch({
    saveRDS(manifest, tmp)
    TRUE
  }, error = function(e) {
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    stop("Failed to write artifact manifest temp file: ", conditionMessage(e), call. = FALSE)
  })
  if (!isTRUE(ok_write) || !file.exists(tmp)) {
    stop("Failed to stage artifact manifest temp file.", call. = FALSE)
  }
  renamed <- suppressWarnings(file.rename(tmp, manifest_file))
  if (!isTRUE(renamed)) {
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    stop("Failed to atomically commit artifact manifest: ", manifest_file, call. = FALSE)
  }
  invisible(manifest_file)
}

.artifact_relative_path <- function(path, root_path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    return(NA_character_)
  }
  path_norm <- sub("/+$", "", .artifact_norm_path(path, must_work = FALSE))
  root_norm <- sub("/+$", "", .artifact_norm_path(root_path, must_work = FALSE))
  if (.artifact_is_subpath(path_norm, root_norm)) {
    p_key <- .artifact_path_key(path_norm)
    r_key <- .artifact_path_key(root_norm)
    if (identical(p_key, r_key)) return(".")
    return(substr(path_norm, nchar(root_norm) + 2L, nchar(path_norm)))
  }
  path_norm
}

#' Register or update an artifact in the NCVROC manifest
#'
#' Atomically records an artifact's category, role, status, relative path,
#' analysis identity, provenance, and cross-artifact references in
#' `<artifact_dir>/artifact_manifest.rds`.
#'
#' @param artifact_dir Character(1). Root directory for organized analysis artifacts.
#' @param category Character(1). One of `"reports"`, `"results"`,
#'   `"candidate_chunks"`, `"checkpoints"`, or `"diagnostics"`.
#' @param role Character(1). Semantic role of the artifact (e.g.
#'   `"nested_cv_report"`, `"final_exhaustive_rds"`, `"final_candidate_chunks"`,
#'   `"outer_fold_checkpoint"`, `"audit_summary"`, `"session_info"`).
#' @param path Optional character(1) path to the artifact file or directory.
#'   Required when `status == "complete"` and `verify_exists = TRUE`. May be
#'   `NULL` when `status` is `"skipped"` or `"failed"`.
#' @param producer Character(1). Component or caller that produced the artifact
#'   (default `"user"`).
#' @param status Character(1). One of `"complete"`, `"in_progress"`, `"skipped"`,
#'   or `"failed"`.
#' @param analysis_identity Optional character(1) analysis identity hash/label.
#' @param reason Optional character(1) reason (e.g. why an optional analysis was
#'   skipped or failed).
#' @param references Optional character vector of related `artifact_id`s or
#'   relative paths (e.g. a result RDS referencing its `candidate_chunks/` dir).
#' @param artifact_id Optional character(1) stable identifier. Defaults to
#'   `"<category>::<relative_path>"` when `path` is provided, or
#'   `"<category>::<role>"` otherwise.
#' @param metadata Optional named list of additional metadata.
#' @param verify_exists Logical(1). When `TRUE` (default) and
#'   `status == "complete"`, verify that `path` exists on disk before recording.
#' @param allow_external Logical(1). When `FALSE` (default), require `path` to
#'   lie inside `<artifact_dir>/<category>/`. Set `TRUE` only for explicit
#'   external references (e.g., cache-hit entries stored under `cache_dir`).
#'
#' @return The registered manifest entry list, invisibly.
#' @export
ncvroc_register_artifact <- function(artifact_dir,
                                     category,
                                     role,
                                     path = NULL,
                                     producer = "user",
                                     status = c("complete", "in_progress", "skipped", "failed"),
                                     analysis_identity = NULL,
                                     reason = NULL,
                                     references = NULL,
                                     artifact_id = NULL,
                                     metadata = NULL,
                                     verify_exists = TRUE,
                                     allow_external = FALSE) {
  status <- match.arg(status)
  if (!is.character(category) || length(category) != 1L || is.na(category) ||
      !(category %in% .ARTIFACT_CATEGORIES)) {
    stop("`category` must be one of: ",
         paste(shQuote(.ARTIFACT_CATEGORIES), collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.character(role) || length(role) != 1L || is.na(role) || !nzchar(trimws(role))) {
    stop("`role` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(producer) || length(producer) != 1L || is.na(producer) || !nzchar(trimws(producer))) {
    stop("`producer` must be a single non-empty character string.", call. = FALSE)
  }

  layout <- ncvroc_artifact_layout(artifact_dir, create = TRUE)
  manifest <- ncvroc_read_manifest(layout$artifact_dir)

  abs_path <- NA_character_
  rel_path <- NA_character_
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
      stop("`path` must be NULL or a single non-empty character path.", call. = FALSE)
    }
    abs_path <- sub("/+$", "", .artifact_norm_path(path, must_work = FALSE))
    if (!isTRUE(allow_external) &&
        !.artifact_is_subpath(abs_path, layout$categories[[category]])) {
      stop("Artifact `path` must lie inside category '", category, "' directory (",
           layout$categories[[category]], "): ", abs_path, call. = FALSE)
    }
    rel_path <- .artifact_relative_path(abs_path, layout$artifact_dir)
  }

  if (identical(status, "complete") && isTRUE(verify_exists)) {
    if (is.na(abs_path) || (!file.exists(abs_path) && !dir.exists(abs_path))) {
      stop("Cannot register `status = 'complete'` for missing artifact path: ",
           abs_path %||% "NULL", call. = FALSE)
    }
  }

  if (is.null(artifact_id)) {
    key_tail <- if (!is.na(rel_path)) rel_path else role
    artifact_id <- paste0(category, "::", key_tail)
  } else if (!is.character(artifact_id) || length(artifact_id) != 1L ||
             is.na(artifact_id) || !nzchar(trimws(artifact_id))) {
    stop("`artifact_id` must be NULL or a single non-empty character string.", call. = FALSE)
  }

  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- manifest$artifacts[[artifact_id]]
  created_at <- if (!is.null(existing) && !is.null(existing$created_at)) {
    existing$created_at
  } else {
    now_utc
  }

  refs_vec <- if (is.null(references)) {
    character(0L)
  } else {
    as.character(references)
  }

  entry <- list(
    artifact_id       = artifact_id,
    category          = category,
    role              = role,
    status            = status,
    relative_path     = rel_path,
    path              = abs_path,
    producer          = producer,
    analysis_identity = if (is.null(analysis_identity)) NA_character_ else as.character(analysis_identity)[1L],
    created_at        = created_at,
    updated_at        = now_utc,
    package_version   = .artifact_pkg_version(),
    schema_version    = .ARTIFACT_MANIFEST_SCHEMA_VERSION,
    reason            = if (is.null(reason)) NA_character_ else as.character(reason)[1L],
    references        = refs_vec,
    metadata          = if (is.null(metadata)) list() else metadata
  )

  manifest$artifacts[[artifact_id]] <- entry
  .artifact_write_manifest_atomic(manifest, layout$artifact_dir)
  invisible(entry)
}

#' List registered NCVROC artifacts from the manifest
#'
#' @param artifact_dir Character(1). Root directory for organized analysis artifacts.
#' @param category Optional character vector of categories to filter by.
#' @param status Optional character vector of statuses to filter by.
#' @param role Optional character vector of roles to filter by.
#'
#' @return A `data.frame` describing registered artifacts (0 rows if none match
#'   or if no manifest has been created yet).
#' @export
ncvroc_list_artifacts <- function(artifact_dir,
                                  category = NULL,
                                  status = NULL,
                                  role = NULL) {
  manifest <- ncvroc_read_manifest(artifact_dir)
  empty_df <- data.frame(
    artifact_id       = character(0L),
    category          = character(0L),
    role              = character(0L),
    status            = character(0L),
    relative_path     = character(0L),
    producer          = character(0L),
    analysis_identity = character(0L),
    created_at        = character(0L),
    updated_at        = character(0L),
    package_version   = character(0L),
    schema_version    = integer(0L),
    reason            = character(0L),
    references        = character(0L),
    stringsAsFactors  = FALSE
  )
  entries <- manifest$artifacts
  if (!length(entries)) return(empty_df)

  df <- do.call(rbind, lapply(entries, function(e) {
    refs <- if (length(e$references)) paste(e$references, collapse = "; ") else NA_character_
    data.frame(
      artifact_id       = as.character(e$artifact_id %||% NA_character_),
      category          = as.character(e$category %||% NA_character_),
      role              = as.character(e$role %||% NA_character_),
      status            = as.character(e$status %||% NA_character_),
      relative_path     = as.character(e$relative_path %||% NA_character_),
      producer          = as.character(e$producer %||% NA_character_),
      analysis_identity = as.character(e$analysis_identity %||% NA_character_),
      created_at        = as.character(e$created_at %||% NA_character_),
      updated_at        = as.character(e$updated_at %||% NA_character_),
      package_version   = as.character(e$package_version %||% NA_character_),
      schema_version    = as.integer(e$schema_version %||% NA_integer_),
      reason            = as.character(e$reason %||% NA_character_),
      references        = refs,
      stringsAsFactors  = FALSE
    )
  }))
  rownames(df) <- NULL

  if (!is.null(category)) df <- df[df$category %in% category, , drop = FALSE]
  if (!is.null(status)) df <- df[df$status %in% status, , drop = FALSE]
  if (!is.null(role)) df <- df[df$role %in% role, , drop = FALSE]
  rownames(df) <- NULL
  df
}

.artifact_validate_cp_fold_envelope <- function(env, stored, fi) {
  if (!is.list(env)) return("envelope is not a list")
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
  if (!identical(env$analysis_identity_hash, stored$analysis_identity_hash)) {
    return("envelope analysis_identity_hash mismatch")
  }
  if (!.cp_is_valid_int_scalar(env$repeat_id, min_val = 1) ||
      !.cp_is_valid_int_scalar(env$outer_fold_id, min_val = 1) ||
      !identical(as.integer(env$repeat_id), as.integer(fi$repeat_id)) ||
      !identical(as.integer(env$outer_fold_id), as.integer(fi$outer_fold_id)) ||
      !identical(env$outer_fold, as.character(fi$fold_name))) {
    return("envelope fold identity mismatch")
  }
  if (!identical(env$split_hash, fi$split_hash)) {
    return("envelope split_hash mismatch")
  }
  if (!identical(env$completion_status, "complete")) {
    return("envelope completion_status is not 'complete'")
  }
  prod <- env$producer
  if (!is.list(prod) ||
      !is.character(prod$backend) || length(prod$backend) != 1L || is.na(prod$backend) ||
      !(prod$backend %in% .CHECKPOINT_SUPPORTED_PRODUCERS) ||
      !identical(suppressWarnings(as.integer(prod$payload_schema_version)), .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION) ||
      !identical(suppressWarnings(as.integer(prod$envelope_schema_version)), .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION) ||
      !identical(prod$statistical_contract_id, .CHECKPOINT_STATISTICAL_CONTRACT_ID)) {
    return("envelope producer metadata invalid or unsupported")
  }
  if (!is.list(env$payload) || is.null(env$payload$selected_items) || is.null(env$payload$predictions)) {
    return("envelope payload is missing required fields")
  }
  if (!identical(env$payload_hash, .cp_hash_object(env$payload))) {
    return("envelope payload_hash mismatch")
  }
  NULL
}

#' Inspect the integrity and completion status of an NCVROC checkpoint directory
#'
#' Performs a read-only verification of `identity.rds`, `pending/`, and `folds/`
#' inside `checkpoint_dir`, including optional `analysis_identity` matching,
#' exact expected fold filenames, absence of extra/orphan files, and SHA-256
#' envelope/payload verification for each fold file.
#'
#' @param checkpoint_dir Character(1). Path to an NCVROC checkpoint directory.
#' @param expected_analysis_identity Optional character(1) SHA-256
#'   `analysis_identity_hash` (for example from
#'   `result$nested_result$settings$checkpoint_provenance$analysis_identity_hash`).
#' @param outer_k Optional integer(1) expected number of outer folds.
#' @param outer_repeats Optional integer(1) expected number of outer repeats.
#'
#' @return A list with `status` (`"missing"`, `"in_progress"`, `"complete"`, or
#'   `"failed"`), `analysis_identity` (character or `NA_character_`), `n_expected`,
#'   `n_valid`, `n_missing`, and `reason`.
#' @export
ncvroc_inspect_checkpoint <- function(checkpoint_dir,
                                      expected_analysis_identity = NULL,
                                      outer_k = NULL,
                                      outer_repeats = NULL) {
  .artifact_validate_dir_arg(checkpoint_dir, "checkpoint_dir")
  make_res <- function(status,
                       analysis_identity = NA_character_,
                       n_expected = 0L,
                       n_valid = 0L,
                       n_missing = 0L,
                       reason = NA_character_) {
    list(
      status            = status,
      analysis_identity = analysis_identity,
      n_expected        = as.integer(n_expected),
      n_valid           = as.integer(n_valid),
      n_missing         = as.integer(n_missing),
      reason            = reason
    )
  }

  if (.artifact_is_symlink_or_reparse(checkpoint_dir)) {
    return(make_res("failed", reason = "checkpoint_dir must not be a symbolic link or reparse point"))
  }
  if (!dir.exists(checkpoint_dir)) {
    return(make_res("missing", reason = "checkpoint_dir does not exist"))
  }

  id_file <- file.path(checkpoint_dir, "identity.rds")
  folds_dir <- file.path(checkpoint_dir, "folds")
  pending_dir <- file.path(checkpoint_dir, "pending")
  for (p in c(id_file, folds_dir, pending_dir)) {
    if (.artifact_is_symlink_or_reparse(p)) {
      return(make_res("failed", reason = paste0("symbolic link or reparse point in checkpoint: ", basename(p))))
    }
  }

  actual_folds <- if (dir.exists(folds_dir)) {
    .cp_filter_windows_desktop_ini(folds_dir, list.files(folds_dir, all.files = TRUE, no.. = TRUE))
  } else {
    character(0L)
  }
  actual_pending <- if (dir.exists(pending_dir)) {
    .cp_filter_windows_desktop_ini(pending_dir, list.files(pending_dir, all.files = TRUE, no.. = TRUE))
  } else {
    character(0L)
  }

  if (!file.exists(id_file)) {
    if (length(actual_folds) > 0L || length(actual_pending) > 0L) {
      return(make_res("failed", reason = "missing identity.rds while fold or pending files exist"))
    }
    return(make_res("in_progress", reason = "empty or uninitialized checkpoint directory"))
  }

  stored <- tryCatch(readRDS(id_file), error = function(e) NULL)
  if (!is.list(stored) || !is.list(stored$core) ||
      !identical(stored$format_id, .CHECKPOINT_CANONICAL_IDENTITY_FORMAT_ID) ||
      !identical(suppressWarnings(as.integer(stored$schema_version)), .CHECKPOINT_CANONICAL_IDENTITY_SCHEMA_VERSION) ||
      !identical(stored$core$hash_algorithm, .CHECKPOINT_HASH_ALGORITHM) ||
      !identical(stored$core$statistical_contract_id, .CHECKPOINT_STATISTICAL_CONTRACT_ID) ||
      !identical(suppressWarnings(as.integer(stored$core$payload_schema_version)), .CHECKPOINT_CANONICAL_PAYLOAD_SCHEMA_VERSION) ||
      !identical(suppressWarnings(as.integer(stored$core$envelope_schema_version)), .CHECKPOINT_CANONICAL_ENVELOPE_SCHEMA_VERSION) ||
      !is.character(stored$analysis_identity_hash) || length(stored$analysis_identity_hash) != 1L ||
      !identical(stored$analysis_identity_hash, .cp_hash_object(stored$core))) {
    return(make_res("failed", reason = "corrupt or unsupported identity.rds"))
  }

  cp_id <- stored$analysis_identity_hash
  fold_ids <- stored$core$fold_identities
  n_exp <- length(fold_ids)
  if (!is.list(fold_ids) || n_exp == 0L) {
    return(make_res("failed", analysis_identity = cp_id, reason = "identity.rds has no fold_identities"))
  }

  if (!is.null(expected_analysis_identity)) {
    if (!is.character(expected_analysis_identity) || length(expected_analysis_identity) != 1L ||
        is.na(expected_analysis_identity) || !identical(cp_id, expected_analysis_identity)) {
      return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                      reason = "checkpoint analysis_identity mismatch"))
    }
  }
  if (!is.null(outer_k) &&
      !identical(suppressWarnings(as.integer(stored$core$options$outer_k)), as.integer(outer_k))) {
    return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                    reason = "checkpoint outer_k mismatch"))
  }
  if (!is.null(outer_repeats) &&
      !identical(suppressWarnings(as.integer(stored$core$options$outer_repeats)), as.integer(outer_repeats))) {
    return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                    reason = "checkpoint outer_repeats mismatch"))
  }

  if (length(actual_pending) > 0L) {
    return(make_res("in_progress", analysis_identity = cp_id, n_expected = n_exp,
                    reason = "unresolved pending checkpoint artifacts"))
  }

  expected_files <- vapply(fold_ids, function(fi) {
    .cp_fold_filename(fi$repeat_id, fi$outer_fold_id)
  }, character(1L))
  unexpected_folds <- setdiff(actual_folds, expected_files)
  if (length(unexpected_folds) > 0L) {
    return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                    reason = paste0("unexpected fold files: ", paste(unexpected_folds, collapse = ", "))))
  }

  n_valid <- 0L
  n_missing <- 0L
  for (i in seq_along(fold_ids)) {
    fi <- fold_ids[[i]]
    fpath <- file.path(folds_dir, expected_files[[i]])
    if (!file.exists(fpath)) {
      n_missing <- n_missing + 1L
      next
    }
    if (dir.exists(fpath) || .artifact_is_symlink_or_reparse(fpath)) {
      return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                      n_valid = n_valid, n_missing = n_missing,
                      reason = paste0("invalid fold file or symlink: ", expected_files[[i]])))
    }
    env <- tryCatch(readRDS(fpath), error = function(e) NULL)
    if (is.null(env)) {
      return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                      n_valid = n_valid, n_missing = n_missing,
                      reason = paste0("corrupt fold RDS: ", expected_files[[i]])))
    }
    err <- .artifact_validate_cp_fold_envelope(env, stored, fi)
    if (!is.null(err)) {
      return(make_res("failed", analysis_identity = cp_id, n_expected = n_exp,
                      n_valid = n_valid, n_missing = n_missing,
                      reason = paste0(expected_files[[i]], ": ", err)))
    }
    n_valid <- n_valid + 1L
  }

  if (n_missing > 0L) {
    return(make_res("in_progress", analysis_identity = cp_id, n_expected = n_exp,
                    n_valid = n_valid, n_missing = n_missing,
                    reason = sprintf("%d/%d folds complete", n_valid, n_exp)))
  }

  make_res("complete", analysis_identity = cp_id, n_expected = n_exp,
           n_valid = n_valid, n_missing = 0L, reason = "all checkpoint folds verified")
}
