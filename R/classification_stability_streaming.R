# ==============================================================================
# Bounded-Memory Streaming, Work-Dir Isolation, 2-Slot Rolling Checkpoints,
# and Verified Publish Helpers for Classification Stability (v0.34.1)
# ==============================================================================

.CS_STREAM_SCHEMA_VERSION <- "0.34.1-cs-stream-v1"
.CS_MANIFEST_SCHEMA_VERSION <- "0.34.1-cs-manifest-v1"

#' Check if a path appears to reside inside a cloud-synced directory
#' @keywords internal
#' @noRd
.cs_is_sync_root_path <- function(path) {
  norm <- gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = FALSE))
  low <- tolower(norm)
  sync_patterns <- c(
    "/googledrive(/|$)",
    "/google drive(/|$)",
    "/my drive(/|$)",
    "/shared drives(/|$)",
    "/onedrive([^/]*)(/|$)",
    "/dropbox(/|$)",
    "/icloud(/|$)",
    "/box sync(/|$)",
    "\\.shortcut-targets-by-id(/|$)"
  )
  if (any(vapply(sync_patterns, function(p) grepl(p, low), logical(1)))) {
    return(TRUE)
  }
  # Also inspect existing ancestor directories for Google Drive / sync markers
  cur <- norm
  for (i in seq_len(12L)) {
    if (dir.exists(cur)) {
      if (file.exists(file.path(cur, ".gd")) ||
          file.exists(file.path(cur, ".tmp.drivedownload"))) {
        return(TRUE)
      }
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  FALSE
}

#' Extract volume identifier for same-volume checks
#' @keywords internal
#' @noRd
.cs_get_volume_id <- function(path) {
  norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    if (grepl("^[A-Za-z]:", norm)) {
      return(tolower(substr(norm, 1L, 2L)))
    }
    if (grepl("^//[^/]+/[^/]+", norm)) {
      m <- regmatches(norm, regexpr("^//[^/]+/[^/]+", norm))
      return(tolower(m))
    }
    return(tolower(norm))
  }
  # POSIX fallback: device id from file.info of deepest existing ancestor
  cur <- norm
  while (!dir.exists(cur) && !file.exists(cur)) {
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  info <- tryCatch(file.info(cur), error = function(e) NULL)
  if (!is.null(info) && "dev" %in% names(info)) as.character(info$dev) else "/"
}

#' Query free disk space in bytes for the volume containing `path`
#' @keywords internal
#' @noRd
.cs_get_free_space_bytes <- function(path) {
  cur <- normalizePath(path, winslash = "/", mustWork = FALSE)
  while (!dir.exists(cur)) {
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  if (.Platform$OS.type == "windows") {
    vol <- .cs_get_volume_id(cur)
    if (grepl("^[a-z]:$", vol)) {
      drive_letter <- substr(vol, 1L, 1L)
      cmd <- sprintf(
        "(Get-PSDrive -Name '%s').Free",
        drive_letter
      )
      out <- tryCatch(
        suppressWarnings(system2("powershell", args = c("-NoProfile", "-Command", cmd),
                                 stdout = TRUE, stderr = FALSE)),
        error = function(e) character(0)
      )
      val <- suppressWarnings(as.numeric(trimws(paste(out, collapse = ""))))
      if (length(val) == 1L && !is.na(val) && is.finite(val)) {
        return(val)
      }
    }
    return(NA_real_)
  }
  out <- tryCatch(
    suppressWarnings(system2("df", args = c("-k", shQuote(cur)), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(out) >= 2L) {
    fields <- strsplit(trimws(out[2L]), "\\s+")[[1L]]
    if (length(fields) >= 4L) {
      kb <- suppressWarnings(as.numeric(fields[4L]))
      if (!is.na(kb) && is.finite(kb)) return(kb * 1024)
    }
  }
  NA_real_
}

#' Generate a unique, collision-free run_id
#' @keywords internal
#' @noRd
.cs_generate_run_id <- function(runs_root) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  for (attempt in seq_len(20L)) {
    suffix <- paste(sample(c(0:9, letters[1:6]), 6L, replace = TRUE), collapse = "")
    cand <- paste0("run_", ts, "_", suffix)
    if (!dir.exists(file.path(runs_root, cand))) {
      return(cand)
    }
  }
  stop("Failed to generate a unique `run_id` under: ", runs_root, call. = FALSE)
}

#' Validate and resolve work_dir, run_id, checkpoints, temp, diagnostics, and result_file
#' @keywords internal
#' @noRd
.cs_resolve_work_layout <- function(work_dir,
                                    run_id,
                                    checkpoint_dir,
                                    temp_dir,
                                    diagnostics_dir,
                                    result_file,
                                    resume = FALSE,
                                    force_recompute = FALSE,
                                    min_free_bytes = 50 * 1024 * 1024) {
  if (is.null(work_dir)) {
    # Legacy mode: preserve exact existing behavior
    if (!is.null(checkpoint_dir) || !is.null(temp_dir) || !is.null(diagnostics_dir)) {
      stop("`checkpoint_dir`, `temp_dir`, and `diagnostics_dir` overrides require `work_dir` to be specified.", call. = FALSE)
    }
    return(list(
      enabled         = FALSE,
      work_dir        = NULL,
      run_id          = NULL,
      run_dir         = NULL,
      checkpoint_dir  = NULL,
      temp_dir        = NULL,
      diagnostics_dir = NULL,
      manifest_file   = NULL,
      result_file     = result_file
    ))
  }

  if (!is.character(work_dir) || length(work_dir) != 1L || is.na(work_dir) || !nzchar(trimws(work_dir))) {
    stop("`work_dir` must be NULL or a single non-empty directory path.", call. = FALSE)
  }
  if (.cs_is_sync_root_path(work_dir)) {
    stop("`work_dir` must be on a local non-synced directory, not inside Google Drive/OneDrive/Dropbox: ",
         work_dir, call. = FALSE)
  }
  work_canon <- .artifact_ensure_dir(work_dir, "work_dir")
  if (.cs_is_sync_root_path(work_canon)) {
    stop("Canonical `work_dir` resolves inside a cloud-synced directory: ", work_canon, call. = FALSE)
  }

  runs_root <- .artifact_ensure_dir(file.path(work_canon, "runs"), "runs_root")

  if (is.null(run_id)) {
    if (isTRUE(resume)) {
      stop("`run_id` must be explicitly specified when `resume = TRUE`.", call. = FALSE)
    }
    run_id <- .cs_generate_run_id(runs_root)
  } else {
    if (!is.character(run_id) || length(run_id) != 1L || is.na(run_id) || !nzchar(trimws(run_id))) {
      stop("`run_id` must be a single non-empty character string.", call. = FALSE)
    }
    if (grepl("[/\\\\:]|\\.\\.", run_id)) {
      stop("`run_id` must not contain path separators or '..': ", run_id, call. = FALSE)
    }
  }

  run_dir <- file.path(runs_root, run_id)
  if (!isTRUE(resume) && !isTRUE(force_recompute) && dir.exists(run_dir)) {
    stop(sprintf(
      "Run directory already exists for `run_id = '%s'` (%s). Specify `resume = TRUE` to resume or omit `run_id` for a new run.",
      run_id, run_dir
    ), call. = FALSE)
  }
  if (isTRUE(resume) && !dir.exists(run_dir)) {
    stop(sprintf(
      "Cannot resume: run directory does not exist for `run_id = '%s'` (%s).",
      run_id, run_dir
    ), call. = FALSE)
  }

  run_canon <- .artifact_ensure_dir(run_dir, "run_dir")

  resolve_sub <- function(override, default_name, label) {
    p <- if (is.null(override)) file.path(run_canon, default_name) else override
    if (!is.character(p) || length(p) != 1L || is.na(p) || !nzchar(trimws(p))) {
      stop("`", label, "` must be a single non-empty directory path.", call. = FALSE)
    }
    if (.cs_is_sync_root_path(p)) {
      stop("`", label, "` must not be inside a cloud-synced directory: ", p, call. = FALSE)
    }
    c_p <- .artifact_ensure_dir(p, label)
    if (.cs_is_sync_root_path(c_p)) {
      stop("Canonical `", label, "` resolves inside a cloud-synced directory: ", c_p, call. = FALSE)
    }
    c_p
  }

  ckpt_canon <- resolve_sub(checkpoint_dir, "checkpoints", "checkpoint_dir")
  temp_canon <- resolve_sub(temp_dir, "temp", "temp_dir")
  diag_canon <- resolve_sub(diagnostics_dir, "diagnostics", "diagnostics_dir")

  # Verify temp_dir and checkpoint_dir share the same volume for atomic replace semantics
  vol_ckpt <- .cs_get_volume_id(ckpt_canon)
  vol_temp <- .cs_get_volume_id(temp_canon)
  if (!identical(vol_ckpt, vol_temp)) {
    stop(sprintf(
      "`checkpoint_dir` (%s, volume %s) and `temp_dir` (%s, volume %s) must reside on the same volume.",
      ckpt_canon, vol_ckpt, temp_canon, vol_temp
    ), call. = FALSE)
  }

  # Verify write permissions & Windows durable atomic replace capability on checkpoint_dir
  cs_check_durable_replace_cpp(ckpt_canon)

  # Verify free disk space
  free_bytes <- .cs_get_free_space_bytes(ckpt_canon)
  if (!is.na(free_bytes) && free_bytes < min_free_bytes) {
    stop(sprintf(
      "Insufficient free disk space in `checkpoint_dir` (%s): %.1f MB free, require at least %.1f MB.",
      ckpt_canon, free_bytes / (1024^2), min_free_bytes / (1024^2)
    ), call. = FALSE)
  }

  # Resolve result_file; reject sync-root or external paths explicitly without silent rewriting
  if (is.null(result_file)) {
    resolved_result <- file.path(ckpt_canon, "classification_stability_result.rds")
  } else {
    if (!is.character(result_file) || length(result_file) != 1L || is.na(result_file) || !nzchar(trimws(result_file))) {
      stop("`result_file` must be NULL or a single non-empty file path.", call. = FALSE)
    }
    if (.cs_is_sync_root_path(result_file)) {
      stop(sprintf(
        "Explicit `result_file` (%s) points inside a cloud-synced directory while `work_dir` (%s) is set. Move `result_file` under `%s` or omit `result_file` to use `%s`.",
        result_file, work_canon, ckpt_canon, file.path(ckpt_canon, "classification_stability_result.rds")
      ), call. = FALSE)
    }
    res_norm <- normalizePath(result_file, winslash = "/", mustWork = FALSE)
    if (!.artifact_is_subpath(res_norm, work_canon) && !.artifact_is_subpath(res_norm, ckpt_canon)) {
      stop(sprintf(
        "Explicit `result_file` (%s) is outside the local work area (%s). Place `result_file` inside `%s` or omit it.",
        res_norm, work_canon, ckpt_canon
      ), call. = FALSE)
    }
    resolved_result <- res_norm
  }

  manifest_file <- file.path(run_canon, "manifest.rds")

  list(
    enabled         = TRUE,
    work_dir        = work_canon,
    run_id          = run_id,
    run_dir         = run_canon,
    checkpoint_dir  = ckpt_canon,
    temp_dir        = temp_canon,
    diagnostics_dir = diag_canon,
    manifest_file   = manifest_file,
    result_file     = resolved_result,
    volume_id       = vol_ckpt,
    free_bytes      = free_bytes
  )
}

#' Return fixed 2-slot rolling checkpoint file paths
#' @keywords internal
#' @noRd
.cs_slot_paths <- function(base_path) {
  c(
    paste0(base_path, ".slot0.rds"),
    paste0(base_path, ".slot1.rds")
  )
}

#' Write one rolling checkpoint slot durably on the same volume
#' @keywords internal
#' @noRd
.cs_save_rolling_slot <- function(base_path, generation, analysis_identity, payload) {
  slots <- .cs_slot_paths(base_path)
  slot_idx <- (as.integer(generation) %% 2L) + 1L
  target_slot <- slots[slot_idx]
  parent_dir <- dirname(target_slot)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
  env_core <- list(
    schema_version    = .CS_STREAM_SCHEMA_VERSION,
    generation        = as.integer(generation),
    analysis_identity = analysis_identity,
    payload           = payload
  )
  env <- c(env_core, list(payload_checksum = .cp_hash_object(env_core)))
  temp_file <- paste0(target_slot, ".tmp.", Sys.getpid())
  saveRDS(env, file = temp_file, compress = FALSE)
  cs_durable_replace_file_cpp(
    normalizePath(temp_file, winslash = "/", mustWork = TRUE),
    normalizePath(target_slot, winslash = "/", mustWork = FALSE)
  )
  invisible(target_slot)
}

#' Load highest valid generation from fixed 2-slot rolling checkpoints
#' @keywords internal
#' @noRd
.cs_load_rolling_slot <- function(base_path, expected_identity) {
  slots <- .cs_slot_paths(base_path)
  exists_mask <- file.exists(slots)
  if (!any(exists_mask)) {
    return(NULL)
  }

  validate_slot <- function(path) {
    env <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(env) || !is.list(env)) {
      return(list(valid = FALSE, reason = "corrupted_rds"))
    }
    if (!identical(env$schema_version, .CS_STREAM_SCHEMA_VERSION)) {
      return(list(valid = FALSE, reason = "schema_mismatch"))
    }
    if (is.null(env$generation) || !is.numeric(env$generation) || length(env$generation) != 1L || is.na(env$generation)) {
      return(list(valid = FALSE, reason = "invalid_generation"))
    }
    env_core <- list(
      schema_version    = env$schema_version,
      generation        = as.integer(env$generation),
      analysis_identity = env$analysis_identity,
      payload           = env$payload
    )
    computed_cs <- .cp_hash_object(env_core)
    if (!identical(env$payload_checksum, computed_cs)) {
      return(list(valid = FALSE, reason = "checksum_mismatch"))
    }
    if (!identical(env$analysis_identity, expected_identity)) {
      return(list(valid = FALSE, reason = "identity_mismatch"))
    }
    list(valid = TRUE, env = env)
  }

  res_list <- lapply(slots[exists_mask], validate_slot)
  # Identity mismatch on a checksum-valid slot must ALWAYS stop immediately
  for (r in res_list) {
    if (!r$valid && identical(r$reason, "identity_mismatch")) {
      stop("Checkpoint analysis identity mismatch; refusing to resume or overwrite.", call. = FALSE)
    }
  }

  valid_res <- Filter(function(r) isTRUE(r$valid), res_list)
  if (length(valid_res) == 0L) {
    stop("All existing rolling checkpoint generations are corrupted or invalid; refusing to fall back to fresh computation.", call. = FALSE)
  }

  gens <- vapply(valid_res, function(r) as.integer(r$env$generation), integer(1))
  best <- valid_res[[which.max(gens)]]$env
  best$recovered_from_previous <- (length(valid_res) < sum(exists_mask))
  best
}

#' Check manifest for work_dir/checkpoint_dir migration mismatches
#' @keywords internal
#' @noRd
.cs_check_manifest_migration <- function(layout, analysis_identity, resume = FALSE, force_recompute = FALSE) {
  if (!layout$enabled || !file.exists(layout$manifest_file)) {
    return(invisible(NULL))
  }
  man <- tryCatch(readRDS(layout$manifest_file), error = function(e) NULL)
  if (is.null(man) || !is.list(man) || !identical(man$schema_version, .CS_MANIFEST_SCHEMA_VERSION)) {
    stop("Run manifest is corrupted or has an unsupported schema: ", layout$manifest_file, call. = FALSE)
  }
  if (!isTRUE(force_recompute) && !identical(man$analysis_identity, analysis_identity)) {
    stop(sprintf(
      "Run manifest (`%s`) analysis identity does not match current settings/data/indices.",
      layout$manifest_file
    ), call. = FALSE)
  }
  if (!identical(man$checkpoint_dir, layout$checkpoint_dir) ||
      !identical(man$result_file, layout$result_file)) {
    old_slots <- .cs_slot_paths(man$result_file)
    new_slots <- .cs_slot_paths(layout$result_file)
    if ((any(file.exists(old_slots)) || file.exists(man$result_file)) &&
        !any(file.exists(new_slots)) && !file.exists(layout$result_file)) {
      stop(sprintf(
        "Previous checkpoint files exist at `%s` (run_id: %s), but none were found at new target (`%s`). Copy the checkpoint generation files explicitly and verify checksums before resuming.",
        man$result_file, man$run_id, layout$result_file
      ), call. = FALSE)
    }
  }
  invisible(man)
}

#' Save or update run manifest
#' @keywords internal
#' @noRd
.cs_write_manifest <- function(layout, analysis_identity, status, extra = list()) {
  if (!layout$enabled) return(invisible(NULL))
  man <- c(
    list(
      schema_version    = .CS_MANIFEST_SCHEMA_VERSION,
      run_id            = layout$run_id,
      run_dir           = layout$run_dir,
      work_dir          = layout$work_dir,
      checkpoint_dir    = layout$checkpoint_dir,
      temp_dir          = layout$temp_dir,
      diagnostics_dir   = layout$diagnostics_dir,
      result_file       = layout$result_file,
      analysis_identity = analysis_identity,
      status            = status,
      updated_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    extra
  )
  temp_man <- paste0(layout$manifest_file, ".tmp.", Sys.getpid())
  saveRDS(man, file = temp_man)
  cs_durable_replace_file_cpp(
    normalizePath(temp_man, winslash = "/", mustWork = TRUE),
    normalizePath(layout$manifest_file, winslash = "/", mustWork = FALSE)
  )
  invisible(layout$manifest_file)
}

#' Map 0-based global candidate ordinal to 1-based item indices
#' @keywords internal
#' @noRd
.cs_ordinal_to_item_indices <- function(ordinal_0, n_cols, model_sizes, size_offsets) {
  n_sizes <- length(model_sizes)
  idx <- findInterval(ordinal_0, size_offsets, rightmost.closed = FALSE)
  if (idx < 1L) idx <- 1L
  if (idx > n_sizes) idx <- n_sizes
  k <- as.integer(model_sizes[idx])
  local_rank <- ordinal_0 - size_offsets[idx]
  cs_unrank_combination_cpp(as.integer(n_cols), k, as.numeric(local_rank))
}

#' Deterministic candidate comparator matching R's exact tie-breaking
#' @keywords internal
#' @noRd
.cs_is_better_candidate_state <- function(cand, best, prefer_fewer_items) {
  if (!isTRUE(cand$has_best)) return(FALSE)
  if (is.null(best) || !isTRUE(best$has_best)) return(TRUE)
  if (cand$best_metric_val != best$best_metric_val) {
    return(cand$best_metric_val > best$best_metric_val)
  }
  if (cand$best_n_items != best$best_n_items) {
    return(if (isTRUE(prefer_fewer_items)) cand$best_n_items < best$best_n_items else cand$best_n_items > best$best_n_items)
  }
  if (cand$best_auc != best$best_auc) {
    return(cand$best_auc > best$best_auc)
  }
  if (cand$best_youden != best$best_youden) {
    return(cand$best_youden > best$best_youden)
  }
  cand$best_ordinal < best$best_ordinal
}

#' Resolve batch size as an exact integer divisor of checkpoint_every_candidates
#' @keywords internal
#' @noRd
.cs_resolve_batch_size <- function(checkpoint_every_candidates, candidate_batch_size = NULL) {
  ckpt_c <- as.numeric(checkpoint_every_candidates)
  if (is.null(candidate_batch_size)) {
    # Choose a bounded batch size that divides checkpoint_every_candidates cleanly
    for (target in c(100000, 50000, 25000, 10000, 5000, 1000, 500, 100, 10, 1)) {
      if (ckpt_c >= target && (ckpt_c %% target) == 0) {
        return(target)
      }
    }
    return(ckpt_c)
  }
  bs <- as.numeric(candidate_batch_size)
  if (is.na(bs) || bs < 1 || floor(bs) != bs) {
    stop("`candidate_batch_size` must be a positive integer.", call. = FALSE)
  }
  if (bs > ckpt_c || (ckpt_c %% bs) != 0) {
    stop("`candidate_batch_size` must be an integer divisor of `checkpoint_every_candidates`.", call. = FALSE)
  }
  bs
}

#' Streamingly evaluate all candidates on one sample with batch-level checkpointing
#' @keywords internal
#' @noRd
.cs_stream_eval_sample <- function(x_sample,
                                   y_sample,
                                   used_items,
                                   model_sizes,
                                   size_offsets,
                                   cutoff_method,
                                   selection_metric,
                                   sensitivity_min,
                                   specificity_min,
                                   prefer_fewer_items,
                                   no_feasible,
                                   eff_threads,
                                   batch_size,
                                   checkpoint_every_candidates,
                                   checkpoint_every_seconds = NULL,
                                   resume_state = NULL,
                                   checkpoint_callback = NULL) {
  if (length(unique(y_sample)) < 2L || sum(y_sample == 1L) < 1L || sum(y_sample == 0L) < 1L) {
    return(list(
      status             = "undefined_score",
      failure_reason     = "single_class_in_sample",
      fallback_used      = FALSE,
      selected_ordinal   = NA_real_,
      selected_item_idx  = integer(0),
      selected_items     = NA_character_,
      selected_n_items   = NA_integer_,
      cutoff             = NA_real_,
      telemetry          = list(wall_ms = 0, barrier_wait_ms = 0, checkpoint_write_ms = 0, candidates_evaluated = 0)
    ))
  }

  n_cols <- ncol(x_sample)
  total_cands <- size_offsets[length(size_offsets)]
  sens_min_c <- if (!is.null(sensitivity_min)) as.numeric(sensitivity_min) else -1.0
  spec_min_c <- if (!is.null(specificity_min)) as.numeric(specificity_min) else -1.0

  phase <- if (!is.null(resume_state)) resume_state$rep_phase else "primary"
  cur_ord <- if (!is.null(resume_state)) as.numeric(resume_state$rep_cursor) else 0
  best_st <- if (!is.null(resume_state)) resume_state$rep_best_state else list(has_best = FALSE, n_feasible = 0)

  total_wall_ms <- 0
  total_barrier_ms <- 0
  total_ckpt_ms <- 0
  total_evals <- 0

  run_pass <- function(active_method, start_ord, init_best) {
    ord <- start_ord
    best <- init_best
    cands_since_ckpt <- 0
    last_ckpt_time <- proc.time()[["elapsed"]]

    while (ord < total_cands) {
      if (isTRUE(check_user_interrupt_cpp())) {
        stop("Classification stability evaluation interrupted by user; latest checkpoint preserved.", call. = FALSE)
      }
      end_ord <- min(ord + batch_size, total_cands)
      res <- cs_eval_candidate_batch_cpp(
        x_sample           = x_sample,
        y_sample           = as.integer(y_sample),
        model_sizes        = as.integer(model_sizes),
        size_offsets       = as.numeric(size_offsets),
        ordinal_start      = ord,
        ordinal_end        = end_ord,
        cutoff_method      = active_method,
        selection_metric   = selection_metric,
        sensitivity_min    = sens_min_c,
        specificity_min    = spec_min_c,
        prefer_fewer_items = isTRUE(prefer_fewer_items),
        num_threads        = as.integer(eff_threads)
      )
      total_evals <<- total_evals + (end_ord - ord)
      total_wall_ms <<- total_wall_ms + res$wall_ms
      w_times <- res$worker_times_ms
      if (length(w_times) > 1L) {
        total_barrier_ms <<- total_barrier_ms + max(0, max(w_times) - min(w_times))
      }

      best$n_feasible <- (if (is.null(best$n_feasible)) 0 else best$n_feasible) + res$n_feasible
      if (.cs_is_better_candidate_state(res, best, prefer_fewer_items)) {
        best$has_best        <- TRUE
        best$best_ordinal    <- res$best_ordinal
        best$best_n_items    <- res$best_n_items
        best$best_metric_val <- res$best_metric_val
        best$best_auc        <- res$best_auc
        best$best_youden     <- res$best_youden
        best$best_cutoff     <- res$best_cutoff
        best$best_sens       <- res$best_sens
        best$best_spec       <- res$best_spec
      }

      cands_since_ckpt <- cands_since_ckpt + (end_ord - ord)
      ord <- end_ord
      now_t <- proc.time()[["elapsed"]]
      time_due <- !is.null(checkpoint_every_seconds) && ((now_t - last_ckpt_time) >= checkpoint_every_seconds)

      if (!is.null(checkpoint_callback) &&
          ord < total_cands &&
          (cands_since_ckpt >= checkpoint_every_candidates || time_due)) {
        t_c0 <- proc.time()[["elapsed"]]
        checkpoint_callback(phase, ord, best)
        t_c1 <- proc.time()[["elapsed"]]
        total_ckpt_ms <<- total_ckpt_ms + (t_c1 - t_c0) * 1000
        cands_since_ckpt <- 0
        last_ckpt_time <- t_c1
      }
    }
    best
  }

  if (identical(phase, "primary")) {
    best_st <- run_pass(cutoff_method, cur_ord, best_st)
    if (!isTRUE(best_st$has_best) && identical(no_feasible, "fallback_youden")) {
      phase <- "fallback_youden"
      cur_ord <- 0
      best_st <- list(has_best = FALSE, n_feasible = 0)
      if (!is.null(checkpoint_callback)) {
        t_c0 <- proc.time()[["elapsed"]]
        checkpoint_callback(phase, cur_ord, best_st)
        total_ckpt_ms <- total_ckpt_ms + (proc.time()[["elapsed"]] - t_c0) * 1000
      }
      best_st <- run_pass("youden", 0, best_st)
    }
  } else {
    best_st <- run_pass("youden", cur_ord, best_st)
  }

  if (!isTRUE(best_st$has_best)) {
    return(list(
      status             = "infeasible_selection",
      failure_reason     = "no_feasible_candidate",
      fallback_used      = FALSE,
      selected_ordinal   = NA_real_,
      selected_item_idx  = integer(0),
      selected_items     = NA_character_,
      selected_n_items   = NA_integer_,
      cutoff             = NA_real_,
      telemetry          = list(
        wall_ms              = total_wall_ms,
        barrier_wait_ms      = total_barrier_ms,
        checkpoint_write_ms  = total_ckpt_ms,
        candidates_evaluated = total_evals
      )
    ))
  }

  sel_idx <- .cs_ordinal_to_item_indices(best_st$best_ordinal, n_cols, model_sizes, size_offsets)
  sel_str <- paste(used_items[sel_idx], collapse = " + ")
  fb_used <- identical(phase, "fallback_youden")

  list(
    status             = if (fb_used) "infeasible_selection" else "valid",
    failure_reason     = if (fb_used) "no_feasible_candidate_fallback_youden" else NA_character_,
    fallback_used      = fb_used,
    selected_ordinal   = best_st$best_ordinal,
    selected_item_idx  = sel_idx,
    selected_items     = sel_str,
    selected_n_items   = as.integer(best_st$best_n_items),
    cutoff             = best_st$best_cutoff,
    telemetry          = list(
      wall_ms              = total_wall_ms,
      barrier_wait_ms      = total_barrier_ms,
      checkpoint_write_ms  = total_ckpt_ms,
      candidates_evaluated = total_evals
    )
  )
}

#' Publish completed classification stability result to a destination directory
#' (e.g., Google Drive) with same-volume atomic rename, re-read verification,
#' and SHA-256 checksum recording.
#'
#' @param result A `"classification_stability_result"` object.
#' @param publish_dir Character(1) destination directory for final published outputs.
#' @param prefix Character(1) filename prefix (default `"classification_stability"`).
#' @param overwrite Logical(1) whether to overwrite existing files in `publish_dir`
#'   (default `FALSE`, which stops on collision).
#'
#' @return A list containing published file paths, SHA-256 checksums, and
#'   `verification_status = "verified"`.
#' @export
ncvroc_publish_classification_stability <- function(result,
                                                    publish_dir,
                                                    prefix = "classification_stability",
                                                    overwrite = FALSE) {
  if (!inherits(result, "classification_stability_result")) {
    stop("`result` must be a `classification_stability_result` object.", call. = FALSE)
  }
  if (!is.character(publish_dir) || length(publish_dir) != 1L || is.na(publish_dir) || !nzchar(trimws(publish_dir))) {
    stop("`publish_dir` must be a single non-empty directory path.", call. = FALSE)
  }
  pub_canon <- .artifact_ensure_dir(publish_dir, "publish_dir")

  rds_target <- file.path(pub_canon, paste0(prefix, ".rds"))
  csv_rep_target <- file.path(pub_canon, paste0(prefix, "_replicates.csv"))
  csv_subj_target <- file.path(pub_canon, paste0(prefix, "_subjects.csv"))
  targets <- c(rds_target, csv_rep_target, csv_subj_target)

  if (!isTRUE(overwrite) && any(file.exists(targets))) {
    existing <- targets[file.exists(targets)]
    stop("Refusing to overwrite existing published output(s): ",
         paste(existing, collapse = ", "), call. = FALSE)
  }

  atomic_publish <- function(write_fn, target_path) {
    tmp_path <- paste0(target_path, ".tmp.", Sys.getpid())
    write_fn(tmp_path)
    if (file.exists(target_path) && isTRUE(overwrite)) {
      unlink(target_path, force = TRUE)
    }
    ok <- file.rename(tmp_path, target_path)
    if (!isTRUE(ok) || !file.exists(target_path)) {
      stop("Failed atomic publish rename to: ", target_path, call. = FALSE)
    }
    .cp_hash_file(target_path)
  }

  rds_sha <- atomic_publish(function(p) saveRDS(result, file = p), rds_target)
  rep_sha <- atomic_publish(function(p) utils::write.csv(result$replicate_summary, file = p, row.names = FALSE), csv_rep_target)
  subj_sha <- atomic_publish(function(p) utils::write.csv(result$subject_summary, file = p, row.names = FALSE), csv_subj_target)

  # Re-read & verify structure and checksums
  reread_rds <- readRDS(rds_target)
  if (!inherits(reread_rds, "classification_stability_result") ||
      !identical(reread_rds$provenance$bootstrap_indices_digest, result$provenance$bootstrap_indices_digest)) {
    stop("Published RDS verification failed after re-read: ", rds_target, call. = FALSE)
  }
  reread_rep <- utils::read.csv(csv_rep_target, stringsAsFactors = FALSE)
  reread_subj <- utils::read.csv(csv_subj_target, stringsAsFactors = FALSE)
  if (nrow(reread_rep) != nrow(result$replicate_summary) ||
      nrow(reread_subj) != nrow(result$subject_summary)) {
    stop("Published CSV row count verification failed after re-read.", call. = FALSE)
  }

  if (!identical(.cp_hash_file(rds_target), rds_sha) ||
      !identical(.cp_hash_file(csv_rep_target), rep_sha) ||
      !identical(.cp_hash_file(csv_subj_target), subj_sha)) {
    stop("Published file SHA-256 checksum verification failed.", call. = FALSE)
  }

  pub_info <- list(
    publish_dir = pub_canon,
    files = list(
      rds        = list(path = rds_target, sha256 = rds_sha),
      replicates = list(path = csv_rep_target, sha256 = rep_sha),
      subjects   = list(path = csv_subj_target, sha256 = subj_sha)
    ),
    verification_status = "verified",
    verified_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  # Update local run manifest if present (never delete local work_dir!)
  man_path <- result$provenance$work_layout$manifest_file
  if (!is.null(man_path) && file.exists(man_path)) {
    man <- readRDS(man_path)
    man$publish <- pub_info
    temp_man <- paste0(man_path, ".tmp.", Sys.getpid())
    saveRDS(man, file = temp_man)
    cs_durable_replace_file_cpp(
      normalizePath(temp_man, winslash = "/", mustWork = TRUE),
      normalizePath(man_path, winslash = "/", mustWork = FALSE)
    )
  }

  pub_info
}
