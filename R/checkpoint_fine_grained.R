# checkpoint_fine_grained.R -- Fine-grained model-size and candidate-batch checkpointing
#
# Extends R/checkpoint.R (v0.28 canonical outer-fold checkpoint contract) with:
#   - model_size (unit_schema_version = 1L)
#   - candidate_batch (unit_schema_version = 1L)
#   - inner_fold explicitly deferred (not justified by R1 contract)
#
# All three granularities ("outer_fold", "model_size", "candidate_batch") are
# backend-neutral and portable between R serial (parallel = "none") and
# Scope B Native (parallel = "native_outer_fold") with tuning = "off".

.CP_FINE_GRANULARITIES                     <- c("outer_fold", "model_size", "candidate_batch")
.CP_ENUM_FORMAT_ID                         <- "NCVROC_candidate_enum_v1"
.CP_ENUM_SCHEMA_VERSION                    <- 1L
.CP_UNIT_MODEL_SIZE_FORMAT_ID              <- "NCVROC_nested_sum_roc_model_size_checkpoint"
.CP_UNIT_MODEL_SIZE_SCHEMA_VERSION         <- 1L
.CP_UNIT_CANDIDATE_BATCH_FORMAT_ID         <- "NCVROC_nested_sum_roc_candidate_batch_checkpoint"
.CP_UNIT_CANDIDATE_BATCH_SCHEMA_VERSION    <- 1L
.CP_DEFAULT_CANDIDATE_BATCH_SIZE           <- 5000L

# ---- Validation & configuration helpers ----

.cp_validate_granularity <- function(checkpoint_granularity) {
  if (!is.character(checkpoint_granularity) || length(checkpoint_granularity) != 1L ||
      is.na(checkpoint_granularity) || !nzchar(checkpoint_granularity)) {
    stop("`checkpoint_granularity` must be a single non-empty character string.", call. = FALSE)
  }
  if (!checkpoint_granularity %in% .CP_FINE_GRANULARITIES) {
    stop(
      sprintf(
        "`checkpoint_granularity` must be one of: %s (got '%s').",
        paste(sprintf("'%s'", .CP_FINE_GRANULARITIES), collapse = ", "),
        checkpoint_granularity
      ),
      call. = FALSE
    )
  }
  checkpoint_granularity
}

.cp_resolve_candidate_batch_size <- function() {
  bs <- .ncvroc_checkpoint_test_seams$batch_size_override
  if (is.null(bs)) {
    bs <- getOption("ncvroc.checkpoint_batch_size", .CP_DEFAULT_CANDIDATE_BATCH_SIZE)
  }
  if (!is.numeric(bs) || length(bs) != 1L || is.na(bs) || !is.finite(bs) ||
      bs < 1 || floor(bs) != bs) {
    stop("Internal candidate batch size must be a positive integer.", call. = FALSE)
  }
  as.integer(bs)
}

# ---- Deterministic candidate enumeration contract (NCVROC_candidate_enum_v1) ----

.cp_build_enumeration_descriptor <- function(items, min_items, max_items) {
  items <- as.character(items)
  n_items <- length(items)
  min_items <- as.integer(min_items)
  max_items <- as.integer(max_items)
  model_sizes <- seq.int(min_items, max_items)
  level_sizes <- vapply(model_sizes, function(k) as.numeric(choose(n_items, k)), numeric(1L))
  names(level_sizes) <- as.character(model_sizes)
  level_starts_0based <- c(0, cumsum(level_sizes)[-length(level_sizes)])
  names(level_starts_0based) <- as.character(model_sizes)
  total_combos <- sum(level_sizes)

  core <- list(
    enum_format         = .CP_ENUM_FORMAT_ID,
    enum_schema_version = .CP_ENUM_SCHEMA_VERSION,
    items               = items,
    min_items           = min_items,
    max_items           = max_items,
    level_sizes         = unname(level_sizes)
  )
  list(
    enum_format         = .CP_ENUM_FORMAT_ID,
    enum_schema_version = .CP_ENUM_SCHEMA_VERSION,
    enumeration_id      = .cp_hash_object(core),
    n_items             = n_items,
    items               = items,
    min_items           = min_items,
    max_items           = max_items,
    model_sizes         = model_sizes,
    level_sizes         = level_sizes,
    level_starts_0based = level_starts_0based,
    total_combos        = total_combos
  )
}

.cp_unrank_combination <- function(n_items, k, local_rank_1based) {
  n_items <- as.integer(n_items)
  k <- as.integer(k)
  r <- as.numeric(local_rank_1based) - 1
  if (is.na(r) || !is.finite(r) || r < 0 || r >= choose(n_items, k) || floor(r) != r) {
    stop("local_rank_1based out of bounds for unranking.", call. = FALSE)
  }
  out <- integer(k)
  next_idx <- 1L
  for (i in seq_len(k)) {
    rem <- k - i
    for (c_idx in seq.int(next_idx, n_items - rem)) {
      cnt <- choose(n_items - c_idx, rem)
      if (r < cnt) {
        out[i] <- c_idx
        next_idx <- c_idx + 1L
        break
      }
      r <- r - cnt
    }
  }
  out
}

.cp_materialize_items_from_local_rank <- function(items, k, local_rank_1based) {
  idx <- .cp_unrank_combination(length(items), k, local_rank_1based)
  paste(items[idx], collapse = ", ")
}

.cp_build_batch_ranges <- function(total_k, batch_size) {
  total_k <- as.numeric(total_k)
  batch_size <- as.numeric(batch_size)
  if (total_k < 1) {
    return(list())
  }
  starts <- seq(1, total_k, by = batch_size)
  lapply(starts, function(st) {
    en <- min(st + batch_size - 1, total_k)
    list(
      rank_start     = as.numeric(st),
      rank_end       = as.numeric(en),
      expected_count = as.numeric(en - st + 1)
    )
  })
}

# ---- Exact strict-total-order candidate sorting and bounded merging ----

.cp_sort_candidates <- function(df, rank_by) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }
  if (anyDuplicated(df$global_rank) > 0L) {
    stop("Duplicate global_rank detected in candidate payload.", call. = FALSE)
  }
  ord <- order(
    -df[[rank_by]],
    -df$youden,
    -df$sensitivity,
    -df$specificity,
    df$n_items,
    df$global_rank,
    na.last = TRUE
  )
  out <- df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cp_merge_top_candidates <- function(df_list, preselect_top_n, rank_by) {
  non_empty <- Filter(function(d) !is.null(d) && nrow(d) > 0L, df_list)
  if (length(non_empty) == 0L) {
    stop("No candidates available to merge.", call. = FALSE)
  }
  combined <- do.call(rbind, non_empty)
  sorted <- .cp_sort_candidates(combined, rank_by)
  keep_n <- min(nrow(sorted), as.integer(preselect_top_n))
  out <- sorted[seq_len(keep_n), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cp_empty_candidates_df <- function() {
  data.frame(
    global_rank = numeric(0L),
    local_rank  = numeric(0L),
    n_items     = integer(0L),
    items       = character(0L),
    auc         = numeric(0L),
    cutoff      = numeric(0L),
    sensitivity = numeric(0L),
    specificity = numeric(0L),
    youden      = numeric(0L),
    accuracy    = numeric(0L),
    ppv         = numeric(0L),
    npv         = numeric(0L),
    stringsAsFactors = FALSE
  )
}

# ---- Hierarchical path helpers (contained under canonical root) ----

.cp_assert_contained_nested <- function(root_canonical, target_path) {
  cur <- target_path
  while (!dir.exists(dirname(cur))) {
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Cannot resolve existing ancestor for checkpoint path: ", target_path, call. = FALSE)
    }
    if (file.exists(parent) && !dir.exists(parent)) {
      stop("Checkpoint parent path exists as a non-directory file: ", parent, call. = FALSE)
    }
    cur <- parent
  }
  .cp_assert_contained(root_canonical, cur, must_exist = FALSE)
}

.cp_units_dir <- function(paths) {
  d <- file.path(paths$root_canonical, "units")
  .cp_assert_contained(paths$root_canonical, d, must_exist = FALSE)
  d
}

.cp_fold_subdir <- function(paths, repeat_id, outer_fold_id) {
  p <- file.path(
    .cp_units_dir(paths),
    sprintf("repeat_%03d_fold_%03d", as.integer(repeat_id), as.integer(outer_fold_id))
  )
  .cp_assert_contained_nested(paths$root_canonical, p)
  p
}

.cp_model_size_path <- function(paths, repeat_id, outer_fold_id, model_size) {
  p <- file.path(
    .cp_fold_subdir(paths, repeat_id, outer_fold_id),
    sprintf("model_size_%03d.rds", as.integer(model_size))
  )
  .cp_assert_contained_nested(paths$root_canonical, p)
  p
}

.cp_batch_subdir <- function(paths, repeat_id, outer_fold_id, model_size) {
  p <- file.path(
    .cp_fold_subdir(paths, repeat_id, outer_fold_id),
    sprintf("model_size_%03d", as.integer(model_size))
  )
  .cp_assert_contained_nested(paths$root_canonical, p)
  p
}

.cp_batch_path <- function(paths, repeat_id, outer_fold_id, model_size, rank_start, rank_end) {
  p <- file.path(
    .cp_batch_subdir(paths, repeat_id, outer_fold_id, model_size),
    sprintf("batch_%012.0f_%012.0f.rds", as.numeric(rank_start), as.numeric(rank_end))
  )
  .cp_assert_contained_nested(paths$root_canonical, p)
  p
}

# ---- Unit IDs & canonical payload validation ----

.cp_make_unit_id <- function(unit_type, repeat_id, outer_fold_id, model_size,
                             rank_start = NULL, rank_end = NULL) {
  if (identical(unit_type, "model_size")) {
    sprintf("r%03d_f%03d_k%03d", as.integer(repeat_id), as.integer(outer_fold_id), as.integer(model_size))
  } else if (identical(unit_type, "candidate_batch")) {
    sprintf("r%03d_f%03d_k%03d_b%.0f_%.0f",
            as.integer(repeat_id), as.integer(outer_fold_id), as.integer(model_size),
            as.numeric(rank_start), as.numeric(rank_end))
  } else {
    sprintf("r%03d_f%03d", as.integer(repeat_id), as.integer(outer_fold_id))
  }
}

.cp_validate_candidates_table <- function(df, enum_desc, model_size, rank_start, rank_end,
                                          preselect_top_n, rank_by) {
  if (!is.data.frame(df)) {
    return("top_k_candidates must be a data.frame.")
  }
  req_cols <- c("global_rank", "local_rank", "n_items", "items",
                "auc", "cutoff", "sensitivity", "specificity", "youden",
                "accuracy", "ppv", "npv")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0L) {
    return(paste("top_k_candidates missing required columns:", paste(miss, collapse = ", ")))
  }
  expected_count <- as.numeric(rank_end - rank_start + 1)
  max_rows <- min(as.integer(preselect_top_n), expected_count)
  if (nrow(df) < 1L || nrow(df) > max_rows) {
    return(sprintf("top_k_candidates row count (%d) out of expected bounds [1, %d].",
                   nrow(df), max_rows))
  }
  if (!is.numeric(df$global_rank) || !is.numeric(df$local_rank) ||
      anyNA(df$global_rank) || anyNA(df$local_rank) ||
      any(!is.finite(df$global_rank)) || any(!is.finite(df$local_rank))) {
    return("top_k_candidates contains invalid global_rank or local_rank.")
  }
  if (!is.integer(df$n_items) || anyNA(df$n_items) || any(df$n_items != as.integer(model_size))) {
    return("top_k_candidates contains invalid n_items.")
  }
  if (any(df$local_rank < rank_start) || any(df$local_rank > rank_end) ||
      any(floor(df$local_rank) != df$local_rank)) {
    return("top_k_candidates local_rank outside covered interval.")
  }
  level_start <- enum_desc$level_starts_0based[[as.character(model_size)]]
  expected_global <- level_start + df$local_rank
  if (any(df$global_rank != expected_global)) {
    return("top_k_candidates global_rank does not match local_rank + level_start.")
  }
  if (anyDuplicated(df$global_rank) > 0L) {
    return("top_k_candidates contains duplicate global_rank.")
  }
  if (!is.character(df$items) || anyNA(df$items) || any(!nzchar(df$items))) {
    return("top_k_candidates contains invalid items strings.")
  }
  for (idx in seq_len(nrow(df))) {
    expected_items <- .cp_materialize_items_from_local_rank(
      enum_desc$items, model_size, df$local_rank[idx]
    )
    if (!identical(df$items[idx], expected_items)) {
      return(sprintf("top_k_candidates row %d items mismatch ('%s' vs '%s').",
                     idx, df$items[idx], expected_items))
    }
  }
  num_cols <- c("auc", "cutoff", "sensitivity", "specificity", "youden", "accuracy", "ppv", "npv")
  for (col in num_cols) {
    v <- df[[col]]
    if (!is.numeric(v) || any(is.nan(v)) || any(is.infinite(v))) {
      return(sprintf("top_k_candidates column '%s' contains NaN or Inf.", col))
    }
  }
  sorted_df <- .cp_sort_candidates(df, rank_by)
  if (!identical(as.numeric(df$global_rank), as.numeric(sorted_df$global_rank))) {
    return("top_k_candidates is not in canonical sorted order.")
  }
  NULL
}

.cp_validate_unit_envelope <- function(env,
                                       unit_type,
                                       identity_record,
                                       enum_desc,
                                       fold_spec,
                                       split_hash,
                                       model_size,
                                       preselect_top_n,
                                       rank_by,
                                       expected_rank_start = NULL,
                                       expected_rank_end = NULL,
                                       paths = NULL,
                                       validate_children = TRUE) {
  if (!is.list(env)) {
    return("Unit checkpoint envelope is not a list.")
  }
  if (!identical(env$unit_type, unit_type)) {
    return(sprintf("Unit type mismatch (expected '%s', got '%s').",
                   unit_type, as.character(env$unit_type)))
  }
  if (identical(unit_type, "candidate_batch")) {
    if (!identical(env$format_id, .CP_UNIT_CANDIDATE_BATCH_FORMAT_ID) ||
        !identical(env$unit_schema_version, .CP_UNIT_CANDIDATE_BATCH_SCHEMA_VERSION)) {
      return("Unsupported candidate_batch format_id or unit_schema_version.")
    }
  } else if (identical(unit_type, "model_size")) {
    if (!identical(env$format_id, .CP_UNIT_MODEL_SIZE_FORMAT_ID) ||
        !identical(env$unit_schema_version, .CP_UNIT_MODEL_SIZE_SCHEMA_VERSION)) {
      return("Unsupported model_size format_id or unit_schema_version.")
    }
  } else {
    return(paste("Unknown unit_type:", unit_type))
  }

  if (!identical(env$completion_status, "COMPLETE")) {
    return(sprintf("Unit completion_status is '%s' (only 'COMPLETE' is reusable).",
                   as.character(env$completion_status)))
  }
  if (!identical(env$analysis_identity_hash, identity_record$analysis_identity_hash)) {
    return("Unit analysis_identity_hash mismatch.")
  }
  if (!identical(env$enumeration_id, enum_desc$enumeration_id) ||
      !identical(env$enum_schema_version, .CP_ENUM_SCHEMA_VERSION)) {
    return("Unit enumeration_id or enum_schema_version mismatch.")
  }
  if (!identical(as.integer(env$repeat_id), as.integer(fold_spec$repeat_id)) ||
      !identical(as.integer(env$outer_fold_id), as.integer(fold_spec$outer_fold_id)) ||
      !identical(env$split_hash, split_hash)) {
    return("Unit repeat_id / outer_fold_id / split_hash mismatch.")
  }
  if (!identical(as.integer(env$model_size), as.integer(model_size))) {
    return("Unit model_size mismatch.")
  }
  if (!is.character(env$producer_backend) || length(env$producer_backend) != 1L ||
      !env$producer_backend %in% .CHECKPOINT_SUPPORTED_PRODUCERS) {
    return("Unsupported or missing unit producer_backend.")
  }
  if (is.null(env$payload) || !is.character(env$payload_hash) ||
      !identical(.cp_hash_object(env$payload), env$payload_hash)) {
    return("Unit payload integrity hash mismatch.")
  }

  level_size <- enum_desc$level_sizes[[as.character(model_size)]]
  if (identical(unit_type, "candidate_batch")) {
    rs <- env$rank_start
    re <- env$rank_end
    if (!is.numeric(rs) || !is.numeric(re) || length(rs) != 1L || length(re) != 1L ||
        is.na(rs) || is.na(re) || rs < 1 || re < rs || re > level_size) {
      return("candidate_batch rank_start/rank_end invalid.")
    }
    if (!is.null(expected_rank_start) && rs != expected_rank_start) {
      return("candidate_batch rank_start does not match expected batch range.")
    }
    if (!is.null(expected_rank_end) && re != expected_rank_end) {
      return("candidate_batch rank_end does not match expected batch range.")
    }
    expected_cnt <- re - rs + 1
    if (!identical(as.numeric(env$expected_count), expected_cnt) ||
        !identical(as.numeric(env$completed_count), expected_cnt)) {
      return("candidate_batch expected_count/completed_count mismatch.")
    }
    expected_uid <- .cp_make_unit_id("candidate_batch", fold_spec$repeat_id, fold_spec$outer_fold_id,
                                     model_size, rs, re)
    if (!identical(env$unit_id, expected_uid)) {
      return("candidate_batch unit_id mismatch.")
    }
    cand_err <- .cp_validate_candidates_table(
      env$payload$top_k_candidates, enum_desc, model_size, rs, re, preselect_top_n, rank_by
    )
    if (!is.null(cand_err)) {
      return(cand_err)
    }
  } else {
    if (!identical(as.numeric(env$total_candidates), level_size) ||
        !identical(as.numeric(env$completed_count), level_size)) {
      return("model_size total_candidates/completed_count mismatch.")
    }
    expected_uid <- .cp_make_unit_id("model_size", fold_spec$repeat_id, fold_spec$outer_fold_id, model_size)
    if (!identical(env$unit_id, expected_uid)) {
      return("model_size unit_id mismatch.")
    }
    cand_err <- .cp_validate_candidates_table(
      env$payload$top_k_candidates, enum_desc, model_size, 1, level_size, preselect_top_n, rank_by
    )
    if (!is.null(cand_err)) {
      return(cand_err)
    }
    if (!is.character(env$dependency_mode) || length(env$dependency_mode) != 1L ||
        !env$dependency_mode %in% c("direct_model_size", "promoted_from_batches")) {
      return("model_size missing or invalid dependency_mode.")
    }
    expected_dep_hash <- .cp_hash_object(list(
      dependency_mode    = env$dependency_mode,
      child_dependencies = env$child_dependencies
    ))
    if (!is.character(env$dependency_hash) || length(env$dependency_hash) != 1L ||
        !identical(env$dependency_hash, expected_dep_hash)) {
      return("model_size dependency_hash mismatch.")
    }
    if (identical(env$dependency_mode, "direct_model_size")) {
      if (!is.list(env$child_dependencies) || length(env$child_dependencies) != 0L) {
        return("direct_model_size unit must have empty child_dependencies.")
      }
      if (!is.null(paths)) {
        b_dir <- .cp_batch_subdir(paths, fold_spec$repeat_id, fold_spec$outer_fold_id, model_size)
        if (dir.exists(b_dir) && length(list.files(b_dir, pattern = "^batch_.*\\.rds$")) > 0L) {
          return("direct_model_size unit conflicts with existing candidate_batch files.")
        }
      }
    } else {
      if (!is.list(env$child_dependencies) || length(env$child_dependencies) < 1L) {
        return("promoted_from_batches model_size unit requires non-empty child_dependencies.")
      }
      if (isTRUE(validate_children)) {
        if (is.null(paths)) {
          return("Internal error: paths required to validate model_size child_dependencies.")
        }
        deps <- env$child_dependencies
        starts <- vapply(deps, function(d) as.numeric(d$rank_start), numeric(1L))
        ends <- vapply(deps, function(d) as.numeric(d$rank_end), numeric(1L))
        ord <- order(starts, ends)
        deps <- deps[ord]
        starts <- starts[ord]
        ends <- ends[ord]
        if (starts[1L] != 1 || ends[length(ends)] != level_size) {
          return("model_size child_dependencies do not cover [1, total_candidates].")
        }
        if (length(deps) > 1L) {
          for (j in seq.int(2L, length(deps))) {
            if (starts[j] != ends[j - 1L] + 1) {
              return("model_size child_dependencies contain a gap or overlap.")
            }
          }
        }
        for (d in deps) {
          c_path <- .cp_batch_path(paths, fold_spec$repeat_id, fold_spec$outer_fold_id,
                                   model_size, d$rank_start, d$rank_end)
          if (!file.exists(c_path)) {
            return(sprintf("Missing child candidate_batch dependency: %s", c_path))
          }
          c_env <- tryCatch(readRDS(c_path), error = function(e) NULL)
          if (is.null(c_env)) {
            return(sprintf("Unreadable child candidate_batch dependency: %s", c_path))
          }
          c_err <- .cp_validate_unit_envelope(
            env = c_env,
            unit_type = "candidate_batch",
            identity_record = identity_record,
            enum_desc = enum_desc,
            fold_spec = fold_spec,
            split_hash = split_hash,
            model_size = model_size,
            preselect_top_n = preselect_top_n,
            rank_by = rank_by,
            expected_rank_start = d$rank_start,
            expected_rank_end = d$rank_end
          )
          if (!is.null(c_err)) {
            return(paste("Invalid child candidate_batch dependency:", c_err))
          }
          if (!identical(c_env$unit_id, d$unit_id) ||
              !identical(c_env$payload_hash, d$payload_hash)) {
            return("Stale or hash-mismatched child candidate_batch dependency.")
          }
        }
      }
    }
  }
  NULL
}

# ---- Envelope builders & atomic unit publication ----

.cp_build_batch_envelope <- function(identity_record,
                                     enum_desc,
                                     fold_spec,
                                     split_hash,
                                     model_size,
                                     rank_start,
                                     rank_end,
                                     batch_size_recorded,
                                     top_k_candidates,
                                     producer_backend) {
  uid <- .cp_make_unit_id("candidate_batch", fold_spec$repeat_id, fold_spec$outer_fold_id,
                          model_size, rank_start, rank_end)
  parent_uid <- .cp_make_unit_id("model_size", fold_spec$repeat_id, fold_spec$outer_fold_id, model_size)
  cnt <- as.numeric(rank_end - rank_start + 1)
  payload <- list(top_k_candidates = top_k_candidates)
  list(
    format_id              = .CP_UNIT_CANDIDATE_BATCH_FORMAT_ID,
    unit_type              = "candidate_batch",
    unit_schema_version    = .CP_UNIT_CANDIDATE_BATCH_SCHEMA_VERSION,
    completion_status      = "COMPLETE",
    analysis_identity_hash = identity_record$analysis_identity_hash,
    enumeration_id         = enum_desc$enumeration_id,
    enum_schema_version    = .CP_ENUM_SCHEMA_VERSION,
    repeat_id              = as.integer(fold_spec$repeat_id),
    outer_fold_id          = as.integer(fold_spec$outer_fold_id),
    fold_name              = as.character(fold_spec$fold_name),
    split_hash             = split_hash,
    model_size             = as.integer(model_size),
    parent_id              = parent_uid,
    unit_id                = uid,
    rank_start             = as.numeric(rank_start),
    rank_end               = as.numeric(rank_end),
    expected_count         = cnt,
    completed_count        = cnt,
    batch_size_recorded    = as.integer(batch_size_recorded),
    producer_backend       = as.character(producer_backend),
    payload_hash           = .cp_hash_object(payload),
    payload                = payload,
    created_at             = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  )
}

.cp_build_model_size_envelope <- function(identity_record,
                                          enum_desc,
                                          fold_spec,
                                          split_hash,
                                          model_size,
                                          top_k_candidates,
                                          producer_backend,
                                          dependency_mode = "direct_model_size",
                                          child_dependencies = list()) {
  uid <- .cp_make_unit_id("model_size", fold_spec$repeat_id, fold_spec$outer_fold_id, model_size)
  parent_uid <- .cp_make_unit_id("outer_fold", fold_spec$repeat_id, fold_spec$outer_fold_id, model_size)
  total_k <- enum_desc$level_sizes[[as.character(model_size)]]
  payload <- list(top_k_candidates = top_k_candidates)
  dep_hash <- .cp_hash_object(list(
    dependency_mode    = dependency_mode,
    child_dependencies = child_dependencies
  ))
  list(
    format_id              = .CP_UNIT_MODEL_SIZE_FORMAT_ID,
    unit_type              = "model_size",
    unit_schema_version    = .CP_UNIT_MODEL_SIZE_SCHEMA_VERSION,
    completion_status      = "COMPLETE",
    analysis_identity_hash = identity_record$analysis_identity_hash,
    enumeration_id         = enum_desc$enumeration_id,
    enum_schema_version    = .CP_ENUM_SCHEMA_VERSION,
    repeat_id              = as.integer(fold_spec$repeat_id),
    outer_fold_id          = as.integer(fold_spec$outer_fold_id),
    fold_name              = as.character(fold_spec$fold_name),
    split_hash             = split_hash,
    model_size             = as.integer(model_size),
    parent_id              = parent_uid,
    unit_id                = uid,
    total_candidates       = total_k,
    completed_count        = total_k,
    dependency_mode        = dependency_mode,
    dependency_hash        = dep_hash,
    child_dependencies     = child_dependencies,
    producer_backend       = as.character(producer_backend),
    payload_hash           = .cp_hash_object(payload),
    payload                = payload,
    created_at             = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  )
}

.cp_publish_unit <- function(paths,
                             target_path,
                             env,
                             unit_type,
                             identity_record,
                             enum_desc,
                             fold_spec,
                             split_hash,
                             model_size,
                             preselect_top_n,
                             rank_by,
                             expected_rank_start = NULL,
                             expected_rank_end = NULL,
                             allow_replace = FALSE) {
  uid <- env$unit_id
  if (identical(unit_type, "candidate_batch") &&
      (identical(.ncvroc_checkpoint_test_seams$fail_during_batch_write, uid) ||
       isTRUE(.ncvroc_checkpoint_test_seams$fail_during_batch_write))) {
    stop("Simulated failure during candidate_batch checkpoint write for ", uid, call. = FALSE)
  }
  if (identical(unit_type, "model_size") &&
      (identical(.ncvroc_checkpoint_test_seams$fail_during_model_size_write, uid) ||
       isTRUE(.ncvroc_checkpoint_test_seams$fail_during_model_size_write))) {
    stop("Simulated failure during model_size checkpoint write for ", uid, call. = FALSE)
  }

  .cp_assert_contained_nested(paths$root_canonical, target_path)
  target_dir <- dirname(target_path)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  .cp_assert_contained(paths$root_canonical, target_dir, must_exist = TRUE)
  .cp_assert_contained(paths$root_canonical, target_path, must_exist = FALSE)

  nonce <- .cp_next_nonce()
  pending_path <- file.path(paths$pending_dir, sprintf("%s_%s_%s.rds.tmp", unit_type, uid, nonce))
  .cp_assert_contained(paths$root_canonical, pending_path, must_exist = FALSE)

  if (identical(.ncvroc_checkpoint_test_seams$fail_during_pending_write, uid) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_during_pending_write)) {
    writeBin(as.raw(c(0x01, 0x02, 0x03)), pending_path)
    stop("Simulated failure during pending unit checkpoint write for ", uid, call. = FALSE)
  }

  saveRDS(env, pending_path, compress = FALSE)

  if (identical(.ncvroc_checkpoint_test_seams$fail_during_readback, uid) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_during_readback)) {
    stop("Simulated failure during unit read-back validation for ", uid, call. = FALSE)
  }

  rb_env <- tryCatch(readRDS(pending_path), error = function(e) {
    stop("Read-back failed for staged unit ", pending_path, ": ", conditionMessage(e), call. = FALSE)
  })
  rb_err <- .cp_validate_unit_envelope(
    env = rb_env,
    unit_type = unit_type,
    identity_record = identity_record,
    enum_desc = enum_desc,
    fold_spec = fold_spec,
    split_hash = split_hash,
    model_size = model_size,
    preselect_top_n = preselect_top_n,
    rank_by = rank_by,
    expected_rank_start = expected_rank_start,
    expected_rank_end = expected_rank_end,
    paths = paths,
    validate_children = TRUE
  )
  if (!is.null(rb_err)) {
    stop("Read-back validation failed for staged unit ", pending_path, ": ", rb_err, call. = FALSE)
  }

  if (identical(.ncvroc_checkpoint_test_seams$fail_before_publish, uid) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_before_publish)) {
    stop("Simulated failure before unit publication for ", uid, call. = FALSE)
  }

  if (file.exists(target_path)) {
    if (!isTRUE(allow_replace)) {
      stop(sprintf("Refusing to overwrite existing %s checkpoint (%s) without force_recompute = TRUE.",
                   unit_type, target_path), call. = FALSE)
    }
    backup_path <- file.path(paths$pending_dir, sprintf("backup_%s_%s_%s.rds", unit_type, uid, nonce))
    .cp_assert_contained(paths$root_canonical, backup_path, must_exist = FALSE)
    if (!isTRUE(file.rename(target_path, backup_path)) || !file.exists(backup_path)) {
      stop("Failed to stage backup of prior unit checkpoint: ", target_path, call. = FALSE)
    }
    pub_ok <- FALSE
    if (!(identical(.ncvroc_checkpoint_test_seams$fail_during_replace, uid) ||
          isTRUE(.ncvroc_checkpoint_test_seams$fail_during_replace))) {
      pub_ok <- isTRUE(file.rename(pending_path, target_path)) && file.exists(target_path)
    }
    if (!pub_ok) {
      file.rename(backup_path, target_path)
      stop("Failed to replace unit checkpoint ", target_path, "; prior valid checkpoint preserved.", call. = FALSE)
    }
    unlink(backup_path)
  } else {
    pub_ok <- isTRUE(file.rename(pending_path, target_path)) && file.exists(target_path)
    if (!pub_ok) {
      stop("Failed to publish unit checkpoint to ", target_path, call. = FALSE)
    }
  }

  if (identical(.ncvroc_checkpoint_test_seams$fail_after_publish, uid) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_after_publish)) {
    stop("Simulated failure after unit publication for ", uid, call. = FALSE)
  }
  invisible(target_path)
}

# ---- Discovery & planning for incomplete folds ----

.cp_inspect_fold_fine_grained <- function(paths,
                                          identity_record,
                                          enum_desc,
                                          fold_spec,
                                          split_hash,
                                          preselect_top_n,
                                          rank_by,
                                          checkpoint_granularity,
                                          batch_size,
                                          resume,
                                          force_recompute) {
  model_sizes <- enum_desc$model_sizes
  reused_model_sizes <- list()
  reused_batches <- list()
  missing_tasks <- list()

  for (k in model_sizes) {
    k_key <- as.character(k)
    total_k <- enum_desc$level_sizes[[k_key]]
    ms_path <- .cp_model_size_path(paths, fold_spec$repeat_id, fold_spec$outer_fold_id, k)

    if (isTRUE(resume) && !isTRUE(force_recompute) && file.exists(ms_path)) {
      ms_env <- tryCatch(readRDS(ms_path), error = function(e) {
        stop("Corrupt or unreadable model_size checkpoint ", ms_path, ": ", conditionMessage(e), call. = FALSE)
      })
      ms_err <- .cp_validate_unit_envelope(
        env = ms_env,
        unit_type = "model_size",
        identity_record = identity_record,
        enum_desc = enum_desc,
        fold_spec = fold_spec,
        split_hash = split_hash,
        model_size = k,
        preselect_top_n = preselect_top_n,
        rank_by = rank_by,
        paths = paths,
        validate_children = TRUE
      )
      if (!is.null(ms_err)) {
        stop("Invalid model_size checkpoint ", ms_path, ": ", ms_err, call. = FALSE)
      }
      reused_model_sizes[[k_key]] <- ms_env
      next
    }

    if (identical(checkpoint_granularity, "model_size")) {
      missing_tasks[[length(missing_tasks) + 1L]] <- list(
        repeat_id     = as.integer(fold_spec$repeat_id),
        outer_fold_id = as.integer(fold_spec$outer_fold_id),
        fold_name     = fold_spec$fold_name,
        model_size    = as.integer(k),
        rank_start    = 1,
        rank_end      = total_k,
        task_kind     = "model_size"
      )
    } else {
      ranges <- .cp_build_batch_ranges(total_k, batch_size)
      expected_files <- vapply(ranges, function(rg) {
        basename(.cp_batch_path(paths, fold_spec$repeat_id, fold_spec$outer_fold_id, k,
                                rg$rank_start, rg$rank_end))
      }, character(1L))

      b_dir <- .cp_batch_subdir(paths, fold_spec$repeat_id, fold_spec$outer_fold_id, k)
      if (isTRUE(resume) && !isTRUE(force_recompute) && dir.exists(b_dir)) {
        existing_files <- list.files(b_dir, pattern = "^batch_.*\\.rds$", full.names = FALSE)
        unexpected <- setdiff(existing_files, expected_files)
        if (length(unexpected) > 0L) {
          stop(
            sprintf(
              "Ambiguous or mismatched candidate_batch files in %s (%s); refusing silent repartitioning.",
              b_dir, paste(unexpected, collapse = ", ")
            ),
            call. = FALSE
          )
        }
      }

      reused_batches[[k_key]] <- list()
      for (rg in ranges) {
        b_path <- .cp_batch_path(paths, fold_spec$repeat_id, fold_spec$outer_fold_id, k,
                                 rg$rank_start, rg$rank_end)
        if (isTRUE(resume) && !isTRUE(force_recompute) && file.exists(b_path)) {
          b_env <- tryCatch(readRDS(b_path), error = function(e) {
            stop("Corrupt or unreadable candidate_batch checkpoint ", b_path, ": ",
                 conditionMessage(e), call. = FALSE)
          })
          b_err <- .cp_validate_unit_envelope(
            env = b_env,
            unit_type = "candidate_batch",
            identity_record = identity_record,
            enum_desc = enum_desc,
            fold_spec = fold_spec,
            split_hash = split_hash,
            model_size = k,
            preselect_top_n = preselect_top_n,
            rank_by = rank_by,
            expected_rank_start = rg$rank_start,
            expected_rank_end = rg$rank_end
          )
          if (!is.null(b_err)) {
            stop("Invalid candidate_batch checkpoint ", b_path, ": ", b_err, call. = FALSE)
          }
          reused_batches[[k_key]][[length(reused_batches[[k_key]]) + 1L]] <- b_env
        } else {
          missing_tasks[[length(missing_tasks) + 1L]] <- list(
            repeat_id     = as.integer(fold_spec$repeat_id),
            outer_fold_id = as.integer(fold_spec$outer_fold_id),
            fold_name     = fold_spec$fold_name,
            model_size    = as.integer(k),
            rank_start    = rg$rank_start,
            rank_end      = rg$rank_end,
            task_kind     = "candidate_batch"
          )
        }
      }
    }
  }

  list(
    reused_model_sizes = reused_model_sizes,
    reused_batches     = reused_batches,
    missing_tasks      = missing_tasks
  )
}

# ---- R evaluation of a single candidate range ----

.cp_eval_range_r <- function(full_data_train,
                             outcome_col,
                             items,
                             positive_label,
                             negative_label,
                             cutoff_method,
                             preselect_by,
                             preselect_top_n,
                             engine,
                             enum_desc,
                             model_size,
                             rank_start,
                             rank_end) {
  chunk_size <- as.integer(rank_end - rank_start + 1)
  chunk_res <- exhaustive_sum_roc(
    data               = full_data_train,
    outcome            = outcome_col,
    items              = items,
    min_items          = as.integer(model_size),
    max_items          = as.integer(model_size),
    positive_label     = positive_label,
    negative_label     = negative_label,
    cutoff_method      = cutoff_method,
    rank_by            = preselect_by,
    top_n              = NULL,
    prefer_fewer_items = TRUE,
    engine             = engine,
    parallel           = "none",
    n_workers          = 1L,
    progress           = FALSE,
    chunk_start        = as.double(rank_start - 1),
    chunk_size         = chunk_size
  )
  local_ranks <- seq.int(as.integer(rank_start), as.integer(rank_end))
  level_start <- enum_desc$level_starts_0based[[as.character(model_size)]]
  chunk_res$local_rank <- as.numeric(local_ranks)
  chunk_res$global_rank <- as.numeric(level_start + local_ranks)
  chunk_res$n_items <- as.integer(chunk_res$n_items)

  cols <- c("global_rank", "local_rank", "n_items", "items",
            "auc", "cutoff", "sensitivity", "specificity", "youden",
            "accuracy", "ppv", "npv")
  chunk_res <- chunk_res[, cols, drop = FALSE]
  .cp_merge_top_candidates(list(chunk_res), preselect_top_n, preselect_by)
}

# ---- Promote model_size units -> outer_fold canonical result & publish ----

.cp_promote_fold_to_outer <- function(prep,
                                      enum_desc,
                                      fold_idx,
                                      ms_envelopes,
                                      full_data,
                                      y_vec,
                                      items,
                                      cutoff_method,
                                      preselect_by,
                                      selection_criterion,
                                      preselect_top_n,
                                      inner_k,
                                      inner_repeats,
                                      seed,
                                      consumer_backend,
                                      force_recompute,
                                      verbose) {
  fold_spec <- prep$fold_specs[[fold_idx]]
  fold_name <- fold_spec$fold_name
  if (identical(.ncvroc_checkpoint_test_seams$fail_before_outer_promotion, fold_name) ||
      isTRUE(.ncvroc_checkpoint_test_seams$fail_before_outer_promotion)) {
    stop("Simulated failure before outer_fold promotion for ", fold_name, call. = FALSE)
  }

  ms_dfs <- lapply(enum_desc$model_sizes, function(k) {
    ms_envelopes[[as.character(k)]]$payload$top_k_candidates
  })
  top_candidates <- .cp_merge_top_candidates(ms_dfs, preselect_top_n, preselect_by)

  # Match outer_folds ordering expected by .evaluate_outer_fold_from_candidates (1-based)
  outer_folds_proxy <- vector("list", length(prep$fold_specs))
  names(outer_folds_proxy) <- vapply(prep$fold_specs, function(s) s$fold_name, character(1L))
  for (j in seq_along(prep$fold_specs)) {
    outer_folds_proxy[[j]] <- as.integer(prep$fold_specs[[j]]$test_indices + 1L)
  }

  precomputed_1based <- lapply(fold_spec$inner_folds, function(cand_f) {
    lapply(cand_f, function(f_idx) as.integer(f_idx + 1L))
  })

  r_fold <- .evaluate_outer_fold_from_candidates(
    top_candidates          = top_candidates,
    i                       = fold_idx,
    outer_folds             = outer_folds_proxy,
    full_data               = full_data,
    y                       = y_vec,
    n_total                 = nrow(full_data),
    selection_criterion     = selection_criterion,
    inner_k                 = inner_k,
    inner_repeats           = inner_repeats,
    cutoff_method           = cutoff_method,
    seed                    = seed,
    progress                = FALSE,
    verbose                 = verbose,
    precomputed_inner_folds = precomputed_1based,
    return_canonical_diag   = TRUE
  )
  canon_payload <- .r_fold_to_canonical(r_fold, fold_spec, items, y_vec = y_vec)
  .cp_publish_fold(
    paths              = prep$paths,
    identity_record    = prep$identity_record,
    fold_spec          = fold_spec,
    split_hash         = prep$identity_record$core$fold_identities[[fold_idx]]$split_hash,
    canon_payload      = canon_payload,
    items              = items,
    producer_backend   = consumer_backend,
    backend_provenance = paste0(consumer_backend, "_promoted"),
    allow_replace      = isTRUE(force_recompute) || file.exists(
      file.path(prep$paths$folds_dir, .cp_fold_filename(fold_spec$repeat_id, fold_spec$outer_fold_id))
    ),
    y_vec              = y_vec
  )
  .canonical_to_r_aggregation_record(canon_payload)
}

# ---- Fine-grained execution drivers (R serial & Scope B Native) ----

.run_fine_grained_checkpointed <- function(full_data,
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
                                           engine,
                                           seed,
                                           num_threads = 1L,
                                           consumer_backend,
                                           checkpoint_dir,
                                           checkpoint_granularity,
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
    consumer_backend    = consumer_backend,
    progress            = progress,
    verbose             = verbose
  )
  on.exit(.cp_release_lock(prep$lock_token), add = TRUE)

  enum_desc <- .cp_build_enumeration_descriptor(items, min_items, max_items)
  batch_size <- .cp_resolve_candidate_batch_size()
  n_folds <- length(prep$fold_specs)

  fold_inspections <- vector("list", n_folds)
  all_tasks <- list()

  fg_stats <- list(
    checkpoint_granularity    = checkpoint_granularity,
    outer_fold_reused         = length(prep$reused_idx),
    model_size_reused         = 0L,
    model_size_computed       = 0L,
    candidate_batch_reused    = 0L,
    candidate_batch_computed  = 0L
  )

  for (idx in prep$compute_idx) {
    spec <- prep$fold_specs[[idx]]
    insp <- .cp_inspect_fold_fine_grained(
      paths                  = prep$paths,
      identity_record        = prep$identity_record,
      enum_desc              = enum_desc,
      fold_spec              = spec,
      split_hash             = prep$identity_record$core$fold_identities[[idx]]$split_hash,
      preselect_top_n        = preselect_top_n,
      rank_by                = preselect_by,
      checkpoint_granularity = checkpoint_granularity,
      batch_size             = batch_size,
      resume                 = resume,
      force_recompute        = force_recompute
    )
    fold_inspections[[idx]] <- insp
    fg_stats$model_size_reused <- fg_stats$model_size_reused + length(insp$reused_model_sizes)
    for (k_key in names(insp$reused_batches)) {
      fg_stats$candidate_batch_reused <- fg_stats$candidate_batch_reused + length(insp$reused_batches[[k_key]])
    }
    for (t in insp$missing_tasks) {
      t$fold_idx <- idx
      all_tasks[[length(all_tasks) + 1L]] <- t
    }
  }

  # Execute missing tasks with cooperative interrupt polling and bounded native waves
  task_results_by_key <- new.env(parent = emptyenv())
  first_error <- NULL
  interrupt_reason <- NULL

  if (length(all_tasks) > 0L) {
    if (identical(consumer_backend, "r_serial")) {
      for (t_i in seq_along(all_tasks)) {
        t <- all_tasks[[t_i]]
        intr_reason <- .cp_poll_interrupt(
          prep$paths,
          stage_label = sprintf("task_%d", t_i),
          unit_index = t_i
        )
        if (!is.null(intr_reason)) {
          interrupt_reason <- intr_reason
          break
        }
        spec <- prep$fold_specs[[t$fold_idx]]
        interrupted_mid_task <- FALSE
        top_df <- tryCatch(
          .cp_eval_range_r(
            full_data_train = full_data[as.integer(spec$train_indices + 1L), , drop = FALSE],
            outcome_col     = outcome_col,
            items           = items,
            positive_label  = positive_label,
            negative_label  = negative_label,
            cutoff_method   = cutoff_method,
            preselect_by    = preselect_by,
            preselect_top_n = preselect_top_n,
            engine          = engine,
            enum_desc       = enum_desc,
            model_size      = t$model_size,
            rank_start      = t$rank_start,
            rank_end        = t$rank_end
          ),
          interrupt = function(e) {
            interrupted_mid_task <<- TRUE
            NULL
          }
        )
        if (isTRUE(interrupted_mid_task)) {
          interrupt_reason <- "r_user_interrupt"
          break
        }
        t_key <- sprintf("%d_%d_%.0f_%.0f", t$fold_idx, t$model_size, t$rank_start, t$rank_end)
        task_results_by_key[[t_key]] <- list(completed = TRUE, top_df = top_df)
        if (identical(t$task_kind, "candidate_batch")) {
          .ncvroc_checkpoint_test_seams$r_batches_executed <-
            .ncvroc_checkpoint_test_seams$r_batches_executed + 1L
        } else {
          .ncvroc_checkpoint_test_seams$model_sizes_executed <-
            .ncvroc_checkpoint_test_seams$model_sizes_executed + 1L
        }
        .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
          as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L
      }
    } else {
      # Bounded-wave Scope B Native parallel batch execution
      wave_size <- max(1L, as.integer(num_threads))
      n_all <- length(all_tasks)
      wave_starts <- seq.int(1L, n_all, by = wave_size)

      for (w_idx in seq_along(wave_starts)) {
        intr_reason <- .cp_poll_interrupt(
          prep$paths,
          stage_label = sprintf("wave_%d", w_idx),
          unit_index = w_idx
        )
        if (!is.null(intr_reason)) {
          interrupt_reason <- intr_reason
          break
        }

        w_start <- wave_starts[[w_idx]]
        w_end <- min(n_all, w_start + wave_size - 1L)
        wave_tasks <- all_tasks[w_start:w_end]

        unique_fold_idxs <- sort(unique(vapply(wave_tasks, function(t) t$fold_idx, integer(1L))))
        train_indices_list <- lapply(unique_fold_idxs, function(fi) {
          as.integer(prep$fold_specs[[fi]]$train_indices)
        })
        slot_map <- stats::setNames(seq_along(unique_fold_idxs) - 1L, as.character(unique_fold_idxs))

        n_t <- length(wave_tasks)
        task_fold_idx <- vapply(wave_tasks, function(t) as.integer(slot_map[[as.character(t$fold_idx)]]), integer(1L))
        task_repeat_id <- vapply(wave_tasks, function(t) as.integer(t$repeat_id), integer(1L))
        task_outer_fold_id <- vapply(wave_tasks, function(t) as.integer(t$outer_fold_id), integer(1L))
        task_model_size <- vapply(wave_tasks, function(t) as.integer(t$model_size), integer(1L))
        task_l_start_0 <- vapply(wave_tasks, function(t) as.numeric(t$rank_start - 1), numeric(1L))
        task_l_end_0 <- vapply(wave_tasks, function(t) as.numeric(t$rank_end), numeric(1L))
        task_level_start_0 <- vapply(wave_tasks, function(t) {
          as.numeric(enum_desc$level_starts_0based[[as.character(t$model_size)]])
        }, numeric(1L))

        .ncvroc_checkpoint_test_seams$native_call_count <-
          .ncvroc_checkpoint_test_seams$native_call_count + 1L

        nat_res <- native_eval_candidate_batches_cpp(
          x                       = x_mat,
          y                       = as.integer(y_vec),
          train_indices_list      = train_indices_list,
          task_fold_idx           = task_fold_idx,
          task_repeat_id          = task_repeat_id,
          task_outer_fold_id      = task_outer_fold_id,
          task_model_size         = task_model_size,
          task_local_rank_start_0 = task_l_start_0,
          task_local_rank_end_0   = task_l_end_0,
          task_level_start_0      = task_level_start_0,
          cutoff_method           = cutoff_method,
          rank_by                 = preselect_by,
          preselect_top_n         = as.integer(preselect_top_n),
          prefer_fewer_items      = TRUE,
          num_threads             = as.integer(num_threads)
        )

        for (ti in seq_len(n_t)) {
          t <- wave_tasks[[ti]]
          r <- nat_res[[ti]]
          t_key <- sprintf("%d_%d_%.0f_%.0f", t$fold_idx, t$model_size, t$rank_start, t$rank_end)
          if (!isTRUE(r$completed) || r$status_code != 0L) {
            if (is.null(first_error)) {
              first_error <- sprintf("Native worker failed for %s k=%d [%.0f, %.0f]: %s",
                                     t$fold_name, t$model_size, t$rank_start, t$rank_end,
                                     as.character(r$status_message))
            }
            task_results_by_key[[t_key]] <- list(completed = FALSE)
          } else {
            df <- r$candidates
            df$items <- vapply(df$local_rank, function(lr) {
              .cp_materialize_items_from_local_rank(items, t$model_size, lr)
            }, character(1L))
            cols <- c("global_rank", "local_rank", "n_items", "items",
                      "auc", "cutoff", "sensitivity", "specificity", "youden",
                      "accuracy", "ppv", "npv")
            df <- df[, cols, drop = FALSE]
            task_results_by_key[[t_key]] <- list(completed = TRUE, top_df = df)
            if (identical(t$task_kind, "candidate_batch")) {
              .ncvroc_checkpoint_test_seams$native_batches_executed <-
                .ncvroc_checkpoint_test_seams$native_batches_executed + 1L
            } else {
              .ncvroc_checkpoint_test_seams$model_sizes_executed <-
                .ncvroc_checkpoint_test_seams$model_sizes_executed + 1L
            }
          }
        }
        .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
          as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L
      }
    }
  }

  # Persist completed units in deterministic order, promote model_size -> outer_fold
  per_fold <- vector("list", n_folds)
  for (i in prep$reused_idx) {
    per_fold[[i]] <- .canonical_to_r_aggregation_record(prep$final_canon[[i]])
  }

  for (idx in prep$compute_idx) {
    spec <- prep$fold_specs[[idx]]
    insp <- fold_inspections[[idx]]
    ms_envelopes <- insp$reused_model_sizes
    fold_complete <- TRUE

    for (k in enum_desc$model_sizes) {
      k_key <- as.character(k)
      if (!is.null(ms_envelopes[[k_key]])) {
        next
      }
      total_k <- enum_desc$level_sizes[[k_key]]
      if (identical(checkpoint_granularity, "model_size")) {
        t_key <- sprintf("%d_%d_%.0f_%.0f", idx, k, 1, total_k)
        tr <- task_results_by_key[[t_key]]
        if (is.null(tr) || !isTRUE(tr$completed)) {
          fold_complete <- FALSE
          next
        }
        ms_env <- .cp_build_model_size_envelope(
          identity_record    = prep$identity_record,
          enum_desc          = enum_desc,
          fold_spec          = spec,
          split_hash         = prep$identity_record$core$fold_identities[[idx]]$split_hash,
          model_size         = k,
          top_k_candidates   = tr$top_df,
          producer_backend   = consumer_backend,
          child_dependencies = list()
        )
        ms_path <- .cp_model_size_path(prep$paths, spec$repeat_id, spec$outer_fold_id, k)
        .cp_publish_unit(
          paths             = prep$paths,
          target_path       = ms_path,
          env               = ms_env,
          unit_type         = "model_size",
          identity_record   = prep$identity_record,
          enum_desc         = enum_desc,
          fold_spec         = spec,
          split_hash        = prep$identity_record$core$fold_identities[[idx]]$split_hash,
          model_size        = k,
          preselect_top_n   = preselect_top_n,
          rank_by           = preselect_by,
          allow_replace     = isTRUE(force_recompute)
        )
        ms_envelopes[[k_key]] <- ms_env
        fg_stats$model_size_computed <- fg_stats$model_size_computed + 1L
      } else {
        ranges <- .cp_build_batch_ranges(total_k, batch_size)
        batch_envs <- list()
        reused_map <- new.env(parent = emptyenv())
        for (be in insp$reused_batches[[k_key]]) {
          reused_map[[sprintf("%.0f_%.0f", be$rank_start, be$rank_end)]] <- be
        }
        all_batches_ok <- TRUE
        for (rg in ranges) {
          r_key <- sprintf("%.0f_%.0f", rg$rank_start, rg$rank_end)
          if (!is.null(reused_map[[r_key]])) {
            batch_envs[[length(batch_envs) + 1L]] <- reused_map[[r_key]]
          } else {
            t_key <- sprintf("%d_%d_%.0f_%.0f", idx, k, rg$rank_start, rg$rank_end)
            tr <- task_results_by_key[[t_key]]
            if (is.null(tr) || !isTRUE(tr$completed)) {
              all_batches_ok <- FALSE
              next
            }
            b_env <- .cp_build_batch_envelope(
              identity_record     = prep$identity_record,
              enum_desc           = enum_desc,
              fold_spec           = spec,
              split_hash          = prep$identity_record$core$fold_identities[[idx]]$split_hash,
              model_size          = k,
              rank_start          = rg$rank_start,
              rank_end            = rg$rank_end,
              batch_size_recorded = batch_size,
              top_k_candidates    = tr$top_df,
              producer_backend    = consumer_backend
            )
            b_path <- .cp_batch_path(prep$paths, spec$repeat_id, spec$outer_fold_id,
                                     k, rg$rank_start, rg$rank_end)
            .cp_publish_unit(
              paths               = prep$paths,
              target_path         = b_path,
              env                 = b_env,
              unit_type           = "candidate_batch",
              identity_record     = prep$identity_record,
              enum_desc           = enum_desc,
              fold_spec           = spec,
              split_hash          = prep$identity_record$core$fold_identities[[idx]]$split_hash,
              model_size          = k,
              preselect_top_n     = preselect_top_n,
              rank_by             = preselect_by,
              expected_rank_start = rg$rank_start,
              expected_rank_end   = rg$rank_end,
              allow_replace       = isTRUE(force_recompute)
            )
            batch_envs[[length(batch_envs) + 1L]] <- b_env
            fg_stats$candidate_batch_computed <- fg_stats$candidate_batch_computed + 1L
          }
        }
        if (!all_batches_ok) {
          fold_complete <- FALSE
          next
        }
        merged_k_df <- .cp_merge_top_candidates(
          lapply(batch_envs, function(b) b$payload$top_k_candidates),
          preselect_top_n,
          preselect_by
        )
        child_deps <- lapply(batch_envs, function(b) {
          list(
            unit_id      = b$unit_id,
            rank_start   = b$rank_start,
            rank_end     = b$rank_end,
            payload_hash = b$payload_hash
          )
        })
        ms_env <- .cp_build_model_size_envelope(
          identity_record    = prep$identity_record,
          enum_desc          = enum_desc,
          fold_spec          = spec,
          split_hash         = prep$identity_record$core$fold_identities[[idx]]$split_hash,
          model_size         = k,
          top_k_candidates   = merged_k_df,
          producer_backend   = consumer_backend,
          dependency_mode    = "promoted_from_batches",
          child_dependencies = child_deps
        )
        ms_path <- .cp_model_size_path(prep$paths, spec$repeat_id, spec$outer_fold_id, k)
        .cp_publish_unit(
          paths             = prep$paths,
          target_path       = ms_path,
          env               = ms_env,
          unit_type         = "model_size",
          identity_record   = prep$identity_record,
          enum_desc         = enum_desc,
          fold_spec         = spec,
          split_hash        = prep$identity_record$core$fold_identities[[idx]]$split_hash,
          model_size        = k,
          preselect_top_n   = preselect_top_n,
          rank_by           = preselect_by,
          allow_replace     = isTRUE(force_recompute)
        )
        ms_envelopes[[k_key]] <- ms_env
        fg_stats$model_size_computed <- fg_stats$model_size_computed + 1L
      }
    }

    if (fold_complete && is.null(first_error)) {
      per_fold[[idx]] <- .cp_promote_fold_to_outer(
        prep                = prep,
        enum_desc           = enum_desc,
        fold_idx            = idx,
        ms_envelopes        = ms_envelopes,
        full_data           = full_data,
        y_vec               = y_vec,
        items               = items,
        cutoff_method       = cutoff_method,
        preselect_by        = preselect_by,
        selection_criterion = selection_criterion,
        preselect_top_n     = preselect_top_n,
        inner_k             = inner_k,
        inner_repeats       = inner_repeats,
        seed                = seed,
        consumer_backend    = consumer_backend,
        force_recompute     = force_recompute,
        verbose             = verbose
      )
      prep$fold_producers[[idx]] <- consumer_backend
    }
  }

  if (!is.null(first_error)) {
    stop(first_error, call. = FALSE)
  }
  if (!is.null(interrupt_reason)) {
    if (identical(checkpoint_granularity, "model_size")) {
      n_ms_per_fold <- length(enum_desc$model_sizes)
      completed_units <- fg_stats$outer_fold_reused * n_ms_per_fold +
        fg_stats$model_size_reused + fg_stats$model_size_computed
      total_units <- n_folds * n_ms_per_fold
      unit_label <- "model_size unit(s)"
    } else {
      n_batches_per_fold <- sum(vapply(enum_desc$model_sizes, function(k) {
        length(.cp_build_batch_ranges(enum_desc$level_sizes[[as.character(k)]], batch_size))
      }, integer(1L)))
      completed_units <- fg_stats$outer_fold_reused * n_batches_per_fold +
        fg_stats$candidate_batch_reused + fg_stats$candidate_batch_computed
      total_units <- n_folds * n_batches_per_fold
      unit_label <- "candidate_batch unit(s)"
    }
    .cp_signal_interrupted(
      paths           = prep$paths,
      reason          = interrupt_reason,
      completed_units = completed_units,
      total_units     = total_units,
      unit_label      = unit_label,
      granularity     = checkpoint_granularity
    )
  }
  if (isTRUE(.ncvroc_checkpoint_test_seams$fail_before_aggregate)) {
    stop("Simulated failure before final aggregation (all completed units persisted).", call. = FALSE)
  }

  prov <- .cp_build_provenance(prep, consumer_backend, resume, force_recompute)
  prov$fine_grained <- fg_stats
  list(per_fold = per_fold, provenance = prov)
}

# ---- Bounded streaming Native outer-fold execution (R3/R4: >2M capable) ----

.run_native_streaming_outer_folds <- function(full_data,
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
                                              seed,
                                              num_threads = 1L,
                                              checkpoint_dir = NULL,
                                              resume = TRUE,
                                              force_recompute = FALSE,
                                              progress = TRUE,
                                              verbose = TRUE) {
  enum_desc <- .cp_build_enumeration_descriptor(items, min_items, max_items)
  batch_size <- .cp_resolve_candidate_batch_size()
  n_folds <- length(outer_folds)
  has_cp <- !is.null(checkpoint_dir)

  if (has_cp) {
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
    fold_specs <- prep$fold_specs
    compute_idx <- prep$compute_idx
    per_fold <- vector("list", n_folds)
    for (i in prep$reused_idx) {
      per_fold[[i]] <- .canonical_to_r_aggregation_record(prep$final_canon[[i]])
    }
    split_hashes <- vapply(prep$states, `[[`, character(1L), "split_hash")
    paths <- prep$paths
  } else {
    n_cands_shortlist <- as.integer(min(as.double(preselect_top_n), enum_desc$total_combos))
    fold_specs <- .build_native_outer_fold_specs(
      outer_folds  = outer_folds,
      y            = y_vec,
      n_total      = length(y_vec),
      inner_k      = inner_k,
      inner_repeats = inner_repeats,
      n_candidates = n_cands_shortlist,
      seed         = seed
    )
    compute_idx <- seq_len(n_folds)
    per_fold <- vector("list", n_folds)
    paths <- NULL
  }

  outer_folds_proxy <- vector("list", n_folds)
  names(outer_folds_proxy) <- vapply(fold_specs, function(s) s$fold_name, character(1L))
  for (j in seq_len(n_folds)) {
    outer_folds_proxy[[j]] <- as.integer(fold_specs[[j]]$test_indices + 1L)
  }

  wave_size <- max(1L, as.integer(num_threads))
  wave_counter <- 0L

  for (idx in compute_idx) {
    spec <- fold_specs[[idx]]
    top_k_acc <- NULL

    # Stream each model_size in bounded batches; never materialize full candidate list
    for (k in enum_desc$model_sizes) {
      k_key <- as.character(k)
      total_k <- enum_desc$level_sizes[[k_key]]
      level_start_0 <- as.numeric(enum_desc$level_starts_0based[[k_key]])
      n_batches_k <- as.integer(ceiling(total_k / batch_size))
      batch_starts <- seq.int(1L, n_batches_k, by = wave_size)

      for (b_wave_start in batch_starts) {
        wave_counter <- wave_counter + 1L
        if (has_cp) {
          intr_reason <- .cp_poll_interrupt(
            paths,
            stage_label = sprintf("stream_wave_%d", wave_counter),
            unit_index  = wave_counter
          )
          if (!is.null(intr_reason)) {
            n_saved <- sum(!vapply(per_fold, is.null, logical(1L)))
            .cp_signal_interrupted(
              paths           = paths,
              reason          = intr_reason,
              completed_units = n_saved,
              total_units     = n_folds,
              unit_label      = "outer fold(s)",
              granularity     = "outer_fold"
            )
          }
        } else {
          if (isTRUE(check_user_interrupt_cpp())) {
            stop("Execution interrupted by user.", call. = FALSE)
          }
        }

        b_wave_end <- min(n_batches_k, b_wave_start + wave_size - 1L)
        b_indices <- seq.int(b_wave_start, b_wave_end)
        n_w <- length(b_indices)
        r_starts_0 <- numeric(n_w)
        r_ends_0 <- numeric(n_w)
        for (wi in seq_len(n_w)) {
          bi <- b_indices[[wi]]
          s_1 <- (as.double(bi) - 1.0) * as.double(batch_size) + 1.0
          e_1 <- min(total_k, as.double(bi) * as.double(batch_size))
          r_starts_0[[wi]] <- s_1 - 1.0
          r_ends_0[[wi]] <- e_1
        }

        .ncvroc_checkpoint_test_seams$native_call_count <-
          as.integer(.ncvroc_checkpoint_test_seams$native_call_count) + 1L

        nat_res <- native_eval_candidate_batches_cpp(
          x                       = x_mat,
          y                       = as.integer(y_vec),
          train_indices_list      = list(as.integer(spec$train_indices)),
          task_fold_idx           = rep(0L, n_w),
          task_repeat_id          = rep(as.integer(spec$repeat_id), n_w),
          task_outer_fold_id      = rep(as.integer(spec$outer_fold_id), n_w),
          task_model_size         = rep(as.integer(k), n_w),
          task_local_rank_start_0 = r_starts_0,
          task_local_rank_end_0   = r_ends_0,
          task_level_start_0      = rep(level_start_0, n_w),
          cutoff_method           = cutoff_method,
          rank_by                 = preselect_by,
          preselect_top_n         = as.integer(preselect_top_n),
          prefer_fewer_items      = TRUE,
          num_threads             = as.integer(num_threads)
        )

        wave_dfs <- vector("list", n_w)
        for (wi in seq_len(n_w)) {
          r <- nat_res[[wi]]
          if (!isTRUE(r$completed) || r$status_code != 0L) {
            stop(
              sprintf(
                "Native streaming batch failed for %s k=%d [%.0f, %.0f]: %s",
                spec$fold_name, k, r_starts_0[[wi]] + 1, r_ends_0[[wi]],
                as.character(r$status_message)
              ),
              call. = FALSE
            )
          }
          df <- r$candidates
          df$items <- vapply(df$local_rank, function(lr) {
            .cp_materialize_items_from_local_rank(items, k, lr)
          }, character(1L))
          cols <- c("global_rank", "local_rank", "n_items", "items",
                    "auc", "cutoff", "sensitivity", "specificity", "youden",
                    "accuracy", "ppv", "npv")
          wave_dfs[[wi]] <- df[, cols, drop = FALSE]
          .ncvroc_checkpoint_test_seams$native_batches_executed <-
            as.integer(.ncvroc_checkpoint_test_seams$native_batches_executed) + 1L
        }

        # Exact bounded global Top-N reduction (keeps <= preselect_top_n rows)
        merge_inputs <- if (is.null(top_k_acc)) wave_dfs else c(list(top_k_acc), wave_dfs)
        top_k_acc <- .cp_merge_top_candidates(merge_inputs, preselect_top_n, preselect_by)
        .ncvroc_checkpoint_test_seams$interrupt_waves_completed <-
          as.integer(.ncvroc_checkpoint_test_seams$interrupt_waves_completed) + 1L
      }
    }

    # Shortlist-only inner CV (row indices independent of total candidate count)
    precomputed_1based <- lapply(spec$inner_folds, function(cand_f) {
      lapply(cand_f, function(f_idx) as.integer(f_idx + 1L))
    })

    r_fold <- .evaluate_outer_fold_from_candidates(
      top_candidates          = top_k_acc,
      i                       = idx,
      outer_folds             = outer_folds_proxy,
      full_data               = full_data,
      y                       = y_vec,
      n_total                 = nrow(full_data),
      selection_criterion     = selection_criterion,
      inner_k                 = inner_k,
      inner_repeats           = inner_repeats,
      cutoff_method           = cutoff_method,
      seed                    = seed,
      progress                = FALSE,
      verbose                 = verbose,
      precomputed_inner_folds = precomputed_1based,
      return_canonical_diag   = TRUE
    )

    if (has_cp) {
      canon_res <- .r_fold_to_canonical(r_fold, spec, items, y_vec = y_vec)
      .cp_publish_fold(
        paths              = paths,
        identity_record    = prep$identity_record,
        fold_spec          = spec,
        split_hash         = split_hashes[[idx]],
        canon_payload      = canon_res,
        items              = items,
        producer_backend   = "scope_b_native_outer_fold",
        backend_provenance = "scope_b_native_streaming",
        allow_replace      = isTRUE(force_recompute),
        y_vec              = y_vec
      )
      prep$final_canon[[idx]] <- canon_res
      prep$fold_producers[[idx]] <- "scope_b_native_outer_fold"
      per_fold[[idx]] <- .canonical_to_r_aggregation_record(canon_res)
    } else {
      per_fold[[idx]] <- r_fold
    }
  }

  prov <- if (has_cp) {
    .cp_build_provenance(prep, "scope_b_native_outer_fold", resume, force_recompute)
  } else {
    NULL
  }
  list(per_fold = per_fold, provenance = prov)
}
