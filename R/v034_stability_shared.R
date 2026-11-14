# ==============================================================================
# Shared Metadata, Adapter & Validation Helpers for v0.34.0 Phase 4
# ==============================================================================

#' Validate and canonicalize a list of selected feature sets against eligible_features
#' @keywords internal
#' @noRd
.v034_canonicalize_selected_sets <- function(selected_sets,
                                             eligible_features = NULL,
                                             valid_mask = NULL) {
  if (is.matrix(selected_sets) || is.data.frame(selected_sets)) {
    mat <- as.matrix(selected_sets)
    if (!is.logical(mat) && !is.numeric(mat)) {
      stop("Selection matrix `Z` must be logical or numeric (0/1).", call. = FALSE)
    }
    if (is.null(eligible_features)) {
      eligible_features <- colnames(mat)
    }
    if (is.null(eligible_features) || length(eligible_features) != ncol(mat)) {
      stop("`eligible_features` must be provided or match `colnames(Z)`.", call. = FALSE)
    }
    if (anyDuplicated(eligible_features)) {
      stop("`eligible_features` must not contain duplicates.", call. = FALSE)
    }
    if (!all(mat %in% c(0, 1, TRUE, FALSE, NA))) {
      stop("Selection matrix `Z` must contain only 0/1 or TRUE/FALSE (or NA for invalid rows).", call. = FALSE)
    }
    sets_out <- vector("list", nrow(mat))
    if (is.null(valid_mask)) {
      valid_mask <- !apply(mat, 1L, anyNA)
    }
    for (i in seq_len(nrow(mat))) {
      if (isTRUE(valid_mask[i])) {
        sets_out[[i]] <- sort(eligible_features[as.logical(mat[i, ])])
      } else {
        sets_out[[i]] <- character(0)
      }
    }
    return(list(
      selected_sets     = sets_out,
      eligible_features = as.character(eligible_features),
      valid_mask        = as.logical(valid_mask)
    ))
  }

  if (!is.list(selected_sets)) {
    stop("`selected_sets` must be a list of character vectors or a binary selection matrix.", call. = FALSE)
  }
  n_iter <- length(selected_sets)
  if (is.null(valid_mask)) {
    valid_mask <- vapply(selected_sets, function(s) !is.null(s) && !anyNA(s), logical(1))
  }
  if (length(valid_mask) != n_iter) {
    stop("`valid_mask` length must match `length(selected_sets)`.", call. = FALSE)
  }

  if (is.null(eligible_features)) {
    all_items <- unique(unlist(selected_sets[valid_mask], use.names = FALSE))
    eligible_features <- sort(as.character(all_items))
  } else {
    eligible_features <- as.character(eligible_features)
    if (anyDuplicated(eligible_features)) {
      stop("`eligible_features` must not contain duplicates.", call. = FALSE)
    }
  }

  canon_sets <- vector("list", n_iter)
  for (i in seq_len(n_iter)) {
    if (!isTRUE(valid_mask[i])) {
      canon_sets[[i]] <- character(0)
      next
    }
    s <- as.character(selected_sets[[i]])
    if (anyDuplicated(s)) {
      stop(sprintf("Duplicate feature(s) detected in selected set %d: %s.",
                   i, paste(unique(s[duplicated(s)]), collapse = ", ")), call. = FALSE)
    }
    unknown <- setdiff(s, eligible_features)
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown feature(s) in selected set %d not in `eligible_features`: %s.",
                   i, paste(unknown, collapse = ", ")), call. = FALSE)
    }
    canon_sets[[i]] <- sort(s)
  }

  list(
    selected_sets     = canon_sets,
    eligible_features = eligible_features,
    valid_mask        = as.logical(valid_mask)
  )
}

#' Build canonical binary matrix Z (M x d) over valid selected sets
#' @keywords internal
#' @noRd
.v034_build_selection_matrix <- function(canon_sets, eligible_features, valid_mask) {
  valid_sets <- canon_sets[valid_mask]
  m_valid <- length(valid_sets)
  d <- length(eligible_features)
  z <- matrix(0L, nrow = m_valid, ncol = d)
  colnames(z) <- eligible_features
  if (m_valid > 0L && d > 0L) {
    for (i in seq_len(m_valid)) {
      if (length(valid_sets[[i]]) > 0L) {
        idx <- match(valid_sets[[i]], eligible_features)
        z[i, idx] <- 1L
      }
    }
  }
  z
}

#' Extract selected sets and metadata from existing NCVROC result objects
#' @keywords internal
#' @noRd
.v034_extract_from_object <- function(x, eligible_features = NULL) {
  if (inherits(x, "subsampling_stability_result")) {
    return(list(
      selected_sets     = x$selected_sets,
      eligible_features = if (is.null(eligible_features)) x$metadata$eligible_features else eligible_features,
      valid_mask        = x$iteration_table$status == "valid",
      resampling_method = x$metadata$resampling_method,
      sample_fraction   = x$metadata$sample_fraction,
      stratified        = x$metadata$stratified,
      independent_units = FALSE
    ))
  }
  if (inherits(x, "classification_stability_result")) {
    rep_df <- x$replicate_summary
    elig <- if (is.null(eligible_features)) x$settings$items else eligible_features
    sets <- lapply(seq_len(nrow(rep_df)), function(i) {
      if (!isTRUE(rep_df$valid[i]) || is.na(rep_df$selected_items[i])) return(character(0))
      .parse_itemset(rep_df$selected_items[i])
    })
    return(list(
      selected_sets     = sets,
      eligible_features = elig,
      valid_mask        = as.logical(rep_df$valid),
      resampling_method = paste0("bootstrap_", x$settings$resampling),
      sample_fraction   = 1.0,
      stratified        = identical(x$settings$resampling, "stratified"),
      independent_units = FALSE
    ))
  }
  if (inherits(x, "ncvroc_analysis") && !is.null(x$result)) {
    x <- x$result
  }
  if (inherits(x, "cross_size_nested_cv_result") || inherits(x, "nested_sum_roc_result")) {
    of <- x$outer_fold_results
    elig <- if (is.null(eligible_features)) {
      if (!is.null(x$settings$item_names)) x$settings$item_names else x$settings$items
    } else eligible_features
    sets <- lapply(seq_len(nrow(of)), function(i) {
      s <- as.character(of$selected_items[i])
      if (is.na(s) || !nzchar(s)) return(character(0))
      .parse_itemset(s)
    })
    return(list(
      selected_sets     = sets,
      eligible_features = elig,
      valid_mask        = !is.na(of$selected_items) & nzchar(as.character(of$selected_items)),
      resampling_method = "nested_cv_outer_folds",
      sample_fraction   = NA_real_,
      stratified        = isTRUE(x$settings$stratified),
      independent_units = FALSE
    ))
  }
  NULL
}

#' Build standardized v0.34.0 metadata list
#' @keywords internal
#' @noRd
.v034_make_metadata <- function(stability_target,
                                selection_procedure = NA_character_,
                                eligible_features = character(0),
                                candidate_scope = NA_character_,
                                resampling_method = NA_character_,
                                sample_fraction = NA_real_,
                                stratified = NA,
                                class_design = NA_character_,
                                requested_iterations = NA_integer_,
                                completed_iterations = NA_integer_,
                                valid_iterations = NA_integer_,
                                failed_iterations = NA_integer_,
                                seed_provenance = NULL,
                                selected_size_distribution = NULL,
                                backend = NA_character_,
                                warnings = character(0)) {
  list(
    schema_version             = "0.34.0",
    stability_target           = stability_target,
    selection_procedure        = selection_procedure,
    eligible_features          = eligible_features,
    n_eligible_features        = length(eligible_features),
    candidate_scope            = candidate_scope,
    resampling_method          = resampling_method,
    sample_fraction            = sample_fraction,
    stratified                 = stratified,
    class_design               = class_design,
    requested_iterations       = as.integer(requested_iterations),
    completed_iterations       = as.integer(completed_iterations),
    valid_iterations           = as.integer(valid_iterations),
    failed_iterations          = as.integer(failed_iterations),
    seed_provenance            = seed_provenance,
    selected_size_distribution = selected_size_distribution,
    backend                    = backend,
    warnings                   = warnings
  )
}
