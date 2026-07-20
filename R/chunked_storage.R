# chunked_storage.R — Chunked RDS reader/writer for large result sets
#
# Internal functions (all @keywords internal):
#   .make_chunk_dir()
#   .write_chunk_rds()
#   .read_chunk_rds()
#   .chunked_reader()
#   .stream_top_n_from_chunks()

#' Create a directory for chunked RDS files
#'
#' Creates a uniquely-named chunk directory. Uses `tempfile()` for uniqueness
#' when no base directory is provided.
#'
#' @param base_dir Character, parent directory, or NULL for tempdir().
#' @param prefix Character, directory name prefix (default "ncvroc_chunks_").
#'
#' @return Character, path to the created directory.
#' @keywords internal
.make_chunk_dir <- function(base_dir = NULL, prefix = "ncvroc_chunks_") {
  if (is.null(base_dir)) {
    base_dir <- tempdir()
  }
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  unique_stub <- basename(tempfile(pattern = ""))
  unique_stub <- gsub("[^A-Za-z0-9]+", "", unique_stub)

  chunk_dir <- file.path(base_dir, paste0(prefix, unique_stub))
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

  chunk_dir
}

#' Write a single chunk as an RDS file
#'
#' @param chunk data.frame, one chunk of candidate results.
#' @param chunk_dir Character, directory to write into.
#' @param chunk_index Integer, zero-based chunk index.
#' @return Invisible NULL.
#' @keywords internal
.write_chunk_rds <- function(chunk, chunk_dir, chunk_index) {
  filename <- sprintf("chunk_%05d.rds", chunk_index)
  saveRDS(chunk, file.path(chunk_dir, filename))
  invisible(NULL)
}

#' Read a single chunk RDS file
#'
#' @param chunk_dir Character, directory containing chunk files.
#' @param chunk_index Integer, zero-based chunk index.
#' @return The data.frame stored in that chunk.
#' @keywords internal
.read_chunk_rds <- function(chunk_dir, chunk_index) {
  filename <- sprintf("chunk_%05d.rds", chunk_index)
  readRDS(file.path(chunk_dir, filename))
}

#' List all chunk files in sorted order
#'
#' @param chunk_dir Character, directory to scan.
#' @return Character vector of absolute paths to chunk RDS files, sorted.
#' @keywords internal
.list_chunk_files <- function(chunk_dir) {
  if (!dir.exists(chunk_dir)) {
    stop("Chunk directory does not exist: ", chunk_dir, call. = FALSE)
  }
  files <- list.files(chunk_dir,
    pattern = "^chunk_[0-9]{5}\\.rds$",
    full.names = TRUE
  )
  sort(files)
}

#' Iterate over all chunks, calling a callback for each
#'
#' Reads each chunk file sequentially, calls `callback(chunk, index)`, and
#' optionally collects results. Peak memory: one chunk at a time.
#'
#' @param chunk_dir Character, directory containing chunk files.
#' @param callback Function with signature `function(chunk, chunk_index)`.
#'   Return value is collected if `collect = TRUE`.
#' @param collect Logical, if TRUE collect callback return values into a list.
#'
#' @return If `collect = TRUE`, a list of callback return values. Otherwise
#'   invisible NULL.
#' @keywords internal
.chunked_reader <- function(chunk_dir, callback, collect = FALSE) {
  files <- .list_chunk_files(chunk_dir)
  if (length(files) == 0) {
    stop("No chunk files found in ", chunk_dir, call. = FALSE)
  }

  if (collect) {
    results <- vector("list", length(files))
    for (i in seq_along(files)) {
      chunk <- readRDS(files[i])
      results[[i]] <- callback(chunk, i - 1L)
    }
    return(results)
  }

  for (i in seq_along(files)) {
    chunk <- readRDS(files[i])
    callback(chunk, i - 1L)
  }

  invisible(NULL)
}

#' Stream top-N candidates from chunked storage
#'
#' Reads chunks sequentially, applies optional condition filters, and keeps
#' a running top-N buffer. Peak memory: one chunk + top_n rows.
#'
#' @param chunk_dir Character, directory containing chunk files.
#' @param rank_by Character, metric column name to rank by.
#' @param top_n Integer, number of top candidates to return.
#' @param sensitivity Optional condition string (e.g. ">= 0.90").
#' @param specificity Optional condition string.
#' @param auc Optional condition string.
#' @param youden Optional condition string.
#' @param accuracy Optional condition string.
#' @param ppv Optional condition string.
#' @param npv Optional condition string.
#' @param n_items Optional condition string.
#' @param cutoff Optional condition string.
#'
#' @return A data.frame of at most `top_n` rows.
#' @keywords internal
.stream_top_n_from_chunks <- function(chunk_dir,
                                       rank_by = c("youden", "auc", "sensitivity",
                                                   "specificity", "accuracy",
                                                   "ppv", "npv"),
                                       top_n = 20,
                                       sensitivity = NULL,
                                       specificity = NULL,
                                       auc          = NULL,
                                       youden       = NULL,
                                       accuracy     = NULL,
                                       ppv          = NULL,
                                       npv          = NULL,
                                       n_items      = NULL,
                                       cutoff       = NULL) {

  rank_by <- match.arg(rank_by)

  conditions <- list(
    sensitivity = sensitivity,
    specificity = specificity,
    auc         = auc,
    youden      = youden,
    accuracy    = accuracy,
    ppv         = ppv,
    npv         = npv,
    n_items     = n_items,
    cutoff      = cutoff
  )
  conditions <- conditions[!vapply(conditions, is.null, logical(1))]

  # Pre-parse conditions
  parsed_conditions <- lapply(names(conditions), function(col) {
    list(col = col, parsed = .parse_condition(conditions[[col]]))
  })

  best_so_far <- NULL

  files <- .list_chunk_files(chunk_dir)

  for (i in seq_along(files)) {
    chunk <- readRDS(files[i])

    # Apply conditions
    for (pc in parsed_conditions) {
      col_vals <- chunk[[pc$col]]
      keep <- switch(pc$parsed$op,
        `>=` = col_vals >= pc$parsed$value,
        `>`  = col_vals >  pc$parsed$value,
        `<=` = col_vals <= pc$parsed$value,
        `<`  = col_vals <  pc$parsed$value,
        `==` = col_vals == pc$parsed$value,
        `!=` = col_vals != pc$parsed$value
      )
      chunk <- chunk[keep, , drop = FALSE]
    }

    if (nrow(chunk) == 0) next

    # Keep running top-N
    combined <- if (is.null(best_so_far)) {
      chunk
    } else {
      rbind(best_so_far, chunk)
    }

    tie_cols <- setdiff(
      c("youden", "auc", "sensitivity", "specificity", "accuracy"),
      rank_by
    )
    tie_cols <- intersect(tie_cols, names(combined))

    ord <- do.call(order, c(
      lapply(c(rank_by, tie_cols), function(nm) -combined[[nm]])
    ))
    combined <- combined[ord, , drop = FALSE]

    best_so_far <- utils::head(combined, top_n)
  }

  if (is.null(best_so_far)) {
    # Return empty data.frame with expected columns
    empty <- data.frame(
      rank = integer(), items = character(), n_items = integer(),
      auc = numeric(), cutoff = numeric(),
      sensitivity = numeric(), specificity = numeric(),
      youden = numeric(), accuracy = numeric(),
      ppv = numeric(), npv = numeric(),
      n_positive = integer(), n_negative = integer(),
      stringsAsFactors = FALSE
    )
    return(empty)
  }

  best_so_far
}

#' Full-load from chunked storage (sequential rbind)
#'
#' Reads all chunks and rbinds them sequentially. Peak memory: one chunk +
#' accumulated result. For very large searches this may still exceed available
#' memory — use `.stream_top_n_from_chunks()` instead.
#'
#' @param chunk_dir Character, directory containing chunk files.
#' @param sensitivity Optional condition string.
#' @param specificity Optional condition string.
#' @param auc Optional condition string.
#' @param youden Optional condition string.
#' @param accuracy Optional condition string.
#' @param ppv Optional condition string.
#' @param npv Optional condition string.
#' @param n_items Optional condition string.
#' @param cutoff Optional condition string.
#'
#' @return A data.frame of all rows matching conditions.
#' @keywords internal
.full_load_chunked <- function(chunk_dir,
                                sensitivity = NULL,
                                specificity = NULL,
                                auc      = NULL,
                                youden   = NULL,
                                accuracy = NULL,
                                ppv      = NULL,
                                npv      = NULL,
                                n_items  = NULL,
                                cutoff   = NULL) {

  conditions <- list(
    sensitivity = sensitivity,
    specificity = specificity,
    auc         = auc,
    youden      = youden,
    accuracy    = accuracy,
    ppv         = ppv,
    npv         = npv,
    n_items     = n_items,
    cutoff      = cutoff
  )
  conditions <- conditions[!vapply(conditions, is.null, logical(1))]

  parsed_conditions <- lapply(names(conditions), function(col) {
    list(col = col, parsed = .parse_condition(conditions[[col]]))
  })

  all_rows <- NULL
  files <- .list_chunk_files(chunk_dir)

  for (i in seq_along(files)) {
    chunk <- readRDS(files[i])

    for (pc in parsed_conditions) {
      col_vals <- chunk[[pc$col]]
      keep <- switch(pc$parsed$op,
        `>=` = col_vals >= pc$parsed$value,
        `>`  = col_vals >  pc$parsed$value,
        `<=` = col_vals <= pc$parsed$value,
        `<`  = col_vals <  pc$parsed$value,
        `==` = col_vals == pc$parsed$value,
        `!=` = col_vals != pc$parsed$value
      )
      chunk <- chunk[keep, , drop = FALSE]
    }

    if (nrow(chunk) == 0) next

    all_rows <- if (is.null(all_rows)) chunk else rbind(all_rows, chunk)
  }

  if (is.null(all_rows)) {
    empty <- data.frame(
      rank = integer(), items = character(), n_items = integer(),
      auc = numeric(), cutoff = numeric(),
      sensitivity = numeric(), specificity = numeric(),
      youden = numeric(), accuracy = numeric(),
      ppv = numeric(), npv = numeric(),
      n_positive = integer(), n_negative = integer(),
      stringsAsFactors = FALSE
    )
    return(empty)
  }

  all_rows
}
