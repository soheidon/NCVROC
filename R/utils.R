# utils.R — Internal utility functions

#' Validate inputs for NCVROC functions
#'
#' Checks that data, outcome, and items meet all requirements.
#' Converts outcome to 0/1 using positive_label/negative_label.
#'
#' @param data A data.frame.
#' @param outcome Character, name of the outcome column.
#' @param items Character vector of item column names.
#' @param positive_label The value representing a positive case.
#' @param negative_label The value representing a negative case.
#'
#' @return A list with elements: data (processed data.frame), outcome_col (character),
#'   items (character vector), y (0/1 numeric vector).
#' @keywords internal
validate_inputs <- function(data, outcome, items, positive_label, negative_label) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!outcome %in% colnames(data)) {
    stop("Outcome column '", outcome, "' not found in `data`.", call. = FALSE)
  }

  missing_items <- setdiff(items, colnames(data))
  if (length(missing_items) > 0) {
    stop(
      "Item column(s) not found in `data`: ",
      paste(missing_items, collapse = ", "), ".",
      call. = FALSE
    )
  }

  y <- data[[outcome]]

  if (anyNA(y)) {
    stop("`outcome` column '", outcome, "' contains NA values. ",
         "Missing values are not supported in v0.1.", call. = FALSE)
  }

  y_vals <- unique(y)
  allowed <- c(positive_label, negative_label)
  invalid <- setdiff(y_vals, allowed)
  if (length(invalid) > 0) {
    stop(
      "`outcome` column '", outcome, "' contains values not matching ",
      "positive_label/negative_label: ",
      paste(invalid, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (length(y_vals) < 2) {
    stop("`outcome` column '", outcome, "' has only one unique value. ",
         "A binary outcome is required.", call. = FALSE)
  }

  item_data <- data[, items, drop = FALSE]
  for (j in seq_along(items)) {
    if (anyNA(item_data[[j]])) {
      stop("Item column '", items[j], "' contains NA values. ",
           "Missing values are not supported in v0.1.", call. = FALSE)
    }
    if (!is.numeric(item_data[[j]])) {
      stop("Item column '", items[j], "' must be numeric.", call. = FALSE)
    }
  }

  # Convert outcome to 0/1
  y_binary <- ifelse(y == positive_label, 1L, 0L)

  prop_pos <- mean(y_binary)
  if (prop_pos < 0.05 || prop_pos > 0.95) {
    warning(
      "Outcome class proportion is extreme (",
      sprintf("%.1f%%", prop_pos * 100),
      " positive). Results may be unstable.",
      call. = FALSE
    )
  }

  list(
    data       = data,
    outcome_col = outcome,
    items      = items,
    y          = y_binary
  )
}

#' Enumerate all item combinations
#'
#' Generates all combinations of items from size `min_items` to `max_items`.
#'
#' @param items Character vector of item names.
#' @param min_items Minimum number of items per combination.
#' @param max_items Maximum number of items per combination.
#'
#' @return A list of character vectors, each a combination of item names.
#' @keywords internal
enumerate_combinations <- function(items, min_items = 1, max_items = 4) {
  n <- length(items)
  if (n == 0) {
    stop("`items` must contain at least one item.", call. = FALSE)
  }
  max_k <- min(max_items, n)
  if (min_items > max_k) {
    stop("`min_items` (", min_items, ") exceeds available items (", n, ").", call. = FALSE)
  }

  combos <- list()
  for (k in min_items:max_k) {
    cmb <- utils::combn(items, k, simplify = FALSE)
    combos <- c(combos, cmb)
  }
  combos
}

#' Format item vector to comma-separated string
#'
#' @param x Character vector of item names.
#' @return A single string like "q1, q2, q3".
#' @keywords internal
format_items <- function(x) {
  paste(x, collapse = ", ")
}

# ---- Constants for chunked storage and caching ----

AUTO_MEMORY_LIMIT  <- 100000L  # combos: <= this → memory, > this → disk
DEFAULT_CHUNK_SIZE <- 200000L  # combos per chunk
CACHE_FORMAT_VERSION <- 1L     # bump when cache storage format changes

# ---- Combinatorial unranking (lexicographic) ----

#' Compute total number of combinations without enumerating
#'
#' @param n Integer, number of items.
#' @param min_k Integer, minimum items per combination.
#' @param max_k Integer, maximum items per combination.
#' @return Integer, total number of combinations.
#' @keywords internal
.count_total_combos <- function(n, min_k, max_k) {
  max_k <- min(max_k, n)
  k <- seq.int(min_k, max_k)
  sum(choose(n, k))
}

#' Decide whether a search is large enough to need chunked evaluation
#'
#' @param n Integer, number of items.
#' @param min_items Integer.
#' @param max_items Integer.
#' @return TRUE if total combos > AUTO_MEMORY_LIMIT.
#' @keywords internal
.is_large_search <- function(n, min_items, max_items) {
  .count_total_combos(n, min_items, max_items) > AUTO_MEMORY_LIMIT
}

#' Resolve "auto" results_storage to effective mode
#'
#' @param n Integer, number of items.
#' @param min_items Integer.
#' @param max_items Integer.
#' @param resolved_mode Character, value from match.arg.
#' @return Character, effective storage mode ("memory", "rds", or "none").
#' @keywords internal
.resolve_auto_storage <- function(n, min_items, max_items, resolved_mode) {
  if (resolved_mode != "auto") return(resolved_mode)
  if (.is_large_search(n, min_items, max_items)) "rds" else "memory"
}

# ---- Combinatorial unranking (lexicographic) ----

#' Unrank a single combination (lexicographic order)
#'
#' Direct lexicographic unranking using 0-based ranks and 0-based item
#' indices. Matches `combn(0:(n-1), k)` column order exactly.
#'
#' At each position, iterate candidate values in ascending order. For each
#' candidate, calculate `choose(n - candidate - 1, remaining_slots)` — the
#' number of combinations beneath that prefix. If `rank < block_size`, select
#' the candidate. Otherwise subtract `block_size` and continue.
#'
#' @param n Integer, total items.
#' @param k Integer, items per combination (0 <= k <= n).
#' @param rank Integer, zero-based combination index (0 <= rank < choose(n, k)).
#' @return Integer vector of length k (0-based item indices).
#' @keywords internal
.combination_unrank <- function(n, k, rank) {
  if (length(n) != 1L || !is.numeric(n) || is.na(n) ||
      n < 0 || n != floor(n)) {
    stop("`n` must be a non-negative integer.", call. = FALSE)
  }
  if (length(k) != 1L || !is.numeric(k) || is.na(k) ||
      k < 0 || k > n || k != floor(k)) {
    stop("`k` must be an integer between 0 and n.", call. = FALSE)
  }
  total <- choose(n, k)
  if (length(rank) != 1L || !is.numeric(rank) || is.na(rank) ||
      rank < 0 || rank != floor(rank) || rank >= total) {
    stop(
      sprintf("`rank` must be an integer between 0 and %.0f.", total - 1),
      call. = FALSE
    )
  }

  if (k == 0L) return(integer())

  result <- integer(k)
  next_min <- 0L
  remaining_rank <- as.double(rank)

  for (position in seq_len(k)) {
    remaining_slots <- k - position
    max_value <- n - remaining_slots - 1L

    for (candidate in seq.int(next_min, max_value)) {
      block_size <- choose(n - candidate - 1L, remaining_slots)

      if (remaining_rank < block_size) {
        result[position] <- candidate
        next_min <- candidate + 1L
        break
      }

      remaining_rank <- remaining_rank - block_size
    }
  }

  result
}

#' Resolve a global combination rank to k and local rank
#'
#' Maps a global rank (index across multiple k-levels) to the correct
#' k and rank-within-k.
#'
#' @param n Integer, total items.
#' @param min_items Integer, minimum items per combination.
#' @param max_items Integer, maximum items per combination.
#' @param global_rank Numeric, zero-based global combination index.
#' @return A list with `k` (integer) and `rank_within_k` (numeric).
#' @keywords internal
.resolve_global_combination_rank <- function(n, min_items, max_items, global_rank) {
  remaining_rank <- as.double(global_rank)
  for (k in seq.int(min_items, max_items)) {
    level_size <- choose(n, k)
    if (remaining_rank < level_size) {
      return(list(k = k, rank_within_k = remaining_rank))
    }
    remaining_rank <- remaining_rank - level_size
  }
  stop("`global_rank` exceeds the total number of combinations.", call. = FALSE)
}

#' Enumerate a chunk of combinations using lexicographic unranking
#'
#' Generates a range of item-name combinations without materializing all
#' combinations. Uses cumulative choose(n, k) to map each global index to
#' the correct k, then unranks via .combination_unrank().
#'
#' @param items Character vector of item names.
#' @param min_items Integer, minimum items per combo.
#' @param max_items Integer, maximum items per combo.
#' @param chunk_start Numeric, zero-based global start index.
#' @param chunk_size Integer, maximum combos in this chunk.
#' @return A list of character vectors (may be shorter than chunk_size near end).
#' @keywords internal
.enumerate_combinations_chunk <- function(items, min_items, max_items,
                                           chunk_start, chunk_size) {
  n <- length(items)
  level_k <- seq.int(min_items, max_items)
  level_sizes <- choose(n, level_k)
  total <- sum(level_sizes)

  if (chunk_start < 0 || chunk_start >= total) {
    stop("`chunk_start` is outside the combination range.", call. = FALSE)
  }

  chunk_end <- min(as.double(chunk_start) + as.double(chunk_size), total)
  ranks <- seq(from = as.double(chunk_start), to = chunk_end - 1, by = 1)
  cumulative_ends <- cumsum(level_sizes)

  lapply(ranks, function(global_rank) {
    level_index <- which(global_rank < cumulative_ends)[1L]
    k <- level_k[level_index]
    level_start <- if (level_index == 1L) 0 else cumulative_ends[level_index - 1L]
    local_rank <- global_rank - level_start

    indices <- .combination_unrank(n = n, k = k, rank = local_rank)
    items[indices + 1L]
  })
}
