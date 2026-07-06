# count_item_combinations.R - Combinatorial counting and preselection helpers
#
# Exported:
#   count_item_combinations()
#   suggest_preselect_top_n()

# Internal helper: parse items_or_n into a single integer n
.parse_n_items <- function(items_or_n) {
  if (is.character(items_or_n)) {
    length(items_or_n)
  } else if (is.numeric(items_or_n) && length(items_or_n) == 1) {
    as.integer(items_or_n)
  } else {
    stop("items_or_n must be a character vector or a single integer.", call. = FALSE)
  }
}

#' Count total item combinations
#'
#' Computes the total number of k-item combinations without generating them,
#' using `choose(n, k)`.
#'
#' @param items_or_n Character vector of item names, or a single integer n.
#' @param min_items Minimum items per combination (default 1).
#' @param max_items Maximum items per combination (default 4).
#' @param detail If FALSE, returns a single numeric value (total).
#'   If TRUE, returns a data.frame with columns `n_items` and `n_combinations`.
#'
#' @return A single numeric value or a data.frame.
#' @export
#'
#' @examples
#' count_item_combinations(103, max_items = 4)
#' count_item_combinations(103, max_items = 4, detail = TRUE)
#' count_item_combinations(letters[1:5], max_items = 10)
count_item_combinations <- function(items_or_n,
                                    min_items = 1,
                                    max_items = 4,
                                    detail = FALSE) {
  n <- .parse_n_items(items_or_n)
  max_items <- min(max_items, n)

  ks <- seq.int(min_items, max_items)
  counts <- choose(n, ks)

  if (detail) {
    return(data.frame(
      n_items        = ks,
      n_combinations = counts,
      stringsAsFactors = FALSE
    ))
  }

  sum(counts)
}

#' Suggest preselect_top_n based on analysis mode
#'
#' Suggests a `preselect_top_n` value given the total number of item
#' combinations and the desired analysis mode.
#'
#' @param items_or_n Character vector of item names, or a single integer n.
#' @param min_items Minimum items per combination (default 1).
#' @param max_items Maximum items per combination (default 4).
#' @param mode One of `"quick"`, `"balanced"`, `"thorough"`, or `"exhaustive"`.
#'   Default `"balanced"`.
#'
#' @return A single integer: the suggested `preselect_top_n`.
#' @export
#'
#' @examples
#' suggest_preselect_top_n(103, max_items = 4, mode = "balanced")
#' suggest_preselect_top_n(5, max_items = 4, mode = "exhaustive")
suggest_preselect_top_n <- function(items_or_n,
                                    min_items = 1,
                                    max_items = 4,
                                    mode = c("balanced", "quick", "thorough", "exhaustive")) {
  mode <- match.arg(mode)
  total <- count_item_combinations(items_or_n, min_items, max_items)

  limits <- c(quick = 100, balanced = 500, thorough = 1000, exhaustive = Inf)
  min(total, limits[mode])
}
