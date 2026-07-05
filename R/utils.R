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
