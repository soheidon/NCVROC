# ncvroc_config.R - Configuration object for NCVROC analyses
#
# Exported:
#   ncvroc_config()
#   print.ncvroc_config()

#' Create an NCVROC configuration object
#'
#' Bundles all analysis parameters into a single configuration object.
#' Use `run_ncvroc()` to execute the analysis from this config.
#'
#' @param outcome Character, column name of the binary outcome variable.
#' @param items Character vector of candidate item names, or NULL.
#' @param min_items Integer, minimum items per combination (default 1).
#' @param max_items Integer, maximum items per combination (default 4).
#' @param mode Analysis mode: `"quick"`, `"balanced"`, `"thorough"`, or
#'   `"exhaustive"`. Default `"balanced"`.
#' @param outer_k Number of outer CV folds (default 5).
#' @param inner_k Number of inner CV folds (default 4).
#' @param outer_repeats Number of outer CV repeats (default 5).
#' @param inner_repeats Number of inner CV repeats (default 1).
#' @param preselect_top_n Number of top candidates to preselect, or NULL
#'   to auto-set based on `mode`.
#' @param preselect_by Metric for preselection (default `"auc"`).
#' @param selection_criterion Metric for model selection (default `"auc"`).
#' @param cutoff_method Cutoff method: `"youden"` or `"closest_topleft"`.
#' @param positive_label Value for positive class (default 1).
#' @param negative_label Value for negative class (default 0).
#' @param stratified Logical, use stratified CV folds (default TRUE).
#' @param engine Computation engine: `"R"` or `"Rcpp"` (default `"Rcpp"`).
#' @param chunk_size Integer, combinations per chunk (default 200000).
#' @param cache One of `"off"`, `"reuse"`, or `"refresh"` (default `"off"`).
#' @param cache_dir Directory for caching, or NULL.
#' @param item_count Concise model-size specification: `"==4"` (exactly 4
#'   items), `"<=4"` (up to 4 items), or `"2:4"` (2 through 4 items).
#'   Cannot be combined with `min_items` or `max_items`. Default NULL.
#'
#' @return A list of class `"ncvroc_config"`.
#' @export
#'
#' @examples
#' cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2, mode = "quick")
#' print(cfg)
ncvroc_config <- function(outcome,
                          items = NULL,
                          min_items = 1,
                          max_items = 4,
                          mode = c("balanced", "quick", "thorough", "exhaustive"),
                          outer_k = 5,
                          inner_k = 4,
                          outer_repeats = 5,
                          inner_repeats = 1,
                          preselect_top_n = NULL,
                          preselect_by = "auc",
                          selection_criterion = "auc",
                          cutoff_method = c("youden", "closest_topleft"),
                          positive_label = 1,
                          negative_label = 0,
                          stratified = TRUE,
                          engine = c("Rcpp", "R"),
                          chunk_size = 200000L,
                          cache = c("off", "reuse", "refresh"),
                          cache_dir = NULL,
                          item_count = NULL) {
  mode <- match.arg(mode)
  cutoff_method <- match.arg(cutoff_method)
  engine <- match.arg(engine)
  cache <- match.arg(cache)

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || chunk_size <= 0L ||
      chunk_size != floor(chunk_size)) {
    stop("chunk_size must be a positive integer.", call. = FALSE)
  }
  chunk_size <- as.integer(chunk_size)

  # item_count validation and resolution
  min_items_missing <- missing(min_items)
  max_items_missing <- missing(max_items)

  if (!is.null(item_count) &&
      (!min_items_missing || !max_items_missing)) {
    stop("Do not specify item_count together with min_items or max_items.",
         call. = FALSE)
  }

  resolved_item_count <- NULL

  if (!is.null(item_count)) {
    n_available <- if (is.null(items)) NULL else length(items)
    parsed <- .parse_item_count(item_count, n_available = n_available)
    min_items <- parsed$min_items
    max_items <- parsed$max_items
    resolved_item_count <- parsed$specification
  }

  if (!is.null(items)) {
    n_items <- length(items)
    total_combinations <- count_item_combinations(items, min_items, max_items)

    if (is.null(preselect_top_n)) {
      preselect_top_n <- suggest_preselect_top_n(items, min_items, max_items, mode)
    } else {
      preselect_top_n <- min(preselect_top_n, total_combinations)
    }
  } else {
    n_items <- NA_integer_
    total_combinations <- NA_real_
    preselect_top_n <- NULL
  }

  config <- list(
    outcome             = outcome,
    items               = items,
    min_items           = min_items,
    max_items           = max_items,
    item_count          = resolved_item_count,
    mode                = mode,
    n_items             = n_items,
    total_combinations  = total_combinations,
    outer_k             = outer_k,
    inner_k             = inner_k,
    outer_repeats       = outer_repeats,
    inner_repeats       = inner_repeats,
    preselect_top_n     = preselect_top_n,
    preselect_by        = preselect_by,
    selection_criterion = selection_criterion,
    cutoff_method       = cutoff_method,
    positive_label      = positive_label,
    negative_label      = negative_label,
    stratified          = stratified,
    engine              = engine,
    chunk_size          = chunk_size,
    cache               = cache,
    cache_dir           = cache_dir
  )

  class(config) <- "ncvroc_config"
  config
}

#' Print an ncvroc_config object
#'
#' @param x An `ncvroc_config` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @export
print.ncvroc_config <- function(x, ...) {
  cat("-- NCVROC Configuration ", paste(rep("-", 46), collapse = ""), "\n", sep = "")
  cat("Outcome:         ", x$outcome, "\n")

  if (!is.null(x$items)) {
    cat("Items:           ", x$n_items, "\n")
    cat("Item set size:   ", x$min_items, "-", x$max_items, "\n")
    if (!is.null(x$item_count)) {
      cat("Item count:      ", .describe_item_count(x$item_count),
          " (", x$item_count, ")\n", sep = "")
    }
    cat("Combinations:    ", format(x$total_combinations, big.mark = ",", scientific = FALSE), "\n")
  } else {
    cat("Items:            (not specified)\n")
    cat("Item set size:   ", x$min_items, "-", x$max_items, "\n")
    if (!is.null(x$item_count)) {
      cat("Item count:      ", .describe_item_count(x$item_count),
          " (", x$item_count, ")\n", sep = "")
    }
  }

  cat("Mode:            ", x$mode, "\n")

  preselect_str <- if (is.null(x$preselect_top_n)) {
    "(not set)"
  } else {
    format(x$preselect_top_n, big.mark = ",", scientific = FALSE)
  }
  cat("Preselect top n: ", preselect_str, "\n")

  if (!is.null(x$preselect_top_n) && x$preselect_top_n >= 100000) {
    cat("[!] Warning: preselect_top_n >= 100,000 - inner CV will be slow.\n")
  }

  cat("CV:              ", x$outer_k, "-fold outer x ",
      x$outer_repeats, " repeats | ",
      x$inner_k, "-fold inner x ",
      x$inner_repeats, " repeats\n", sep = "")
  cat("Labels:           positive =", x$positive_label,
      "| negative =", x$negative_label, "\n")
  cat("Stratified:      ", x$stratified, "\n")
  cat("Engine:          ", x$engine, "\n")
  cat("Chunk size:      ", format(x$chunk_size, big.mark = ",", scientific = FALSE), "\n")
  cat("Cache:           ", x$cache, "\n")
  if (!is.null(x$cache_dir)) {
    cat("Cache dir:       ", x$cache_dir, "\n")
  }
  cat(paste(rep("-", 50), collapse = ""), "\n", sep = "")

  invisible(x)
}
