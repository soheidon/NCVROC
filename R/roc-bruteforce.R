# roc-bruteforce.R — Exhaustive ROC without cross-validation
#
# Internal:
#   .roc_bruteforce_impl()
#
# Exported:
#   roc_bruteforce()
#   roc_bf()
#   print.roc_bruteforce_result()

# ---- Internal implementation ----

.roc_bruteforce_impl <- function(data, outcome_name, item_names,
                                 min_items, max_items,
                                 cutoff_method,
                                 positive_label, negative_label,
                                 engine, rank_by, top_n,
                                 progress,
                                 save_results, output_dir,
                                 results_storage, results_name, results_dir,
                                 item_count = NULL,
                                 min_items_missing = TRUE,
                                 max_items_missing = TRUE) {

  # ---- Data preparation ----
  n_original <- nrow(data)
  analysis_dat <- .prepare_ncvroc_data(data, outcome_name, item_names)
  n_analyzed <- nrow(analysis_dat)

  # ---- item_count validation and resolution ----
  if (!is.null(item_count) &&
      (!min_items_missing || !max_items_missing)) {
    stop("Do not specify item_count together with min_items or max_items.",
         call. = FALSE)
  }

  resolved_item_count <- NULL

  if (!is.null(item_count)) {
    parsed <- .parse_item_count(item_count, n_available = length(item_names))
    min_items <- parsed$min_items
    max_items <- parsed$max_items
    resolved_item_count <- parsed$specification
  }

  # ---- Argument validation ----
  engine          <- match.arg(engine,          c("Rcpp", "R"))
  rank_by         <- match.arg(rank_by,         c("auc", "youden", "sensitivity", "specificity", "accuracy"))
  results_storage <- match.arg(results_storage, c("rds", "memory", "none"))

  if (!is.null(top_n)) {
    if (length(top_n) != 1 || !is.numeric(top_n) || is.na(top_n) ||
        top_n < 0 || top_n != floor(top_n)) {
      stop("top_n must be NULL, 0, or a single non-negative integer.", call. = FALSE)
    }
  }

  # ---- Core calculation ----
  full_results <- exhaustive_sum_roc(
    data                = analysis_dat,
    outcome             = outcome_name,
    items               = item_names,
    min_items           = min_items,
    max_items           = max_items,
    positive_label      = positive_label,
    negative_label      = negative_label,
    cutoff_method       = cutoff_method,
    rank_by             = rank_by,
    top_n               = NULL,
    prefer_fewer_items  = TRUE,
    engine              = engine,
    progress            = progress
  )

  # ---- Derived values (before any mutation) ----
  n_combinations <- nrow(full_results)

  # ---- Slice ----
  best_model <- if (nrow(full_results) > 0) {
    full_results[1, , drop = FALSE]
  } else {
    full_results[0, , drop = FALSE]
  }

  if (is.null(top_n)) {
    candidates <- full_results
  } else if (top_n == 0) {
    candidates <- full_results[0, , drop = FALSE]
  } else {
    candidates <- utils::head(full_results, top_n)
  }

  # ---- Save (CSV export, unchanged from v0.8.0) ----
  if (save_results) {
    ok <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) {
      stop("Could not create output_dir: ", output_dir, call. = FALSE)
    }
    utils::write.csv(full_results,
      file.path(output_dir, "roc_bruteforce_results.csv"), row.names = FALSE)
    if (nrow(candidates) > 0) {
      utils::write.csv(candidates,
        file.path(output_dir, "roc_bruteforce_candidates.csv"), row.names = FALSE)
    }
    if (nrow(best_model) > 0) {
      utils::write.csv(best_model,
        file.path(output_dir, "roc_bruteforce_best_model.csv"), row.names = FALSE)
    }
  }

  # ---- Results storage (RDS / memory / none) ----
  metadata <- list(
    function_name   = "roc_bruteforce",
    outcome         = outcome_name,
    items           = item_names,
    n_items         = length(item_names),
    min_items       = min_items,
    max_items       = max_items,
    item_count      = resolved_item_count,
    rank_by         = rank_by,
    cutoff_method   = cutoff_method,
    positive_label  = positive_label,
    negative_label  = negative_label,
    engine          = engine,
    created_at      = Sys.time(),
    package_version = as.character(utils::packageVersion("NCVROC"))
  )

  results_file <- NULL

  if (results_storage == "rds") {
    results_file <- .make_results_path(
      results_dir  = results_dir,
      prefix       = "roc_bruteforce",
      outcome      = outcome_name,
      n_items      = length(item_names),
      min_items    = min_items,
      max_items    = max_items,
      rank_by      = rank_by,
      results_name = results_name
    )
    attr(full_results, "ncvroc_metadata") <- metadata
    saveRDS(full_results, results_file)
    results <- NULL
  } else if (results_storage == "memory") {
    attr(full_results, "ncvroc_metadata") <- metadata
    results <- full_results
    results_file <- NULL
  } else {
    results <- NULL
    results_file <- NULL
  }

  if (results_storage != "memory") {
    rm(full_results)
  }

  # ---- Return ----
  structure(list(
    results         = results,
    results_file    = results_file,
    results_storage = results_storage,
    candidates      = candidates,
    best_model      = best_model,
    outcome         = outcome_name,
    items           = item_names,
    min_items       = min_items,
    max_items       = max_items,
    item_count      = resolved_item_count,
    cutoff_method   = cutoff_method,
    positive_label  = positive_label,
    negative_label  = negative_label,
    engine          = engine,
    rank_by         = rank_by,
    top_n           = top_n,
    n_original      = n_original,
    n_analyzed      = n_analyzed,
    n_combinations  = n_combinations
  ), class = "roc_bruteforce_result")
}

# ---- Public functions ----

#' Exhaustive item-combination ROC analysis without cross-validation
#'
#' Evaluates all requested item combinations directly on the full dataset
#' and returns the best-performing item subsets by ROC metrics.
#'
#' `roc_bruteforce()` reports performance on the same data used to select
#' items and cutoffs. These estimates are *apparent* (resubstitution)
#' performance and may be optimistic. Use `ncvroc()` for nested
#' cross-validated performance estimation.
#'
#' @param data A data.frame containing outcome and item columns.
#' @param outcome Column name (bare symbol or character string).
#' @param items Candidate items: bare range (`Q1:Q112`), bare names with
#'   `c()`, character vector, existing variable, or numeric positions.
#' @param min_items Integer, minimum items per combination (default 1).
#' @param max_items Integer, maximum items per combination (default 4).
#' @param cutoff_method Cutoff method: `"youden"` or `"closest_topleft"`.
#' @param positive_label Value for positive class (default 1).
#' @param negative_label Value for negative class (default 0).
#' @param engine Computation engine: `"R"` or `"Rcpp"` (default `"Rcpp"`).
#' @param rank_by Metric for ranking: `"auc"`, `"youden"`, `"sensitivity"`,
#'   `"specificity"`, or `"accuracy"`.
#' @param top_n Number of top candidates to return (NULL for all, 0 for none).
#' @param progress Logical, show progress bar (default `interactive()`).
#' @param save_results Logical, write CSV outputs (default FALSE).
#' @param output_dir Directory for saved CSVs (default `"."`).
#' @param results_storage Where to store the full results table:
#'   `"rds"` (save to RDS file, default), `"memory"` (keep in RAM), or
#'   `"none"` (discard).
#' @param results_name Optional label prefix for the RDS filename. Analysis
#'   conditions (item count, k range, rank_by) are always appended.
#' @param results_dir Directory for the full results RDS file, or NULL
#'   (default) to use the current working directory.
#' @param item_count Concise model-size specification: `"==4"` (exactly 4
#'   items), `"<=4"` (up to 4 items), or `"2:4"` (2 through 4 items).
#'   Cannot be combined with `min_items` or `max_items`. Default NULL.
#'
#' @return An object of class `"roc_bruteforce_result"`. Use `$results`
#'   for the full table, `$candidates` for the `top_n` subset, and
#'   `$best_model` for the top-ranked model.
#' @export
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   y  = rep(c(0, 1), each = 20),
#'   Q1 = rbinom(40, 2, 0.4),
#'   Q2 = rbinom(40, 2, 0.5),
#'   Q3 = rbinom(40, 2, 0.6)
#' )
#' result <- roc_bruteforce(d, y, Q1:Q3, max_items = 2, engine = "R",
#'   top_n = 5, progress = FALSE)
#' result
#' result$best_model
#' result$candidates
roc_bruteforce <- function(data,
                           outcome,
                           items,
                           min_items = 1,
                           max_items = 4,
                           cutoff_method = "youden",
                           positive_label = 1,
                           negative_label = 0,
                           engine = c("Rcpp", "R"),
                           rank_by = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                           top_n = 20,
                           progress = interactive(),
                           save_results = FALSE,
                           output_dir = ".",
                           results_storage = c("rds", "memory", "none"),
                           results_name    = NULL,
                           results_dir     = NULL,
                           item_count      = NULL) {

  caller_env   <- parent.frame()
  outcome_expr <- substitute(outcome)
  items_expr   <- substitute(items)
  outcome_name <- .resolve_outcome(outcome_expr, caller_env)
  item_names   <- .resolve_items(data, items_expr, caller_env)

  min_items_missing <- missing(min_items)
  max_items_missing <- missing(max_items)

  # Validate new arguments early
  results_storage <- match.arg(results_storage)

  if (!is.null(results_name) &&
      (length(results_name) != 1L || !is.character(results_name) ||
       is.na(results_name) || !nzchar(trimws(results_name)))) {
    stop("results_name must be NULL or a single non-empty character string.",
         call. = FALSE)
  }

  if (!is.null(results_dir) &&
      (length(results_dir) != 1L || !is.character(results_dir) ||
       is.na(results_dir) || !nzchar(trimws(results_dir)))) {
    stop("results_dir must be NULL or a single non-empty path.",
         call. = FALSE)
  }

  .roc_bruteforce_impl(
    data               = data,
    outcome_name       = outcome_name,
    item_names         = item_names,
    min_items          = min_items,
    max_items          = max_items,
    cutoff_method      = cutoff_method,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    rank_by            = rank_by,
    top_n              = top_n,
    progress           = progress,
    save_results       = save_results,
    output_dir         = output_dir,
    results_storage    = results_storage,
    results_name       = results_name,
    results_dir        = results_dir,
    item_count         = item_count,
    min_items_missing  = min_items_missing,
    max_items_missing  = max_items_missing
  )
}

#' @rdname roc_bruteforce
#' @export
roc_bf <- function(data,
                   outcome,
                   items,
                   min_items = 1,
                   max_items = 4,
                   cutoff_method = "youden",
                   positive_label = 1,
                   negative_label = 0,
                   engine = c("Rcpp", "R"),
                   rank_by = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                   top_n = 20,
                   progress = interactive(),
                   save_results = FALSE,
                   output_dir = ".",
                   results_storage = c("rds", "memory", "none"),
                   results_name    = NULL,
                   results_dir     = NULL,
                   item_count      = NULL) {

  caller_env   <- parent.frame()
  outcome_expr <- substitute(outcome)
  items_expr   <- substitute(items)
  outcome_name <- .resolve_outcome(outcome_expr, caller_env)
  item_names   <- .resolve_items(data, items_expr, caller_env)

  min_items_missing <- missing(min_items)
  max_items_missing <- missing(max_items)

  # Validate new arguments early
  results_storage <- match.arg(results_storage)

  if (!is.null(results_name) &&
      (length(results_name) != 1L || !is.character(results_name) ||
       is.na(results_name) || !nzchar(trimws(results_name)))) {
    stop("results_name must be NULL or a single non-empty character string.",
         call. = FALSE)
  }

  if (!is.null(results_dir) &&
      (length(results_dir) != 1L || !is.character(results_dir) ||
       is.na(results_dir) || !nzchar(trimws(results_dir)))) {
    stop("results_dir must be NULL or a single non-empty path.",
         call. = FALSE)
  }

  .roc_bruteforce_impl(
    data               = data,
    outcome_name       = outcome_name,
    item_names         = item_names,
    min_items          = min_items,
    max_items          = max_items,
    cutoff_method      = cutoff_method,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    rank_by            = rank_by,
    top_n              = top_n,
    progress           = progress,
    save_results       = save_results,
    output_dir         = output_dir,
    results_storage    = results_storage,
    results_name       = results_name,
    results_dir        = results_dir,
    item_count         = item_count,
    min_items_missing  = min_items_missing,
    max_items_missing  = max_items_missing
  )
}

# ---- Print method ----

#' Print a brute-force ROC search result
#'
#' @param x A `roc_bruteforce_result` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @export
print.roc_bruteforce_result <- function(x, ...) {
  cat("NCVROC brute-force ROC search\n\n")
  cat("Analyzed observations:", x$n_analyzed, "\n")
  if (x$n_original != x$n_analyzed) {
    cat("  (", x$n_original - x$n_analyzed,
        " rows removed due to missing values)\n", sep = "")
  }
  cat("Candidate items:              ", length(x$items), "\n")
  cat("Item combinations evaluated:  ",
      format(x$n_combinations, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Engine:                       ", x$engine, "\n")
  if (!is.null(x$item_count)) {
    cat("Item count:                   ", .describe_item_count(x$item_count),
        " (", x$item_count, ")\n", sep = "")
  }
  cat("Ranked by:                    ", x$rank_by, "\n")

  # Storage info (never loads RDS)
  if (!is.null(x$results_storage)) {
    if (x$results_storage == "rds") {
      if (!is.null(x$results_file)) {
        if (!file.exists(x$results_file)) {
          cat("Full results: stored RDS file is missing\n")
        } else {
          cat("Full results: stored in ", x$results_file, "\n", sep = "")
        }
      }
    } else if (x$results_storage == "none") {
      cat("Full results: not stored\n")
    }
  }

  cat("\n")

  if (!is.null(x$best_model) && nrow(x$best_model) > 0) {
    cat("Best model:\n")
    print(x$best_model)
    cat("\n")
  }

  n_cand <- nrow(x$candidates)
  if (n_cand > 0) {
    cat("Candidates (", n_cand, " rows):\n", sep = "")
    print(if (is.null(x$top_n) && n_cand > 20) {
      utils::head(x$candidates, 20)
    } else {
      x$candidates
    })
  }

  cat("\nPerformance is calculated on the same data used for item and cutoff\n")
  cat("selection and may be optimistic. Use ncvroc() for nested cross-validated\n")
  cat("performance estimation.\n")

  invisible(x)
}
