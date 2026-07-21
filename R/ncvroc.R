# ncvroc.R - Main entry function with formula-like item selection
#
# Internal helpers:
#   .resolve_outcome()
#   .resolve_items()
#   .parse_condition()
#   .parse_item_count()
#   .describe_item_count()
#   .make_results_path()
#   .read_results_from_storage()
#   .evaluate_final_exhaustive()
#
# Exported:
#   ncvroc()
#   print.ncvroc_analysis()
#   plot.ncvroc_analysis()
#   ncvroc_results()

# ---- Internal helpers ----

#' Resolve outcome argument to a single column name
#'
#' @param outcome_expr Unevaluated expression from substitute(outcome).
#' @param env The caller's environment.
#' @return A single character column name.
#' @keywords internal
.resolve_outcome <- function(outcome_expr, env) {
  val <- try(eval(outcome_expr, envir = env), silent = TRUE)
  if (!inherits(val, "try-error") && is.character(val) && length(val) == 1) {
    return(val)
  }
  deparse(outcome_expr)
}

#' Prepare NCVROC analysis data with factor-safe numeric conversion
#'
#' Converts outcome and item columns to numeric, treating factors safely
#' (via as.character() before as.numeric()), rejecting non-numeric values,
#' and removing rows with missing values.
#'
#' @param data A data.frame.
#' @param outcome_name Character column name for the outcome.
#' @param item_names Character vector of item column names.
#' @return A data.frame with only the selected columns, all numeric.
#' @keywords internal
.prepare_ncvroc_data <- function(data, outcome_name, item_names) {
  selected <- data[c(outcome_name, item_names)]

  safe_numeric <- function(x, name) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
      converted <- suppressWarnings(as.numeric(x))
      invalid <- !is.na(x) & is.na(converted)
      if (any(invalid)) {
        stop("Column '", name, "' contains non-numeric values.", call. = FALSE)
      }
      return(converted)
    }
    if (!is.numeric(x)) {
      stop("Column '", name, "' must be numeric or coercible to numeric.", call. = FALSE)
    }
    as.numeric(x)
  }

  selected[[outcome_name]] <- safe_numeric(selected[[outcome_name]], outcome_name)
  selected[item_names] <- Map(safe_numeric, selected[item_names], names(selected[item_names]))

  selected[complete.cases(selected), , drop = FALSE]
}

#' Resolve items argument to a character vector of column names
#'
#' Supports bare column ranges (Q1:Q112), bare names with c(),
#' character vector literals, existing variables, and numeric positions.
#'
#' @param data A data.frame.
#' @param items_expr Unevaluated expression from substitute(items).
#' @param env The caller's environment.
#' @return Character vector of column names.
#' @keywords internal
.resolve_items <- function(data, items_expr, env) {
  # Phase 1: literal evaluation (character vectors, variables, numeric ranges)
  val <- try(eval(items_expr, envir = env), silent = TRUE)
  if (!inherits(val, "try-error")) {
    if (is.character(val)) return(val)
    if (is.numeric(val) && length(val) > 0) return(names(data)[val])
  }

  # Phase 2: dummy data.frame + subset() for bare column expressions
  j <- seq_along(data)
  names(j) <- names(data)
  dummy <- as.data.frame(as.list(j), stringsAsFactors = FALSE)
  selected <- eval(
    substitute(subset(DUMMY, select = EXPR), list(DUMMY = dummy, EXPR = items_expr))
  )
  names(selected)
}

# ---- item_count parser ----

#' Parse an item_count specification string
#'
#' Parses a concise string like `"==4"`, `"<=4"`, or `"2:4"` into resolved
#' `min_items` and `max_items` values.
#'
#' @param item_count A single character string, or NULL.
#' @param n_available Integer count of available candidate items, or NULL to
#'   defer validation (used by `ncvroc_config()` when `items = NULL`).
#' @return A list with `min_items`, `max_items`, and `specification` (the
#'   normalized value), or NULL if `item_count` is NULL.
#' @keywords internal
.parse_item_count <- function(item_count, n_available = NULL) {
  if (is.null(item_count)) return(NULL)

  if (length(item_count) != 1L || !is.character(item_count) ||
      is.na(item_count) || !nzchar(trimws(item_count))) {
    stop("item_count must be NULL or a single condition such as ",
         "'==4', '<=4', or '2:4'.", call. = FALSE)
  }

  value <- gsub("\\s+", "", item_count)

  if (grepl("^==[0-9]+$", value)) {
    n <- as.integer(sub("^==", "", value))
    min_items <- n; max_items <- n
  } else if (grepl("^<=[0-9]+$", value)) {
    n <- as.integer(sub("^<=", "", value))
    min_items <- 1L; max_items <- n
  } else if (grepl("^[0-9]+:[0-9]+$", value)) {
    parts <- as.integer(strsplit(value, ":", fixed = TRUE)[[1L]])
    min_items <- parts[[1L]]; max_items <- parts[[2L]]
  } else {
    stop("Unsupported item_count specification: '", item_count,
         "'. Use formats such as '==4', '<=4', or '2:4'.", call. = FALSE)
  }

  if (min_items < 1L)
    stop("item_count must select at least one item.", call. = FALSE)
  if (min_items > max_items)
    stop("The lower item count cannot exceed the upper item count.", call. = FALSE)
  if (!is.null(n_available) && max_items > n_available)
    stop("item_count requests up to ", max_items, " items, but only ",
         n_available, " candidate items are available.", call. = FALSE)

  list(min_items = min_items, max_items = max_items, specification = value)
}

#' Describe an item_count specification in plain language
#'
#' Converts a normalized specification string (e.g. `"==4"`) into a
#' human-readable description for print methods.
#'
#' @param specification A normalized item_count specification string, or NULL.
#' @return A character string like `"exactly 4"`, `"up to 4"`, or `"2 to 4"`,
#'   or NULL if specification is NULL.
#' @keywords internal
.describe_item_count <- function(specification) {
  if (is.null(specification)) return(NULL)
  if (grepl("^==", specification))
    return(paste0("exactly ", sub("^==", "", specification)))
  if (grepl("^<=", specification))
    return(paste0("up to ", sub("^<=", "", specification)))
  if (grepl("^[0-9]+:[0-9]+$", specification))
    return(gsub(":", " to ", specification, fixed = TRUE))
  specification
}

# ---- Results storage helpers ----

#' Generate a unique RDS file path for storing full candidate results
#'
#' Creates a timestamped, uniquely-named RDS file path. Uses `tempfile()` for
#' uniqueness to avoid consuming `.Random.seed`.
#'
#' @param results_dir Directory for the RDS file, or NULL for tempdir().
#' @param prefix Function name prefix (e.g. "roc_bruteforce").
#' @param outcome Outcome column name.
#' @param n_items Number of candidate items.
#' @param min_items Minimum items per combination.
#' @param max_items Maximum items per combination.
#' @param rank_by Ranking metric.
#' @param results_name Optional label prefix for the filename.
#' @return A unique file path (character string).
#' @keywords internal
.make_results_path <- function(results_dir, prefix, outcome, n_items,
                                min_items, max_items, rank_by,
                                results_name = NULL) {
  if (is.null(results_dir)) {
    results_dir <- tempdir()
  }
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(results_dir)) {
    stop("Could not create results_dir: ", results_dir, call. = FALSE)
  }

  if (is.null(results_name)) {
    base <- paste0(prefix, "_", outcome)
  } else {
    base <- results_name
  }
  base <- paste0(base, "_p", n_items, "_k", min_items, "-", max_items, "_", rank_by)
  base <- gsub("[^A-Za-z0-9_-]+", "_", base)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  unique_stub <- basename(tempfile(pattern = ""))
  unique_stub <- gsub("[^A-Za-z0-9]+", "", unique_stub)

  file.path(results_dir, paste0(base, "_", timestamp, "_", unique_stub, ".rds"))
}

#' Read full candidate results from memory or storage
#'
#' Returns the full candidate table, reading from disk if necessary.
#' Errors if results are not available.
#'
#' @param dat In-memory data.frame or NULL.
#' @param file Path to RDS file or NULL.
#' @param chunk_dir Path to chunk directory or NULL.
#' @param storage_backend Character, "memory", "single_rds", "chunked_rds", or "none".
#' @param context Label for error messages.
#' @return A data.frame of candidate results.
#' @keywords internal
.read_results_from_storage <- function(dat, file, chunk_dir = NULL,
                                        storage_backend = NULL, context = "results") {
  if (!is.null(dat)) return(dat)

  # chunked_rds: need allow_full_load=TRUE to get here, use .full_load_chunked
  if (!is.null(chunk_dir) && dir.exists(chunk_dir)) {
    return(.full_load_chunked(chunk_dir))
  }

  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("The stored ", context, " file no longer exists: ", file, call. = FALSE)
    }
    return(readRDS(file))
  }
  stop("Full ", context, " are not available. Re-run the analysis with ",
       'results_storage = "memory" or "rds".', call. = FALSE)
}

# ---- Final exhaustive search (chunked or full) ----

#' Evaluate the final exhaustive search, optionally chunked
#'
#' If n_combos > AUTO_MEMORY_LIMIT, evaluates in chunks and writes each to a
#' chunk directory. If caching is enabled, chunks go directly into the
#' .building-<pid>/chunks/ directory. Otherwise they go to tempdir().
#'
#' @param analysis_dat data.frame, prepared analysis data.
#' @param outcome_name Character, outcome column name.
#' @param item_names Character vector, item column names.
#' @param min_items Integer.
#' @param max_items Integer.
#' @param positive_label Scalar.
#' @param negative_label Scalar.
#' @param cutoff_method Character.
#' @param final_rank_by Character.
#' @param engine Character.
#' @param progress Logical.
#' @param storage_mode Character: "memory", "rds", or "none".
#' @param results_dir Character or NULL.
#' @param results_name Character or NULL.
#' @param chunk_size Integer.
#' @param cache Character: "off", "reuse", or "refresh".
#' @param cache_dir Character or NULL.
#' @param building_dir Character or NULL — if caching, the .building-<pid>/ dir.
#' @param resolved_item_count Character or NULL.
#'
#' @return A list with elements: final_exhaustive_ranked (data.frame or NULL),
#'   final_exhaustive_file (path or NULL), final_n_combinations (integer),
#'   storage_backend (character), chunk_dir (path or NULL),
#'   chunk_prefix (character), results_name (character or NULL).
#' @keywords internal
.evaluate_final_exhaustive <- function(analysis_dat, outcome_name, item_names,
                                        min_items, max_items,
                                        positive_label, negative_label,
                                        cutoff_method, final_rank_by, engine,
                                        progress,
                                        storage_mode, results_dir, results_name,
                                        chunk_size,
                                        cache, cache_dir, building_dir,
                                        resolved_item_count,
                                        function_name = "ncvroc") {
  total_combos <- .count_total_combos(length(item_names), min_items, max_items)
  is_large <- total_combos > AUTO_MEMORY_LIMIT

  # Resolve effective storage mode
  if (storage_mode == "auto") {
    effective_storage <- if (is_large) "chunked_rds" else "memory"
  } else if (storage_mode == "rds") {
    effective_storage <- if (is_large) "chunked_rds" else "single_rds"
  } else if (storage_mode == "memory") {
    effective_storage <- "memory"
  } else {
    effective_storage <- "none"
  }

  metadata <- list(
    function_name   = function_name,
    outcome         = outcome_name,
    items           = item_names,
    n_items         = length(item_names),
    min_items       = min_items,
    max_items       = max_items,
    item_count      = resolved_item_count,
    rank_by         = final_rank_by,
    cutoff_method   = cutoff_method,
    positive_label  = positive_label,
    negative_label  = negative_label,
    engine          = engine,
    created_at      = Sys.time(),
    package_version = as.character(utils::packageVersion("NCVROC"))
  )

  # --- None storage: still evaluate (for final_candidates/final_model) but discard ---
  if (effective_storage == "none") {
    if (!is_large) {
      final_exhaustive_ranked <- exhaustive_sum_roc(
        data              = analysis_dat,
        outcome           = outcome_name,
        items             = item_names,
        min_items         = min_items,
        max_items         = max_items,
        positive_label    = positive_label,
        negative_label    = negative_label,
        cutoff_method     = cutoff_method,
        rank_by           = final_rank_by,
        top_n             = NULL,
        prefer_fewer_items = TRUE,
        engine            = engine,
        progress          = progress
      )
      return(list(
        final_exhaustive_ranked = NULL,
        final_exhaustive_file   = NULL,
        final_n_combinations    = nrow(final_exhaustive_ranked),
        storage_backend         = "none",
        chunk_dir               = NULL,
        chunk_prefix            = NULL,
        results_name            = NULL,
        full_table_for_slicing  = final_exhaustive_ranked
      ))
    }

    # Large + none: stream chunks, keep only top-ranked in a buffer
    # for slicing final_model/final_candidates. Use streaming_top_n logic.
    chunk_start <- 0.0
    best_so_far <- NULL
    while (chunk_start < total_combos) {
      chunk <- exhaustive_sum_roc(
        data              = analysis_dat,
        outcome           = outcome_name,
        items             = item_names,
        min_items         = min_items,
        max_items         = max_items,
        positive_label    = positive_label,
        negative_label    = negative_label,
        cutoff_method     = cutoff_method,
        rank_by           = final_rank_by,
        top_n             = NULL,
        prefer_fewer_items = TRUE,
        engine            = engine,
        progress          = FALSE,
        chunk_start       = chunk_start,
        chunk_size        = chunk_size
      )
      combined <- if (is.null(best_so_far)) chunk else rbind(best_so_far, chunk)
      ord <- do.call(order, c(
        lapply(c(final_rank_by, "youden", "auc", "sensitivity", "specificity"),
               function(nm) if (nm %in% names(combined)) -combined[[nm]] else NULL),
        list(combined$n_items)
      ))
      best_so_far <- utils::head(combined[ord, , drop = FALSE], 100L)
      chunk_start <- chunk_start + chunk_size
    }
    final_exhaustive_ranked <- best_so_far

    return(list(
      final_exhaustive_ranked = NULL,
      final_exhaustive_file   = NULL,
      final_n_combinations    = total_combos,
      storage_backend         = "none",
      chunk_dir               = NULL,
      chunk_prefix            = NULL,
      results_name            = NULL,
      full_table_for_slicing  = final_exhaustive_ranked
    ))
  }

  # --- Memory storage: keep in RAM ---
  if (effective_storage == "memory") {
    if (is_large) {
      warning(
        total_combos, " total combinations exceeds the memory limit (",
        AUTO_MEMORY_LIMIT, ") but results_storage = \"memory\" was requested. ",
        "Consider using results_storage = \"auto\" or \"rds\" to avoid high memory usage.",
        call. = FALSE
      )
    }
    final_exhaustive_ranked <- exhaustive_sum_roc(
      data              = analysis_dat,
      outcome           = outcome_name,
      items             = item_names,
      min_items         = min_items,
      max_items         = max_items,
      positive_label    = positive_label,
      negative_label    = negative_label,
      cutoff_method     = cutoff_method,
      rank_by           = final_rank_by,
      top_n             = NULL,
      prefer_fewer_items = TRUE,
      engine            = engine,
      progress          = progress
    )
    attr(final_exhaustive_ranked, "ncvroc_metadata") <- metadata
    return(list(
      final_exhaustive_ranked = final_exhaustive_ranked,
      final_exhaustive_file   = NULL,
      final_n_combinations    = nrow(final_exhaustive_ranked),
      storage_backend         = "memory",
      chunk_dir               = NULL,
      chunk_prefix            = NULL,
      results_name            = NULL,
      full_table_for_slicing  = NULL
    ))
  }

  # --- Single RDS storage ---
  if (effective_storage == "single_rds") {
    final_exhaustive_ranked <- exhaustive_sum_roc(
      data              = analysis_dat,
      outcome           = outcome_name,
      items             = item_names,
      min_items         = min_items,
      max_items         = max_items,
      positive_label    = positive_label,
      negative_label    = negative_label,
      cutoff_method     = cutoff_method,
      rank_by           = final_rank_by,
      top_n             = NULL,
      prefer_fewer_items = TRUE,
      engine            = engine,
      progress          = progress
    )
    attr(final_exhaustive_ranked, "ncvroc_metadata") <- metadata

    if (!is.null(building_dir)) {
      # Cache: write to building directory
      rds_file <- file.path(building_dir, "full_results.rds")
      saveRDS(final_exhaustive_ranked, rds_file)
      return(list(
        final_exhaustive_ranked = NULL,
        final_exhaustive_file   = rds_file,
        final_n_combinations    = nrow(final_exhaustive_ranked),
        storage_backend         = "single_rds",
        chunk_dir               = NULL,
        chunk_prefix            = NULL,
        results_name            = "full_results.rds",
        full_table_for_slicing  = final_exhaustive_ranked
      ))
    } else {
      results_dir_use <- if (is.null(results_dir)) tempdir() else results_dir
      final_exhaustive_file <- .make_results_path(
        results_dir  = results_dir_use,
        prefix       = "ncvroc_final",
        outcome      = outcome_name,
        n_items      = length(item_names),
        min_items    = min_items,
        max_items    = max_items,
        rank_by      = final_rank_by,
        results_name = results_name
      )
      saveRDS(final_exhaustive_ranked, final_exhaustive_file)
      return(list(
        final_exhaustive_ranked = NULL,
        final_exhaustive_file   = final_exhaustive_file,
        final_n_combinations    = nrow(final_exhaustive_ranked),
        storage_backend         = "single_rds",
        chunk_dir               = NULL,
        chunk_prefix            = NULL,
        results_name            = NULL,
        full_table_for_slicing  = final_exhaustive_ranked
      ))
    }
  }

  # --- Chunked RDS storage ---
  if (effective_storage == "chunked_rds") {
    if (!is.null(building_dir)) {
      chunk_dir <- file.path(building_dir, "chunks")
      dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      chunk_base_dir <- if (is.null(results_dir)) tempdir() else results_dir
      chunk_dir <- .make_chunk_dir(chunk_base_dir)
    }

    chunk_start <- 0.0
    chunk_index <- 0L
    first_chunk <- NULL

    while (chunk_start < total_combos) {
      chunk <- exhaustive_sum_roc(
        data              = analysis_dat,
        outcome           = outcome_name,
        items             = item_names,
        min_items         = min_items,
        max_items         = max_items,
        positive_label    = positive_label,
        negative_label    = negative_label,
        cutoff_method     = cutoff_method,
        rank_by           = final_rank_by,
        top_n             = NULL,
        prefer_fewer_items = TRUE,
        engine            = engine,
        progress          = FALSE,
        chunk_start       = chunk_start,
        chunk_size        = chunk_size
      )
      .write_chunk_rds(chunk, chunk_dir, chunk_index)
      if (is.null(first_chunk)) first_chunk <- chunk

      chunk_start <- chunk_start + chunk_size
      chunk_index <- chunk_index + 1L
    }

    return(list(
      final_exhaustive_ranked = first_chunk,
      final_exhaustive_file   = NULL,
      final_n_combinations    = total_combos,
      storage_backend         = "chunked_rds",
      chunk_dir               = chunk_dir,
      chunk_prefix            = "chunk",
      results_name            = NULL,
      full_table_for_slicing  = NULL
    ))
  }

  stop("Unexpected storage mode: ", effective_storage, call. = FALSE)
}

# ---- Main function ----

#' Run a complete NCVROC analysis
#'
#' Primary user-facing function for nested cross-validation of combinatorial
#' ROC-based item-set selection. Resolves outcome and item columns using
#' base-R style selection, prepares data, creates a configuration, runs
#' nested CV, and optionally performs a final exhaustive search.
#'
#' @param data A data.frame containing outcome and item columns.
#' @param outcome Column name (bare symbol or character string).
#' @param items Candidate items: bare range (`Q1:Q112`), bare names with
#'   `c()`, character vector, existing variable, or numeric positions.
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
#' @param seed Integer seed for reproducibility (default NULL).
#' @param final_search Logical, run exhaustive search on full dataset
#'   after nested CV (default TRUE).
#' @param final_top_n Number of top final candidates to store (`NULL` for
#'   all, `0` for none). Default 20.
#' @param final_rank_by Metric for ranking final full-data candidate table:
#'   `"auc"`, `"youden"`, `"sensitivity"`, `"specificity"`, or `"accuracy"`.
#' @param save_results Logical, write CSV outputs (default FALSE).
#' @param output_dir Directory for saved CSVs (default `"."`).
#' @param results_storage Where to store the full final exhaustive results:
#'   `"auto"` (default, small→RAM, large→disk), `"memory"` (keep in RAM),
#'   `"rds"` (save to disk), or `"none"` (discard).
#' @param results_name Optional label prefix for the RDS filename (only used
#'   when results_storage saves a single RDS).
#' @param results_dir Directory for the full results RDS file, or NULL
#'   (default) to use `tempdir()`. Only used for single-RDS storage.
#' @param chunk_size Integer, combinations per chunk (default 200000).
#' @param cache One of `"off"` (default), `"reuse"`, or `"refresh"`.
#' @param cache_dir Directory for result caching, or NULL (must be set
#'   when `cache != "off"`).
#' @param progress Logical, show progress bars (default TRUE).
#' @param verbose Logical, print diagnostic messages (default TRUE).
#' @param return Return mode: `"full"` or `"summary"` (default `"full"`).
#' @param item_count Concise model-size specification: `"==4"` (exactly 4
#'   items), `"<=4"` (up to 4 items), or `"2:4"` (2 through 4 items).
#'   Cannot be combined with `min_items` or `max_items`. Default NULL.
#'
#' @return An object of class `"ncvroc_analysis"`.
#' @export
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   y  = sample(0:1, 60, replace = TRUE),
#'   Q1 = sample(0:2, 60, replace = TRUE),
#'   Q2 = sample(0:2, 60, replace = TRUE),
#'   Q3 = sample(0:2, 60, replace = TRUE)
#' )
#' \donttest{
#' result <- ncvroc(d, y, Q1:Q3, max_items = 2, mode = "quick",
#'   outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
#'   seed = 42, final_search = FALSE)
#' print(result)
#' }
ncvroc <- function(data,
                   outcome,
                   items,
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
                   cutoff_method = "youden",
                   positive_label = 1,
                   negative_label = 0,
                   stratified = TRUE,
                   engine = "Rcpp",
                   seed = NULL,
                   final_search = TRUE,
                   final_top_n = 20,
                   final_rank_by = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                   save_results = FALSE,
                   output_dir = ".",
                   results_storage = c("auto", "memory", "rds", "none"),
                   results_name    = NULL,
                   results_dir     = NULL,
                   chunk_size      = 200000L,
                   cache           = c("off", "reuse", "refresh"),
                   cache_dir       = NULL,
                   progress = TRUE,
                   verbose = TRUE,
                   return = "full",
                   item_count = NULL) {

  # ---- 1. Capture & resolve ----
  caller_env   <- parent.frame()
  outcome_expr <- substitute(outcome)
  items_expr   <- substitute(items)
  outcome_name <- .resolve_outcome(outcome_expr, caller_env)
  item_names   <- .resolve_items(data, items_expr, caller_env)
  final_rank_by <- match.arg(final_rank_by)
  results_storage <- match.arg(results_storage)
  cache <- match.arg(cache)

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

  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || chunk_size <= 0L ||
      chunk_size != floor(chunk_size)) {
    stop("chunk_size must be a positive integer.", call. = FALSE)
  }
  chunk_size <- as.integer(chunk_size)

  # ---- 1b. Cache validation ----
  if (cache != "off" && is.null(cache_dir)) {
    stop("cache_dir must be set when cache = \"", cache, "\".", call. = FALSE)
  }

  if (!is.null(cache_dir)) {
    if (length(cache_dir) != 1L || !is.character(cache_dir) ||
        is.na(cache_dir) || !nzchar(trimws(cache_dir))) {
      stop("cache_dir must be a single non-empty path.", call. = FALSE)
    }
  }

  # ---- 1c. item_count validation and resolution ----
  min_items_missing <- missing(min_items)
  max_items_missing <- missing(max_items)

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

  # ---- 2. Prepare analysis data ----
  n_original <- nrow(data)
  analysis_dat <- .prepare_ncvroc_data(data, outcome_name, item_names)
  n_analyzed <- nrow(analysis_dat)

  # ---- 3. Create config ----
  cfg <- ncvroc_config(
    outcome             = outcome_name,
    items               = item_names,
    min_items           = min_items,
    max_items           = max_items,
    mode                = mode,
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

  # ---- 3b. Compute cache key and check for existing cache ----
  cache_key <- NULL
  cache_load_result <- NULL

  if (cache != "off") {
    cache_key <- .compute_cache_key(
      cache_data          = analysis_dat,
      cache_outcome       = outcome_name,
      cache_items         = item_names,
      min_items           = min_items,
      max_items           = max_items,
      mode                = mode,
      outer_k             = outer_k,
      inner_k             = inner_k,
      outer_repeats       = outer_repeats,
      inner_repeats       = inner_repeats,
      cutoff_method       = cutoff_method,
      selection_criterion = selection_criterion,
      preselect_top_n     = cfg$preselect_top_n,
      preselect_by        = preselect_by,
      final_search        = final_search,
      final_rank_by       = final_rank_by,
      engine              = engine,
      seed                = seed,
      positive_label      = positive_label,
      negative_label      = negative_label,
      stratified          = stratified,
      chunk_size          = chunk_size
    )

    if (cache == "reuse") {
      cache_load_result <- .load_cache(cache_dir, cache_key)
      if (!is.null(cache_load_result)) {
        cache_load_result$loaded_from_cache <- TRUE
        return(cache_load_result)
      }
    }
  }

  # ---- 4. Nested CV ----
  nested_result <- run_ncvroc(
    data     = analysis_dat,
    items    = item_names,
    config   = cfg,
    seed     = seed,
    progress = progress,
    verbose  = verbose,
    return   = return
  )

  # ---- 5. Optional final exhaustive search ----
  final_exhaustive_ranked  <- NULL
  final_exhaustive_file    <- NULL
  final_n_combinations     <- 0L
  final_storage_backend    <- NULL
  final_chunk_dir          <- NULL
  final_chunk_prefix       <- NULL
  full_table_for_slicing   <- NULL

  building_dir <- NULL

  if (final_search) {
    if (cache != "off") {
      pid <- Sys.getpid()
      building_dir <- file.path(cache_dir, paste0(cache_key, ".building-", pid))
      dir.create(building_dir, recursive = TRUE, showWarnings = FALSE)
    }

    eval_result <- .evaluate_final_exhaustive(
      analysis_dat       = analysis_dat,
      outcome_name       = outcome_name,
      item_names         = item_names,
      min_items          = min_items,
      max_items          = max_items,
      positive_label     = positive_label,
      negative_label     = negative_label,
      cutoff_method      = cutoff_method,
      final_rank_by      = final_rank_by,
      engine             = engine,
      progress           = progress,
      storage_mode       = results_storage,
      results_dir        = results_dir,
      results_name       = results_name,
      chunk_size         = chunk_size,
      cache              = cache,
      cache_dir          = cache_dir,
      building_dir       = building_dir,
      resolved_item_count = resolved_item_count
    )

    final_exhaustive_ranked <- eval_result$final_exhaustive_ranked
    final_exhaustive_file   <- eval_result$final_exhaustive_file
    final_n_combinations    <- eval_result$final_n_combinations
    final_storage_backend   <- eval_result$storage_backend
    final_chunk_dir         <- eval_result$chunk_dir
    final_chunk_prefix      <- eval_result$chunk_prefix
    full_table_for_slicing  <- eval_result$full_table_for_slicing
  }

  # ---- 5b. Slice final candidates ----
  table_for_slicing <- if (!is.null(full_table_for_slicing)) {
    full_table_for_slicing
  } else {
    final_exhaustive_ranked
  }

  if (final_search && !is.null(table_for_slicing)) {
    final_model <- table_for_slicing[1, , drop = FALSE]
    if (is.null(final_top_n)) {
      final_candidates <- table_for_slicing
    } else if (final_top_n > 0) {
      final_candidates <- utils::head(table_for_slicing, final_top_n)
    } else {
      final_candidates <- NULL
    }
  } else if (final_search) {
    # Chunked or single_rds: first chunk was stored; load it for final_model
    if (final_storage_backend == "chunked_rds" && !is.null(final_chunk_dir)) {
      chunk <- .read_chunk_rds(final_chunk_dir, 0L)
      final_model <- chunk[1, , drop = FALSE]
      is_final_top_n_null <- is.null(final_top_n)
      has_final_top_n_pos <- isTRUE(final_top_n > 0)
      if (is.null(final_top_n)) {
        final_candidates <- chunk
      } else if (final_top_n > 0) {
        final_candidates <- utils::head(chunk, final_top_n)
      } else {
        final_candidates <- NULL
      }
    } else {
      final_model <- NULL
      final_candidates <- NULL
    }
  } else {
    final_model <- NULL
    final_candidates <- NULL
  }

  # ---- 6. Optional save (CSV export, unchanged from v0.8.0) ----
  if (save_results) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    fold_info <- do.call(rbind, lapply(nested_result$outer_results, function(x) {
      data.frame(
        outer_fold     = x$outer_fold,
        selected_items = x$selected_items,
        n_items        = x$n_items,
        auc            = x$auc,
        sensitivity    = x$sensitivity,
        specificity    = x$specificity,
        youden         = x$youden,
        accuracy       = x$accuracy,
        ppv            = x$ppv,
        npv            = x$npv,
        cutoff         = x$cutoff,
        stringsAsFactors = FALSE
      )
    }))

    utils::write.csv(fold_info,
      file.path(output_dir, "nested_cv_outer_fold_results.csv"), row.names = FALSE)
    utils::write.csv(
      data.frame(selected_models = nested_result$selected_models,
                 stringsAsFactors = FALSE),
      file.path(output_dir, "nested_cv_selected_models.csv"), row.names = FALSE)
    utils::write.csv(nested_result$outer_predictions,
      file.path(output_dir, "nested_cv_outer_predictions.csv"), row.names = FALSE)
    utils::write.csv(nested_result$summary,
      file.path(output_dir, "nested_cv_summary.csv"), row.names = FALSE)
    utils::write.csv(nested_result$selected_model_frequency,
      file.path(output_dir, "nested_cv_selected_model_frequency.csv"), row.names = FALSE)
    full_for_csv <- if (!is.null(full_table_for_slicing)) {
      full_table_for_slicing
    } else {
      final_exhaustive_ranked
    }
    if (!is.null(full_for_csv)) {
      utils::write.csv(full_for_csv,
        file.path(output_dir, "final_exhaustive_results_ranked.csv"), row.names = FALSE)
    }
    if (!is.null(final_candidates)) {
      utils::write.csv(final_candidates,
        file.path(output_dir, "final_candidates.csv"), row.names = FALSE)
    }
    if (!is.null(final_model)) {
      utils::write.csv(final_model,
        file.path(output_dir, "final_model.csv"), row.names = FALSE)
    }
  }

  # ---- 7. Build return object ----
  result <- list(
    config                    = cfg,
    data                      = analysis_dat,
    outcome                   = outcome_name,
    items                     = item_names,
    n_original                = n_original,
    n_analyzed                = n_analyzed,
    nested_result             = nested_result,
    nested_cv_summary         = nested_result$summary,
    selected_model_frequency  = nested_result$selected_model_frequency,
    outer_predictions         = nested_result$outer_predictions,
    final_search              = final_search,
    final_exhaustive_ranked   = final_exhaustive_ranked,
    final_exhaustive_file     = final_exhaustive_file,
    final_results_storage     = results_storage,
    storage_backend           = final_storage_backend,
    final_n_combinations      = final_n_combinations,
    final_candidates          = final_candidates,
    final_model               = final_model,
    final_top_n               = final_top_n,
    final_rank_by             = final_rank_by,
    item_count                = resolved_item_count,
    chunk_dir                 = final_chunk_dir,
    chunk_prefix              = final_chunk_prefix,
    chunk_size                = chunk_size,
    cache_key                 = cache_key,
    cache_dir                 = if (!is.null(cache_dir)) normalizePath(cache_dir, winslash = "/", mustWork = FALSE) else NULL,
    cache_entry_dir           = NULL,
    loaded_from_cache         = FALSE
  )

  # ---- 8. Save to cache if enabled ----
  if (cache != "off" && !is.null(building_dir)) {
    metadata_list <- list(
      function_name   = "ncvroc",
      outcome         = outcome_name,
      items           = item_names,
      min_items       = min_items,
      max_items       = max_items,
      item_count      = resolved_item_count,
      rank_by         = final_rank_by,
      cutoff_method   = cutoff_method,
      positive_label  = positive_label,
      negative_label  = negative_label,
      engine          = engine,
      created_at      = Sys.time(),
      package_version = as.character(utils::packageVersion("NCVROC"))
    )

    result <- .save_cache(
      result          = result,
      full_results    = full_table_for_slicing,
      cache_dir       = cache_dir,
      cache_key       = cache_key,
      metadata_list   = metadata_list,
      storage_backend = final_storage_backend
    )
  }

  class(result) <- "ncvroc_analysis"
  result
}

# ---- Print method ----

#' Print an NCVROC analysis result
#'
#' @param x An `ncvroc_analysis` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#' @export
print.ncvroc_analysis <- function(x, ...) {
  cfg <- x$config

  cat("NCVROC analysis\n\n")

  if (!is.null(x$loaded_from_cache) && x$loaded_from_cache) {
    cat("Loaded from cache: ", x$cache_entry_dir, "\n\n", sep = "")
  }

  cat("Analyzed observations:", x$n_analyzed, "\n")
  if (!is.null(x$n_original) && x$n_original != x$n_analyzed) {
    cat("  (", x$n_original - x$n_analyzed,
        " rows removed due to missing values)\n", sep = "")
  }
  cat("Outcome:              ", cfg$outcome, "\n", sep = "")
  cat("Items:                ", cfg$n_items, "\n", sep = "")
  cat("Candidate item sizes:  ", cfg$min_items, " to ", cfg$max_items, "\n", sep = "")
  if (!is.null(x$item_count)) {
    cat("Item count:            ", .describe_item_count(x$item_count),
        " (", x$item_count, ")\n", sep = "")
  }
  cat("Total combinations:   ", format(cfg$total_combinations, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("Mode:                 ", cfg$mode, "\n", sep = "")

  preselect_str <- if (is.null(cfg$preselect_top_n)) {
    "(not set)"
  } else {
    format(cfg$preselect_top_n, big.mark = ",", scientific = FALSE)
  }
  cat("Preselected candidates:", preselect_str, "\n", sep = "")

  if (!is.null(cfg$preselect_top_n) && cfg$preselect_top_n >= 100000) {
    cat("[!] Warning: preselect_top_n >= 100,000 - inner CV will be slow.\n")
  }

  cat("Outer CV:             ", cfg$outer_k, "-fold x ",
      cfg$outer_repeats, " repeats\n", sep = "")
  cat("Inner CV:             ", cfg$inner_k, "-fold x ",
      cfg$inner_repeats, " repeats\n", sep = "")
  cat("Engine:               ", cfg$engine, "\n", sep = "")

  if (!is.null(x$nested_cv_summary)) {
    smry <- x$nested_cv_summary
    cat("\nNested CV summary:")
    if (!is.null(smry$auc))         cat("\n  Mean AUC:          ", signif(mean(smry$auc), 4))
    if (!is.null(smry$sensitivity)) cat("\n  Mean sensitivity:  ", signif(mean(smry$sensitivity), 4))
    if (!is.null(smry$specificity)) cat("\n  Mean specificity:  ", signif(mean(smry$specificity), 4))
    cat("\n")
  }

  cat("\nFinal exhaustive search: ", if (!is.null(x$final_n_combinations) && x$final_n_combinations > 0) "yes" else "no", "\n", sep = "")

  if (!is.null(x$final_n_combinations) && x$final_n_combinations > 0) {
    cat("Final candidate ranking:  ", x$final_rank_by, "\n", sep = "")
    cat("Final combinations:       ",
        format(x$final_n_combinations, big.mark = ",", scientific = FALSE), "\n", sep = "")
    n_shown <- if (is.null(x$final_candidates)) 0 else nrow(x$final_candidates)
    cat("Final candidates shown:   ", n_shown, "\n", sep = "")

    if (!is.null(x$storage_backend)) {
      if (x$storage_backend == "chunked_rds") {
        cat("Full results: ", format(x$final_n_combinations, big.mark = ",", scientific = FALSE),
            " candidates in chunk files\n", sep = "")
        if (!is.null(x$chunk_dir)) {
          cat("  Chunk directory: ", x$chunk_dir, "\n", sep = "")
        }
      } else if (x$storage_backend == "single_rds") {
        if (!is.null(x$final_exhaustive_file)) {
          if (!file.exists(x$final_exhaustive_file)) {
            cat("Full results: stored RDS file is missing\n")
          } else {
            cat("Full results: stored in ", x$final_exhaustive_file, "\n", sep = "")
          }
        }
      } else if (x$storage_backend == "none") {
        cat("Full results: not stored\n")
      }
    } else {
      # Backward compat: old results_storage field
      if (!is.null(x$final_results_storage)) {
        if (x$final_results_storage == "rds") {
          if (!is.null(x$final_exhaustive_file)) {
            if (!file.exists(x$final_exhaustive_file)) {
              cat("Full results: stored RDS file is missing\n")
            } else {
              cat("Full results: stored in ", x$final_exhaustive_file, "\n", sep = "")
            }
          }
        } else if (x$final_results_storage == "none") {
          cat("Full results: not stored\n")
        }
      }
    }
  }

  if (!is.null(x$final_model)) {
    cat("\nBest final model:\n")
    print(x$final_model)
  }

  if (!is.null(x$final_candidates)) {
    cat("\nTop final candidate models:\n")
    print(if (is.null(x$final_top_n) && nrow(x$final_candidates) > 20) {
      utils::head(x$final_candidates, 20)
    } else {
      x$final_candidates
    })
  }

  invisible(x)
}

# ---- Plot method ----

#' Plot an NCVROC analysis result
#'
#' @param x An `ncvroc_analysis` object.
#' @param which What to plot: `"all"` (both selection and AUC),
#'   `"selection"` (model selection frequency), or `"auc"` (per-fold AUC).
#' @param ... Passed to `plot.ncvroc_result()`.
#'
#' @return Invisibly returns `x`.
#' @export
plot.ncvroc_analysis <- function(x,
                                  which = c("all", "selection", "auc"),
                                  ...) {
  if (is.null(x$nested_result)) {
    stop("x$nested_result is NULL; plot() requires a nested CV result.")
  }
  which <- match.arg(which)
  if (which == "all") {
    plot(x$nested_result, which = "selection", ...)
    plot(x$nested_result, which = "auc", ...)
  } else {
    plot(x$nested_result, which = which, ...)
  }
  invisible(x)
}

# ---- Condition-based final model reporting ----

#' Parse a clinical constraint condition string
#'
#' Parses a condition like `">= 0.90"` or `"< 4"` into an operator and a
#' numeric value. Only 6 operators are accepted: `>=`, `>`, `<=`, `<`, `==`, `!=`.
#'
#' @param condition A single character string (e.g. `">= 0.90"`), or NULL.
#' @param colname Optional column name for error messages (not currently used).
#' @return A list with elements `op` and `value`, or NULL if condition is NULL.
#' @keywords internal
.parse_condition <- function(condition, colname = NULL) {
  if (is.null(condition)) return(NULL)

  if (!is.character(condition) || length(condition) != 1 || is.na(condition)) {
    stop("Condition must be a string like '>= 0.90', '< 4', or '== 3'.", call. = FALSE)
  }

  m <- regmatches(
    condition,
    regexec("^\\s*(>=|<=|==|!=|>|<)\\s*(-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?)\\s*$", condition)
  )[[1]]

  if (length(m) != 3) {
    stop("Condition must be a string like '>= 0.90', '< 4', or '== 3'.", call. = FALSE)
  }

  value <- suppressWarnings(as.numeric(m[3]))

  if (is.na(value)) {
    stop("Condition must contain a numeric value.", call. = FALSE)
  }

  list(op = m[2], value = value)
}

#' Filter and rank final exhaustive results by clinical constraints
#'
#' From a complete `ncvroc_analysis` result, filter the final exhaustive
#' candidate table by clinical constraints (e.g. sensitivity >= 0.90) and
#' return the top matching models.
#'
#' @param x An `ncvroc_analysis` or `roc_bruteforce_result` object.
#' @param sensitivity Condition on sensitivity, e.g. `">= 0.90"`.
#' @param specificity Condition on specificity, e.g. `">= 0.85"`.
#' @param auc Condition on AUC.
#' @param youden Condition on Youden's J.
#' @param accuracy Condition on accuracy.
#' @param ppv Condition on positive predictive value.
#' @param npv Condition on negative predictive value.
#' @param n_items Condition on number of items, e.g. `"<= 3"`.
#' @param cutoff Condition on cutoff value.
#' @param rank_by Metric for ranking matching candidates: `"youden"`, `"auc"`,
#'   `"sensitivity"`, `"specificity"`, `"accuracy"`, `"ppv"`, or `"npv"`.
#' @param top_n Number of top candidates to return (NULL for all, 0 for none).
#' @param allow_full_load Logical. If `TRUE` and storage is chunked, load all
#'   chunks (may use large amounts of memory). If `FALSE` (default) and
#'   `top_n` is NULL with chunked storage, errors.
#'
#' @return A data.frame of matching candidates, sorted by `rank_by` descending.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' d <- data.frame(
#'   y  = sample(0:1, 60, replace = TRUE),
#'   Q1 = sample(0:2, 60, replace = TRUE),
#'   Q2 = sample(0:2, 60, replace = TRUE),
#'   Q3 = sample(0:2, 60, replace = TRUE),
#'   Q4 = sample(0:2, 60, replace = TRUE),
#'   Q5 = sample(0:2, 60, replace = TRUE)
#' )
#' result <- ncvroc(d, y, Q1:Q5, max_items = 2, mode = "quick",
#'   outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
#'   seed = 42, final_search = TRUE)
#' ncvroc_results(result, sensitivity = ">= 0.90", specificity = ">= 0.85")
#' }
ncvroc_results <- function(x,
                           sensitivity = NULL,
                           specificity = NULL,
                           auc          = NULL,
                           youden       = NULL,
                           accuracy     = NULL,
                           ppv          = NULL,
                           npv          = NULL,
                           n_items      = NULL,
                           cutoff       = NULL,
                           rank_by = c("youden", "auc", "sensitivity", "specificity",
                                       "accuracy", "ppv", "npv"),
                           top_n = 20,
                           allow_full_load = FALSE) {

  rank_by <- match.arg(rank_by)

  if (!is.null(top_n)) {
    if (length(top_n) != 1 || !is.numeric(top_n) || is.na(top_n) || top_n < 0 || top_n != floor(top_n)) {
      stop("top_n must be NULL, 0, or a single non-negative integer.", call. = FALSE)
    }
  }

  if (!isTRUE(allow_full_load) && !isFALSE(allow_full_load)) {
    stop("allow_full_load must be TRUE or FALSE.", call. = FALSE)
  }

  # ---- Identify storage backend ----
  is_chunked <- !is.null(x$storage_backend) && x$storage_backend == "chunked_rds"
  is_single_rds <- !is.null(x$storage_backend) && x$storage_backend == "single_rds"
  is_memory <- !is.null(x$storage_backend) && x$storage_backend == "memory"
  is_none <- !is.null(x$storage_backend) && x$storage_backend == "none"

  # ---- Chunked path ----
  if (is_chunked) {
    if (is.null(x$chunk_dir) || !dir.exists(x$chunk_dir)) {
      stop("Chunked results directory no longer exists: ",
           x$chunk_dir %||% "NULL", call. = FALSE)
    }

    if (!is.null(top_n) && top_n > 0) {
      dat <- .stream_top_n_from_chunks(
        chunk_dir    = x$chunk_dir,
        rank_by      = rank_by,
        top_n        = top_n,
        sensitivity  = sensitivity,
        specificity  = specificity,
        auc          = auc,
        youden       = youden,
        accuracy     = accuracy,
        ppv          = ppv,
        npv          = npv,
        n_items      = n_items,
        cutoff       = cutoff
      )
      dat$rank      <- seq_len(nrow(dat))
      rownames(dat) <- NULL
      return(dat)
    }

    if (is.null(top_n) && !allow_full_load) {
      stop(
        "Full retrieval from chunked storage may require very large amounts of ",
        "memory. Specify a finite top_n, or set allow_full_load = TRUE explicitly.",
        call. = FALSE
      )
    }

    if (is.null(top_n) && allow_full_load) {
      dat <- .full_load_chunked(
        chunk_dir    = x$chunk_dir,
        sensitivity  = sensitivity,
        specificity  = specificity,
        auc          = auc,
        youden       = youden,
        accuracy     = accuracy,
        ppv          = ppv,
        npv          = npv,
        n_items      = n_items,
        cutoff       = cutoff
      )
      dat$rank      <- seq_len(nrow(dat))
      rownames(dat) <- NULL
      return(dat)
    }

    # top_n == 0: empty result
    dat <- .full_load_chunked(
      chunk_dir = x$chunk_dir,
      sensitivity = sensitivity,
      specificity = specificity,
      auc = auc, youden = youden,
      accuracy = accuracy, ppv = ppv, npv = npv,
      n_items = n_items, cutoff = cutoff
    )[0, , drop = FALSE]
    dat$rank      <- integer(0)
    rownames(dat) <- NULL
    return(dat)
  }

  # ---- None path ----
  if (is_none) {
    stop(
      "Full results were not stored. Re-run the analysis with ",
      'results_storage = "memory", "rds", or "auto".',
      call. = FALSE
    )
  }

  # ---- Legacy / memory / single_rds path ----
  if (inherits(x, "roc_bruteforce_result")) {
    dat <- .read_results_from_storage(x$results, x$results_file, x$chunk_dir,
                                       x$storage_backend, "results")
  } else if (inherits(x, "ncvroc_analysis")) {
    if (isFALSE(x$final_search)) {
      stop(
        "Final exhaustive search was not performed. ",
        "Re-run ncvroc() with final_search = TRUE.",
        call. = FALSE
      )
    }
    dat <- .read_results_from_storage(
      x$final_exhaustive_ranked, x$final_exhaustive_file, x$chunk_dir,
      x$storage_backend, "results"
    )
  } else {
    stop("x must be an ncvroc_analysis or roc_bruteforce_result object.", call. = FALSE)
  }

  # ---- Filter by conditions ----
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

  for (col in names(conditions)) {
    cond <- conditions[[col]]
    if (is.null(cond)) next

    parsed <- .parse_condition(cond, col)

    if (!col %in% names(dat)) {
      stop(sprintf("Column '%s' not found in results.", col), call. = FALSE)
    }

    col_vals <- dat[[col]]

    keep <- switch(parsed$op,
      `>=` = col_vals >= parsed$value,
      `>`  = col_vals >  parsed$value,
      `<=` = col_vals <= parsed$value,
      `<`  = col_vals <  parsed$value,
      `==` = col_vals == parsed$value,
      `!=` = col_vals != parsed$value
    )

    dat <- dat[keep, , drop = FALSE]
  }

  # ---- Sort ----
  tie_cols <- setdiff(c("youden", "auc", "sensitivity", "specificity", "accuracy"), rank_by)
  tie_cols <- intersect(tie_cols, names(dat))

  ord <- do.call(order, c(
    lapply(c(rank_by, tie_cols), function(nm) -dat[[nm]])
  ))

  dat <- dat[ord, , drop = FALSE]

  if (is.null(top_n)) {
    dat
  } else if (top_n == 0) {
    dat[0, , drop = FALSE]
  } else {
    utils::head(dat, top_n)
  }
}

# Backward compat %||% operator
`%||%` <- function(x, y) if (is.null(x)) y else x
