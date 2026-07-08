# ncvroc.R - Main entry function with formula-like item selection
#
# Internal helpers:
#   .resolve_outcome()
#   .resolve_items()
#   .parse_condition()
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
#' @param progress Logical, show progress bars (default TRUE).
#' @param verbose Logical, print diagnostic messages (default TRUE).
#' @param return Return mode: `"full"` or `"summary"` (default `"full"`).
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
                   progress = TRUE,
                   verbose = TRUE,
                   return = "full") {

  # ---- 1. Capture & resolve ----
  caller_env   <- parent.frame()
  outcome_expr <- substitute(outcome)
  items_expr   <- substitute(items)
  outcome_name <- .resolve_outcome(outcome_expr, caller_env)
  item_names   <- .resolve_items(data, items_expr, caller_env)
  final_rank_by <- match.arg(final_rank_by)

  # ---- 2. Prepare analysis data ----
  analysis_dat <- subset(data, select = c(outcome_name, item_names))
  analysis_dat[[outcome_name]] <- as.numeric(analysis_dat[[outcome_name]])
  analysis_dat[item_names] <- lapply(analysis_dat[item_names], as.numeric)
  analysis_dat <- analysis_dat[complete.cases(analysis_dat), ]

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
    engine              = engine
  )

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
  if (final_search) {
    final_exhaustive_ranked <- exhaustive_sum_roc(
      data                = analysis_dat,
      outcome             = outcome_name,
      items               = item_names,
      min_items           = min_items,
      max_items           = max_items,
      positive_label      = positive_label,
      negative_label      = negative_label,
      cutoff_method       = cutoff_method,
      rank_by             = final_rank_by,
      top_n               = NULL,
      prefer_fewer_items  = TRUE,
      engine              = engine,
      progress            = progress
    )
  } else {
    final_exhaustive_ranked <- NULL
  }

  # ---- 5b. Slice final candidates ----
  if (final_search && !is.null(final_exhaustive_ranked)) {
    final_model <- final_exhaustive_ranked[1, , drop = FALSE]
    if (is.null(final_top_n)) {
      final_candidates <- final_exhaustive_ranked
    } else if (final_top_n > 0) {
      final_candidates <- utils::head(final_exhaustive_ranked, final_top_n)
    } else {
      final_candidates <- NULL
    }
  } else {
    final_model <- NULL
    final_candidates <- NULL
  }

  # ---- 6. Optional save ----
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
    if (!is.null(final_exhaustive_ranked)) {
      utils::write.csv(final_exhaustive_ranked,
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

  # ---- 7. Return ----
  result <- list(
    config                    = cfg,
    data                      = analysis_dat,
    outcome                   = outcome_name,
    items                     = item_names,
    nested_result             = nested_result,
    nested_cv_summary         = nested_result$summary,
    selected_model_frequency  = nested_result$selected_model_frequency,
    outer_predictions         = nested_result$outer_predictions,
    final_exhaustive_ranked   = final_exhaustive_ranked,
    final_candidates          = final_candidates,
    final_model               = final_model,
    final_top_n               = final_top_n,
    final_rank_by             = final_rank_by
  )
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

  cat("Outcome:              ", cfg$outcome, "\n", sep = "")
  cat("Items:                ", cfg$n_items, "\n", sep = "")
  cat("Candidate item sizes:  ", cfg$min_items, " to ", cfg$max_items, "\n", sep = "")
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

  cat("\nFinal exhaustive search: ", if (is.null(x$final_exhaustive_ranked)) "no" else "yes", "\n", sep = "")

  if (!is.null(x$final_exhaustive_ranked)) {
    cat("Final candidate ranking: ", x$final_rank_by, "\n", sep = "")
    n_shown <- if (is.null(x$final_candidates)) 0 else nrow(x$final_candidates)
    cat("Final candidates shown:  ", n_shown, "\n", sep = "")
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
#' @param x An `ncvroc_analysis` object (must have `final_exhaustive_ranked`).
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
                           top_n = 20) {

  if (is.null(x$final_exhaustive_ranked)) {
    stop("x$final_exhaustive_ranked is NULL. Re-run ncvroc() with final_search = TRUE.", call. = FALSE)
  }

  rank_by <- match.arg(rank_by)

  if (!is.null(top_n)) {
    if (length(top_n) != 1 || !is.numeric(top_n) || is.na(top_n) || top_n < 0 || top_n != floor(top_n)) {
      stop("top_n must be NULL, 0, or a single non-negative integer.", call. = FALSE)
    }
  }

  dat <- x$final_exhaustive_ranked

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
      stop(sprintf("Column '%s' not found in final_exhaustive_ranked.", col), call. = FALSE)
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

  # Sort by rank_by descending with tiebreakers
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
