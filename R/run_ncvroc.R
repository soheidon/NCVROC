# run_ncvroc.R - Run nested CV analysis from a config object
#
# Exported:
#   run_ncvroc()

#' Run nested CV analysis from an NCVROC configuration
#'
#' Thin wrapper around `nested_sum_roc()` that reads all parameters from
#' a configuration object created by `ncvroc_config()`.
#'
#' @param data A data.frame containing outcome and item columns.
#' @param items Character vector of candidate item names.
#' @param config An `ncvroc_config` object from `ncvroc_config()`.
#' @param seed Integer seed for reproducibility (default NULL).
#' @param progress Logical, show progress bars (default TRUE).
#' @param verbose Logical, print diagnostic messages (default TRUE).
#' @param return Return mode: `"full"` or `"summary"` (default `"full"`).
#'
#' @return An `ncvroc_result` object from `nested_sum_roc()`.
#' @export
#'
#' @examples
#' cfg <- ncvroc_config("y", items = paste0("q", 1:5), max_items = 2, mode = "quick")
#' d <- data.frame(
#'   y  = sample(0:1, 100, replace = TRUE),
#'   q1 = sample(0:2, 100, replace = TRUE),
#'   q2 = sample(0:2, 100, replace = TRUE),
#'   q3 = sample(0:2, 100, replace = TRUE),
#'   q4 = sample(0:2, 100, replace = TRUE),
#'   q5 = sample(0:2, 100, replace = TRUE)
#' )
#' \donttest{
#' result <- run_ncvroc(d, paste0("q", 1:5), cfg, seed = 42)
#' }
run_ncvroc <- function(data,
                       items,
                       config,
                       seed = NULL,
                       progress = TRUE,
                       verbose = TRUE,
                       return = c("full", "summary")) {
  if (!inherits(config, "ncvroc_config")) {
    stop("config must be an object created by ncvroc_config().", call. = FALSE)
  }

  return <- match.arg(return)

  # If preselect_top_n is NULL in config (items were NULL at config time),
  # auto-compute from the items now provided
  preselect_top_n <- config$preselect_top_n
  if (is.null(preselect_top_n)) {
    preselect_top_n <- suggest_preselect_top_n(
      items, config$min_items, config$max_items, config$mode
    )
  }

  nested_sum_roc(
    data                = data,
    outcome             = config$outcome,
    items               = items,
    min_items           = config$min_items,
    max_items           = config$max_items,
    positive_label      = config$positive_label,
    negative_label      = config$negative_label,
    cutoff_method       = config$cutoff_method,
    preselect_top_n     = preselect_top_n,
    preselect_by        = config$preselect_by,
    selection_criterion = config$selection_criterion,
    outer_k             = config$outer_k,
    inner_k             = config$inner_k,
    outer_repeats       = config$outer_repeats,
    inner_repeats       = config$inner_repeats,
    stratified          = config$stratified,
    seed                = seed,
    engine              = config$engine,
    progress            = progress,
    verbose             = verbose,
    return              = return
  )
}
