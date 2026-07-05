# fit_final_sum_scale.R — Fit final scale on full data (apparent performance only)

#' Fit final screening scale on full data
#'
#' A thin convenience wrapper around [exhaustive_sum_roc()] for building the
#' final candidate screening scale on the complete dataset after nested CV
#' validation is done.
#'
#' **Important:** The performance metrics returned by this function are
#' **apparent in-sample estimates** and should not be interpreted as
#' internally validated performance. Use [nested_sum_roc()] for nested
#' cross-validated performance estimation.
#'
#' This function does exactly the same computation as [exhaustive_sum_roc()]
#' and returns the same data.frame. The only difference is that it tags the
#' result with `attr("performance_type") <- "apparent"` to clearly flag
#' that these are NOT cross-validated estimates.
#'
#' @param data A data.frame containing item columns and a binary outcome column.
#' @param outcome Character string naming the binary outcome column.
#' @param items Character vector of item column names.
#' @param min_items Integer, minimum number of items per combination (default 1).
#' @param max_items Integer, maximum number of items per combination (default 4).
#' @param positive_label Value in `outcome` representing a positive case (default 1).
#' @param negative_label Value in `outcome` representing a negative case (default 0).
#' @param cutoff_method Method for determining the optimal cutoff. One of
#'   `"youden"` or `"closest_topleft"`. Default `"youden"`.
#' @param rank_by Metric for ranking models. One of `"auc"`, `"youden"`,
#'   `"sensitivity"`, `"specificity"`, or `"accuracy"`. Default `"auc"`.
#' @param top_n Integer, return only the top N models (default 20).
#' @param engine Character, computation engine. Only `"R"` is available in v0.1.
#' @param progress Logical, show progress bar? Default `TRUE`.
#'
#' @return A data.frame with the same structure as [exhaustive_sum_roc()]:
#'   columns `rank`, `items`, `n_items`, `auc`, `cutoff`, `sensitivity`,
#'   `specificity`, `youden`, `accuracy`, `ppv`, `npv`, `n_positive`,
#'   `n_negative`. Has attribute `performance_type` set to `"apparent"`.
#'
#' @examples
#' d <- data.frame(
#'   y  = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
#'   q1 = c(2, 1, 2, 0, 1, 1, 2, 2, 0, 1),
#'   q2 = c(1, 2, 1, 1, 0, 0, 2, 1, 0, 1),
#'   q3 = c(2, 2, 1, 0, 1, 0, 2, 1, 1, 0)
#' )
#' # Run nested CV for validated estimates first
#' # result <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
#' #   max_items = 2, outer_k = 3, inner_k = 2, seed = 42)
#' # Then fit final scale on full data
#' final <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2)
#' head(final)
#' attr(final, "performance_type")
#'
#' @export
fit_final_sum_scale <- function(data,
                                outcome,
                                items,
                                min_items = 1,
                                max_items = 4,
                                positive_label = 1,
                                negative_label = 0,
                                cutoff_method = c("youden", "closest_topleft"),
                                rank_by = c("auc", "youden", "sensitivity",
                                            "specificity", "accuracy"),
                                top_n = 20,
                                engine = "R",
                                progress = TRUE) {
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)

  result <- exhaustive_sum_roc(
    data             = data,
    outcome          = outcome,
    items            = items,
    min_items        = min_items,
    max_items        = max_items,
    positive_label   = positive_label,
    negative_label   = negative_label,
    cutoff_method    = cutoff_method,
    rank_by          = rank_by,
    top_n            = top_n,
    prefer_fewer_items = TRUE,
    engine           = engine,
    progress         = progress
  )

  attr(result, "performance_type") <- "apparent"
  result
}
