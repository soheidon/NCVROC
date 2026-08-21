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
#' Confidence intervals for final-model performance quantify sampling
#' uncertainty for the fitted model evaluated on the full dataset. They do not
#' account for uncertainty introduced by model or cutoff selection and should
#' not be interpreted as cross-validated confidence intervals.
#'
#' This function evaluates combinations via [exhaustive_sum_roc()], computes
#' confidence intervals for the top models (if `ci = TRUE`), and tags the
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
#' @param ci Logical. If `TRUE` (default), compute confidence intervals for
#'   AUC (DeLong) and classification metrics (Clopper-Pearson exact binomial).
#' @param conf_level Numeric confidence level in (0, 1), default 0.95.
#' @param engine Character, computation engine. `"R"` (default) or `"Rcpp"`.
#' @param progress Logical, show progress bar? Default `TRUE`.
#'
#' @details
#' When `ci = TRUE`, confidence intervals are calculated as follows:
#' \itemize{
#'   \item \strong{AUC CI}: Non-parametric asymptotic normal approximation using
#'     the method of DeLong et al. (1988), computed efficiently from score frequency
#'     distributions in O(K) time.
#'   \item \strong{Sensitivity, Specificity, Accuracy, PPV, NPV CI}: Exact binomial
#'     confidence intervals using the Clopper-Pearson method (Clopper & Pearson, 1934)
#'     via Beta distribution quantiles (\code{\link[stats]{qbeta}}).
#' }
#'
#' \strong{Important note on CI interpretation:} Confidence intervals for
#' final-model performance quantify sampling uncertainty for the fitted model
#' evaluated on the full dataset. They do not account for uncertainty introduced
#' by model or cutoff selection and should not be interpreted as cross-validated
#' confidence intervals. Use \code{\link{nested_sum_roc}} for cross-validated
#' performance estimation.
#'
#' @references
#' DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas
#' under two or more correlated receiver operating characteristic curves: a
#' nonparametric approach. \emph{Biometrics}, 44(3), 837--845.
#' \doi{10.2307/2531595}
#'
#' Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial
#' limits illustrated in the case of the binomial. \emph{Biometrika}, 26(4),
#' 404--413. \doi{10.1093/biomet/26.4.404}
#'
#' @return A data.frame with columns: `rank`, `items`, `n_items`, `auc`,
#'   `cutoff`, `sensitivity`, `specificity`, `youden`, `accuracy`, `ppv`,
#'   `npv`, `n_positive`, `n_negative`. When `ci = TRUE`, also includes:
#'   \itemize{
#'     \item \code{auc_lower}, \code{auc_upper}: DeLong confidence limits for AUC.
#'     \item \code{sensitivity_lower}, \code{sensitivity_upper}: Clopper-Pearson exact limits for sensitivity.
#'     \item \code{specificity_lower}, \code{specificity_upper}: Clopper-Pearson exact limits for specificity.
#'     \item \code{accuracy_lower}, \code{accuracy_upper}: Clopper-Pearson exact limits for accuracy.
#'     \item \code{ppv_lower}, \code{ppv_upper}: Clopper-Pearson exact limits for positive predictive value.
#'     \item \code{npv_lower}, \code{npv_upper}: Clopper-Pearson exact limits for negative predictive value.
#'   }
#'   Has attribute `performance_type` set to `"apparent"`.
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
                                ci = TRUE,
                                conf_level = 0.95,
                                engine = c("R", "Rcpp"),
                                progress = TRUE) {
  cutoff_method <- match.arg(cutoff_method)
  rank_by <- match.arg(rank_by)
  engine <- match.arg(engine)

  result <- exhaustive_sum_roc(
    data               = data,
    outcome            = outcome,
    items              = items,
    min_items          = min_items,
    max_items          = max_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    cutoff_method      = cutoff_method,
    rank_by            = rank_by,
    top_n              = top_n,
    prefer_fewer_items = TRUE,
    ci                 = ci,
    conf_level         = conf_level,
    engine             = engine,
    progress           = progress
  )

  attr(result, "performance_type") <- "apparent"
  result
}
