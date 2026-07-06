#' NCVROC: Nested Cross-Validation for Combinatorial ROC-based Selection
#'
#' @description
#' Develops short item-based screening scales through combinatorial
#' item-set selection, ROC-based evaluation, and nested cross-validation.
#'
#' @useDynLib NCVROC, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom graphics plot.new text
#' @importFrom stats complete.cases sd setNames
#' @keywords internal
"_PACKAGE"
