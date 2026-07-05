# zzz.R — Package startup hooks

#' @importFrom graphics plot.new text
#' @importFrom stats sd setNames
#' @useDynLib NCVROC, .registration = TRUE
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("NCVROC v", utils::packageVersion("NCVROC"))
}

.onLoad <- function(libname, pkgname) {
  # reserved for future use (e.g., Rcpp module loading in v0.2)
}
