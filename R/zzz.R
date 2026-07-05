.onAttach <- function(libname, pkgname) {
  packageStartupMessage("NCVROC v", utils::packageVersion("NCVROC"))
}
