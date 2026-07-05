# NCVROC 0.2.0

- Added single-thread Rcpp backend for `exhaustive_sum_roc()`.
  Use `engine = "Rcpp"` to enable native C++ computation (~7x speedup on typical workloads).
  `engine = "R"` remains the default and produces identical results.

- `nested_sum_roc()` and `fit_final_sum_scale()` propagate `engine` to the inner
  exhaustive search, so the Rcpp engine can be used in nested CV workflows as well.

- Fixed Rcpp namespace initialization: `library(NCVROC)` alone is now sufficient
  for the Rcpp engine; `library(Rcpp)` is no longer required beforehand.

- All existing tests pass with both engines, confirming numerical equivalence
  between the R and Rcpp backends.
