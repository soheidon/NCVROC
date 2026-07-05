# helper-common.R — Shared test helpers loaded by all test files

make_test_data <- function() {
  data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1)
  )
}

# Alias for nested tests
make_nested_test_data <- make_test_data
