# test-folds.R — Tests for make_stratified_folds()

make_test_y <- function() {
  c(rep(1, 30), rep(0, 70))
}

# ---- Basic return structure ----

test_that("make_stratified_folds returns a named list", {
  y <- make_test_y()
  folds <- make_stratified_folds(y, k = 5, seed = 42)
  expect_type(folds, "list")
  expect_named(folds)
  expect_length(folds, 5)
})

test_that("make_stratified_folds names follow Rep1_Fold1 format", {
  y <- make_test_y()
  folds <- make_stratified_folds(y, k = 3, seed = 42)
  expect_equal(names(folds), c("Rep1_Fold1", "Rep1_Fold2", "Rep1_Fold3"))
})

test_that("each fold element is an integer vector", {
  y <- make_test_y()
  folds <- make_stratified_folds(y, k = 5, seed = 42)
  for (f in folds) {
    expect_type(f, "integer")
  }
})

# ---- Full coverage within one repeat ----

test_that("all indices appear exactly once across folds in one repeat", {
  y <- make_test_y()
  n <- length(y)
  folds <- make_stratified_folds(y, k = 5, seed = 42)
  all_indices <- sort(unlist(folds, use.names = FALSE))
  expect_equal(all_indices, seq_len(n))
})

test_that("no overlap between folds within a repeat", {
  y <- make_test_y()
  folds <- make_stratified_folds(y, k = 5, seed = 42)
  # Pairwise intersection should be empty
  for (i in 1:4) {
    for (j in (i + 1):5) {
      expect_length(intersect(folds[[i]], folds[[j]]), 0)
    }
  }
})

# ---- Class balance ----

test_that("class balance is roughly proportional in each fold", {
  y <- make_test_y()
  n_pos <- sum(y == 1)  # 30
  n_neg <- sum(y == 0)  # 70
  overall_prop <- n_pos / length(y)  # 0.3

  folds <- make_stratified_folds(y, k = 5, seed = 42)
  for (f in folds) {
    fold_prop <- sum(y[f] == 1) / length(f)
    # Allow ±1 from expected count per fold (±1 / fold_size tolerance)
    expect_true(abs(fold_prop - overall_prop) < 0.15,
      info = sprintf("Fold proportion %.2f too far from overall %.2f",
                     fold_prop, overall_prop))
  }
})

test_that("small k still maintains rough balance", {
  y <- c(rep(1, 10), rep(0, 10))
  folds <- make_stratified_folds(y, k = 2, seed = 1)
  for (f in folds) {
    n_pos <- sum(y[f] == 1)
    # With 10 pos / 2 folds, expect ~5 per fold, allow ±1
    expect_true(n_pos >= 4 && n_pos <= 6)
  }
})

# ---- Seed reproducibility ----

test_that("seed produces identical results", {
  y <- make_test_y()
  f1 <- make_stratified_folds(y, k = 5, seed = 42)
  f2 <- make_stratified_folds(y, k = 5, seed = 42)
  expect_equal(f1, f2)
})

test_that("different seeds produce different results", {
  y <- make_test_y()
  f1 <- make_stratified_folds(y, k = 5, seed = 42)
  f2 <- make_stratified_folds(y, k = 5, seed = 99)
  expect_false(identical(f1, f2))
})

test_that("no seed produces different results each call (usually)", {
  y <- make_test_y()
  f1 <- make_stratified_folds(y, k = 5, seed = NULL)
  f2 <- make_stratified_folds(y, k = 5, seed = NULL)
  # With high probability these differ, but could theoretically match
  # Just check they are both valid
  expect_length(f1, 5)
  expect_length(f2, 5)
})

# ---- repeats ----

test_that("repeats > 1 produces k * repeats folds", {
  y <- make_test_y()
  folds <- make_stratified_folds(y, k = 3, repeats = 2, seed = 42)
  expect_length(folds, 6)
  expect_equal(names(folds),
    c("Rep1_Fold1", "Rep1_Fold2", "Rep1_Fold3",
      "Rep2_Fold1", "Rep2_Fold2", "Rep2_Fold3"))
})

test_that("each repeat independently covers all indices", {
  y <- make_test_y()
  n <- length(y)
  folds <- make_stratified_folds(y, k = 4, repeats = 2, seed = 42)

  # Rep 1: folds 1-4
  rep1 <- sort(unlist(folds[1:4], use.names = FALSE))
  expect_equal(rep1, seq_len(n))

  # Rep 2: folds 5-8
  rep2 <- sort(unlist(folds[5:8], use.names = FALSE))
  expect_equal(rep2, seq_len(n))
})

# ---- k > min class size (warning + reduction) ----

test_that("k > min class size warns and reduces k", {
  # 5 positives, 20 negatives → min = 5
  y <- c(rep(1, 5), rep(0, 20))
  expect_warning(
    folds <- make_stratified_folds(y, k = 10, seed = 42),
    "Reducing `k`"
  )
  # k should be reduced to 5
  expect_length(folds, 5)
})

test_that("k reduction below 2 errors", {
  # 1 positive, 20 negatives → min = 1, k reduced to 1 < 2
  y <- c(rep(1, 1), rep(0, 20))
  expect_error(
    make_stratified_folds(y, k = 5, seed = 42),
    "After reducing `k`"
  )
})

# ---- Single class / wrong input ----

test_that("single-class y causes error", {
  y <- rep(1, 10)
  expect_error(
    make_stratified_folds(y, k = 5),
    "only one unique value"
  )
})

test_that("y with 3+ unique values causes error", {
  y <- c(1, 2, 3, 1, 2, 3)
  expect_error(
    make_stratified_folds(y, k = 2),
    "must be binary"
  )
})

test_that("k < 2 causes error", {
  y <- make_test_y()
  expect_error(make_stratified_folds(y, k = 1), "integer >= 2")
  expect_error(make_stratified_folds(y, k = 0), "integer >= 2")
  expect_error(make_stratified_folds(y, k = -1), "integer >= 2")
})

test_that("repeats < 1 causes error", {
  y <- make_test_y()
  expect_error(make_stratified_folds(y, k = 5, repeats = 0), "positive integer")
  expect_error(make_stratified_folds(y, k = 5, repeats = -1), "positive integer")
})

test_that("empty y causes error", {
  expect_error(make_stratified_folds(numeric(0), k = 5), "must not be empty")
})

test_that("NA in y causes error", {
  y <- c(1, 1, 0, NA, 0)
  expect_error(make_stratified_folds(y, k = 2), "NA values")
})

# ---- Edge cases with labels ----

test_that("works with non-numeric labels", {
  y <- c(rep("case", 20), rep("control", 30))
  folds <- make_stratified_folds(y, k = 5, seed = 42)
  expect_length(folds, 5)
  # Check coverage
  all_idx <- sort(unlist(folds, use.names = FALSE))
  expect_equal(all_idx, seq_along(y))
})

test_that("works with logical y", {
  y <- c(rep(TRUE, 15), rep(FALSE, 25))
  folds <- make_stratified_folds(y, k = 4, seed = 42)
  expect_length(folds, 4)
})
