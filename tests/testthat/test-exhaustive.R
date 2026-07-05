# test-exhaustive.R — Tests for exhaustive_sum_roc()

# ---- Basic return structure ----

test_that("exhaustive_sum_roc returns a data.frame", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("exhaustive_sum_roc has all required columns", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  expected_cols <- c("rank", "items", "n_items", "auc", "cutoff",
                     "sensitivity", "specificity", "youden", "accuracy",
                     "ppv", "npv", "n_positive", "n_negative")
  expect_true(all(expected_cols %in% colnames(res)))
})

test_that("exhaustive_sum_roc returns correct number of rows", {
  d <- make_test_data()
  # C(3,1)=3, C(3,2)=3, C(3,3)=1 → 7 combinations
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 3,
                            progress = FALSE)
  expect_equal(nrow(res), 7)
})

# ---- AUC bounds ----

test_that("AUC values are in [0, 1] or NA", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  for (auc_val in res$auc) {
    if (!is.na(auc_val)) {
      expect_true(auc_val >= 0 && auc_val <= 1,
                  info = sprintf("AUC = %f out of bounds", auc_val))
    }
  }
})

# ---- Sorting ----

test_that("exhaustive_sum_roc sorts by rank_by descending", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            rank_by = "auc", progress = FALSE)
  expect_equal(res$auc, sort(res$auc, decreasing = TRUE))
})

test_that("exhaustive_sum_roc sorts by youden descending", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            rank_by = "youden", progress = FALSE)
  expect_equal(res$youden, sort(res$youden, decreasing = TRUE))
})

test_that("exhaustive_sum_roc rank column is sequential", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  expect_equal(res$rank, seq_len(nrow(res)))
})

# ---- prefer_fewer_items ----

test_that("prefer_fewer_items = TRUE puts fewer items first on ties", {
  # Create data where different-length combos have same AUC
  set.seed(42)
  d <- data.frame(
    y = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0)
  )
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                            rank_by = "auc", prefer_fewer_items = TRUE,
                            progress = FALSE)
  # Both 1-item combos should come before the 2-item combo if AUCs are similar,
  # but AUCs may differ. Just verify ordering is valid.
  # If 2 models have same auc, the one with fewer items should be first.
  for (i in seq_len(nrow(res) - 1)) {
    if (res$auc[i] == res$auc[i + 1]) {
      expect_true(res$n_items[i] <= res$n_items[i + 1],
                  info = sprintf("Row %d has n_items=%d, row %d has n_items=%d with same AUC",
                                 i, res$n_items[i], i + 1, res$n_items[i + 1]))
    }
  }
})

test_that("prefer_fewer_items = FALSE does not enforce item-count ordering", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                            rank_by = "auc", prefer_fewer_items = FALSE,
                            progress = FALSE)
  # Just check it doesn't error — actual ordering depends on the data
  expect_s3_class(res, "data.frame")
})

# ---- top_n ----

test_that("top_n truncates results", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            top_n = 3, progress = FALSE)
  expect_equal(nrow(res), 3)
  expect_equal(res$rank, 1:3)
})

test_that("top_n = NULL returns all results", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            top_n = NULL, progress = FALSE)
  expect_equal(nrow(res), 6)  # C(3,1) + C(3,2) = 3 + 3 = 6
})

test_that("top_n = 0 errors", {
  d <- make_test_data()
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1"), top_n = 0, progress = FALSE),
    "positive integer"
  )
})

test_that("top_n negative errors", {
  d <- make_test_data()
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1"), top_n = -1, progress = FALSE),
    "positive integer"
  )
})

# ---- min_items / max_items ----

test_that("min_items and max_items control combination sizes", {
  d <- make_test_data()
  # min_items=2, max_items=2: only 2-item combos: C(3,2)=3
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"),
                            min_items = 2, max_items = 2, progress = FALSE)
  expect_equal(nrow(res), 3)
  expect_true(all(res$n_items == 2))
})

test_that("max_items = 1 returns only single-item models", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"),
                            max_items = 1, progress = FALSE)
  expect_equal(nrow(res), 3)
  expect_true(all(res$n_items == 1))
})

# ---- positive_label / negative_label ----

test_that("positive_label / negative_label conversion works", {
  d <- data.frame(
    y  = c("case", "case", "control", "control", "case",
           "control", "case", "case", "control", "control"),
    q1 = c(2, 1, 2, 0, 1, 1, 2, 2, 0, 1),
    stringsAsFactors = FALSE
  )
  res <- exhaustive_sum_roc(d, "y", c("q1"),
                            positive_label = "case",
                            negative_label = "control",
                            progress = FALSE)
  expect_equal(res$n_positive, 5)
  expect_equal(res$n_negative, 5)
})

# ---- cutoff_method ----

test_that("cutoff_method = 'closest_topleft' works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                            cutoff_method = "closest_topleft",
                            progress = FALSE)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("cutoff", "sensitivity", "specificity") %in% colnames(res)))
})

test_that("cutoff_method = 'youden' works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                            cutoff_method = "youden",
                            progress = FALSE)
  expect_s3_class(res, "data.frame")
})

# ---- engine argument ----

test_that("engine = 'R' works (only option in v0.1)", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1"), engine = "R", progress = FALSE)
  expect_s3_class(res, "data.frame")
})

# ---- progress flag ----

test_that("progress = FALSE runs without error", {
  d <- make_test_data()
  expect_silent(
    exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2, progress = FALSE)
  )
})

# ---- items column format ----

test_that("items column is comma-separated string", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  expect_type(res$items, "character")
  # Multi-item combos should contain ", "
  multi <- res[res$n_items > 1, ]
  if (nrow(multi) > 0) {
    expect_true(all(grepl(", ", multi$items, fixed = TRUE)))
  }
})

# ---- Edge cases ----

test_that("NA in data causes error", {
  d <- make_test_data()
  d$q1[3] <- NA
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2, progress = FALSE),
    "NA values"
  )
})

test_that("Non-binary outcome (3+ values) causes error", {
  d <- data.frame(
    y  = c(1, 2, 0, 1, 0),
    q1 = c(1, 2, 0, 1, 0)
  )
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1"), positive_label = 1,
                       negative_label = 0, progress = FALSE),
    "not matching positive_label"
  )
})

test_that("All-same outcome causes error", {
  d <- data.frame(
    y  = c(1, 1, 1),
    q1 = c(0, 1, 2)
  )
  expect_error(
    exhaustive_sum_roc(d, "y", c("q1"), progress = FALSE),
    "only one unique value"
  )
})

test_that("Single item with max_items=1 works", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1"), max_items = 1, progress = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$items, "q1")
  expect_equal(res$n_items, 1)
})

test_that("Single item with max_items=2 clips to 1 row", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1"), max_items = 2, progress = FALSE)
  expect_equal(nrow(res), 1)
})

# ---- Output values sanity ----

test_that("sensitivity and specificity are in [0, 1]", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  for (s in res$sensitivity) {
    expect_true(s >= 0 && s <= 1)
  }
  for (s in res$specificity) {
    expect_true(s >= 0 && s <= 1)
  }
})

test_that("youden = sensitivity + specificity - 1", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2,
                            progress = FALSE)
  youden_check <- res$sensitivity + res$specificity - 1
  # Allow small floating point differences
  expect_equal(res$youden, youden_check, tolerance = 1e-12)
})

test_that("n_positive and n_negative are correct", {
  d <- make_test_data()
  res <- exhaustive_sum_roc(d, "y", c("q1", "q2"), max_items = 2,
                            progress = FALSE)
  n_pos <- sum(d$y == 1)
  n_neg <- sum(d$y == 0)
  expect_equal(unique(res$n_positive), n_pos)
  expect_equal(unique(res$n_negative), n_neg)
})
