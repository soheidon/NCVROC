# test-nested.R — Tests for nested_sum_roc()

# ---- Return structure ----

test_that("nested_sum_roc returns object of class ncvroc_result", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
})

test_that("nested_sum_roc result has all 6 required elements", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expected <- c("summary", "outer_results", "selected_models",
                "selected_model_frequency", "outer_predictions", "settings")
  expect_true(all(expected %in% names(res)))
})

test_that("summary is a data.frame with correct columns", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res$summary, "data.frame")
  expected_cols <- c("outer_fold", "selected_items", "n_items", "auc",
                     "cutoff", "sensitivity", "specificity", "youden",
                     "accuracy", "ppv", "npv")
  expect_true(all(expected_cols %in% colnames(res$summary)))
})

test_that("outer_results length equals outer_k", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_length(res$outer_results, 3)
})

# ---- Data integrity ----

test_that("outer_predictions has nrow(data) rows (single repeat)", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_equal(nrow(res$outer_predictions), nrow(d))
})

test_that("each row index appears exactly once in outer_predictions", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_equal(sort(res$outer_predictions$row_index), seq_len(nrow(d)))
})

test_that("outer_predictions true_outcome matches original y", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  # After converting labels, y = 1 means positive
  n_pos <- sum(d$y == 1)
  n_pos_pred <- sum(res$outer_predictions$true_outcome == 1)
  expect_equal(n_pos_pred, n_pos)
})

test_that("selected_model_frequency frequencies sum to 1.0", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_equal(sum(res$selected_model_frequency$frequency), 1.0)
})

# ---- Model selection validity ----

test_that("selected items are valid subsets of input items", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  for (sel in res$selected_models) {
    sel_items <- .parse_itemset(sel)
    expect_true(all(sel_items %in% c("q1", "q2", "q3")))
  }
})

test_that("n_items in summary is between min_items and max_items", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    min_items = 1, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_true(all(res$summary$n_items >= 1))
  expect_true(all(res$summary$n_items <= 2))
})

# ---- Reproducibility ----

test_that("same seed produces identical results", {
  d <- make_nested_test_data()
  res1 <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  res2 <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_identical(res1$summary, res2$summary)
  expect_identical(res1$selected_models, res2$selected_models)
})

test_that("no seed runs without error", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
})

# ---- Parameter handling ----

test_that("preselect_top_n clips when candidates < N", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2"),
    max_items = 2, outer_k = 3, inner_k = 2,
    preselect_top_n = 100, seed = 42,
    progress = FALSE, verbose = FALSE)
  # Should work without error (clips to available candidates)
  expect_s3_class(res, "ncvroc_result")
})

test_that("selection_criterion = 'youden' works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2,
    selection_criterion = "youden", seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
})

test_that("preselect_by = 'youden' works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2,
    preselect_by = "youden", seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
})

test_that("cutoff_method = 'closest_topleft' works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2,
    cutoff_method = "closest_topleft", seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
})

test_that("stratified = FALSE causes error", {
  d <- make_nested_test_data()
  expect_error(
    nested_sum_roc(d, "y", c("q1"), stratified = FALSE,
      progress = FALSE, verbose = FALSE),
    "stratified = TRUE"
  )
})

test_that("inner_repeats > 1 warns and uses 1", {
  d <- make_nested_test_data()
  expect_warning(
    res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
      max_items = 2, outer_k = 3, inner_k = 2,
      inner_repeats = 2, seed = 42,
      progress = FALSE, verbose = FALSE),
    "inner_repeats"
  )
  expect_s3_class(res, "ncvroc_result")
})

test_that("output_dir non-NULL warns", {
  d <- make_nested_test_data()
  expect_warning(
    res <- nested_sum_roc(d, "y", c("q1"),
      outer_k = 3, inner_k = 2,
      output_dir = "/tmp/test", seed = 42,
      progress = FALSE, verbose = FALSE),
    "CSV output"
  )
  expect_s3_class(res, "ncvroc_result")
})

# ---- Edge cases ----

test_that("constant outcome causes error", {
  d <- data.frame(
    y = rep(1, 10),
    q1 = 1:10
  )
  expect_error(
    nested_sum_roc(d, "y", c("q1"), outer_k = 3, inner_k = 2,
      progress = FALSE, verbose = FALSE),
    "only one unique value"
  )
})

test_that("NA in data causes error", {
  d <- make_nested_test_data()
  d$q1[1] <- NA
  expect_error(
    nested_sum_roc(d, "y", c("q1", "q2", "q3"),
      max_items = 2, outer_k = 3, inner_k = 2,
      progress = FALSE, verbose = FALSE),
    "NA values"
  )
})

test_that("outer_k = 2, inner_k = 2 minimal case works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 2, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  expect_length(res$outer_results, 2)
})

test_that("max_items = 1 works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 1, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  expect_true(all(res$summary$n_items == 1))
})

# ---- outer_repeats ----

test_that("outer_repeats > 1 works", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2"),
    max_items = 2, outer_k = 2, outer_repeats = 2,
    inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_s3_class(res, "ncvroc_result")
  # outer_k * outer_repeats folds
  expect_length(res$outer_results, 4)
  expect_equal(nrow(res$summary), 4)
  # Each row appears outer_repeats times in predictions
  expect_equal(nrow(res$outer_predictions), nrow(d) * 2)
})

# ---- S3 methods smoke test ----

test_that("print.ncvroc_result produces output", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_output(print(res), "NCVROC")
})

test_that("summary.ncvroc_result produces output", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_output(summary(res), "Mean AUC")
})

test_that("plot.ncvroc_result with selection renders without error", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_silent(plot(res, which = "selection"))
})

test_that("plot.ncvroc_result with auc renders without error", {
  d <- make_nested_test_data()
  res <- nested_sum_roc(d, "y", c("q1", "q2", "q3"),
    max_items = 2, outer_k = 3, inner_k = 2, seed = 42,
    progress = FALSE, verbose = FALSE)
  expect_silent(plot(res, which = "auc"))
})
