# test-execution-preview.R - Phase A execution preview API and visualization tests

.preview_test_data <- function(n_items = 6L, n_pos = 15L, n_neg = 15L) {
  set.seed(42L)
  n_total <- n_pos + n_neg
  mat <- matrix(sample(0:2, n_total * n_items, replace = TRUE), nrow = n_total, ncol = n_items)
  df <- as.data.frame(mat)
  names(df) <- paste0("Q", seq_len(n_items))
  df$y <- rep(c(0L, 1L), c(n_neg, n_pos))
  df
}

test_that("plan_ncvroc_execution returns valid ncvroc_execution_plan object for cross_size_cv", {
  d <- .preview_test_data(6L)
  plan <- plan_ncvroc_execution(
    data = d, outcome = y, workflow = "cross_size_cv",
    model_sizes = 1:2, folds = 3, repeats = 1
  )

  expect_s3_class(plan, "ncvroc_execution_plan")
  expect_identical(plan$workflow, "cross_size_cv")
  expect_type(plan$workload, "list")
  expect_true(plan$workload$total_candidates > 0)
  expect_type(plan$selected_plan, "list")
  expect_true(plan$selected_plan$resource_count >= 1L)
  expect_type(plan$benchmark_table, "list")
})

test_that("plan_ncvroc_execution works for exhaustive and nested workflows", {
  d <- .preview_test_data(5L, n_pos = 10L, n_neg = 10L)

  plan_ex <- plan_ncvroc_execution(
    data = d, outcome = y, workflow = "exhaustive",
    model_sizes = 1:2
  )
  expect_s3_class(plan_ex, "ncvroc_execution_plan")
  expect_identical(plan_ex$workflow, "exhaustive")

  plan_nested <- plan_ncvroc_execution(
    data = d, outcome = y, workflow = "nested_sum_roc",
    model_sizes = 1:2, outer_folds = 2, inner_folds = 2
  )
  expect_s3_class(plan_nested, "ncvroc_execution_plan")
  expect_identical(plan_nested$workflow, "nested_sum_roc")
})

test_that("plan_ncvroc_execution preserves the caller RNG state for every workflow", {
  d <- .preview_test_data(4L, n_pos = 10L, n_neg = 10L)
  calls <- list(
    exhaustive = function() plan_ncvroc_execution(
      d, y, workflow = "exhaustive", model_sizes = 1:2, max_resources = 1L
    ),
    cross_size_cv = function() plan_ncvroc_execution(
      d, y, workflow = "cross_size_cv", model_sizes = 1:2,
      folds = 2L, max_resources = 1L
    ),
    nested_sum_roc = function() plan_ncvroc_execution(
      d, y, workflow = "nested_sum_roc", model_sizes = 1:2,
      outer_folds = 2L, inner_folds = 2L, max_resources = 1L
    ),
    cross_size_nested_cv = function() plan_ncvroc_execution(
      d, y, workflow = "cross_size_nested_cv", model_sizes = 1:2,
      outer_folds = 2L, inner_folds = 2L, max_resources = 1L
    )
  )

  for (workflow in names(calls)) {
    set.seed(20260829L)
    before <- .Random.seed
    calls[[workflow]]()
    expect_identical(.Random.seed, before, info = workflow)
  }
})

test_that("print and format methods for ncvroc_execution_plan return human-readable text", {
  d <- .preview_test_data(6L)
  plan <- plan_ncvroc_execution(
    data = d, outcome = y, workflow = "cross_size_cv",
    model_sizes = 1:2, folds = 3
  )

  formatted <- format(plan)
  expect_type(formatted, "character")
  expect_match(formatted, "NCVROC Execution Plan & Runtime Preview")
  expect_match(formatted, "Workflow: cross_size_cv")
  expect_match(formatted, "Total Candidates:")
  expect_match(formatted, "Selected Execution Plan")

  # Print should run cleanly
  expect_output(print(plan), "NCVROC Execution Plan & Runtime Preview")
})

test_that("plot.ncvroc_execution_plan generates base R plots without error", {
  d <- .preview_test_data(10L)
  plan <- plan_ncvroc_execution(
    data = d, outcome = y, workflow = "cross_size_cv",
    model_sizes = 1:3, folds = 3
  )

  pdf_tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_tmp), add = TRUE)
  grDevices::pdf(pdf_tmp)

  expect_message(plot(plan, type = "runtime"), "No successful")
  expect_message(plot(plan, type = "speedup"), "No successful")
  expect_message(plot(plan, type = "efficiency"), "No successful")
  expect_message(plot(plan, type = "all"), "No successful")

  grDevices::dev.off()
})
