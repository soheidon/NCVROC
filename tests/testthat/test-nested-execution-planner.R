# test-nested-execution-planner.R

testthat::test_that(".planner_generate_nested_legal_plans creates valid nested plan tables", {
  # 1. Serial only when resources = 1
  p1 <- .planner_generate_nested_legal_plans(
    api = "nested_sum_roc",
    available_resources = 1L,
    n_outer_tasks = 5L,
    inner_candidate_tasks = 50L,
    engine = "Rcpp"
  )
  testthat::expect_equal(nrow(p1), 1L)
  testthat::expect_equal(p1$plan_id[[1L]], "none_1")
  testthat::expect_equal(p1$backend_priority[[1L]], 1L)

  # 2. Multi-core Rcpp generates threads, outer, and hybrid plans
  p8 <- .planner_generate_nested_legal_plans(
    api = "nested_sum_roc",
    available_resources = 8L,
    n_outer_tasks = 5L,
    inner_candidate_tasks = 50L,
    engine = "Rcpp",
    is_chunks_allowed = FALSE,
    is_hybrid_allowed = TRUE
  )
  testthat::expect_true("none" %in% p8$parallel)
  testthat::expect_true("threads" %in% p8$parallel)
  testthat::expect_true("outer" %in% p8$parallel)
  testthat::expect_true("hybrid" %in% p8$parallel)

  # Check priority values
  testthat::expect_equal(p8$backend_priority[p8$parallel == "none"][[1L]], 1L)
  testthat::expect_equal(p8$backend_priority[p8$parallel == "threads"][[1L]], 2L)
  testthat::expect_equal(p8$backend_priority[p8$parallel == "outer"][[1L]], 3L)
  testthat::expect_equal(p8$backend_priority[p8$parallel == "hybrid"][[1L]], 5L)

  # Check resource caps
  testthat::expect_true(all(p8$resource_count <= 8L))
  testthat::expect_true(all(p8$outer_workers <= 5L))

  # 3. Engine = R prohibits threads and hybrid
  pR <- .planner_generate_nested_legal_plans(
    api = "nested_sum_roc",
    available_resources = 8L,
    n_outer_tasks = 5L,
    inner_candidate_tasks = 50L,
    engine = "R",
    is_chunks_allowed = FALSE,
    is_hybrid_allowed = TRUE
  )
  testthat::expect_false("threads" %in% pR$parallel)
  testthat::expect_false("hybrid" %in% pR$parallel)
  testthat::expect_true(all(pR$parallel %in% c("none", "outer")))
})

testthat::test_that("nested_sum_roc statistical exactness across tuning modes", {
  set.seed(42)
  n <- 80
  d <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE)
  )

  # tuning = "off"
  set.seed(123)
  res_off <- nested_sum_roc(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_k = 3, inner_k = 2,
    seed = 100, engine = "R", tuning = "off",
    progress = FALSE, verbose = FALSE
  )
  rng_off <- .Random.seed

  # tuning = "auto"
  set.seed(123)
  res_auto <- nested_sum_roc(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_k = 3, inner_k = 2,
    seed = 100, engine = "R", tuning = "auto",
    progress = FALSE, verbose = FALSE
  )
  rng_auto <- .Random.seed

  # tuning = "always"
  set.seed(123)
  res_always <- nested_sum_roc(
    data = d, outcome = "y", items = c("q1", "q2", "q3", "q4"),
    min_items = 1, max_items = 2, outer_k = 3, inner_k = 2,
    seed = 100, engine = "R", tuning = "always",
    progress = FALSE, verbose = FALSE
  )
  rng_always <- .Random.seed

  # Statistical exactness checks
  testthat::expect_equal(res_off$summary, res_auto$summary)
  testthat::expect_equal(res_off$summary, res_always$summary)

  testthat::expect_equal(res_off$selected_models, res_auto$selected_models)
  testthat::expect_equal(res_off$selected_models, res_always$selected_models)

  testthat::expect_equal(res_off$selected_model_frequency, res_auto$selected_model_frequency)
  testthat::expect_equal(res_off$selected_model_frequency, res_always$selected_model_frequency)

  testthat::expect_equal(res_off$outer_predictions, res_auto$outer_predictions)
  testthat::expect_equal(res_off$outer_predictions, res_always$outer_predictions)

  # RNG state preservation check
  testthat::expect_equal(rng_off, rng_auto)
  testthat::expect_equal(rng_off, rng_always)

  # Metadata attachment contract
  testthat::expect_null(res_off$settings$execution_plan)
  testthat::expect_null(attr(res_off, "execution_plan"))

  testthat::expect_true(is.list(res_auto$settings$execution_plan))
  testthat::expect_equal(res_auto$settings$execution_plan$target_api, "nested_sum_roc")
  testthat::expect_equal(res_auto$settings$execution_plan$tuning_mode, "auto")

  testthat::expect_true(is.list(res_always$settings$execution_plan))
  testthat::expect_equal(res_always$settings$execution_plan$target_api, "nested_sum_roc")
  testthat::expect_equal(res_always$settings$execution_plan$tuning_mode, "always")
})

testthat::test_that("nested_sum_roc with Rcpp engine and hybrid execution plan", {
  set.seed(42)
  n <- 60
  d <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE)
  )

  # Rcpp with tuning = "always"
  res <- nested_sum_roc(
    data = d, outcome = "y", items = c("q1", "q2", "q3"),
    min_items = 1, max_items = 2, outer_k = 2, inner_k = 2,
    seed = 42, engine = "Rcpp", tuning = "always",
    progress = FALSE, verbose = FALSE
  )

  testthat::expect_s3_class(res, "ncvroc_result")
  testthat::expect_true(is.list(res$settings$execution_plan))
  testthat::expect_equal(res$settings$execution_plan$target_api, "nested_sum_roc")
  testthat::expect_equal(res$settings$execution_plan$outer_folds, 2L)
  testthat::expect_equal(res$settings$execution_plan$inner_folds, 2L)

  # Formatting helper check
  formatted <- .planner_format_execution_plan(res)
  testthat::expect_true(grepl("Target API:\\s+nested_sum_roc", formatted))
  testthat::expect_true(grepl("Nested CV Structure:", formatted))
})
