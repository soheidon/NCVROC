# Phase 1.1 exhaustive_sum_roc execution-planner integration tests

.planner_test_data <- function(n_items = 5L) {
  set.seed(710L)
  data.frame(
    y = rep(c(0L, 1L), each = 10L),
    setNames(as.data.frame(matrix(sample(0:2, 20L * n_items, TRUE), 20L, n_items)),
             paste0("q", seq_len(n_items)))
  )
}

.with_mocked_exhaustive_controller <- function(controller, fun) {
  original <- get(".planner_exhaustive_controller", envir = asNamespace("NCVROC"))
  assignInNamespace(".planner_exhaustive_controller", controller, ns = "NCVROC")
  on.exit(assignInNamespace(".planner_exhaustive_controller", original, ns = "NCVROC"), add = TRUE)
  fun()
}

.forced_execution_plan <- function(parallel, n_workers = 2L, warn = FALSE,
                                   reason = NA_character_) {
  list(
    plan = data.frame(parallel = parallel, n_workers = as.integer(n_workers),
                      resource_count = as.integer(n_workers), stringsAsFactors = FALSE),
    metadata = list(selected_parallel = parallel, selected_n_workers = as.integer(n_workers),
                    selected_resource_count = as.integer(n_workers),
                    fallback_reason = reason, decision_reason = reason),
    warn = warn
  )
}

test_that("tuning off is byte-for-byte compatible and carries no metadata", {
  d <- .planner_test_data(3L)
  baseline <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                                 engine = "Rcpp", parallel = "none", progress = FALSE)
  off <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                            engine = "Rcpp", parallel = "none", progress = FALSE,
                            tuning = "off")
  expect_identical(off, baseline)
  expect_null(attr(off, "execution_plan"))
})

test_that("auto small and always degenerate preserve exhaustive statistics", {
  d <- .planner_test_data(3L)
  off <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                            engine = "Rcpp", progress = FALSE)
  auto <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                             engine = "Rcpp", progress = FALSE, tuning = "auto")
  always <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                               engine = "Rcpp", progress = FALSE, tuning = "always")
  expect_false(attr(auto, "execution_plan")$backend_benchmark_performed)
  expect_match(attr(always, "execution_plan")$decision_reason, "degenerate")
  expect_identical(attr(always, "execution_plan")$micro_pilot_candidates$total, 6L)
  attr(auto, "execution_plan") <- NULL
  attr(always, "execution_plan") <- NULL
  expect_identical(auto, off)
  expect_identical(always, off)
})

test_that("controller compares identical deterministic pilot ranks across plans", {
  d <- .planner_test_data(5L)
  seen <- list()
  executor <- function(x_mat, y, items, min_items, max_items, cutoff_method,
                       engine, global_ranks, plan, timer) {
    if (length(global_ranks) == 25L) {
      seen[[length(seen) + 1L]] <<- global_ranks
    }
    elapsed <- switch(plan$parallel[[1L]], none = 4, threads = 1, chunks = 2)
    list(elapsed = elapsed, success = TRUE, failure_reason = NA_character_)
  }
  plan <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 3L, "youden", "Rcpp", "always",
    "none", 2L, 5L,
    dependencies = list(resource_detector = function() 2L, benchmark_executor = executor)
  )
  expect_true(plan$metadata$backend_benchmark_performed)
  expect_identical(plan$plan$parallel[[1L]], "chunks")
  expect_true(length(seen) >= 3L)
  expect_true(all(vapply(seen, identical, logical(1), seen[[1L]])))
  expect_false(any(c("global_rank", "items", "combination") %in%
                     names(plan$metadata$micro_pilot_candidates)))
  expect_lte(plan$metadata$micro_pilot_candidates$total, 96L)
})

test_that("R engine creates no threads automatic plans and all failures use manual fallback", {
  d <- .planner_test_data(5L)
  plans_seen <- character()
  failed <- function(x_mat, y, items, min_items, max_items, cutoff_method,
                     engine, global_ranks, plan, timer) {
    plans_seen <<- c(plans_seen, plan$parallel[[1L]])
    if (length(global_ranks) < 25L) {
      return(list(elapsed = 0.01, success = TRUE, failure_reason = NA_character_))
    }
    list(elapsed = NA_real_, success = FALSE, failure_reason = "injected failure")
  }
  result <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 3L, "closest_topleft", "R", "always",
    "chunks", 2L, 5L,
    dependencies = list(resource_detector = function() 2L, benchmark_executor = failed)
  )
  expect_false("threads" %in% plans_seen)
  expect_identical(result$plan$parallel[[1L]], "chunks")
  expect_true(result$warn)
  expect_match(result$metadata$fallback_reason, "all benchmark plans failed")
  one_worker <- NCVROC:::.planner_manual_exhaustive_plan("chunks", 1L, 25, 5L)
  expect_identical(one_worker$n_workers[[1L]], 1L)
})

test_that("planning preserves RNG, metadata shape, and single chunks skip planning", {
  d <- .planner_test_data(5L)
  set.seed(911L); before <- .Random.seed
  planned <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 3L,
                                engine = "Rcpp", progress = FALSE, tuning = "always")
  expect_identical(.Random.seed, before)
  expect_false("execution_plan" %in% names(planned))
  expect_s3_class(planned, "data.frame")
  chunk <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 3L,
                              engine = "Rcpp", progress = FALSE,
                              chunk_start = 0, chunk_size = 2L, tuning = "always")
  expect_null(attr(chunk, "execution_plan"))
})

test_that("sparse threaded benchmark primitive matches the serial Rcpp evaluator", {
  d <- .planner_test_data(4L)
  x <- as.matrix(d[, -1L]); ranks <- c(0, 2, 4, 8)
  sparse <- NCVROC:::evaluate_combos_cpp_sparse_parallel(
    x, d$y, 1L, 2L, "youden", ranks, 2L
  )
  combos <- lapply(ranks, function(rank) {
    found <- NCVROC:::.resolve_global_combination_rank(4L, 1L, 2L, rank)
    NCVROC:::.combination_unrank(4L, found$k, found$rank_within_k)
  })
  serial <- NCVROC:::evaluate_combos_cpp(x, d$y, combos, "youden")
  expect_equal(sparse, serial)
})

test_that("chunk timing expression encloses its full worker lifecycle", {
  events <- character()
  timer <- function(expr) {
    events <<- c(events, "timer_start")
    force(expr)
    events <<- c(events, "timer_end")
    1
  }
  lifecycle <- function(plan, setup, evaluate) {
    NCVROC:::.planner_with_chunk_benchmark_lifecycle(
      plan, setup, evaluate,
      cluster_factory = function(workers) { events <<- c(events, "create"); list() },
      cluster_stopper = function(cl) events <<- c(events, "stop")
    )
  }
  elapsed <- timer(lifecycle(
    data.frame(parallel = "chunks", n_workers = 2L, resource_count = 2L),
    setup = function(cl) events <<- c(events, "setup"),
    evaluate = function(cl) events <<- c(events, "evaluate")
  ))
  expect_identical(elapsed, 1)
  expect_identical(events, c("timer_start", "create", "setup", "evaluate", "stop", "timer_end"))
})

test_that("thread plans honor the final 1000-candidate grain task cap", {
  d <- .planner_test_data(13L)
  executor <- function(x_mat, y, items, min_items, max_items, cutoff_method,
                       engine, global_ranks, plan, timer) {
    list(elapsed = 1, success = TRUE, failure_reason = NA_character_)
  }
  below <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 3L, "youden", "Rcpp", "always",
    "none", NULL, 5L, list(resource_detector = function() 8L,
                             benchmark_executor = executor, clock = function() 0)
  )
  expect_false(any(grepl("^threads_", below$metadata$benchmark_table$plan_id)))
  above <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 4L, "youden", "Rcpp", "always",
    "none", NULL, 5L, list(resource_detector = function() 8L,
                             benchmark_executor = executor, clock = function() 0)
  )
  threads <- above$metadata$benchmark_table[above$metadata$benchmark_table$parallel == "threads", , drop = FALSE]
  expect_true(nrow(threads) > 0L)
  expect_lte(max(threads$n_workers), ceiling(above$metadata$total_candidates / 1000))
  expect_false(any(threads$n_workers == 1L))
})

test_that("clock and failed micro-pilot flags deterministically govern fallback", {
  d <- .planner_test_data(5L)
  clock_values <- c(10, 11, 12, 13, 14, 15)
  clock <- function() {
    value <- clock_values[[1L]]
    clock_values <<- clock_values[-1L]
    value
  }
  failed_pilot <- function(x_mat, y, items, min_items, max_items, cutoff_method,
                           engine, global_ranks, plan, timer) {
    list(elapsed = 1, success = FALSE, failure_reason = "injected")
  }
  result <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 3L, "youden", "Rcpp", "always",
    "chunks", 1L, 0L, list(benchmark_executor = failed_pilot, clock = clock)
  )
  expect_true(is.na(result$metadata$estimated_serial_runtime))
  expect_identical(result$plan$n_workers[[1L]], 1L)
  expect_match(result$metadata$fallback_reason, "runtime estimate unavailable")
  expect_equal(result$metadata$planner_elapsed, 1)
})

test_that("benchmark all-failure fallback retains NA estimated runtime", {
  d <- .planner_test_data(5L)
  failing <- function(x_mat, y, items, min_items, max_items, cutoff_method,
                      engine, global_ranks, plan, timer) {
    if (length(global_ranks) < 25L) return(list(elapsed = 1, success = TRUE, failure_reason = NA_character_))
    list(elapsed = NA_real_, success = FALSE, failure_reason = "all failed")
  }
  result <- NCVROC:::.planner_exhaustive_controller(
    as.matrix(d[, -1L]), d$y, names(d)[-1L], 1L, 3L, "youden", "Rcpp", "always",
    "chunks", 2L, 5L, list(resource_detector = function() 2L,
                             benchmark_executor = failing, clock = function() 0)
  )
  expect_true(is.na(result$metadata$estimated_runtime))
  expect_match(result$metadata$fallback_reason, "all benchmark plans failed")
  expect_identical(result$metadata$decision_reason, result$metadata$fallback_reason)
  expect_true(result$warn)
})

test_that("closest-to-topleft ties and top_n remain exact under tuning", {
  d <- data.frame(y = c(1L, 1L, 0L, 0L), q1 = 1, q2 = 1, q3 = 1)
  off <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                            cutoff_method = "closest_topleft", top_n = 2L,
                            engine = "Rcpp", progress = FALSE)
  tuned <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                              cutoff_method = "closest_topleft", top_n = 2L,
                              engine = "Rcpp", progress = FALSE, tuning = "always")
  attr(tuned, "execution_plan") <- NULL
  expect_identical(tuned, off)
  expect_identical(tuned$items, c("q1", "q2"))
})

test_that("pilot output is discarded and final tuning evaluates the full space", {
  d <- .planner_test_data(10L)
  result <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 4L,
                               engine = "Rcpp", parallel = "none", progress = FALSE,
                               tuning = "always")
  metadata <- attr(result, "execution_plan")
  expect_identical(nrow(result), 385L)
  expect_identical(metadata$micro_pilot_candidates$total, 96L)
  expect_gt(nrow(result), metadata$micro_pilot_candidates$total)
  expect_false(any(c("global_rank", "rank_within_size", "items") %in%
                     names(metadata$micro_pilot_candidates)))
})

test_that("public tuning routes forced threads and chunks plans without changing statistics", {
  d <- .planner_test_data(3L)
  off <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                            engine = "Rcpp", progress = FALSE, parallel = "none")
  for (backend in c("threads", "chunks")) {
    forced <- .forced_execution_plan(backend)
    tuned <- .with_mocked_exhaustive_controller(
      function(...) forced,
      function() exhaustive_sum_roc(
        d, "y", names(d)[-1L], max_items = 2L, engine = "Rcpp",
        progress = FALSE, parallel = "none", chunk_size = 2L, tuning = "always"
      )
    )
    metadata <- attr(tuned, "execution_plan")
    attr(tuned, "execution_plan") <- NULL
    expect_identical(tuned, off)
    expect_identical(metadata$selected_parallel, backend)
    expect_identical(metadata$selected_n_workers, 2L)
  }
})

test_that("public tuning off never invokes the planner controller", {
  d <- .planner_test_data(3L)
  result <- .with_mocked_exhaustive_controller(
    function(...) stop("planner must not run"),
    function() exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                                   engine = "Rcpp", progress = FALSE, tuning = "off")
  )
  expect_s3_class(result, "data.frame")
  expect_null(attr(result, "execution_plan"))
})

test_that("public fallback emits one warning and follows the manual route", {
  d <- .planner_test_data(3L)
  baseline <- exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                                 engine = "Rcpp", parallel = "threads", n_workers = 2L,
                                 progress = FALSE)
  fallback <- .forced_execution_plan(
    "threads", 2L, warn = TRUE,
    reason = "all benchmark plans failed; using manual plan"
  )
  warnings <- character()
  result <- withCallingHandlers(
    .with_mocked_exhaustive_controller(
      function(...) fallback,
      function() exhaustive_sum_roc(d, "y", names(d)[-1L], max_items = 2L,
                                     engine = "Rcpp", parallel = "threads", n_workers = 2L,
                                     progress = FALSE, tuning = "always")
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  metadata <- attr(result, "execution_plan")
  attr(result, "execution_plan") <- NULL
  expect_identical(result, baseline)
  expect_length(warnings, 1L)
  expect_match(warnings[[1L]], "all benchmark plans failed")
  expect_identical(metadata$selected_parallel, "threads")
})
