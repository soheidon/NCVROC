# test-config.R — Tests for configuration helpers (v0.3)

# ---- count_item_combinations ----

test_that("count_item_combinations() returns correct totals", {
  # 103 items, max 4: C(103,1)+C(103,2)+C(103,3)+C(103,4)
  expect_equal(count_item_combinations(103, max_items = 4), 4603482)
  # 5 items, 2-3: C(5,2)+C(5,3) = 10+10 = 20
  expect_equal(count_item_combinations(5, min_items = 2, max_items = 3), 20)
})

test_that("count_item_combinations() clips max_items to n", {
  # 5 items but max_items=10 → clipped to 5
  expect_equal(count_item_combinations(letters[1:5], max_items = 10), 31)
})

test_that("count_item_combinations() detail = TRUE returns a data.frame", {
  res <- count_item_combinations(103, max_items = 4, detail = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  expect_equal(colnames(res), c("n_items", "n_combinations"))
  expect_equal(res$n_items, 1:4)
  expect_equal(res$n_combinations, c(103, 5253, 176851, 4421275))
})

test_that("count_item_combinations() accepts a character vector", {
  items <- letters[1:10]
  expect_equal(count_item_combinations(items, max_items = 2),
               choose(10, 1) + choose(10, 2))
})

test_that("count_item_combinations() errors on bad input", {
  expect_error(count_item_combinations(NULL, max_items = 4),
               "items_or_n must be")
  expect_error(count_item_combinations(c(1, 2, 3), max_items = 4),
               "items_or_n must be")
})

# ---- suggest_preselect_top_n ----

test_that("suggest_preselect_top_n() returns expected values per mode", {
  # 103 items, max 4: total = 4603482
  expect_equal(suggest_preselect_top_n(103, max_items = 4, mode = "quick"), 100)
  expect_equal(suggest_preselect_top_n(103, max_items = 4, mode = "balanced"), 500)
  expect_equal(suggest_preselect_top_n(103, max_items = 4, mode = "thorough"), 1000)
  expect_equal(suggest_preselect_top_n(103, max_items = 4, mode = "exhaustive"), 4603482)
})

test_that("suggest_preselect_top_n() caps at total combinations", {
  # 5 items, max 4: total = 30, so all modes return at most 30
  expect_equal(suggest_preselect_top_n(5, max_items = 4, mode = "quick"), 30)
  expect_equal(suggest_preselect_top_n(5, max_items = 4, mode = "balanced"), 30)
  expect_equal(suggest_preselect_top_n(5, max_items = 4, mode = "exhaustive"), 30)
})

test_that("suggest_preselect_top_n() works with character items", {
  items <- letters[1:5]
  total <- count_item_combinations(items, max_items = 4)
  expect_equal(suggest_preselect_top_n(items, max_items = 4, mode = "exhaustive"), total)
})

# ---- ncvroc_config ----

test_that("ncvroc_config() creates an object of correct class", {
  cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2, mode = "quick")
  expect_s3_class(cfg, "ncvroc_config")
  expect_type(cfg, "list")
})

test_that("ncvroc_config() auto-sets preselect_top_n from mode", {
  # 5 items, max 2: total = choose(5,1)+choose(5,2) = 5+10 = 15
  # quick  = min(100, 15) = 15
  cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2, mode = "quick")
  expect_equal(cfg$preselect_top_n, 15)

  cfg2 <- ncvroc_config("y", items = letters[1:5], max_items = 2, mode = "exhaustive")
  expect_equal(cfg2$preselect_top_n, 15)
})

test_that("ncvroc_config() respects explicit preselect_top_n", {
  cfg <- ncvroc_config("y", items = letters[1:10], max_items = 2,
                       preselect_top_n = 30)
  # total = 55, explicit 30 < 55 → kept as-is
  expect_equal(cfg$preselect_top_n, 30)
})

test_that("ncvroc_config() clips preselect_top_n to total_combinations", {
  # 5 items, choose(5,2) = 15 total, but explicit 999 → clipped to 15
  cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2,
                       preselect_top_n = 999)
  expect_equal(cfg$preselect_top_n, 15)
})

test_that("ncvroc_config() with items = NULL leaves derived fields NA/NULL", {
  cfg <- ncvroc_config("y", items = NULL)
  expect_true(is.na(cfg$n_items))
  expect_true(is.na(cfg$total_combinations))
  expect_null(cfg$preselect_top_n)
})

test_that("ncvroc_config() stores all expected fields", {
  cfg <- ncvroc_config("outcome_var", items = letters[1:20], mode = "thorough")
  expect_equal(cfg$outcome, "outcome_var")
  expect_equal(cfg$mode, "thorough")
  expect_equal(cfg$min_items, 1)
  expect_equal(cfg$max_items, 4)
  expect_equal(cfg$outer_k, 5)
  expect_equal(cfg$inner_k, 4)
  expect_equal(cfg$outer_repeats, 5)
  expect_equal(cfg$inner_repeats, 1)
  expect_equal(cfg$positive_label, 1)
  expect_equal(cfg$negative_label, 0)
  expect_equal(cfg$stratified, TRUE)
  expect_equal(cfg$engine, "Rcpp")
  expect_equal(cfg$cutoff_method, "youden")
})

test_that("ncvroc_config() validates mode and engine", {
  expect_error(ncvroc_config("y", mode = "invalid"), "should be one of")
  expect_error(ncvroc_config("y", engine = "python"), "should be one of")
})

test_that("ncvroc_config() with items = NULL and explicit preselect_top_n", {
  cfg <- ncvroc_config("y", items = NULL, preselect_top_n = 50)
  expect_equal(cfg$preselect_top_n, NULL)
})

# ---- print.ncvroc_config ----

test_that("print.ncvroc_config() works without error", {
  cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2, mode = "quick")
  expect_output(print(cfg), "NCVROC Configuration")
  expect_output(print(cfg), "Outcome:")
  expect_output(print(cfg), "Items:")
  expect_output(print(cfg), "Mode:")
})

test_that("print.ncvroc_config() handles items = NULL", {
  cfg <- ncvroc_config("y", items = NULL)
  expect_output(print(cfg), "\\(not specified\\)")
})

test_that("print.ncvroc_config() warns at high preselect_top_n", {
  # 103 items, max 2 → total = 5356, but with preselect_top_n = 100000
  # it gets clipped to 5356. Need total >= 100000 to actually trigger warn.
  # 103 items, max 4 → total = 4.6M, setting preselect_top_n = 100000
  cfg <- ncvroc_config("y", items = as.character(1:103), max_items = 4,
                       preselect_top_n = 100000)
  expect_output(print(cfg), "Warning.*100,000")
})

test_that("print.ncvroc_config() returns its input invisibly", {
  cfg <- ncvroc_config("y", items = letters[1:5], max_items = 2)
  out <- print(cfg)
  expect_identical(out, cfg)
})

# ---- run_ncvroc ----

test_that("run_ncvroc() errors on non-config input", {
  d <- make_test_data()
  expect_error(
    run_ncvroc(d, c("q1", "q2", "q3"), list(outcome = "y")),
    "config must be an object created by ncvroc_config"
  )
})

test_that("run_ncvroc() returns an ncvroc_result object", {
  d <- make_test_data()
  cfg <- ncvroc_config("y", items = c("q1", "q2", "q3"),
                       max_items = 2, mode = "exhaustive",
                       outer_k = 3, inner_k = 2, engine = "R")
  result <- run_ncvroc(d, c("q1", "q2", "q3"), cfg, seed = 42,
                       progress = FALSE, verbose = FALSE)
  expect_s3_class(result, "ncvroc_result")
  expect_s3_class(result$summary, "data.frame")
})

test_that("run_ncvroc() with return = 'summary' respects the option", {
  d <- make_test_data()
  cfg <- ncvroc_config("y", items = c("q1", "q2", "q3"),
                       max_items = 2, mode = "exhaustive",
                       outer_k = 3, inner_k = 2, engine = "R")
  result <- run_ncvroc(d, c("q1", "q2", "q3"), cfg, seed = 42,
                       progress = FALSE, verbose = FALSE, return = "summary")
  expect_s3_class(result, "ncvroc_result")
})

test_that("run_ncvroc() passes positive_label/negative_label from config", {
  d <- make_test_data()
  # Flip labels to check config-driven label mapping
  d_flipped <- d
  d_flipped$y <- ifelse(d$y == 1, 2, -1)

  cfg <- ncvroc_config("y", items = c("q1", "q2", "q3"),
                       max_items = 2, mode = "exhaustive",
                       outer_k = 3, inner_k = 2, outer_repeats = 1,
                       engine = "R",
                       positive_label = 2, negative_label = -1)
  result <- run_ncvroc(d_flipped, c("q1", "q2", "q3"), cfg, seed = 42,
                       progress = FALSE, verbose = FALSE)
  expect_s3_class(result, "ncvroc_result")

  # Compare with direct call using same labels
  direct <- nested_sum_roc(d_flipped, "y", c("q1", "q2", "q3"),
                           max_items = 2, positive_label = 2, negative_label = -1,
                           outer_k = 3, inner_k = 2, outer_repeats = 1,
                           seed = 42,
                           engine = "R", progress = FALSE, verbose = FALSE)
  expect_equal(result$summary$auc, direct$summary$auc)
})

test_that("run_ncvroc() auto-computes preselect_top_n when NULL", {
  d <- make_test_data()
  cfg_no_items <- ncvroc_config("y", items = NULL, max_items = 2,
                                mode = "exhaustive", outer_k = 3, inner_k = 2,
                                engine = "R")
  expect_null(cfg_no_items$preselect_top_n)

  result <- run_ncvroc(d, c("q1", "q2", "q3"), cfg_no_items, seed = 42,
                       progress = FALSE, verbose = FALSE)
  expect_s3_class(result, "ncvroc_result")
})

test_that("run_ncvroc() auto-suggests preselect_top_n with items=NULL, mode=balanced", {
  dat <- data.frame(
    y  = c(0, 0, 0, 1, 1, 1, 0, 1),
    x1 = c(0, 1, 0, 1, 1, 0, 0, 1),
    x2 = c(1, 1, 0, 1, 0, 0, 1, 1),
    x3 = c(0, 0, 1, 1, 0, 1, 0, 1)
  )
  items <- c("x1", "x2", "x3")

  cfg <- ncvroc_config(
    outcome = "y",
    items = NULL,
    max_items = 2,
    mode = "balanced",
    outer_k = 2,
    inner_k = 2,
    outer_repeats = 1,
    inner_repeats = 1,
    engine = "R"
  )

  expect_null(cfg$preselect_top_n)

  result <- run_ncvroc(
    data = dat,
    items = items,
    config = cfg,
    seed = 42,
    progress = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "ncvroc_result")
})

# ---- item_count ----

test_that("ncvroc_config() with item_count '==4' stores resolved range", {
  cfg <- ncvroc_config("y", items = letters[1:5], item_count = "==3", mode = "quick")
  expect_equal(cfg$item_count, "==3")
  expect_equal(cfg$min_items, 3)
  expect_equal(cfg$max_items, 3)
})

test_that("ncvroc_config() with item_count '<=4' stores resolved range", {
  cfg <- ncvroc_config("y", items = letters[1:5], item_count = "<=3", mode = "quick")
  expect_equal(cfg$item_count, "<=3")
  expect_equal(cfg$min_items, 1)
  expect_equal(cfg$max_items, 3)
})

test_that("ncvroc_config() item_count + explicit min_items errors", {
  expect_error(
    ncvroc_config("y", items = letters[1:5], item_count = "==2", min_items = 2),
    "Do not specify item_count together"
  )
})

test_that("ncvroc_config(items=NULL, item_count='==4') stores deferred values", {
  cfg <- ncvroc_config("y", items = NULL, item_count = "==4")
  expect_equal(cfg$item_count, "==4")
  expect_equal(cfg$min_items, 4)
  expect_equal(cfg$max_items, 4)
  expect_true(is.na(cfg$n_items))
})

test_that("ncvroc_config(items=NULL) + item_count > items at run time errors", {
  cfg <- ncvroc_config("y", items = NULL, item_count = "==10",
                        outer_k = 3, inner_k = 2, engine = "R")
  d <- make_test_data()  # only q1-q3
  expect_error(
    run_ncvroc(d, c("q1", "q2", "q3"), cfg, seed = 42, progress = FALSE,
               verbose = FALSE),
    "only.*candidate items are available"
  )
})

test_that("ncvroc_config(items=NULL) + item_count <= items at run time works", {
  cfg <- ncvroc_config("y", items = NULL, item_count = "<=2",
                        outer_k = 3, inner_k = 2, engine = "R")
  d <- make_test_data()  # q1-q3, choose 3 items is enough for <=2
  result <- run_ncvroc(d, c("q1", "q2", "q3"), cfg, seed = 42,
                       progress = FALSE, verbose = FALSE)
  expect_s3_class(result, "ncvroc_result")
})

test_that("print.ncvroc_config shows item_count", {
  cfg <- ncvroc_config("y", items = letters[1:5], item_count = "<=3", mode = "quick")
  expect_output(print(cfg), "up to 3.*\\(<=3\\)")
})

test_that("print.ncvroc_config shows item_count with items=NULL", {
  cfg <- ncvroc_config("y", items = NULL, item_count = "==4")
  expect_output(print(cfg), "exactly 4.*\\(==4\\)")
})

test_that("item_count is the last formal argument in ncvroc_config()", {
  old_names <- c("outcome", "items", "min_items", "max_items", "mode",
                 "outer_k", "inner_k", "outer_repeats", "inner_repeats",
                 "preselect_top_n", "preselect_by",
                 "selection_criterion", "cutoff_method",
                 "positive_label", "negative_label",
                 "stratified", "engine")
  expect_identical(head(names(formals(ncvroc_config)), length(old_names)), old_names)
  expect_identical(tail(names(formals(ncvroc_config)), 1L), "item_count")
})
