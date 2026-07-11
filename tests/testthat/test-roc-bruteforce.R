# test-roc-bruteforce.R — Tests for roc_bruteforce() and roc_bf()

make_bf_data <- function() {
  data.frame(
    y  = rep(c(0, 1), each = 6),
    Q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1),
    Q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2),
    Q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2)
  )
}

# ---- Basic return structure ----

test_that("roc_bruteforce() returns class roc_bruteforce_result", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_s3_class(res, "roc_bruteforce_result")
})

test_that("roc_bf() returns same class", {
  d <- make_bf_data()
  res <- roc_bf(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                 engine = "R", progress = FALSE)
  expect_s3_class(res, "roc_bruteforce_result")
})

test_that("roc_bf() and roc_bruteforce() produce identical results", {
  d <- make_bf_data()
  res1 <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                          engine = "R", progress = FALSE)
  res2 <- roc_bf(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                  engine = "R", progress = FALSE)
  expect_equal(res1$results, res2$results)
  expect_equal(res1$best_model, res2$best_model)
  expect_equal(res1$candidates, res2$candidates)
})

# ---- NSE: bare outcome and bare item range ----

test_that("roc_bruteforce() works with bare outcome", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, y, c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$outcome, "y")
})

test_that("roc_bruteforce() works with bare item range", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", Q1:Q3, max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$items, c("Q1", "Q2", "Q3"))
})

test_that("roc_bf() works with bare outcome", {
  d <- make_bf_data()
  res <- roc_bf(d, y, c("Q1", "Q2", "Q3"), max_items = 2,
                 engine = "R", progress = FALSE)
  expect_equal(res$outcome, "y")
})

test_that("roc_bf() works with bare item range", {
  d <- make_bf_data()
  res <- roc_bf(d, y, Q1:Q3, max_items = 2,
                 engine = "R", progress = FALSE)
  expect_equal(res$items, c("Q1", "Q2", "Q3"))
})

test_that("roc_bruteforce() works with character outcome", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$outcome, "y")
})

test_that("roc_bruteforce() works with character item vector", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$items, c("Q1", "Q2", "Q3"))
})

test_that("roc_bruteforce() works with numeric column positions", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", 2:4, max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$items, c("Q1", "Q2", "Q3"))
})

# ---- Engine ----

test_that("roc_bruteforce() works with engine = 'R'", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$engine, "R")
})

test_that("roc_bruteforce() works with engine = 'Rcpp'", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "Rcpp", progress = FALSE)
  expect_equal(res$engine, "Rcpp")
})

test_that("R and Rcpp engines produce equivalent results", {
  d <- make_bf_data()
  res_r   <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                             engine = "R", progress = FALSE)
  res_rcpp <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                              engine = "Rcpp", progress = FALSE)
  expect_equal(res_r$results$auc, res_rcpp$results$auc, tolerance = 1e-10)
  expect_equal(res_r$results$sensitivity, res_rcpp$results$sensitivity, tolerance = 1e-10)
  expect_equal(res_r$results$specificity, res_rcpp$results$specificity, tolerance = 1e-10)
})

# ---- rank_by ----

test_that("roc_bruteforce() sorts by auc correctly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         rank_by = "auc", engine = "R", progress = FALSE)
  expect_equal(res$results$auc, sort(res$results$auc, decreasing = TRUE))
})

test_that("roc_bruteforce() sorts by youden correctly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         rank_by = "youden", engine = "R", progress = FALSE)
  expect_equal(res$results$youden, sort(res$results$youden, decreasing = TRUE))
})

# ---- top_n ----

test_that("top_n = 1 returns one candidate", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         top_n = 1, engine = "R", progress = FALSE)
  expect_equal(nrow(res$candidates), 1)
})

test_that("top_n = NULL retains all rows", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         top_n = NULL, engine = "R", progress = FALSE)
  expect_equal(nrow(res$candidates), nrow(res$results))
})

test_that("top_n = 0 returns zero rows", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         top_n = 0, engine = "R", progress = FALSE)
  expect_equal(nrow(res$candidates), 0)
})

test_that("top_n = -1 throws error", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    top_n = -1, engine = "R", progress = FALSE),
    "top_n"
  )
})

test_that("top_n = 1.5 throws error", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    top_n = 1.5, engine = "R", progress = FALSE),
    "top_n"
  )
})

test_that("top_n = c(1, 2) throws error", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    top_n = c(1, 2), engine = "R", progress = FALSE),
    "top_n"
  )
})

# ---- Invalid arguments ----

test_that("Invalid engine throws error", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "Python", progress = FALSE)
  )
})

test_that("Invalid rank_by throws error", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    rank_by = "f1", engine = "R", progress = FALSE)
  )
})

# ---- Missing data handling ----

test_that("Missing outcome rows are removed", {
  d <- make_bf_data()
  d$y[1] <- NA_real_
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_true(res$n_original != res$n_analyzed)
  expect_equal(res$n_analyzed, nrow(d) - 1)
})

test_that("Missing item rows are removed", {
  d <- make_bf_data()
  d$Q1[3] <- NA_real_
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_true(res$n_original != res$n_analyzed)
})

# ---- Result consistency ----

test_that("best_model is first row of results", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$best_model$items, res$results$items[1])
  expect_equal(res$best_model$auc, res$results$auc[1])
})

test_that("n_combinations matches nrow(results)", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_equal(res$n_combinations, nrow(res$results))
})

# ---- Print method ----

test_that("print.roc_bruteforce_result returns invisibly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  out <- capture.output(ret <- print(res))
  expect_identical(ret, res)
  expect_true(length(out) > 0)
})

# ---- Save results ----

test_that("save_results = TRUE creates expected CSV files", {
  d <- make_bf_data()
  tmp <- file.path(tempdir(), "ncvroc_bf_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         save_results = TRUE, output_dir = tmp)
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_results.csv")))
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_candidates.csv")))
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_best_model.csv")))
  unlink(tmp, recursive = TRUE)
})

test_that("output_dir blocked by a file at path throws error", {
  d <- make_bf_data()
  tmp <- file.path(tempdir(), "ncvroc_bf_blocked")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  file.create(tmp)  # create a file where a directory is needed
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE,
                    save_results = TRUE, output_dir = tmp),
    "Could not create"
  )
  unlink(tmp)
})

# ---- ncvroc_results() integration ----

test_that("ncvroc_results() filters roc_bruteforce_result correctly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  filtered <- ncvroc_results(res, sensitivity = ">= 0.50",
                              rank_by = "auc", top_n = 3)
  expect_s3_class(filtered, "data.frame")
  expect_true(all(filtered$sensitivity >= 0.50))
})

# ---- Factor safety ----

test_that("factor item columns are handled safely", {
  d <- make_bf_data()
  d$Q1 <- factor(d$Q1)
  d$Q2 <- factor(d$Q2)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_s3_class(res, "roc_bruteforce_result")
})

test_that("factor outcome column is handled safely", {
  d <- make_bf_data()
  d$y <- factor(d$y)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_s3_class(res, "roc_bruteforce_result")
})

test_that("non-numeric character values throw error", {
  d <- make_bf_data()
  d$Q1 <- as.character(d$Q1)
  d$Q1[1] <- "bad"
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE),
    "non-numeric"
  )
})
