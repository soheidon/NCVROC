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
                          engine = "R", progress = FALSE, results_storage = "memory")
  res2 <- roc_bf(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                  engine = "R", progress = FALSE, results_storage = "memory")
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
                             engine = "R", progress = FALSE, results_storage = "memory")
  res_rcpp <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                              engine = "Rcpp", progress = FALSE, results_storage = "memory")
  expect_equal(res_r$results$auc, res_rcpp$results$auc, tolerance = 1e-10)
  expect_equal(res_r$results$sensitivity, res_rcpp$results$sensitivity, tolerance = 1e-10)
  expect_equal(res_r$results$specificity, res_rcpp$results$specificity, tolerance = 1e-10)
})

# ---- rank_by ----

test_that("roc_bruteforce() sorts by auc correctly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         rank_by = "auc", engine = "R", progress = FALSE,
                         results_storage = "memory")
  expect_equal(res$results$auc, sort(res$results$auc, decreasing = TRUE))
})

test_that("roc_bruteforce() sorts by youden correctly", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         rank_by = "youden", engine = "R", progress = FALSE,
                         results_storage = "memory")
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
                         top_n = NULL, engine = "R", progress = FALSE,
                         results_storage = "memory")
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
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_equal(res$best_model$items, res$results$items[1])
  expect_equal(res$best_model$auc, res$results$auc[1])
})

test_that("n_combinations matches nrow(results) with memory storage", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_equal(res$n_combinations, nrow(res$results))
})

test_that("n_combinations is correct with RDS storage (results is NULL)", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_null(res$results)
  expect_gt(res$n_combinations, 0)
  expect_true(!is.null(res$results_file))
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

# ---- results_storage ----

test_that("default results_storage = 'rds' sets results to NULL", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_null(res$results)
  expect_true(!is.null(res$results_file))
  expect_equal(res$results_storage, "rds")
})

test_that("results_storage = 'memory' keeps results in-memory", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_s3_class(res$results, "data.frame")
  expect_null(res$results_file)
  expect_gt(nrow(res$results), 0)
})

test_that("results_storage = 'none' discards results", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE, results_storage = "none")
  expect_null(res$results)
  expect_null(res$results_file)
  expect_equal(res$results_storage, "none")
})

test_that("results_storage = 'none' causes ncvroc_results() to error", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE, results_storage = "none")
  expect_error(
    ncvroc_results(res),
    "not available"
  )
})

test_that("ncvroc_results() reads from RDS transparently", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_storage = "rds")
  filtered <- ncvroc_results(res, sensitivity = ">= 0.50",
                              rank_by = "auc", top_n = 3)
  expect_s3_class(filtered, "data.frame")
  expect_true(all(filtered$sensitivity >= 0.50))
})

test_that("results_dir saves RDS to specified path", {
  d <- make_bf_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_bf_storage_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_dir = tmp)
  expect_true(dir.exists(tmp))
  expect_true(file.exists(res$results_file))
  expect_equal(normalizePath(dirname(res$results_file), winslash = "/", mustWork = FALSE),
               normalizePath(tmp, winslash = "/", mustWork = FALSE))
})

test_that("results_name appears as prefix in RDS filename", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_name = "my_study")
  fname <- basename(res$results_file)
  expect_match(fname, "^my_study_")
  expect_match(fname, "_p3_k1-2_auc_")
})

test_that("RDS file contains ncvroc_metadata attribute", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  full <- readRDS(res$results_file)
  meta <- attr(full, "ncvroc_metadata")
  expect_type(meta, "list")
  expect_equal(meta$function_name, "roc_bruteforce")
  expect_equal(meta$outcome, "y")
  expect_equal(meta$items, c("Q1", "Q2", "Q3"))
  expect_equal(meta$rank_by, "auc")
})

test_that("RDS roundtrip is identical to memory mode", {
  d <- make_bf_data()
  res_mem <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                             engine = "R", progress = FALSE,
                             results_storage = "memory")
  res_rds <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                             engine = "R", progress = FALSE,
                             results_storage = "rds")
  rds_data <- readRDS(res_rds$results_file)
  expect_equal(rds_data, res_mem$results)
})

test_that("deleted RDS file causes ncvroc_results() to error", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  unlink(res$results_file)
  expect_error(
    ncvroc_results(res),
    "no longer exists"
  )
})

test_that("print survives missing RDS file", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  unlink(res$results_file)
  expect_output(
    print(res),
    "stored RDS file is missing"
  )
})

test_that("seed is not consumed by filename generation", {
  d <- make_bf_data()
  set.seed(42)
  state_before <- .Random.seed
  roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                  engine = "R", progress = FALSE)
  state_after <- .Random.seed
  expect_equal(state_after, state_before)
})

test_that("print shows storage info for RDS mode", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)
  expect_output(
    print(res),
    "stored in"
  )
})

test_that("'none' storage shows 'not stored' in print", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_storage = "none")
  expect_output(
    print(res),
    "not stored"
  )
})

test_that("save_results CSV still works with rds storage", {
  d <- make_bf_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_bf_csv_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_storage = "rds",
                         save_results = TRUE, output_dir = tmp)
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_results.csv")))
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_candidates.csv")))
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_best_model.csv")))
})

test_that("save_results CSV still works with none storage", {
  d <- make_bf_data()
  tmp <- file.path(tempdir(), paste0("ncvroc_bf_csv_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE,
                         results_storage = "none",
                         save_results = TRUE, output_dir = tmp)
  expect_true(file.exists(file.path(tmp, "roc_bruteforce_results.csv")))
})

# ---- Argument validation ----

test_that("invalid results_storage errors", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE, results_storage = "invalid"),
    "should be one of"
  )
})

test_that("empty results_name errors", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE, results_name = ""),
    "non-empty character string"
  )
})

test_that("length-2 results_name errors", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE, results_name = c("a", "b")),
    "non-empty character string"
  )
})

test_that("empty results_dir errors", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                    engine = "R", progress = FALSE, results_dir = ""),
    "non-empty path"
  )
})

test_that("default RDS storage uses the working directory", {
  d <- make_bf_data()
  old_wd <- getwd()
  tmp <- tempfile("ncvroc-wd-")
  dir.create(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), max_items = 2,
                         engine = "R", progress = FALSE)

  expect_equal(normalizePath(dirname(res$results_file)),
               normalizePath(tmp))
  expect_true(file.exists(res$results_file))
})

# ---- item_count ----

test_that("item_count '==3' limits combinations in roc_bruteforce()", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "==2",
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_equal(res$item_count, "==2")
  expect_equal(res$min_items, 2)
  expect_equal(res$max_items, 2)
  ranked <- ncvroc_results(res, top_n = NULL)
  expect_true(all(ranked$n_items == 2))
})

test_that("item_count '<=3' in roc_bruteforce()", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "<=2",
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_equal(res$item_count, "<=2")
  expect_equal(res$min_items, 1)
  expect_equal(res$max_items, 2)
})

test_that("item_count '2:3' in roc_bruteforce()", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "1:2",
                         engine = "R", progress = FALSE, results_storage = "memory")
  expect_equal(res$item_count, "1:2")
  expect_equal(res$min_items, 1)
  expect_equal(res$max_items, 2)
})

test_that("item_count works with roc_bf() NSE", {
  d <- make_bf_data()
  res <- roc_bf(d, y, Q1:Q3, item_count = "==2",
                engine = "R", progress = FALSE)
  expect_equal(res$item_count, "==2")
  expect_equal(res$min_items, 2)
  expect_equal(res$max_items, 2)
})

test_that("item_count + explicit min_items via roc_bf() errors", {
  d <- make_bf_data()
  expect_error(
    roc_bf(d, "y", c("Q1", "Q2", "Q3"), item_count = "==2", min_items = 2,
           engine = "R", progress = FALSE),
    "Do not specify item_count together"
  )
})

test_that("item_count + explicit max_items via roc_bruteforce() errors", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "<=2", max_items = 3,
                    engine = "R", progress = FALSE),
    "Do not specify item_count together"
  )
})

test_that("R and Rcpp engines equivalent with item_count", {
  d <- make_bf_data()
  res_r <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "==2",
                           engine = "R", progress = FALSE, results_storage = "memory")
  res_rcpp <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "==2",
                              engine = "Rcpp", progress = FALSE, results_storage = "memory")
  expect_equal(res_r$results$auc, res_rcpp$results$auc)
  expect_equal(res_r$n_combinations, res_rcpp$n_combinations)
})

test_that("item_count reflects in RDS filename range", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "==2",
                         engine = "R", progress = FALSE)
  expect_match(res$results_file, "_k2-2_")
  expect_equal(res$item_count, "==2")
})

test_that("item_count > candidate items errors in roc_bruteforce()", {
  d <- make_bf_data()
  expect_error(
    roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "<=10",
                    engine = "R", progress = FALSE),
    "only.*candidate items are available"
  )
})

test_that("print.roc_bruteforce_result shows item_count", {
  d <- make_bf_data()
  res <- roc_bruteforce(d, "y", c("Q1", "Q2", "Q3"), item_count = "<=2",
                         engine = "R", progress = FALSE)
  expect_output(print(res), "up to 2.*\\(<=2\\)")
})

test_that("item_count is the last formal argument in roc_bruteforce()", {
  old_names <- c("data", "outcome", "items", "min_items", "max_items",
                 "cutoff_method", "positive_label", "negative_label",
                 "engine", "rank_by", "top_n", "progress",
                 "save_results", "output_dir",
                 "results_storage", "results_name", "results_dir")
  expect_identical(head(names(formals(roc_bruteforce)), length(old_names)), old_names)
  expect_identical(tail(names(formals(roc_bruteforce)), 1L), "item_count")
})

test_that("item_count is the last formal argument in roc_bf()", {
  old_names <- c("data", "outcome", "items", "min_items", "max_items",
                 "cutoff_method", "positive_label", "negative_label",
                 "engine", "rank_by", "top_n", "progress",
                 "save_results", "output_dir",
                 "results_storage", "results_name", "results_dir")
  expect_identical(head(names(formals(roc_bf)), length(old_names)), old_names)
  expect_identical(tail(names(formals(roc_bf)), 1L), "item_count")
})

test_that("roc_bf() with item_count and bare column NSE no missing() error", {
  d <- make_bf_data()
  res <- roc_bf(d, y, Q1:Q3, item_count = "<=2",
                engine = "R", progress = FALSE)
  expect_equal(res$max_items, 2)
})
