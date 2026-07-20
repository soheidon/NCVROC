# test-chunked.R — Tests for chunked storage, streaming top-N, and auto mode

# ---- Test helpers ----

make_chunk_test_data <- function(n = 50, seed = 42) {
  set.seed(seed)
  data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE)
  )
}

CHUNK_ITEMS <- paste0("Q", 1:5)

# ---- .count_total_combos ----

test_that(".count_total_combos works correctly", {
  expect_equal(.count_total_combos(5, 1, 1), 5)
  expect_equal(.count_total_combos(5, 1, 2), 15)
  expect_equal(.count_total_combos(3, 1, 3), 7)
  expect_equal(.count_total_combos(5, 2, 3), 20)
  expect_equal(.count_total_combos(3, 3, 3), 1)
})

# ---- .combination_unrank ----

test_that(".combination_unrank matches combn() for n=1..8", {
  skip_on_cran()
  for (n in 1:8) {
    for (k in 0:n) {
      total <- choose(n, k)
      if (total == 0) next
      for (rank in seq_len(min(total, 20)) - 1L) {
        idx <- .combination_unrank(n, k, rank) + 1L  # 0-based → 1-based
        expected <- sort(combn(n, k, simplify = TRUE)[, rank + 1])
        expect_equal(idx, expected,
          label = sprintf("n=%d, k=%d, rank=%d", n, k, rank))
      }
    }
  }
})

# ---- .enumerate_combinations_chunk ----

test_that("chunk enumeration matches full enumeration", {
  items <- LETTERS[1:4]
  full <- enumerate_combinations(items, min_items = 1, max_items = 2)

  for (cs in c(1, 2, 3, 5, 7, 10)) {
    total <- .count_total_combos(4, 1, 2)
    chunked <- list()
    cs_d <- 0.0
    while (cs_d < total) {
      chunk <- .enumerate_combinations_chunk(items, 1, 2, cs_d, cs)
      chunked <- c(chunked, chunk)
      cs_d <- cs_d + cs
    }
    expect_equal(length(chunked), length(full),
      label = paste("chunk_size", cs))
    for (i in seq_along(full)) {
      expect_equal(chunked[[i]], full[[i]],
        label = paste("combo", i, "chunk_size", cs))
    }
  }
})

# ---- exhaustive_sum_roc chunked path ----

test_that("exhaustive_sum_roc chunked path produces same results as full", {
  d <- make_chunk_test_data()

  full <- exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3],
    max_items = 2, engine = "R", top_n = NULL, progress = FALSE)

  chunked <- NULL
  total <- .count_total_combos(3, 1, 2)
  cs <- 2.0
  idx <- 0.0
  while (idx < total) {
    chunk <- exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3],
      max_items = 2, engine = "R", top_n = NULL, progress = FALSE,
      chunk_start = idx, chunk_size = 2)
    chunked <- if (is.null(chunked)) chunk else rbind(chunked, chunk)
    idx <- idx + cs
  }

  # Sort both by items for comparison
  full_s <- full[order(full$items), ]
  chunked_s <- chunked[order(chunked$items), ]
  expect_equal(full_s$items, chunked_s$items)
  expect_equal(full_s$auc, chunked_s$auc, tolerance = 1e-10)
  expect_equal(full_s$n_items, chunked_s$n_items)
})

test_that("chunked Rcpp engine produces same results as chunked R engine", {
  d <- make_chunk_test_data()

  total <- .count_total_combos(3, 1, 2)
  chunk_r <- NULL
  chunk_rcpp <- NULL
  idx <- 0.0
  while (idx < total) {
    cr <- exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3],
      max_items = 2, engine = "R", top_n = NULL, progress = FALSE,
      chunk_start = idx, chunk_size = 3)
    crcpp <- exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3],
      max_items = 2, engine = "Rcpp", top_n = NULL, progress = FALSE,
      chunk_start = idx, chunk_size = 3)
    chunk_r <- if (is.null(chunk_r)) cr else rbind(chunk_r, cr)
    chunk_rcpp <- if (is.null(chunk_rcpp)) crcpp else rbind(chunk_rcpp, crcpp)
    idx <- idx + 3
  }

  sr <- chunk_r[order(chunk_r$items), ]
  sc <- chunk_rcpp[order(chunk_rcpp$items), ]
  expect_equal(sr$items, sc$items)
  expect_equal(sr$auc, sc$auc, tolerance = 1e-10)
})

# ---- .make_chunk_dir / .write_chunk_rds / .read_chunk_rds ----

test_that("chunk dir create and read/write works", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(dir.exists(chunk_dir))

  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 4:6, b = letters[4:6])
  .write_chunk_rds(df1, chunk_dir, 0)
  .write_chunk_rds(df2, chunk_dir, 1)

  expect_equal(nrow(.read_chunk_rds(chunk_dir, 0)), 3)
  expect_equal(nrow(.read_chunk_rds(chunk_dir, 1)), 3)
})

# ---- .list_chunk_files ----

test_that(".list_chunk_files returns files in order", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  for (i in 0:4) {
    saveRDS(data.frame(x = i), file.path(chunk_dir, sprintf("chunk_%05d.rds", i)))
  }

  files <- .list_chunk_files(chunk_dir)
  expect_length(files, 5)
  expect_true(all(grepl("chunk_[0-9]{5}\\.rds$", files)))
})

# ---- .chunked_reader ----

test_that(".chunked_reader collects results", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  saveRDS(data.frame(x = 1:3), file.path(chunk_dir, "chunk_00000.rds"))
  saveRDS(data.frame(x = 4:6), file.path(chunk_dir, "chunk_00001.rds"))

  results <- .chunked_reader(chunk_dir, function(chunk, idx) {
    data.frame(n = nrow(chunk), idx = idx)
  }, collect = TRUE)

  expect_length(results, 2)
  expect_equal(results[[1]]$n, 3)
  expect_equal(results[[2]]$idx, 1L)
})

# ---- .stream_top_n_from_chunks ----

test_that(".stream_top_n_from_chunks keeps running top-N", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Chunk 1: items with auc 0.8, 0.6, 0.4
  df1 <- data.frame(
    rank = 1:3, items = c("A", "B", "C"), n_items = c(1, 1, 1),
    auc = c(0.8, 0.6, 0.4), cutoff = c(1, 1, 1),
    sensitivity = c(0.7, 0.6, 0.5), specificity = c(0.7, 0.6, 0.5),
    youden = c(0.4, 0.2, 0.0), accuracy = c(0.7, 0.6, 0.5),
    ppv = c(0.7, 0.6, 0.5), npv = c(0.7, 0.6, 0.5),
    n_positive = c(10, 10, 10), n_negative = c(10, 10, 10),
    stringsAsFactors = FALSE
  )
  # Chunk 2: items with auc 0.9, 0.5
  df2 <- data.frame(
    rank = 1:2, items = c("D", "E"), n_items = c(1, 1),
    auc = c(0.9, 0.5), cutoff = c(1, 1),
    sensitivity = c(0.8, 0.5), specificity = c(0.8, 0.5),
    youden = c(0.6, 0.0), accuracy = c(0.8, 0.5),
    ppv = c(0.8, 0.5), npv = c(0.8, 0.5),
    n_positive = c(10, 10), n_negative = c(10, 10),
    stringsAsFactors = FALSE
  )

  saveRDS(df1, file.path(chunk_dir, "chunk_00000.rds"))
  saveRDS(df2, file.path(chunk_dir, "chunk_00001.rds"))

  top3 <- .stream_top_n_from_chunks(chunk_dir, rank_by = "auc", top_n = 3)
  expect_equal(nrow(top3), 3)
  expect_equal(top3$items[1], "D")  # auc 0.9
  expect_equal(top3$auc[1], 0.9)
  expect_equal(top3$items[2], "A")  # auc 0.8
})

test_that(".stream_top_n_from_chunks with conditions", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- data.frame(
    rank = 1:4, items = c("A", "B", "C", "D"), n_items = c(1, 1, 2, 2),
    auc = c(0.9, 0.8, 0.85, 0.7), cutoff = c(1, 1, 2, 2),
    sensitivity = c(0.9, 0.7, 0.85, 0.6), specificity = c(0.8, 0.7, 0.8, 0.6),
    youden = c(0.7, 0.4, 0.65, 0.2), accuracy = c(0.85, 0.7, 0.83, 0.6),
    ppv = c(0.8, 0.7, 0.8, 0.6), npv = c(0.9, 0.7, 0.85, 0.6),
    n_positive = c(10, 10, 10, 10), n_negative = c(10, 10, 10, 10),
    stringsAsFactors = FALSE
  )
  saveRDS(df, file.path(chunk_dir, "chunk_00000.rds"))

  filtered <- .stream_top_n_from_chunks(chunk_dir, rank_by = "auc", top_n = 5,
    sensitivity = ">= 0.80")
  expect_true(all(filtered$sensitivity >= 0.80))
  expect_equal(nrow(filtered), 2)  # A and C only
})

# ---- ncvroc auto mode ----

test_that("ncvroc with auto storage uses memory for small search", {
  d <- make_chunk_test_data()
  result <- ncvroc(d, y, Q1:Q3, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = TRUE, results_storage = "auto")
  expect_equal(result$storage_backend, "memory")
  expect_gt(result$final_n_combinations, 0)
})

# ---- roc_bruteforce auto mode ----

test_that("roc_bruteforce with auto storage uses memory for small search", {
  d <- make_chunk_test_data()
  res <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    results_storage = "auto", progress = FALSE)
  expect_equal(res$storage_backend, "memory")
  expect_gt(res$n_combinations, 0)
})

# ---- chunk_size parameter ----

test_that("chunk_size is stored in ncvroc result", {
  d <- make_chunk_test_data()
  result <- ncvroc(d, y, Q1:Q3, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = TRUE, results_storage = "auto",
    chunk_size = 50000L)
  expect_equal(result$chunk_size, 50000L)
})

test_that("chunk_size validation errors on bad input", {
  d <- make_chunk_test_data()
  expect_error(
    ncvroc(d, y, Q1:Q3, max_items = 2, mode = "quick",
      outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
      seed = 42, final_search = FALSE, chunk_size = -1),
    "positive integer"
  )
  expect_error(
    ncvroc(d, y, Q1:Q3, max_items = 2, mode = "quick",
      outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
      seed = 42, final_search = FALSE, chunk_size = 1.5),
    "positive integer"
  )
})

# ---- .resolve_global_combination_rank ----

test_that(".resolve_global_combination_rank resolves correctly", {
  # n=5, total = C(5,1)+C(5,2) = 5+10 = 15
  # Global rank 0 → k=1, rank_within_k=0
  r <- .resolve_global_combination_rank(5, 1, 2, 0)
  expect_equal(r$k, 1)
  expect_equal(r$rank_within_k, 0)

  # Global rank 4 → k=1, rank_within_k=4
  r <- .resolve_global_combination_rank(5, 1, 2, 4)
  expect_equal(r$k, 1)
  expect_equal(r$rank_within_k, 4)

  # Global rank 5 → k=2, rank_within_k=0
  r <- .resolve_global_combination_rank(5, 1, 2, 5)
  expect_equal(r$k, 2)
  expect_equal(r$rank_within_k, 0)

  # Global rank 14 → k=2, rank_within_k=9
  r <- .resolve_global_combination_rank(5, 1, 2, 14)
  expect_equal(r$k, 2)
  expect_equal(r$rank_within_k, 9)
})

# ---- ncvroc_results with chunked storage ----

test_that("ncvroc_results() can stream from chunked storage", {
  skip("Manual test: requires a large enough search to trigger chunked mode")
  # This test would need AUTO_MEMORY_LIMIT combinations to trigger chunking.
  # With AUTO_MEMORY_LIMIT = 100,000 this requires too many items for a fast test.
  # Tested manually in development.
})

# ---- .full_load_chunked ----

test_that(".full_load_chunked reads all chunks", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  df1 <- data.frame(
    rank = 1:2, items = c("A", "B"), n_items = c(1, 1),
    auc = c(0.8, 0.6), cutoff = c(1, 1),
    sensitivity = c(0.7, 0.6), specificity = c(0.7, 0.6),
    youden = c(0.4, 0.2), accuracy = c(0.7, 0.6),
    ppv = c(0.7, 0.6), npv = c(0.7, 0.6),
    n_positive = c(10, 10), n_negative = c(10, 10),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    rank = 1:2, items = c("C", "D"), n_items = c(1, 1),
    auc = c(0.9, 0.5), cutoff = c(1, 1),
    sensitivity = c(0.8, 0.5), specificity = c(0.8, 0.5),
    youden = c(0.6, 0.0), accuracy = c(0.8, 0.5),
    ppv = c(0.8, 0.5), npv = c(0.8, 0.5),
    n_positive = c(10, 10), n_negative = c(10, 10),
    stringsAsFactors = FALSE
  )

  saveRDS(df1, file.path(chunk_dir, "chunk_00000.rds"))
  saveRDS(df2, file.path(chunk_dir, "chunk_00001.rds"))

  all_data <- .full_load_chunked(chunk_dir)
  expect_equal(nrow(all_data), 4)
  expect_equal(sort(all_data$items), c("A", "B", "C", "D"))
})

test_that(".full_load_chunked with conditions filters correctly", {
  tmp <- tempdir()
  chunk_dir <- .make_chunk_dir(tmp)
  on.exit(unlink(chunk_dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- data.frame(
    rank = 1:4, items = c("A", "B", "C", "D"), n_items = c(1, 1, 2, 2),
    auc = c(0.9, 0.8, 0.85, 0.7), cutoff = c(1, 1, 2, 2),
    sensitivity = c(0.9, 0.7, 0.85, 0.6), specificity = c(0.8, 0.7, 0.8, 0.6),
    youden = c(0.7, 0.4, 0.65, 0.2), accuracy = c(0.85, 0.7, 0.83, 0.6),
    ppv = c(0.8, 0.7, 0.8, 0.6), npv = c(0.9, 0.7, 0.85, 0.6),
    n_positive = c(10, 10, 10, 10), n_negative = c(10, 10, 10, 10),
    stringsAsFactors = FALSE
  )
  saveRDS(df, file.path(chunk_dir, "chunk_00000.rds"))

  filtered <- .full_load_chunked(chunk_dir, sensitivity = ">= 0.80")
  expect_true(all(filtered$sensitivity >= 0.80))
  expect_equal(nrow(filtered), 2)
})

# ---- Argument validation ----

test_that("chunk_start must be non-negative", {
  d <- make_chunk_test_data()
  expect_error(
    exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3], max_items = 2,
      engine = "R", progress = FALSE, chunk_start = -1, chunk_size = 5),
    "non-negative"
  )
})

test_that("chunk_size must be positive integer", {
  d <- make_chunk_test_data()
  expect_error(
    exhaustive_sum_roc(d, "y", CHUNK_ITEMS[1:3], max_items = 2,
      engine = "R", progress = FALSE, chunk_start = 0, chunk_size = 0),
    "positive integer"
  )
})

# ---- results_dir with chunked_rds ----

test_that("chunked_rds: results_dir is respected when cache is off", {
  # A: chunked + explicit results_dir -> chunks in results_dir
  old_limit <- NCVROC:::AUTO_MEMORY_LIMIT
  assignInNamespace("AUTO_MEMORY_LIMIT", 1L, "NCVROC")
  on.exit(assignInNamespace("AUTO_MEMORY_LIMIT", old_limit, "NCVROC"), add = TRUE)

  # Use a directory outside tempdir() so the tempdir() exclusion check is meaningful
  results_dir <- normalizePath(file.path(getwd(), paste0("test_chunked_dir_", Sys.getpid())),
                                winslash = "/", mustWork = FALSE)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(results_dir, recursive = TRUE), add = TRUE)

  out <- ncvroc(make_chunk_test_data(), y, Q1:Q4, max_items = 2,
                results_storage = "rds", results_dir = results_dir,
                cache = "off", outer_k = 2, inner_k = 2, outer_repeats = 1,
                engine = "R", seed = 42, final_search = TRUE,
                progress = FALSE, verbose = FALSE)

  expect_equal(out$storage_backend, "chunked_rds")
  expect_true(grepl(results_dir,
                    normalizePath(out$chunk_dir, winslash = "/", mustWork = FALSE),
                    fixed = TRUE))
  # chunks must NOT be under tempdir()
  expect_false(grepl(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                     normalizePath(out$chunk_dir, winslash = "/", mustWork = FALSE),
                     fixed = TRUE))
})

test_that("chunked_rds: results_dir = NULL falls back to tempdir()", {
  # B: chunked + results_dir = NULL -> chunks in tempdir()
  old_limit <- NCVROC:::AUTO_MEMORY_LIMIT
  assignInNamespace("AUTO_MEMORY_LIMIT", 1L, "NCVROC")
  on.exit(assignInNamespace("AUTO_MEMORY_LIMIT", old_limit, "NCVROC"), add = TRUE)

  out <- ncvroc(make_chunk_test_data(), y, Q1:Q4, max_items = 2,
                results_storage = "rds", results_dir = NULL,
                cache = "off", outer_k = 2, inner_k = 2, outer_repeats = 1,
                engine = "R", seed = 42, final_search = TRUE,
                progress = FALSE, verbose = FALSE)

  expect_equal(out$storage_backend, "chunked_rds")
  expect_true(grepl(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                    normalizePath(out$chunk_dir, winslash = "/", mustWork = FALSE),
                    fixed = TRUE))
})

test_that("single_rds: results_dir is respected (regression guard)", {
  # C: single_rds + results_dir -> file in results_dir
  results_dir <- normalizePath(file.path(getwd(), paste0("test_single_dir_", Sys.getpid())),
                                winslash = "/", mustWork = FALSE)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(results_dir, recursive = TRUE), add = TRUE)

  out <- ncvroc(make_chunk_test_data(), y, Q1:Q4, max_items = 2,
                results_storage = "rds", results_dir = results_dir,
                cache = "off", outer_k = 2, inner_k = 2, outer_repeats = 1,
                engine = "R", seed = 42, final_search = TRUE,
                progress = FALSE, verbose = FALSE)

  # With 10 combos and default AUTO_MEMORY_LIMIT = 100000, this is single_rds
  expect_equal(out$storage_backend, "single_rds")
  expect_true(grepl(results_dir,
                    normalizePath(out$final_exhaustive_file, winslash = "/", mustWork = FALSE),
                    fixed = TRUE))
})

test_that("chunked_rds: cache enabled -> building_dir/chunks, not results_dir", {
  # D: cache enabled -> building_dir takes precedence over results_dir
  old_limit <- NCVROC:::AUTO_MEMORY_LIMIT
  assignInNamespace("AUTO_MEMORY_LIMIT", 1L, "NCVROC")
  on.exit(assignInNamespace("AUTO_MEMORY_LIMIT", old_limit, "NCVROC"), add = TRUE)

  cache_dir <- normalizePath(file.path(getwd(), paste0("test_cache_", Sys.getpid())),
                              winslash = "/", mustWork = FALSE)
  results_dir <- normalizePath(file.path(getwd(), paste0("test_results_", Sys.getpid())),
                                winslash = "/", mustWork = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  on.exit(unlink(results_dir, recursive = TRUE), add = TRUE)

  out <- ncvroc(make_chunk_test_data(), y, Q1:Q4, max_items = 2,
                results_storage = "rds", results_dir = results_dir,
                cache = "reuse", cache_dir = cache_dir,
                outer_k = 2, inner_k = 2, outer_repeats = 1,
                engine = "R", seed = 42, final_search = TRUE,
                progress = FALSE, verbose = FALSE)

  expect_equal(out$storage_backend, "chunked_rds")
  # chunks go to cache_dir, NOT results_dir
  expect_true(grepl(cache_dir,
                    normalizePath(out$chunk_dir, winslash = "/", mustWork = FALSE),
                    fixed = TRUE))
  expect_false(grepl(results_dir,
                     normalizePath(out$chunk_dir, winslash = "/", mustWork = FALSE),
                     fixed = TRUE))
})
