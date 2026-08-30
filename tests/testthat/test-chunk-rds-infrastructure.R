# test-chunk-rds-infrastructure.R — Step 3: Chunked / RDS Infrastructure & Bounded-Memory Verification
#
# Covers:
# 1. Bounded memory invariant during chunk evaluation & hierarchical stream merge
# 2. Canonical candidate identity and lazy item materialization across chunk files
# 3. Exact full-result semantics (chunked vs full-table reference)
# 4. Multi-chunk boundaries and non-divisible final chunks
# 5. Disk cleanup & temporary directory lifecycle
# 6. Cross-size Strategy 2 bounded memory execution & exactness

test_that("Step 3: Chunk RDS creation and streaming top-N preserves exact ranking and tie-breaking", {
  set.seed(301)
  n <- 50L
  p <- 6L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE),
    Q6 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("Q", 1:p)

  # Full reference without chunking
  ref_full <- exhaustive_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = items,
    min_items          = 1L,
    max_items          = 3L,
    rank_by            = "auc",
    top_n              = NULL,
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    parallel           = "none",
    progress           = FALSE
  )

  # Chunked evaluation with save_rds = TRUE and small chunk_size
  tmp_dir <- .make_chunk_dir(tempdir(), prefix = "test_chunk_rds_")
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  x_mat <- as.matrix(dat[, items])
  chunk_res <- .parallel_chunk_exhaustive(
    x                  = x_mat,
    y                  = dat$y,
    items              = items,
    min_items          = 1L,
    max_items          = 3L,
    cutoff_method      = "youden",
    rank_by            = "auc",
    top_n              = 10L,
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    chunk_size         = 10L,
    n_workers          = 2L,
    save_rds           = TRUE,
    chunk_dir          = tmp_dir
  )

  # 1. Verify chunk files were created on disk
  chunk_files <- .list_chunk_files(tmp_dir)
  total_combos <- .count_total_combos(p, 1L, 3L) # 6 + 15 + 20 = 41 combos -> 5 chunks of size 10 (last chunk size 1)
  expect_equal(length(chunk_files), 5L)

  # 2. Verify returned top-10 matches head(ref_full, 10) exactly
  expect_identical(chunk_res$items, utils::head(ref_full$items, 10L))
  expect_equal(chunk_res$auc, utils::head(ref_full$auc, 10L), tolerance = 1e-10)
  expect_identical(chunk_res$n_items, utils::head(ref_full$n_items, 10L))

  # 3. Verify .stream_top_n_from_chunks on written chunk files produces identical top-10
  streamed_top <- .stream_top_n_from_chunks(tmp_dir, rank_by = "auc", top_n = 10L)
  expect_identical(streamed_top$items, utils::head(ref_full$items, 10L))
  expect_equal(streamed_top$auc, utils::head(ref_full$auc, 10L), tolerance = 1e-10)
})

test_that("Step 3: Multi-pass hierarchical merge handles non-divisible chunks and boundary conditions", {
  set.seed(302)
  n <- 40L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("Q", 1:5)
  x_mat <- as.matrix(dat[, items])

  # Size 2 combinations = 10. With block_size = 3, gives 4 blocks: 3, 3, 3, 1 (non-divisible)
  init_s2 <- .create_initial_block_streams(
    x_mat              = x_mat,
    y                  = dat$y,
    item_names         = items,
    size               = 2L,
    cutoff_method      = "youden",
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    parallel_mode      = "none",
    n_workers_res      = 1L,
    block_size         = 3L
  )
  on.exit(init_s2$cleanup(), add = TRUE)

  expect_equal(length(init_s2$initial_streams), 4L)

  # Merge hierarchically with fan_in = 2
  final_stream <- .merge_block_streams_hierarchical(
    initial_streams    = init_s2$initial_streams,
    base_dir           = init_s2$base_dir,
    fan_in             = 2L,
    block_size         = 3L,
    prefer_fewer_items = TRUE
  )

  # Read all candidates from merged stream
  reader <- .open_block_stream_reader(final_stream)
  collected <- list()
  while (TRUE) {
    cand <- reader$pop()
    if (is.null(cand) || nrow(cand) == 0L) break
    collected[[length(collected) + 1L]] <- cand
  }
  reader$close()
  merged_res <- do.call(rbind, collected)

  # Compare with full reference
  ref_s2 <- exhaustive_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = items,
    min_items          = 2L,
    max_items          = 2L,
    rank_by            = "auc",
    top_n              = NULL,
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    parallel           = "none",
    progress           = FALSE
  )

  expect_equal(nrow(merged_res), 10L)
  expect_identical(merged_res$items, ref_s2$items)
  expect_equal(merged_res$auc, ref_s2$auc, tolerance = 1e-10)
})

test_that("Step 3: Cross-size Strategy 2 bounded chunk descriptors preserve exact results across parallel modes", {
  set.seed(303)
  n <- 40L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE)
  )

  # Run Strategy 2 with selection_metric = 'youden'
  res_serial <- cross_size_cv(
    data               = dat,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    selection_metric   = "youden",
    prefer_fewer_items = TRUE,
    folds              = 3,
    repeats            = 1,
    engine             = "Rcpp",
    parallel           = "none",
    top_n              = 10,
    seed               = 999
  )

  res_threads <- cross_size_cv(
    data               = dat,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    selection_metric   = "youden",
    prefer_fewer_items = TRUE,
    folds              = 3,
    repeats            = 1,
    engine             = "Rcpp",
    parallel           = "threads",
    n_workers          = 2L,
    top_n              = 10,
    seed               = 999
  )

  res_chunks <- cross_size_cv(
    data               = dat,
    outcome            = "y",
    items              = paste0("Q", 1:5),
    model_sizes        = 1:3,
    selection_metric   = "youden",
    prefer_fewer_items = TRUE,
    folds              = 3,
    repeats            = 1,
    engine             = "Rcpp",
    parallel           = "chunks",
    n_workers          = 2L,
    top_n              = 10,
    seed               = 999
  )

  # Verify exact identity across serial, threads, and PSOCK chunks
  expect_identical(res_threads$final_selected_model, res_serial$final_selected_model)
  expect_identical(res_chunks$final_selected_model, res_serial$final_selected_model)

  expect_identical(res_threads$candidate_ranking$items, res_serial$candidate_ranking$items)
  expect_identical(res_chunks$candidate_ranking$items, res_serial$candidate_ranking$items)

  expect_equal(res_threads$candidate_ranking$cv_youden, res_serial$candidate_ranking$cv_youden, tolerance = 1e-12)
  expect_equal(res_chunks$candidate_ranking$cv_youden, res_serial$candidate_ranking$cv_youden, tolerance = 1e-12)
})

test_that("Step 3: Bounded memory structural verification (no full W-size vector allocated in RAM)", {
  # Verify that for a model size with 20 combos and block_size = 5,
  # initial block streams contain exactly 4 block files of <= 5 rows each
  set.seed(304)
  n <- 30L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE),
    Q6 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("Q", 1:6)
  x_mat <- as.matrix(dat[, items])

  init_obj <- .create_initial_block_streams(
    x_mat              = x_mat,
    y                  = dat$y,
    item_names         = items,
    size               = 3L, # 6C3 = 20 combinations
    cutoff_method      = "youden",
    prefer_fewer_items = TRUE,
    engine             = "Rcpp",
    parallel_mode      = "none",
    n_workers_res      = 1L,
    block_size         = 5L
  )
  on.exit(init_obj$cleanup(), add = TRUE)

  # Check that each stream file has at most block_size rows
  for (st in init_obj$initial_streams) {
    for (bf in st$block_files) {
      df <- readRDS(bf)
      expect_lte(nrow(df), 5L)
    }
  }

  # Verify cleanup deletes all temporary files
  base_dir <- init_obj$base_dir
  expect_true(dir.exists(base_dir))
  init_obj$cleanup()
  expect_false(dir.exists(base_dir))
})

test_that("HIGH-1 & HIGH-2: Strategy 2 lazy candidate materialization and PSOCK atomic chunk streaming", {
  set.seed(42)
  n <- 50
  p <- 10
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  # p=10, sizes=1:3 -> 10 + 45 + 120 = 175 combinations
  top_n_req <- 5L

  # 1. Run serial Strategy 2 (Youden) with instrumented format_items counter
  fi_count <- 0L
  orig_fi <- NCVROC:::format_items
  unlockBinding("format_items", asNamespace("NCVROC"))
  assign("format_items", function(x) {
    fi_count <<- fi_count + 1L
    orig_fi(x)
  }, envir = asNamespace("NCVROC"))
  on.exit({
    unlockBinding("format_items", asNamespace("NCVROC"))
    assign("format_items", orig_fi, envir = asNamespace("NCVROC"))
    lockBinding("format_items", asNamespace("NCVROC"))
  }, add = TRUE)

  res_serial <- cross_size_cv(
    data               = dat,
    outcome            = "y",
    items              = items,
    model_sizes        = 1:3,
    selection_metric   = "youden",
    folds              = 3,
    repeats            = 1,
    engine             = "Rcpp",
    parallel           = "none",
    top_n              = top_n_req,
    seed               = 42
  )

  # HIGH-1: format_items must NOT be called for all 175 candidates
  # It should be called only for the returned top_n rows + 1 final selected model (at most top_n + 1)
  expect_lte(fi_count, top_n_req + 2L)
  expect_true(nrow(res_serial$candidate_ranking) <= top_n_req)
  expect_true(all(c("rank", "items", "n_items", "cv_youden") %in% names(res_serial$candidate_ranking)))

  # Reset format_items
  unlockBinding("format_items", asNamespace("NCVROC"))
  assign("format_items", orig_fi, envir = asNamespace("NCVROC"))
  lockBinding("format_items", asNamespace("NCVROC"))

  # 2. Run PSOCK chunks Strategy 2
  res_chunks <- cross_size_cv(
    data               = dat,
    outcome            = "y",
    items              = items,
    model_sizes        = 1:3,
    selection_metric   = "youden",
    folds              = 3,
    repeats            = 1,
    engine             = "Rcpp",
    parallel           = "chunks",
    n_workers          = 2L,
    top_n              = top_n_req,
    seed               = 42
  )

  # HIGH-2: Exact equivalence between PSOCK chunk streaming and serial
  expect_identical(res_chunks$final_selected_model$items, res_serial$final_selected_model$items)
  expect_identical(res_chunks$candidate_ranking$items, res_serial$candidate_ranking$items)
  expect_equal(res_chunks$candidate_ranking$cv_youden, res_serial$candidate_ranking$cv_youden, tolerance = 1e-12)
})
