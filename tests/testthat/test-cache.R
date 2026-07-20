# test-cache.R — Tests for result caching

# ---- Test helpers ----

make_cache_test_data <- function(n = 50, seed = 42) {
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

CACHE_ITEMS <- paste0("Q", 1:5)

# ---- .compute_cache_key ----

test_that(".compute_cache_key returns 32-char hex or NULL", {
  d <- make_cache_test_data()
  dat <- .prepare_ncvroc_data(d, "y", CACHE_ITEMS)

  key <- .compute_cache_key(
    cache_data = dat, cache_outcome = "y", cache_items = CACHE_ITEMS,
    min_items = 1, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    cutoff_method = "youden", selection_criterion = "auc",
    preselect_top_n = 10, preselect_by = "auc",
    final_search = TRUE, final_rank_by = "auc",
    engine = "R", seed = 42,
    positive_label = 1, negative_label = 0, stratified = TRUE,
    chunk_size = 200000L
  )

  expect_type(key, "character")
  expect_equal(nchar(key), 32)
  expect_true(grepl("^[0-9a-f]{32}$", key))
})

test_that(".compute_cache_key returns NULL for NULL data", {
  key <- .compute_cache_key(
    cache_data = NULL, cache_outcome = "y", cache_items = CACHE_ITEMS,
    min_items = 1, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    cutoff_method = "youden", selection_criterion = "auc",
    preselect_top_n = 10, preselect_by = "auc",
    final_search = TRUE, final_rank_by = "auc",
    engine = "R", seed = 42,
    positive_label = 1, negative_label = 0, stratified = TRUE,
    chunk_size = 200000L
  )
  expect_null(key)
})

test_that("identical inputs produce identical cache keys", {
  d <- make_cache_test_data()
  dat <- .prepare_ncvroc_data(d, "y", CACHE_ITEMS)

  args <- list(
    cache_data = dat, cache_outcome = "y", cache_items = CACHE_ITEMS,
    min_items = 1, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    cutoff_method = "youden", selection_criterion = "auc",
    preselect_top_n = 10, preselect_by = "auc",
    final_search = TRUE, final_rank_by = "auc",
    engine = "R", seed = 42,
    positive_label = 1, negative_label = 0, stratified = TRUE,
    chunk_size = 200000L
  )

  key1 <- do.call(.compute_cache_key, args)
  key2 <- do.call(.compute_cache_key, args)
  expect_equal(key1, key2)
})

test_that("different data produces different cache keys", {
  d1 <- make_cache_test_data(seed = 42)
  d2 <- make_cache_test_data(seed = 99)
  dat1 <- .prepare_ncvroc_data(d1, "y", CACHE_ITEMS)
  dat2 <- .prepare_ncvroc_data(d2, "y", CACHE_ITEMS)

  base_args <- list(
    cache_outcome = "y", cache_items = CACHE_ITEMS,
    min_items = 1, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    cutoff_method = "youden", selection_criterion = "auc",
    preselect_top_n = 10, preselect_by = "auc",
    final_search = TRUE, final_rank_by = "auc",
    engine = "R", seed = 42,
    positive_label = 1, negative_label = 0, stratified = TRUE,
    chunk_size = 200000L
  )

  key1 <- do.call(.compute_cache_key, c(list(cache_data = dat1), base_args))
  key2 <- do.call(.compute_cache_key, c(list(cache_data = dat2), base_args))
  expect_true(key1 != key2)
})

test_that("different params produce different cache keys", {
  d <- make_cache_test_data()
  dat <- .prepare_ncvroc_data(d, "y", CACHE_ITEMS)

  base_args <- list(
    cache_data = dat, cache_outcome = "y", cache_items = CACHE_ITEMS,
    min_items = 1, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, inner_repeats = 1,
    cutoff_method = "youden", selection_criterion = "auc",
    preselect_top_n = 10, preselect_by = "auc",
    final_search = TRUE, final_rank_by = "auc",
    engine = "R", seed = 42,
    positive_label = 1, negative_label = 0, stratified = TRUE,
    chunk_size = 200000L
  )

  key1 <- do.call(.compute_cache_key, base_args)
  key2 <- do.call(.compute_cache_key, modifyList(base_args, list(seed = 99)))
  expect_true(key1 != key2)
})

# ---- .load_cache / .save_cache ----

test_that("save_cache + load_cache round-trips correctly", {
  cache_dir <- file.path(tempdir(), paste0("cache_test_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- list(
    config = list(outcome = "y"),
    storage_backend = "memory",
    final_exhaustive_ranked = data.frame(a = 1:3),
    loaded_from_cache = FALSE
  )

  metadata_list <- list(function_name = "test", created_at = Sys.time())

  saved <- .save_cache(result, full_results = NULL, cache_dir, "abc123",
                        metadata_list, storage_backend = "memory")
  expect_false(saved$loaded_from_cache)

  loaded <- .load_cache(cache_dir, "abc123")
  expect_false(is.null(loaded))
  expect_true(loaded$loaded_from_cache)
})

test_that("load_cache returns NULL for missing key", {
  cache_dir <- file.path(tempdir(), paste0("cache_test_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(cache_dir, recursive = TRUE)

  loaded <- .load_cache(cache_dir, "nonexistent_key")
  expect_null(loaded)
})

test_that("load_cache returns NULL for incomplete build", {
  cache_dir <- file.path(tempdir(), paste0("cache_test_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(cache_dir, recursive = TRUE)

  # Create a building directory (incomplete)
  building <- file.path(cache_dir, "test_key")
  dir.create(building)
  saveRDS(list(complete = FALSE), file.path(building, "metadata.rds"))
  saveRDS(list(x = 1), file.path(building, "result.rds"))

  loaded <- .load_cache(cache_dir, "test_key")
  expect_null(loaded)
})

test_that("atomic write cleans up old entry on update", {
  cache_dir <- file.path(tempdir(), paste0("cache_test_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result1 <- list(v = 1, storage_backend = "memory",
                  loaded_from_cache = FALSE, cache_entry_dir = NULL)
  result2 <- list(v = 2, storage_backend = "memory",
                  loaded_from_cache = FALSE, cache_entry_dir = NULL)

  .save_cache(result1, NULL, cache_dir, "key1",
              list(function_name = "f"), "memory")
  loaded1 <- .load_cache(cache_dir, "key1")
  expect_equal(loaded1$v, 1)

  .save_cache(result2, NULL, cache_dir, "key1",
              list(function_name = "f"), "memory")
  loaded2 <- .load_cache(cache_dir, "key1")
  expect_equal(loaded2$v, 2)
})

# ---- Cache with roc_bruteforce ----

test_that("cache reuse avoids re-computation in roc_bruteforce", {
  d <- make_cache_test_data()
  cache_dir <- file.path(tempdir(), paste0("cache_bf_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res1 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "memory",
    cache = "reuse", cache_dir = cache_dir)
  expect_false(res1$loaded_from_cache)

  res2 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "memory",
    cache = "reuse", cache_dir = cache_dir)
  expect_true(res2$loaded_from_cache)
  expect_equal(res1$n_combinations, res2$n_combinations)
})

# ---- Cache with ncvroc ----

test_that("cache reuse works for ncvroc", {
  d <- make_cache_test_data()
  cache_dir <- file.path(tempdir(), paste0("cache_ncvroc_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res1 <- ncvroc(d, y, Q1:Q5, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = TRUE, results_storage = "auto",
    cache = "reuse", cache_dir = cache_dir, verbose = FALSE)
  expect_false(res1$loaded_from_cache)

  res2 <- ncvroc(d, y, Q1:Q5, max_items = 2, mode = "quick",
    outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
    seed = 42, final_search = TRUE, results_storage = "auto",
    cache = "reuse", cache_dir = cache_dir, verbose = FALSE)
  expect_true(res2$loaded_from_cache)
  expect_equal(res1$final_n_combinations, res2$final_n_combinations)
})

test_that("cache refresh forces re-computation", {
  d <- make_cache_test_data()
  cache_dir <- file.path(tempdir(), paste0("cache_refresh_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res1 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "memory",
    cache = "reuse", cache_dir = cache_dir)
  expect_false(res1$loaded_from_cache)

  res2 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "memory",
    cache = "refresh", cache_dir = cache_dir)
  expect_false(res2$loaded_from_cache)  # refresh = re-compute
})

# ---- Cache validation ----

test_that("cache != off without cache_dir errors", {
  d <- make_cache_test_data()
  expect_error(
    roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
      progress = FALSE, cache = "reuse"),
    "cache_dir must be set"
  )
})

test_that("empty cache_dir errors", {
  d <- make_cache_test_data()
  expect_error(
    roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
      progress = FALSE, cache = "reuse", cache_dir = ""),
    "cache_dir must be"
  )
})

test_that("cache_key is stored in result objects", {
  d <- make_cache_test_data()
  cache_dir <- file.path(tempdir(), paste0("cache_key_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "memory",
    cache = "reuse", cache_dir = cache_dir)
  expect_true(!is.null(res$cache_key))
  expect_equal(nchar(res$cache_key), 32)
})

# ---- .cleanup_building_cache ----

test_that(".cleanup_building_cache removes incomplete build", {
  building <- file.path(tempdir(), paste0("building_test_", sample.int(1e6, 1)))
  dir.create(building)
  saveRDS(list(complete = FALSE), file.path(building, "metadata.rds"))

  expect_true(dir.exists(building))
  .cleanup_building_cache(building)
  expect_false(dir.exists(building))
})

test_that(".cleanup_building_cache preserves complete build", {
  building <- file.path(tempdir(), paste0("building_test_", sample.int(1e6, 1)))
  dir.create(building)
  saveRDS(list(complete = TRUE), file.path(building, "metadata.rds"))

  .cleanup_building_cache(building)
  expect_true(dir.exists(building))
  unlink(building, recursive = TRUE, force = TRUE)
})

# ---- Cache with single_rds storage ----

test_that("cache works with results_storage = 'rds'", {
  d <- make_cache_test_data()
  cache_dir <- file.path(tempdir(), paste0("cache_rds_", sample.int(1e6, 1)))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  res1 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "rds",
    cache = "reuse", cache_dir = cache_dir)
  expect_equal(res1$storage_backend, "single_rds")
  expect_false(res1$loaded_from_cache)

  res2 <- roc_bruteforce(d, "y", Q1:Q5, max_items = 2, engine = "R",
    progress = FALSE, results_storage = "rds",
    cache = "reuse", cache_dir = cache_dir)
  expect_true(res2$loaded_from_cache)
  expect_equal(res2$storage_backend, "single_rds")
})
