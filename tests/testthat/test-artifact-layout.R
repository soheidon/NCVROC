test_that("ncvroc_artifact_layout and ncvroc_artifact_path resolve 5 categories and reject unsafe paths", {
  root <- tempfile("ncvroc_layout_ 日本語 テスト_")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  layout <- ncvroc_artifact_layout(root, create = FALSE)
  expect_s3_class(layout, "ncvroc_artifact_layout")
  expect_false(dir.exists(root))
  expect_identical(
    names(layout$categories),
    c("reports", "results", "candidate_chunks", "checkpoints", "diagnostics")
  )
  expect_identical(basename(layout$manifest_file), "artifact_manifest.rds")

  p_rep <- ncvroc_artifact_path(root, "reports", filename = "summary.csv", create = TRUE)
  p_res <- ncvroc_artifact_path(root, "results", filename = "model.rds", create = TRUE)
  p_chk <- ncvroc_artifact_path(root, "candidate_chunks", subdir = "size_5", create = TRUE)
  p_cp  <- ncvroc_artifact_path(root, "checkpoints", subdir = "run1", filename = "fold_state", create = TRUE)
  p_dia <- ncvroc_artifact_path(root, "diagnostics", filename = "session_info.txt", create = TRUE)

  expect_true(dir.exists(dirname(p_rep)))
  expect_true(dir.exists(dirname(p_res)))
  expect_true(dir.exists(p_chk))
  expect_true(dir.exists(dirname(p_cp)))
  expect_true(dir.exists(dirname(p_dia)))

  expect_error(ncvroc_artifact_path(root, "invalid_cat", filename = "a.rds"), "`category` must be one of")
  expect_error(ncvroc_artifact_path(root, "reports", filename = "../escape.csv"), "must not contain")
  expect_error(ncvroc_artifact_path(root, "reports", subdir = "/abs/path"), "must be a relative path")

  bad_file <- tempfile("ncvroc_not_dir_")
  writeLines("x", bad_file)
  on.exit(unlink(bad_file, force = TRUE), add = TRUE)
  expect_error(ncvroc_artifact_layout(bad_file, create = TRUE), "not a directory")

  # Non-directory category path must be rejected even with create = FALSE
  bad_cat_root <- tempfile("ncvroc_bad_cat_")
  dir.create(bad_cat_root, recursive = TRUE)
  on.exit(unlink(bad_cat_root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("not a dir", file.path(bad_cat_root, "reports"))
  expect_error(ncvroc_artifact_layout(bad_cat_root, create = FALSE), "not a directory")

  # Symlinked category directory must be rejected even with create = FALSE (when OS permits symlink creation)
  sym_root <- tempfile("ncvroc_sym_cat_")
  sym_target <- tempfile("ncvroc_sym_target_")
  dir.create(sym_root, recursive = TRUE)
  dir.create(sym_target, recursive = TRUE)
  on.exit({
    unlink(sym_root, recursive = TRUE, force = TRUE)
    unlink(sym_target, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  sym_ok <- suppressWarnings(file.symlink(sym_target, file.path(sym_root, "reports")))
  if (isTRUE(sym_ok)) {
    expect_error(ncvroc_artifact_layout(sym_root, create = FALSE), "symbolic link or reparse point")
  }
})

test_that("manifest distinguishes RDS roles, records skipped/failed without fabrication, and rejects corruption", {
  root <- tempfile("ncvroc_manifest_test_")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  res_rds <- ncvroc_artifact_path(root, "results", filename = "nested_result.rds", create = TRUE)
  saveRDS(list(ok = TRUE), res_rds)
  chunk_dir <- ncvroc_artifact_path(root, "candidate_chunks", subdir = "chunks_1", create = TRUE)
  saveRDS(data.frame(a = 1), file.path(chunk_dir, "chunk_0000.rds"))
  cp_dir <- ncvroc_artifact_path(root, "checkpoints", subdir = "cp_1", create = TRUE)
  saveRDS(list(fold = 1L), file.path(cp_dir, "fold_001.rds"))

  c_entry <- ncvroc_register_artifact(
    root, "candidate_chunks", "final_candidate_chunks", path = chunk_dir,
    producer = "test", status = "complete", analysis_identity = "id_123"
  )
  ncvroc_register_artifact(
    root, "results", "nested_result", path = res_rds,
    producer = "test", status = "complete", analysis_identity = "id_123",
    references = c_entry$artifact_id
  )
  ncvroc_register_artifact(
    root, "checkpoints", "outer_fold_checkpoint", path = cp_dir,
    producer = "test", status = "in_progress", analysis_identity = "id_123"
  )
  ncvroc_register_artifact(
    root, "results", "optional_5_item", path = NULL,
    producer = "rmd", status = "skipped", analysis_identity = "id_123",
    reason = "disabled by run flag"
  )
  ncvroc_register_artifact(
    root, "reports", "failed_export", path = NULL,
    producer = "rmd", status = "failed", analysis_identity = "id_123",
    reason = "simulated write failure"
  )

  # Must reject paths outside the specified category unless allow_external = TRUE
  ext_file <- tempfile("ncvroc_external_", fileext = ".rds")
  saveRDS(list(x = 1), ext_file)
  on.exit(unlink(ext_file, force = TRUE), add = TRUE)
  expect_error(
    ncvroc_register_artifact(root, "checkpoints", "misplaced", path = res_rds),
    "must lie inside category 'checkpoints'"
  )
  expect_error(
    ncvroc_register_artifact(root, "results", "outside", path = ext_file),
    "must lie inside category 'results'"
  )

  # Must not allow complete status for non-existent file
  missing_path <- ncvroc_artifact_path(root, "results", filename = "does_not_exist.rds")
  expect_error(
    ncvroc_register_artifact(root, "results", "missing", path = missing_path, status = "complete"),
    "missing artifact path"
  )
  expect_false(file.exists(missing_path))

  tbl <- ncvroc_list_artifacts(root)
  expect_identical(nrow(tbl), 5L)
  expect_identical(tbl$status[tbl$role == "optional_5_item"], "skipped")
  expect_true(is.na(tbl$relative_path[tbl$role == "optional_5_item"]))
  expect_identical(tbl$reason[tbl$role == "optional_5_item"], "disabled by run flag")
  expect_identical(tbl$status[tbl$role == "failed_export"], "failed")
  expect_identical(tbl$category[tbl$role == "nested_result"], "results")
  expect_identical(tbl$category[tbl$role == "final_candidate_chunks"], "candidate_chunks")
  expect_identical(tbl$category[tbl$role == "outer_fold_checkpoint"], "checkpoints")
  expect_match(tbl$references[tbl$role == "nested_result"], "candidate_chunks::")

  # Corrupted manifest must fail loudly
  manifest_file <- ncvroc_artifact_layout(root)$manifest_file
  writeLines("corrupt", manifest_file)
  expect_error(ncvroc_read_manifest(root), "Failed to read artifact manifest")
})

test_that("ncvroc with artifact_dir separates reports/results/checkpoints, preserves stats, and keeps diagnostics off", {
  set.seed(101)
  n <- 30
  p <- 4
  df <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), nrow = n))
  names(df) <- paste0("q", 1:p)
  df$y <- sample(0:1, n, replace = TRUE)

  legacy_out <- tempfile("ncvroc_legacy_out_")
  art_root <- tempfile("ncvroc_art_ 日本語_")
  on.exit({
    unlink(legacy_out, recursive = TRUE, force = TRUE)
    unlink(art_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  res_legacy <- ncvroc(
    df, "y", paste0("q", 1:p),
    min_items = 1, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "none",
    save_results = TRUE, output_dir = legacy_out,
    results_storage = "rds", results_dir = legacy_out,
    progress = FALSE, verbose = FALSE
  )
  expect_false(file.exists(file.path(legacy_out, "artifact_manifest.rds")))
  expect_false(dir.exists(file.path(legacy_out, "reports")))

  cp_dir <- ncvroc_artifact_path(art_root, "checkpoints", subdir = "cv_run")
  res_art <- ncvroc(
    df, "y", paste0("q", 1:p),
    min_items = 1, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "none",
    save_results = TRUE,
    results_storage = "rds",
    checkpoint_dir = cp_dir,
    artifact_dir = art_root,
    progress = FALSE, verbose = FALSE
  )

  # Statistical invariance
  expect_equal(res_art$nested_cv_summary, res_legacy$nested_cv_summary, tolerance = 1e-12)
  expect_identical(res_art$nested_result$selected_models, res_legacy$nested_result$selected_models)
  expect_equal(res_art$outer_predictions, res_legacy$outer_predictions, tolerance = 1e-12)
  expect_equal(res_art$final_candidates, res_legacy$final_candidates, tolerance = 1e-12)

  # 5-category placement & manifest
  expect_true(file.exists(file.path(art_root, "reports", "nested_cv_summary.csv")))
  expect_true(dir.exists(cp_dir))
  expect_true(file.exists(res_art$final_exhaustive_file))
  expect_true(startsWith(
    normalizePath(res_art$final_exhaustive_file, winslash = "/"),
    normalizePath(file.path(art_root, "results"), winslash = "/")
  ))
  # Diagnostics must remain opt-in (no diagnostic logs auto-created)
  expect_length(list.files(file.path(art_root, "diagnostics"), pattern = "^native-diagnostic-"), 0L)

  tbl <- ncvroc_list_artifacts(art_root)
  expect_true("reports" %in% tbl$category)
  expect_true("results" %in% tbl$category)
  expect_true("checkpoints" %in% tbl$category)
  expect_true(all(tbl$status == "complete"))

  # Conflicting output_dir / results_dir / checkpoint_dir must error
  expect_error(
    ncvroc(df, "y", paste0("q", 1:p), outer_k = 3, inner_k = 2,
           artifact_dir = art_root, output_dir = tempfile(), progress = FALSE, verbose = FALSE),
    "Conflicting `output_dir` and `artifact_dir`"
  )
  expect_error(
    ncvroc(df, "y", paste0("q", 1:p), outer_k = 3, inner_k = 2,
           artifact_dir = art_root, results_dir = tempfile(), progress = FALSE, verbose = FALSE),
    "Conflicting `results_dir` and `artifact_dir`"
  )
  expect_error(
    ncvroc(df, "y", paste0("q", 1:p), outer_k = 3, inner_k = 2,
           artifact_dir = art_root, checkpoint_dir = tempfile(), progress = FALSE, verbose = FALSE),
    "Conflicting `checkpoint_dir` and `artifact_dir`"
  )

  # Checkpoint resume inside artifact_dir/checkpoints works and preserves desktop.ini tolerance
  writeLines("[.ShellClassInfo]", file.path(cp_dir, "desktop.ini"))
  res_resumed <- ncvroc(
    df, "y", paste0("q", 1:p),
    min_items = 1, max_items = 2,
    outer_k = 3, inner_k = 2, seed = 77L,
    engine = "Rcpp", parallel = "none",
    checkpoint_dir = cp_dir, resume = TRUE,
    artifact_dir = art_root,
    progress = FALSE, verbose = FALSE
  )
  expect_identical(res_resumed$nested_result$settings$checkpoint_provenance$n_reused, 15L)
  expect_equal(res_resumed$nested_cv_summary, res_legacy$nested_cv_summary, tolerance = 1e-12)
})

test_that("ncvroc with artifact_dir routes chunked_rds to candidate_chunks and cache key is path-invariant", {
  set.seed(202)
  n <- 20
  p <- 42
  df <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), nrow = n))
  names(df) <- paste0("q", 1:p)
  df$y <- sample(0:1, n, replace = TRUE)

  art_root <- tempfile("ncvroc_chunk_art_")
  cache_dir <- tempfile("ncvroc_chunk_cache_")
  on.exit({
    unlink(art_root, recursive = TRUE, force = TRUE)
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  res_chunk <- ncvroc(
    df, "y", paste0("q", 1:p),
    item_count = "==4",
    outer_k = 2, inner_k = 2, seed = 88L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    final_search = TRUE,
    results_storage = "rds",
    cache = "off",
    artifact_dir = art_root,
    progress = FALSE, verbose = FALSE
  )

  expect_identical(res_chunk$storage_backend, "chunked_rds")
  expect_true(!is.null(res_chunk$chunk_dir) && dir.exists(res_chunk$chunk_dir))
  expect_true(startsWith(
    normalizePath(res_chunk$chunk_dir, winslash = "/"),
    normalizePath(file.path(art_root, "candidate_chunks"), winslash = "/")
  ))
  tbl <- ncvroc_list_artifacts(art_root, category = "candidate_chunks")
  expect_identical(nrow(tbl), 1L)
  expect_identical(tbl$role, "final_candidate_chunks")

  # Read back from candidate_chunks and verify missing chunk fails loudly
  loaded_tbl <- ncvroc_results(res_chunk, top_n = 5L)
  expect_true(is.data.frame(loaded_tbl) && nrow(loaded_tbl) > 0L)
  unlink(res_chunk$chunk_dir, recursive = TRUE, force = TRUE)
  expect_error(ncvroc_results(res_chunk, top_n = 5L))

  # Cache miss -> hit works and cache_key does not depend on artifact_dir
  res_cache1 <- ncvroc(
    df, "y", paste0("q", 1:p),
    item_count = "==4",
    outer_k = 2, inner_k = 2, seed = 88L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    final_search = TRUE,
    results_storage = "rds",
    cache = "reuse", cache_dir = cache_dir,
    artifact_dir = art_root,
    progress = FALSE, verbose = FALSE
  )
  art_root2 <- tempfile("ncvroc_chunk_art2_")
  on.exit(unlink(art_root2, recursive = TRUE, force = TRUE), add = TRUE)
  res_hit <- ncvroc(
    df, "y", paste0("q", 1:p),
    item_count = "==4",
    outer_k = 2, inner_k = 2, seed = 88L,
    engine = "Rcpp", parallel = "native_outer_fold", n_workers = 1L,
    final_search = TRUE,
    results_storage = "rds",
    cache = "reuse", cache_dir = cache_dir,
    artifact_dir = art_root2,
    progress = FALSE, verbose = FALSE
  )
  expect_true(isTRUE(res_hit$loaded_from_cache))
  expect_identical(res_hit$cache_key, res_cache1$cache_key)
  expect_identical(
    normalizePath(res_hit$artifact_dir, winslash = "/"),
    normalizePath(art_root2, winslash = "/")
  )
  hit_tbl <- ncvroc_list_artifacts(art_root2)
  expect_true("results" %in% hit_tbl$category)
  expect_true(all(hit_tbl$producer == "ncvroc_cache"))
})

test_that("ncvroc_artifact_path rejects symlinks in subdir and intermediate segments", {
  root <- tempfile("ncvroc_subdir_sym_")
  target_dir <- tempfile("ncvroc_subdir_sym_target_")
  dir.create(target_dir, recursive = TRUE)
  on.exit({
    unlink(root, recursive = TRUE, force = TRUE)
    unlink(target_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  ncvroc_artifact_layout(root, create = TRUE)
  outer_dir <- file.path(root, "checkpoints", "outer")
  dir.create(outer_dir, recursive = TRUE)
  link_path <- file.path(outer_dir, "linked_mid")

  sym_ok <- suppressWarnings(file.symlink(target_dir, link_path))
  skip_if_not(isTRUE(sym_ok), "Symbolic links cannot be created in this environment")

  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid", create = TRUE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid/nested/leaf", filename = "cp.rds", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid/nested/leaf", filename = "cp.rds", create = TRUE),
    "symbolic link or reparse point"
  )
  expect_false(dir.exists(file.path(target_dir, "nested")))
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", filename = "outer/linked_mid/file.rds", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", filename = "outer/linked_mid/file.rds", create = TRUE),
    "symbolic link or reparse point"
  )
})

test_that("ncvroc_artifact_path rejects Windows directory junctions where Sys.readlink is empty", {
  skip_if_not(.Platform$OS.type == "windows", "Windows junction test only applies on Windows")

  root <- tempfile("ncvroc_subdir_junc_")
  target_dir <- tempfile("ncvroc_subdir_junc_target_")
  dir.create(target_dir, recursive = TRUE)
  on.exit({
    unlink(root, recursive = TRUE, force = TRUE)
    unlink(target_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  ncvroc_artifact_layout(root, create = TRUE)
  outer_dir <- file.path(root, "checkpoints", "outer")
  dir.create(outer_dir, recursive = TRUE)
  link_path <- file.path(outer_dir, "linked_mid")

  status <- suppressWarnings(system2(
    "cmd",
    c("/c", "mklink", "/J",
      shQuote(normalizePath(link_path, winslash = "\\", mustWork = FALSE)),
      shQuote(normalizePath(target_dir, winslash = "\\", mustWork = TRUE))),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(identical(status, 0L) && dir.exists(link_path),
              "Windows directory junctions (mklink /J) cannot be created in this environment")

  rl <- suppressWarnings(Sys.readlink(link_path))
  expect_true(is.na(rl) || !nzchar(rl))

  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid", create = TRUE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid/nested/leaf", filename = "cp.rds", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", subdir = "outer/linked_mid/nested/leaf", filename = "cp.rds", create = TRUE),
    "symbolic link or reparse point"
  )
  expect_false(dir.exists(file.path(target_dir, "nested")))
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", filename = "outer/linked_mid/file.rds", create = FALSE),
    "symbolic link or reparse point"
  )
  expect_error(
    ncvroc_artifact_path(root, "checkpoints", filename = "outer/linked_mid/file.rds", create = TRUE),
    "symbolic link or reparse point"
  )
})

test_that("ncvroc_inspect_checkpoint distinguishes empty, valid complete, identity mismatch, missing, extra, and corrupt folds", {
  skip_if_not_installed("openssl")
  set.seed(123)
  n <- 40L
  df <- data.frame(
    q1 = sample(0:2, n, replace = TRUE),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    y  = sample(0:1, n, replace = TRUE)
  )

  art_root <- tempfile("ncvroc_cp_inspect_")
  on.exit(unlink(art_root, recursive = TRUE, force = TRUE), add = TRUE)

  # 1. Empty checkpoint directory -> in_progress (never complete)
  empty_cp <- ncvroc_artifact_path(art_root, "checkpoints", subdir = "ns", filename = "empty_cp", create = TRUE)
  dir.create(empty_cp, recursive = TRUE)
  empty_info <- ncvroc_inspect_checkpoint(empty_cp, outer_k = 2L, outer_repeats = 1L)
  expect_identical(empty_info$status, "in_progress")

  # 2. Run real checkpointed ncvroc -> complete and matches analysis_identity
  valid_cp <- ncvroc_artifact_path(art_root, "checkpoints", subdir = "ns", filename = "valid_cp", create = TRUE)
  res <- ncvroc(
    df, "y", c("q1", "q2", "q3"),
    item_count = "==1",
    outer_k = 2L, inner_k = 2L, outer_repeats = 1L, inner_repeats = 1L,
    seed = 77L, engine = "Rcpp", parallel = "none",
    final_search = FALSE, cache = "off",
    checkpoint_dir = valid_cp, checkpoint_granularity = "outer_fold",
    artifact_dir = art_root,
    progress = FALSE, verbose = FALSE
  )
  cp_id <- res$nested_result$settings$checkpoint_provenance$analysis_identity_hash
  expect_true(is.character(cp_id) && nzchar(cp_id))

  ok_info <- ncvroc_inspect_checkpoint(
    valid_cp,
    expected_analysis_identity = cp_id,
    outer_k = 2L,
    outer_repeats = 1L
  )
  expect_identical(ok_info$status, "complete")
  expect_identical(ok_info$n_valid, 2L)
  expect_identical(ok_info$n_missing, 0L)

  # 3. Identity mismatch -> failed
  mismatch_info <- ncvroc_inspect_checkpoint(
    valid_cp,
    expected_analysis_identity = paste0(rep("0", 64), collapse = ""),
    outer_k = 2L,
    outer_repeats = 1L
  )
  expect_identical(mismatch_info$status, "failed")

  # 4. Extra/unexpected fold file -> failed
  cp_extra <- tempfile("ncvroc_cp_extra_")
  dir.create(cp_extra, recursive = TRUE)
  on.exit(unlink(cp_extra, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(list.files(valid_cp, full.names = TRUE), cp_extra, recursive = TRUE)
  extra_fold <- file.path(cp_extra, "folds", "fold_r99_f99.rds")
  saveRDS(list(fake = TRUE), extra_fold)
  extra_info <- ncvroc_inspect_checkpoint(cp_extra, expected_analysis_identity = cp_id, outer_k = 2L, outer_repeats = 1L)
  expect_identical(extra_info$status, "failed")

  # 5. Corrupt fold RDS -> failed
  cp_corrupt <- tempfile("ncvroc_cp_corrupt_")
  dir.create(cp_corrupt, recursive = TRUE)
  on.exit(unlink(cp_corrupt, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(list.files(valid_cp, full.names = TRUE), cp_corrupt, recursive = TRUE)
  fold_files <- list.files(file.path(cp_corrupt, "folds"), full.names = TRUE)
  writeLines("not_an_rds_file", fold_files[[1L]])
  corrupt_info <- ncvroc_inspect_checkpoint(cp_corrupt, expected_analysis_identity = cp_id, outer_k = 2L, outer_repeats = 1L)
  expect_identical(corrupt_info$status, "failed")

  # 6. Missing fold file -> in_progress
  cp_missing <- tempfile("ncvroc_cp_missing_")
  dir.create(cp_missing, recursive = TRUE)
  on.exit(unlink(cp_missing, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(list.files(valid_cp, full.names = TRUE), cp_missing, recursive = TRUE)
  unlink(list.files(file.path(cp_missing, "folds"), full.names = TRUE)[[1L]])
  missing_info <- ncvroc_inspect_checkpoint(cp_missing, expected_analysis_identity = cp_id, outer_k = 2L, outer_repeats = 1L)
  expect_identical(missing_info$status, "in_progress")
  expect_identical(missing_info$n_missing, 1L)
  expect_identical(missing_info$n_valid, 1L)
})
