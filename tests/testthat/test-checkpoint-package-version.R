test_that("checkpoint version resolver reads the loaded NCVROC namespace", {
  expected <- as.character(getNamespaceVersion("NCVROC"))

  expect_identical(NCVROC:::.cp_resolve_package_version(), expected)
  expect_error(
    NCVROC:::.cp_resolve_package_version("NCVROC_namespace_not_loaded"),
    "namespace 'NCVROC_namespace_not_loaded' is not loaded"
  )
})

test_that("checkpoint identity uses the loaded namespace with stale installed metadata present", {
  namespace_version <- as.character(getNamespaceVersion("NCVROC"))
  old_lib_paths <- .libPaths()
  fake_library <- tempfile("ncvroc_stale_package_library_")
  fake_package <- file.path(fake_library, "NCVROC")
  dir.create(fake_package, recursive = TRUE)
  on.exit({
    .libPaths(old_lib_paths)
    unlink(fake_library, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  writeLines(
    c(
      "Package: NCVROC",
      "Version: 0.1.0",
      "Title: Temporary stale NCVROC metadata",
      "Description: Temporary fixture for checkpoint version resolution.",
      "License: MIT",
      "Author: NCVROC test",
      "Maintainer: NCVROC test <test@example.invalid>"
    ),
    file.path(fake_package, "DESCRIPTION")
  )
  .libPaths(c(fake_library, old_lib_paths))

  stale_metadata_version <- as.character(utils::packageDescription(
    "NCVROC",
    lib.loc = fake_library
  )$Version)
  expect_identical(stale_metadata_version, "0.1.0")
  expect_identical(NCVROC:::.cp_resolve_package_version(), namespace_version)

  identity <- NCVROC:::.cp_build_analysis_identity(
    x_mat = matrix(numeric(), nrow = 0L, ncol = 0L),
    y = integer(),
    items = character(),
    outcome_col = "y",
    positive_label = "1",
    negative_label = "0",
    fold_specs_ordered = list(),
    min_items = 1L,
    max_items = 1L,
    cutoff_method = "youden",
    preselect_top_n = 1L,
    preselect_by = "auc",
    selection_criterion = "auc",
    outer_k = 2L,
    inner_k = 2L,
    outer_repeats = 1L,
    inner_repeats = 1L,
    stratified = TRUE,
    seed = 1L,
    package_version = namespace_version
  )
  expect_identical(identity$provenance$package_version, namespace_version)
})

test_that("version resolution failure occurs before checkpoint artifacts are touched", {
  checkpoint_dir <- tempfile("ncvroc_version_resolution_failure_")
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sentinel <- file.path(checkpoint_dir, "identity.rds")
  writeBin(as.raw(c(0x4e, 0x43, 0x56, 0x52, 0x4f, 0x43)), sentinel)
  sentinel_hash <- unname(tools::md5sum(sentinel))

  local_mocked_bindings(
    .cp_resolve_package_version = function(...) {
      stop("simulated namespace version resolution failure", call. = FALSE)
    },
    .package = "NCVROC"
  )

  expect_error(
    NCVROC:::.cp_prepare_run(
      x_mat = matrix(numeric(), nrow = 0L, ncol = 0L),
      y_vec = integer(),
      outer_folds = list(),
      items = character(),
      outcome_col = "y",
      positive_label = "1",
      negative_label = "0",
      min_items = 1L,
      max_items = 1L,
      cutoff_method = "youden",
      preselect_by = "auc",
      selection_criterion = "auc",
      preselect_top_n = 1L,
      outer_k = 2L,
      inner_k = 2L,
      outer_repeats = 1L,
      inner_repeats = 1L,
      stratified = TRUE,
      seed = 1L,
      checkpoint_dir = checkpoint_dir,
      resume = TRUE,
      force_recompute = FALSE,
      consumer_backend = "scope_b_native_streaming",
      progress = FALSE,
      verbose = FALSE
    ),
    "simulated namespace version resolution failure"
  )

  expect_identical(unname(tools::md5sum(sentinel)), sentinel_hash)
  expect_identical(list.files(checkpoint_dir, all.files = TRUE, no.. = TRUE), "identity.rds")
})

test_that("loaded namespace version survives stale or missing package DESCRIPTION metadata", {
  skip_if_not_installed("pkgload")

  rscript_name <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  rscript <- file.path(R.home("bin"), rscript_name)
  skip_if_not(file.exists(rscript), "Rscript is unavailable for the isolated source-load check")

  source_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  script_file <- tempfile("ncvroc_namespace_version_probe_", fileext = ".R")
  on.exit(unlink(script_file, force = TRUE), add = TRUE)
  writeLines(
    c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "source_root <- args[[1L]]",
      "suppressPackageStartupMessages(pkgload::load_all(source_root, quiet = TRUE))",
      "fixture_name <- 'NCVROCVersionFixture'",
      "install_fixture <- function(version, fixture_lib) {",
      "  dir.create(fixture_lib)",
      "  fixture_root <- file.path(tempfile('ncvroc_version_fixture_source_'), fixture_name)",
      "  dir.create(file.path(fixture_root, 'R'), recursive = TRUE)",
      "  writeLines(c(",
      "    paste0('Package: ', fixture_name),",
      "    paste0('Version: ', version),",
      "    'Title: Checkpoint Version Resolver Fixture',",
      "    'Description: Temporary package fixture for resolver testing.',",
      "    'License: MIT',",
      "    'Author: NCVROC test',",
      "    'Maintainer: NCVROC test <test@example.invalid>'",
      "  ), file.path(fixture_root, 'DESCRIPTION'))",
      "  writeLines(character(), file.path(fixture_root, 'NAMESPACE'))",
      "  writeLines('fixture_marker <- TRUE', file.path(fixture_root, 'R', 'fixture.R'))",
      "  utils::install.packages(fixture_root, repos = NULL, lib = fixture_lib, type = 'source', quiet = TRUE)",
      "}",
      "stale_lib <- tempfile('ncvroc_version_stale_lib_')",
      "active_lib <- tempfile('ncvroc_version_active_lib_')",
      "install_fixture('0.1.0', stale_lib)",
      "install_fixture('9.8.7', active_lib)",
      ".libPaths(c(stale_lib, active_lib, .libPaths()))",
      "loadNamespace(fixture_name, lib.loc = active_lib)",
      "expected <- as.character(getNamespaceVersion(fixture_name))",
      "resolver <- getFromNamespace('.cp_resolve_package_version', 'NCVROC')",
      "stale_metadata <- as.character(utils::packageDescription(fixture_name, lib.loc = stale_lib)$Version)",
      "legacy_stale <- as.character(utils::packageVersion(fixture_name))",
      "resolved_stale <- resolver(fixture_name)",
      "if (!identical(stale_metadata, '0.1.0') || !identical(legacy_stale, expected) || !identical(resolved_stale, expected)) {",
      "  cat('STALE_METADATA_CHECK=FALSE\\n')",
      "  quit(save = 'no', status = 2L)",
      "}",
      "cat('STALE_METADATA_CHECK=TRUE\\n')",
      "empty_lib <- tempfile('ncvroc_version_empty_lib_')",
      "dir.create(empty_lib)",
      ".libPaths(empty_lib)",
      "unlink(c(file.path(active_lib, fixture_name, 'DESCRIPTION'), file.path(active_lib, fixture_name, 'Meta', 'package.rds')))",
      "legacy_missing <- tryCatch(utils::packageVersion(fixture_name), error = identity)",
      "resolved_missing <- resolver(fixture_name)",
      "ok <- inherits(legacy_missing, 'error') && identical(resolved_missing, expected)",
      "cat('MISSING_METADATA_FAILURE=', inherits(legacy_missing, 'error'), '\\n', sep = '')",
      "cat('NAMESPACE_RESOLVER_MATCH=', identical(resolved_missing, expected), '\\n', sep = '')",
      "unlink(c(stale_lib, active_lib, empty_lib), recursive = TRUE, force = TRUE)",
      "quit(save = 'no', status = if (ok) 0L else 3L)"
    ),
    script_file
  )

  output <- suppressWarnings(system2(
    rscript,
    args = c("--vanilla", shQuote(script_file), shQuote(source_root)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(as.integer(status), 0L, info = paste(output, collapse = "\n"))
  expect_true(any(grepl("STALE_METADATA_CHECK=TRUE", output, fixed = TRUE)))
  expect_true(any(grepl("MISSING_METADATA_FAILURE=TRUE", output, fixed = TRUE)))
  expect_true(any(grepl("NAMESPACE_RESOLVER_MATCH=TRUE", output, fixed = TRUE)))
})
