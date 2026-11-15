.make_windows_metadata_checkpoint_data <- function() {
  set.seed(771L)
  outcome <- rep(c(0L, 1L), each = 18L)
  x <- matrix(stats::rnorm(36L * 4L), nrow = 36L, ncol = 4L)
  x[outcome == 1L, 1L] <- x[outcome == 1L, 1L] + 0.8
  data <- as.data.frame(x)
  names(data) <- paste0("q", seq_len(ncol(x)))
  data$y <- outcome
  data
}

.run_windows_metadata_checkpoint <- function(data, backend, checkpoint_dir = NULL) {
  nested_sum_roc(
    data = data,
    outcome = "y",
    items = paste0("q", seq_len(ncol(data) - 1L)),
    min_items = 1L,
    max_items = 2L,
    preselect_top_n = 8L,
    outer_k = 3L,
    inner_k = 2L,
    outer_repeats = 1L,
    seed = 771L,
    engine = "Rcpp",
    parallel = backend,
    n_workers = 1L,
    checkpoint_dir = checkpoint_dir,
    checkpoint_granularity = "outer_fold",
    resume = TRUE,
    progress = FALSE,
    verbose = FALSE
  )
}

test_that("only plain desktop.ini is ignorable Windows folds metadata", {
  directory <- tempfile("ncvroc_desktop_ini_predicate_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  ini_path <- file.path(directory, "desktop.ini")
  ini_bytes <- as.raw(c(0x5b, 0x56, 0x69, 0x65, 0x77, 0x53, 0x74, 0x61, 0x74, 0x65, 0x5d))
  writeBin(ini_bytes, ini_path)

  expect_true(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = TRUE
  ))
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = FALSE
  ))
  expect_identical(
    NCVROC:::.cp_filter_windows_desktop_ini(
      directory, c("desktop.ini", "unexpected.txt"), windows = TRUE
    ),
    "unexpected.txt"
  )
  expect_identical(
    NCVROC:::.cp_filter_windows_desktop_ini(
      directory, c("desktop.ini", "unexpected.txt"), windows = FALSE
    ),
    c("desktop.ini", "unexpected.txt")
  )

  upper_path <- file.path(directory, "DESKTOP.INI")
  writeBin(ini_bytes, upper_path)
  expect_true(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "DESKTOP.INI", windows = TRUE
  ))

  directory_holder <- file.path(directory, "directory_entry")
  dir.create(directory_holder)
  directory_named_ini <- file.path(directory_holder, "desktop.ini")
  dir.create(directory_named_ini)
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory_holder, basename(directory_named_ini), windows = TRUE
  ))

  # Controlled link results prove that positive, missing, and failed link
  # inspection all fail closed without relying on Windows symlink privileges.
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = TRUE,
    readlink_fn = function(path) "target"
  ))
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = TRUE,
    readlink_fn = function(path) NA_character_
  ))
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = TRUE,
    readlink_fn = function(path) stop("link metadata unavailable")
  ))
})

test_that("desktop.ini does not block Native checkpoint save and full resume on Windows", {
  skip_if_not(identical(.Platform$OS.type, "windows"))

  data <- .make_windows_metadata_checkpoint_data()
  ini_bytes <- as.raw(c(0x5b, 0x45, 0x78, 0x70, 0x6c, 0x6f, 0x72, 0x65, 0x72, 0x5d))

  for (backend in c("native_outer_fold", "native_streaming")) {
    reference <- .run_windows_metadata_checkpoint(data, backend)
    checkpoint_dir <- tempfile(paste0("ncvroc_desktop_ini_", backend, "_"))
    on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
    paths <- NCVROC:::.cp_resolve_root(checkpoint_dir)
    ini_path <- file.path(paths$folds_dir, "desktop.ini")
    writeBin(ini_bytes, ini_path)
    ini_hash <- unname(tools::md5sum(ini_path))

    first <- .run_windows_metadata_checkpoint(data, backend, checkpoint_dir)
    expect_equal(first$summary, reference$summary, tolerance = 1e-12)
    expect_equal(first$outer_predictions, reference$outer_predictions, tolerance = 1e-12)
    expect_identical(unname(tools::md5sum(ini_path)), ini_hash)
    expect_identical(readBin(ini_path, "raw", n = length(ini_bytes)), ini_bytes)
    expect_equal(first$settings$checkpoint_provenance$n_reused, 0L)

    resumed <- .run_windows_metadata_checkpoint(data, backend, checkpoint_dir)
    expect_equal(resumed$summary, reference$summary, tolerance = 1e-12)
    expect_equal(resumed$outer_predictions, reference$outer_predictions, tolerance = 1e-12)
    expect_equal(
      resumed$settings$checkpoint_provenance$n_reused,
      length(resumed$outer_results)
    )
    expect_identical(unname(tools::md5sum(ini_path)), ini_hash)

    unknown_path <- file.path(paths$folds_dir, "unexpected.txt")
    writeLines("leave this file untouched", unknown_path)
    unknown_hash <- unname(tools::md5sum(unknown_path))
    expect_error(
      .run_windows_metadata_checkpoint(data, backend, checkpoint_dir),
      "Unexpected or orphan checkpoint artifact"
    )
    expect_identical(unname(tools::md5sum(unknown_path)), unknown_hash)
    unlink(unknown_path)

    pending_ini <- file.path(paths$pending_dir, "desktop.ini")
    writeBin(ini_bytes, pending_ini)
    pending_hash <- unname(tools::md5sum(pending_ini))
    pending_resumed <- .run_windows_metadata_checkpoint(data, backend, checkpoint_dir)
    expect_equal(pending_resumed$summary, reference$summary, tolerance = 1e-12)
    expect_equal(pending_resumed$outer_predictions, reference$outer_predictions, tolerance = 1e-12)
    expect_equal(pending_resumed$settings$checkpoint_provenance$n_reused, length(pending_resumed$outer_results))
    expect_identical(unname(tools::md5sum(pending_ini)), pending_hash)

    pending_unknown <- file.path(paths$pending_dir, "unfinished_fold.rds.tmp")
    writeLines("unfinished write", pending_unknown)
    unknown_pending_hash <- unname(tools::md5sum(pending_unknown))
    expect_error(
      .run_windows_metadata_checkpoint(data, backend, checkpoint_dir),
      "Unresolved pending checkpoint artifact.*unfinished_fold.rds.tmp"
    )
    expect_identical(unname(tools::md5sum(pending_unknown)), unknown_pending_hash)
    expect_identical(unname(tools::md5sum(pending_ini)), pending_hash)
    unlink(pending_unknown)

    unlink(pending_ini)
    dir.create(pending_ini)
    expect_error(
      .run_windows_metadata_checkpoint(data, backend, checkpoint_dir),
      "Unresolved pending checkpoint artifact.*desktop.ini"
    )
    unlink(pending_ini, recursive = TRUE)
    writeBin(ini_bytes, pending_ini)
    pending_hash <- unname(tools::md5sum(pending_ini))

    first_fold <- file.path(
      paths$folds_dir,
      NCVROC:::.cp_fold_filename(1L, 1L)
    )
    valid_fold_hash <- unname(tools::md5sum(first_fold))
    writeBin(as.raw(c(0x01, 0x02, 0x03)), first_fold)
    corrupt_hash <- unname(tools::md5sum(first_fold))
    expect_error(
      .run_windows_metadata_checkpoint(data, backend, checkpoint_dir),
      "Corrupt checkpoint file"
    )
    expect_identical(unname(tools::md5sum(first_fold)), corrupt_hash)
    expect_identical(unname(tools::md5sum(ini_path)), ini_hash)
    expect_identical(unname(tools::md5sum(pending_ini)), pending_hash)
    expect_false(identical(valid_fold_hash, corrupt_hash))
  }
})

test_that("pending desktop.ini alone permits Native Streaming save and resume", {
  skip_if_not(identical(.Platform$OS.type, "windows"))

  data <- .make_windows_metadata_checkpoint_data()
  checkpoint_dir <- tempfile("ncvroc_pending_desktop_ini_only_")
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- NCVROC:::.cp_resolve_root(checkpoint_dir)
  ini_path <- file.path(paths$pending_dir, "desktop.ini")
  ini_bytes <- as.raw(c(0x5b, 0x50, 0x65, 0x6e, 0x64, 0x69, 0x6e, 0x67, 0x5d))
  writeBin(ini_bytes, ini_path)
  ini_hash <- unname(tools::md5sum(ini_path))

  first <- .run_windows_metadata_checkpoint(data, "native_streaming", checkpoint_dir)
  expect_equal(first$settings$checkpoint_provenance$n_reused, 0L)
  expect_equal(first$settings$checkpoint_provenance$n_computed, length(first$outer_results))
  expect_identical(unname(tools::md5sum(ini_path)), ini_hash)
  expect_identical(readBin(ini_path, "raw", n = length(ini_bytes)), ini_bytes)

  resumed <- .run_windows_metadata_checkpoint(data, "native_streaming", checkpoint_dir)
  expect_equal(resumed$summary, first$summary, tolerance = 1e-12)
  expect_equal(resumed$outer_predictions, first$outer_predictions, tolerance = 1e-12)
  expect_equal(resumed$settings$checkpoint_provenance$n_reused, length(resumed$outer_results))
  expect_identical(unname(tools::md5sum(ini_path)), ini_hash)
})

test_that("desktop.ini remains an orphan on non-Windows systems and directories", {
  directory <- tempfile("ncvroc_desktop_ini_unknown_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("metadata", file.path(directory, "desktop.ini"))
  directory_holder <- file.path(directory, "directory_entry")
  dir.create(directory_holder)
  dir.create(file.path(directory_holder, "desktop.ini"))

  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory, "desktop.ini", windows = FALSE
  ))
  expect_false(NCVROC:::.cp_is_ignorable_windows_desktop_ini(
    directory_holder, "desktop.ini", windows = TRUE
  ))
})

test_that("desktop.ini survives a Native Streaming interruption and resume", {
  skip_if_not(identical(.Platform$OS.type, "windows"))

  seams <- NCVROC:::.ncvroc_checkpoint_test_seams
  old_interrupt <- seams$simulate_interrupt_at
  old_call_count <- seams$native_call_count
  on.exit({
    seams$simulate_interrupt_at <- old_interrupt
    seams$native_call_count <- old_call_count
  }, add = TRUE)

  data <- .make_windows_metadata_checkpoint_data()
  reference <- .run_windows_metadata_checkpoint(data, "native_streaming")
  checkpoint_dir <- tempfile("ncvroc_desktop_ini_interrupt_")
  on.exit(unlink(checkpoint_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- NCVROC:::.cp_resolve_root(checkpoint_dir)
  ini_path <- file.path(paths$folds_dir, "desktop.ini")
  ini_bytes <- as.raw(c(0x5b, 0x49, 0x6e, 0x74, 0x65, 0x72, 0x72, 0x75, 0x70, 0x74, 0x5d))

  seams$native_call_count <- 0L
  # Interrupt inside the first streaming fold after its second native wave;
  # no completed fold is expected to be reusable from this partial run.
  seams$simulate_interrupt_at <- 2L
  interrupted <- tryCatch(
    .run_windows_metadata_checkpoint(data, "native_streaming", checkpoint_dir),
    error = function(e) e
  )
  expect_s3_class(interrupted, "ncvroc_interrupted")
  writeBin(ini_bytes, ini_path)
  ini_hash <- unname(tools::md5sum(ini_path))

  seams$simulate_interrupt_at <- NULL
  resumed <- .run_windows_metadata_checkpoint(data, "native_streaming", checkpoint_dir)
  expect_equal(resumed$summary, reference$summary, tolerance = 1e-12)
  expect_equal(resumed$outer_predictions, reference$outer_predictions, tolerance = 1e-12)
  expect_equal(resumed$settings$checkpoint_provenance$n_reused, 0L)
  expect_identical(unname(tools::md5sum(ini_path)), ini_hash)
})
