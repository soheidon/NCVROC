test_that("v0.34.1 streaming Rcpp matches R across all cutoff methods, tie-breakers, and non-integer scores", {
  set.seed(34101)
  n <- 28L
  # Integer 0/1/2 items (tests bounded integer fast path)
  df_int <- data.frame(
    q1 = c(sample(0:2, 14, replace = TRUE), sample(1:2, 14, replace = TRUE)),
    q2 = c(sample(0:1, 14, replace = TRUE), sample(1:2, 14, replace = TRUE)),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE),
    q5 = sample(0:2, n, replace = TRUE),
    y  = c(rep(0L, 14L), rep(1L, 14L))
  )
  items <- paste0("q", 1:5)

  configs <- list(
    list(cutoff_method = "youden",              smin = NULL, spmin = NULL, nf = "record_failure",  pfi = TRUE,  sm = "auc"),
    list(cutoff_method = "closest_topleft",     smin = NULL, spmin = NULL, nf = "record_failure",  pfi = FALSE, sm = "youden"),
    list(cutoff_method = "sensitivity_target",  smin = 0.70, spmin = NULL, nf = "record_failure",  pfi = TRUE,  sm = "auc"),
    list(cutoff_method = "sensitivity_target",  smin = 0.99, spmin = NULL, nf = "fallback_youden", pfi = TRUE,  sm = "auc"),
    list(cutoff_method = "clinical_constraint", smin = 0.65, spmin = 0.65, nf = "record_failure",  pfi = TRUE,  sm = "accuracy"),
    list(cutoff_method = "clinical_constraint", smin = 0.99, spmin = 0.99, nf = "fallback_youden", pfi = TRUE,  sm = "auc")
  )

  for (cfg in configs) {
    res_r <- classification_stability_roc(
      data = df_int, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3,
      resampling = "ordinary", bootstrap_reps = 5L,
      cutoff_method = cfg$cutoff_method,
      sensitivity_min = cfg$smin, specificity_min = cfg$spmin,
      selection_metric = cfg$sm, prefer_fewer_items = cfg$pfi,
      no_feasible = cfg$nf, engine = "R", parallel = "none",
      seed = 901L, progress = FALSE
    )
    res_cpp <- classification_stability_roc(
      data = df_int, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3,
      resampling = "ordinary", bootstrap_reps = 5L,
      cutoff_method = cfg$cutoff_method,
      sensitivity_min = cfg$smin, specificity_min = cfg$spmin,
      selection_metric = cfg$sm, prefer_fewer_items = cfg$pfi,
      no_feasible = cfg$nf, engine = "Rcpp", parallel = "threads", n_workers = 2L,
      checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
      seed = 901L, progress = FALSE
    )
    expect_equal(res_cpp$reference_model$selected_items, res_r$reference_model$selected_items)
    expect_equal(res_cpp$reference_model$cutoff, res_r$reference_model$cutoff, tolerance = 1e-12)
    expect_equal(res_cpp$replicate_summary, res_r$replicate_summary, tolerance = 1e-12)
    expect_equal(res_cpp$subject_summary, res_r$subject_summary, tolerance = 1e-12)
  }

  # Continuous (floating-point) predictors test general std::map fallback path
  df_dbl <- data.frame(
    q1 = c(rnorm(14, 0), rnorm(14, 1.8)),
    q2 = c(rnorm(14, 0), rnorm(14, 1.2)),
    q3 = rnorm(n),
    q4 = rnorm(n),
    y  = c(rep(0L, 14L), rep(1L, 14L))
  )
  r_dbl <- classification_stability_roc(
    data = df_dbl, outcome = "y", mode = "full_procedure",
    items = paste0("q", 1:4), model_sizes = 1:3,
    bootstrap_reps = 4L, cutoff_method = "sensitivity_target", sensitivity_min = 0.75,
    engine = "R", seed = 902L, progress = FALSE
  )
  cpp_dbl <- classification_stability_roc(
    data = df_dbl, outcome = "y", mode = "full_procedure",
    items = paste0("q", 1:4), model_sizes = 1:3,
    bootstrap_reps = 4L, cutoff_method = "sensitivity_target", sensitivity_min = 0.75,
    engine = "Rcpp", parallel = "threads", n_workers = 2L,
    checkpoint_every_candidates = 6L, candidate_batch_size = 3L,
    seed = 902L, progress = FALSE
  )
  expect_equal(cpp_dbl$replicate_summary$selected_items, r_dbl$replicate_summary$selected_items)
  expect_equal(cpp_dbl$replicate_summary$status, r_dbl$replicate_summary$status)
  expect_equal(cpp_dbl$replicate_summary$cutoff, r_dbl$replicate_summary$cutoff, tolerance = 1e-12)
  # Also verify compare_classification_stability reports coverage
  r_fix_dbl <- cutoff_stability_roc(
    data = df_dbl, outcome = "y", candidate_set = c("q1", "q2"),
    bootstrap_reps = 4L, cutoff_method = "sensitivity_target", sensitivity_min = 0.75,
    seed = 902L, progress = FALSE
  )
  cmp_dbl <- compare_classification_stability(r_fix_dbl, cpp_dbl, data = df_dbl)
  expect_true(cmp_dbl$comparable)
  expect_equal(cmp_dbl$coverage, cmp_dbl$n_common_valid / cmp_dbl$n_requested)
})

test_that("v0.34.1 intra-replicate batch checkpoint, 2-slot recovery, and corruption guards work", {
  set.seed(34102)
  n <- 30L
  df <- data.frame(
    q1 = c(sample(0:2, 15, replace = TRUE), sample(1:2, 15, replace = TRUE)),
    q2 = c(sample(0:2, 15, replace = TRUE), sample(1:2, 15, replace = TRUE)),
    q3 = sample(0:2, n, replace = TRUE),
    q4 = sample(0:2, n, replace = TRUE),
    q5 = sample(0:2, n, replace = TRUE),
    q6 = sample(0:2, n, replace = TRUE),
    y  = c(rep(0L, 15L), rep(1L, 15L))
  )
  items <- paste0("q", 1:6) # 1:3 => 6 + 15 + 20 = 41 candidates

  work_base <- tempfile("ncvroc_work_")
  dir.create(work_base, recursive = TRUE)
  on.exit(unlink(work_base, recursive = TRUE, force = TRUE), add = TRUE)

  # Uninterrupted reference
  res_ref <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:3,
    bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
    checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
    seed = 777L, progress = FALSE
  )

  # Run with work_dir & rolling 2-slot checkpoints
  res_work <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:3,
    bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
    work_dir = work_base, run_id = "test_run_01",
    checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
    checkpoint_every = 1L, seed = 777L, progress = FALSE
  )
  expect_equal(res_work$subject_summary, res_ref$subject_summary, tolerance = 1e-12)
  expect_equal(res_work$replicate_summary, res_ref$replicate_summary, tolerance = 1e-12)

  # Real mid-reference-fit interruption & resume
  old_opt <- getOption("ncvroc.cs_test_interrupt_after_ckpts")
  on.exit(options(ncvroc.cs_test_interrupt_after_ckpts = old_opt), add = TRUE)
  options(ncvroc.cs_test_interrupt_after_ckpts = 2L) # interrupts inside reference fit at cursor = 20
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3,
      bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
      work_dir = work_base, run_id = "test_ref_interrupt",
      checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
      checkpoint_every = 25L, seed = 777L, progress = FALSE
    ),
    "Simulated mid-replicate interruption"
  )
  options(ncvroc.cs_test_interrupt_after_ckpts = NULL)

  ref_int_file <- file.path(work_base, "runs", "test_ref_interrupt", "checkpoints", "classification_stability_result.rds")
  ref_int_loaded <- NCVROC:::.cs_load_rolling_slot(ref_int_file, res_work$provenance$analysis_identity)
  expect_false(isTRUE(ref_int_loaded$payload$ref_done))
  expect_equal(ref_int_loaded$payload$ref_stream_state$rep_cursor, 20)

  res_ref_resume <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:3,
    bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
    work_dir = work_base, run_id = "test_ref_interrupt", resume = TRUE,
    checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
    checkpoint_every = 25L, seed = 777L, progress = FALSE
  )
  expect_equal(res_ref_resume$subject_summary, res_ref$subject_summary, tolerance = 1e-12)
  expect_equal(res_ref_resume$replicate_summary, res_ref$replicate_summary, tolerance = 1e-12)
  expect_equal(res_ref_resume$provenance$telemetry$candidates_evaluated, 41 * 5 - 20)

  # Real mid-replicate interruption & resume via deterministic checkpoint hook
  # 4 intra-ref + 1 ref-done + 4 intra-rep1 + 1 rep1-done + 2 intra-rep2 = 12th checkpoint
  options(ncvroc.cs_test_interrupt_after_ckpts = 12L) # interrupts inside rep 2 at cursor = 20
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3,
      bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
      work_dir = work_base, run_id = "test_real_interrupt",
      checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
      checkpoint_every = 25L, seed = 777L, progress = FALSE
    ),
    "Simulated mid-replicate interruption"
  )
  options(ncvroc.cs_test_interrupt_after_ckpts = NULL)

  # Verify the saved checkpoint actually has rep_cursor > 0 inside replicate 2
  int_res_file <- file.path(work_base, "runs", "test_real_interrupt", "checkpoints", "classification_stability_result.rds")
  int_loaded <- NCVROC:::.cs_load_rolling_slot(int_res_file, res_work$provenance$analysis_identity)
  expect_true(isTRUE(int_loaded$payload$ref_done))
  expect_equal(int_loaded$payload$completed_reps, 1L)
  expect_equal(int_loaded$payload$resume_rep_state$rep_id, 2L)
  expect_true(int_loaded$payload$resume_rep_state$rep_cursor > 0)
  skipped_cands <- 41 + 41 + int_loaded$payload$resume_rep_state$rep_cursor # ref (41) + rep 1 (41) + partial rep 2

  res_real_resume <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:3,
    bootstrap_reps = 4L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
    work_dir = work_base, run_id = "test_real_interrupt", resume = TRUE,
    checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
    checkpoint_every = 1L, seed = 777L, progress = FALSE
  )
  expect_equal(res_real_resume$subject_summary, res_ref$subject_summary, tolerance = 1e-12)
  expect_equal(res_real_resume$replicate_summary, res_ref$replicate_summary, tolerance = 1e-12)
  # Resumed run must NOT re-evaluate ref + already-completed replicate 1 + partial replicate 2 batches
  expect_equal(res_real_resume$provenance$telemetry$candidates_evaluated, 41 * 5 - skipped_cands)

  # Verify fixed 2-slot files only (slot0 and slot1)
  ckpt_dir <- res_work$provenance$work_layout$checkpoint_dir
  slot_files <- list.files(ckpt_dir, pattern = "\\.slot[01]\\.rds$", full.names = TRUE)
  expect_equal(length(slot_files), 2L)

  # Simulate mid-replicate interruption: modify the highest generation slot to point to rep 2 cursor 20
  res_file <- res_work$provenance$work_layout$result_file
  slots <- NCVROC:::.cs_slot_paths(res_file)
  s0 <- readRDS(slots[1])
  s1 <- readRDS(slots[2])
  newer_path <- if (s0$generation > s1$generation) slots[1] else slots[2]
  older_path <- if (s0$generation > s1$generation) slots[2] else slots[1]
  newer_env <- readRDS(newer_path)

  # Build a valid mid-replicate state at rep 2, cursor = 20 (partial batch)
  partial_payload <- list(
    completed_reps = 1L,
    valid_count = rep(1L, n),
    pos_count = as.integer(rowSums(as.matrix(df[, trimws(strsplit(res_ref$replicate_summary$selected_items[1], "\\+")[[1]])])) >= res_ref$replicate_summary$cutoff[1]),
    flip_count = as.integer((as.integer(rowSums(as.matrix(df[, trimws(strsplit(res_ref$replicate_summary$selected_items[1], "\\+")[[1]])])) >= res_ref$replicate_summary$cutoff[1])) != res_ref$subject_summary$ref_class),
    rep_rows = newer_env$payload$rep_rows[1],
    resume_rep_state = list(
      rep_id = 2L,
      rep_phase = "primary",
      rep_cursor = 0,
      rep_best_state = list(has_best = FALSE, n_feasible = 0)
    )
  )
  # Save partial state at generation + 1, and corrupt generation + 2 to test 1-slot recovery
  NCVROC:::.cs_save_rolling_slot(res_file, newer_env$generation + 1L, newer_env$analysis_identity, partial_payload)
  corrupt_slot <- slots[((newer_env$generation + 2L) %% 2L) + 1L]
  writeBin(as.raw(c(0x00, 0x01, 0x02)), corrupt_slot)

  res_recovered <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:3,
    bootstrap_reps = 4L, engine = "Rcpp", parallel = "none",
    work_dir = work_base, run_id = "test_run_01", resume = TRUE,
    checkpoint_every_candidates = 10L, candidate_batch_size = 5L,
    seed = 777L, progress = FALSE
  )
  expect_true(res_recovered$provenance$telemetry$recovered_from_previous_generation)
  expect_equal(res_recovered$subject_summary, res_ref$subject_summary, tolerance = 1e-12)
  expect_equal(res_recovered$replicate_summary, res_ref$replicate_summary, tolerance = 1e-12)

  # Corrupt BOTH slots -> must stop, never fall back to fresh calculation
  writeBin(as.raw(c(0x00, 0x01)), slots[1])
  writeBin(as.raw(c(0x00, 0x01)), slots[2])
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3,
      bootstrap_reps = 4L, engine = "Rcpp",
      work_dir = work_base, run_id = "test_run_01", resume = TRUE,
      seed = 777L, progress = FALSE
    ),
    "corrupted"
  )
})

test_that("v0.34.1 work_dir sync-root rejection, result_file conflict, migration check, and Drive publish work", {
  set.seed(34103)
  n <- 20L
  df <- data.frame(
    q1 = c(sample(0:2, 10, replace = TRUE), sample(1:2, 10, replace = TRUE)),
    q2 = sample(0:2, n, replace = TRUE),
    q3 = sample(0:2, n, replace = TRUE),
    y  = c(rep(0L, 10L), rep(1L, 10L))
  )
  tmp_root <- tempfile("ncvroc_pub_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

  fake_drive <- file.path(tmp_root, "GoogleDrive", "results_integrated_20261116")
  local_work <- file.path(tmp_root, "local_ssd_work")
  dir.create(fake_drive, recursive = TRUE)
  dir.create(local_work, recursive = TRUE)

  # 1. Sync-root work_dir must be rejected
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = c("q1", "q2", "q3"), model_sizes = 1:2,
      bootstrap_reps = 2L, engine = "Rcpp",
      work_dir = fake_drive, progress = FALSE
    ),
    "non-synced|cloud-synced"
  )

  # 2. Explicit result_file on sync root while work_dir is local must stop without silent rewriting
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = c("q1", "q2", "q3"), model_sizes = 1:2,
      bootstrap_reps = 2L, engine = "Rcpp",
      work_dir = local_work,
      result_file = file.path(fake_drive, "bad_result.rds"),
      progress = FALSE
    ),
    "cloud-synced"
  )

  # 3. Successful run + publish + re-read verification + no overwrite + no local auto-delete
  res <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 3L, engine = "Rcpp",
    work_dir = local_work, seed = 88L, progress = FALSE
  )
  run_dir <- res$provenance$work_layout$run_dir
  expect_true(dir.exists(run_dir))

  pub <- ncvroc_publish_classification_stability(res, publish_dir = fake_drive, prefix = "pilot_test")
  expect_equal(pub$verification_status, "verified")
  expect_true(file.exists(pub$files$rds$path))
  expect_true(dir.exists(run_dir)) # local work_dir must NOT be auto-deleted

  # Refusing overwrite when overwrite = FALSE
  expect_error(
    ncvroc_publish_classification_stability(res, publish_dir = fake_drive, prefix = "pilot_test", overwrite = FALSE),
    "Refusing to overwrite"
  )

  # Duplicate run_id on new run without resume=TRUE must stop
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = c("q1", "q2", "q3"), model_sizes = 1:2,
      bootstrap_reps = 3L, engine = "Rcpp",
      work_dir = local_work, run_id = res$provenance$work_layout$run_id,
      resume = FALSE, seed = 88L, progress = FALSE
    ),
    "already exists"
  )

  # Migration check: pointing result_file to a new filename for an existing run_id stops
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = c("q1", "q2", "q3"), model_sizes = 1:2,
      bootstrap_reps = 3L, engine = "Rcpp",
      work_dir = local_work, run_id = res$provenance$work_layout$run_id,
      result_file = file.path(res$provenance$work_layout$checkpoint_dir, "renamed_ckpt.rds"),
      resume = TRUE, seed = 88L, progress = FALSE
    ),
    "Previous checkpoint files exist"
  )

  # Migration check: pointing checkpoint_dir to an empty new folder for an existing run_id stops
  new_ckpt <- file.path(tmp_root, "new_ckpt_loc")
  dir.create(new_ckpt)
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = c("q1", "q2", "q3"), model_sizes = 1:2,
      bootstrap_reps = 3L, engine = "Rcpp",
      work_dir = local_work, run_id = res$provenance$work_layout$run_id,
      checkpoint_dir = new_ckpt, temp_dir = new_ckpt,
      resume = TRUE, seed = 88L, progress = FALSE
    ),
    "Previous checkpoint files exist"
  )
})
