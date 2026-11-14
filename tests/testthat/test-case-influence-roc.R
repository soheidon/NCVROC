test_that("case_influence_roc evaluates LOO re-selection, target sets, and anonymization", {
  set.seed(505)
  dat <- data.frame(
    y  = rep(c(1, 0), each = 12L),
    x1 = c(rnorm(12, 1.5), rnorm(12, 0)),
    x2 = c(rnorm(12, 1.0), rnorm(12, 0)),
    x3 = rnorm(24),
    x4 = rnorm(24)
  )
  rownames(dat) <- paste0("PATIENT_SECRET_", seq_len(nrow(dat)))

  res_all <- case_influence_roc(
    data                = dat,
    outcome             = "y",
    selection_procedure = "apparent",
    case_subset         = "all",
    items               = c("x1", "x2", "x3", "x4"),
    model_sizes         = 1:2,
    engine              = "Rcpp",
    seed                = 11L,
    progress            = FALSE
  )
  expect_s3_class(res_all, "case_influence_roc_result")
  expect_equal(nrow(res_all$case_table), 24L)
  expect_true(res_all$anonymized)
  expect_false("case_label" %in% names(res_all$case_table))
  expect_false(any(grepl("PATIENT_SECRET", capture.output(print(res_all)))))

  # cutoff_diff is finite ONLY when exact_match is TRUE
  same_idx <- which(res_all$case_table$exact_match)
  diff_idx <- which(!res_all$case_table$exact_match)
  if (length(same_idx) > 0L) {
    expect_true(all(!is.na(res_all$case_table$cutoff_diff[same_idx])))
  }
  if (length(diff_idx) > 0L) {
    expect_true(all(is.na(res_all$case_table$cutoff_diff[diff_idx])))
  }

  # Target sets (a), (b), (c) are separated
  expect_true(all(c("auc_full_loo", "flip_rate_full", "auc_rem_loo", "flip_rate_rem",
                    "pred_s0_on_excluded", "pred_loo_on_excluded", "flipped_on_excluded") %in%
                    names(res_all$case_table)))
  expect_true(grepl("asymmetric", res_all$target_comparison_notes$target_excluded))

  # Empty-empty Jaccard is 1.0
  expect_equal(NCVROC:::.v033_compute_jaccard(character(0), character(0)), 1.0)
  expect_equal(NCVROC:::.v033_compute_jaccard(character(0), "x1"), 0.0)

  # S3 print / summary / plot
  expect_output(print(res_all), "Leave-One-Case-Out")
  s_all <- summary(res_all)
  expect_s3_class(s_all, "summary.case_influence_roc_result")
  expect_output(print(s_all), "Target Set Separation Notes")
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  on.exit(unlink(tf), add = TRUE)
  expect_silent(plot(res_all, metric = "jaccard"))
  grDevices::dev.off()
})

test_that("case_influence_roc supports deletion subsets, invalid states, and selection_result/stability integration", {
  set.seed(606)
  dat <- data.frame(
    y  = c(rep(1, 5), rep(0, 5)),
    x1 = c(rnorm(5, 2), rnorm(5, 0)),
    x2 = c(rnorm(5, 1.5), rnorm(5, 0)),
    x3 = rnorm(10)
  )

  # Deletion subsets: positive, negative, selected
  res_pos <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "positive",
    items = c("x1", "x2", "x3"), model_sizes = 1:2, engine = "R", progress = FALSE
  )
  expect_equal(res_pos$case_table$case_index, 1:5)

  res_neg <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "negative",
    items = c("x1", "x2", "x3"), model_sizes = 1:2, engine = "R", progress = FALSE
  )
  expect_equal(res_neg$case_table$case_index, 6:10)

  res_sel <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "selected",
    case_indices = c(2L, 8L), items = c("x1", "x2", "x3"), model_sizes = 1:2,
    engine = "R", progress = FALSE
  )
  expect_equal(res_sel$case_table$case_index, c(2L, 8L))

  # Invalid state: stratified folds = 5, n_pos = 5 -> removing a positive case leaves 4 < 5 folds!
  res_inv <- case_influence_roc(
    dat, "y", selection_procedure = "cv", case_subset = "selected",
    case_indices = c(1L, 6L), items = c("x1", "x2", "x3"), model_sizes = 1:2,
    folds = 5L, stratified = TRUE, seed = 10L, progress = FALSE
  )
  expect_equal(res_inv$case_table$status, c("invalid_insufficient_folds", "invalid_insufficient_folds"))
  expect_equal(res_inv$summary_stats$n_invalid, 2L)

  # Invalid single-class state: 1 positive case
  dat_1pos <- dat[5:10, ]
  res_1pos <- case_influence_roc(
    dat_1pos, "y", selection_procedure = "apparent", case_subset = "selected",
    case_indices = c(1L, 2L), items = c("x1", "x2"), model_sizes = 1L,
    engine = "R", progress = FALSE
  )
  expect_equal(res_1pos$case_table$status[1], "invalid_single_class")
  expect_equal(res_1pos$case_table$status[2], "valid")

  # Integration with selection_result and stability_result (v0.32)
  cv_ref <- cross_size_cv(
    dat, "y", c("x1", "x2", "x3"), model_sizes = 1:2,
    folds = 3L, repeats = 1L, seed = 55L, progress = FALSE
  )
  stab_ref <- classification_stability_roc(
    dat, "y", mode = "full_procedure", selection_result = cv_ref,
    bootstrap_reps = 10L, seed = 55L, progress = FALSE
  )
  res_cv <- case_influence_roc(
    dat, "y", selection_procedure = "cv", case_subset = "selected",
    case_indices = c(1L, 6L), selection_result = cv_ref, stability_result = stab_ref,
    folds = 3L, seed = 55L, progress = FALSE
  )
  expect_false(is.null(res_cv$stability_context))
  expect_true(grepl("separate", res_cv$stability_context$estimand_separation_note))
  expect_true(all(!is.na(res_cv$case_table$baseline_rank_in_loo)))
})

test_that("case_influence_roc checkpoint save/resume matches uninterrupted run", {
  set.seed(707)
  dat <- data.frame(
    y  = rep(c(1, 0), each = 8L),
    x1 = c(rnorm(8, 1.5), rnorm(8, 0)),
    x2 = rnorm(16),
    x3 = rnorm(16)
  )
  cp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cp_file), add = TRUE)

  res_full <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "selected",
    case_indices = 1:6, items = c("x1", "x2", "x3"), model_sizes = 1:2,
    seed = 21L, progress = FALSE
  )

  res_part <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "selected",
    case_indices = 1:6, items = c("x1", "x2", "x3"), model_sizes = 1:2,
    seed = 21L, result_file = cp_file, checkpoint_every = 3L, progress = FALSE
  )
  chk <- readRDS(cp_file)
  chk$completed_n <- 3L
  chk$row_list[4:6] <- list(NULL)
  saveRDS(chk, cp_file)

  res_resumed <- case_influence_roc(
    dat, "y", selection_procedure = "apparent", case_subset = "selected",
    case_indices = 1:6, items = c("x1", "x2", "x3"), model_sizes = 1:2,
    seed = 21L, result_file = cp_file, checkpoint_every = 2L, progress = FALSE
  )
  expect_equal(res_resumed$case_table, res_full$case_table)

  # Incompatible checkpoint triggers error when data or settings change
  dat_mod <- dat
  dat_mod$x1[1L] <- dat_mod$x1[1L] + 10.0
  expect_error(
    case_influence_roc(
      dat_mod, "y", selection_procedure = "apparent", case_subset = "selected",
      case_indices = 1:6, items = c("x1", "x2", "x3"), model_sizes = 1:2,
      seed = 21L, result_file = cp_file, progress = FALSE
    ),
    "does not match"
  )
  expect_error(
    case_influence_roc(
      dat, "y", selection_procedure = "apparent", case_subset = "selected",
      case_indices = 1:6, items = c("x1", "x2", "x3"), model_sizes = 1:2,
      engine = "R", seed = 21L, result_file = cp_file, progress = FALSE
    ),
    "does not match"
  )
})
