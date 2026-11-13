test_that("Fixed-candidate cutoff stability & CII match independent reference, zero case, and cutoff-only flips", {
  set.seed(601)
  n <- 24L
  df <- data.frame(
    q1 = c(rnorm(12, 0), rnorm(12, 2.0)),
    q2 = c(rnorm(12, 0), rnorm(12, 1.5)),
    q3 = rnorm(n),
    y  = c(rep(0L, 12L), rep(1L, 12L))
  )

  # 1. Zero case: identical sample in every bootstrap replicate => CII == 0 for all subjects
  ident_idx <- replicate(5L, seq_len(n), simplify = FALSE)
  res_zero <- cutoff_stability_roc(
    data = df, outcome = "y", candidate_set = c("q1", "q2"),
    bootstrap_reps = 5L, bootstrap_indices = ident_idx,
    cutoff_method = "youden", progress = FALSE
  )
  expect_s3_class(res_zero, "classification_stability_result")
  expect_equal(res_zero$cutoff_summary$n_valid, 5L)
  expect_equal(res_zero$cutoff_summary$failure_rate, 0)
  expect_true(all(res_zero$subject_summary$cii == 0))
  expect_true(all(res_zero$subject_summary$valid_replicates == 5L))

  # 2. Cutoff-only variation: match independent manual calculation
  seed_before <- .Random.seed
  res_fix <- cutoff_stability_roc(
    data = df, outcome = "y", candidate_set = c("q1", "q2"),
    resampling = "ordinary", bootstrap_reps = 10L,
    cutoff_method = "youden", seed = 42L, progress = FALSE
  )
  # .Random.seed must be preserved when seed is supplied
  expect_identical(.Random.seed, seed_before)
  expect_equal(res_fix$candidate_universe$scope, "fixed_candidate")
  expect_equal(res_fix$score_instability$status, "applicable_invariant")
  expect_equal(res_fix$subject_summary$subject_index, seq_len(n))

  # Independent reference check using the same seed
  boot_ref <- NCVROC:::.cs_generate_bootstrap_indices(df$y, n, 10L, "ordinary", 42L)
  x_mat <- as.matrix(df[, c("q1", "q2")])
  ref_scores <- rowSums(x_mat)
  ref_class <- as.integer(ref_scores >= res_fix$reference_model$cutoff)
  manual_flips <- integer(n)
  manual_pos <- integer(n)
  for (b in 1:10) {
    idx <- boot_ref[[b]]
    sc_b <- rowSums(x_mat[idx, , drop = FALSE])
    fr <- NCVROC:::compute_score_frequencies(sc_b, df$y[idx])
    rm <- NCVROC:::compute_roc_metrics_from_table(fr$pos_counts, fr$neg_counts)
    cut_b <- NCVROC:::select_operating_point(rm, "youden")$cutoff
    expect_equal(res_fix$replicate_summary$cutoff[b], cut_b, tolerance = 1e-12)
    pred_b <- as.integer(ref_scores >= cut_b)
    manual_pos <- manual_pos + pred_b
    manual_flips <- manual_flips + as.integer(pred_b != ref_class)
  }
  expect_equal(res_fix$subject_summary$flip_count, manual_flips)
  expect_equal(res_fix$subject_summary$positive_count, manual_pos)
  expect_equal(res_fix$subject_summary$cii, manual_flips / 10, tolerance = 1e-12)
})

test_that("Clinical constraints, all-fail NA CII, single-class undefined_score, and stratified vs ordinary bootstrap work", {
  set.seed(602)
  n <- 20L
  df <- data.frame(
    q1 = rnorm(n),
    q2 = rnorm(n),
    y  = c(rep(0L, 10L), rep(1L, 10L))
  )

  # 1. Impossible clinical constraint -> all replicates infeasible_selection, CII must be NA (NOT 0!)
  res_all_fail <- cutoff_stability_roc(
    data = df, outcome = "y", candidate_set = c("q1", "q2"),
    bootstrap_reps = 6L, cutoff_method = "clinical_constraint",
    sensitivity_min = 1.0, specificity_min = 1.0,
    no_feasible = "record_failure", seed = 10L, progress = FALSE
  )
  expect_equal(res_all_fail$cutoff_summary$n_valid, 0L)
  expect_equal(res_all_fail$cutoff_summary$failure_rate, 1.0)
  expect_true(all(is.na(res_all_fail$subject_summary$cii)))
  expect_true(is.na(res_all_fail$cii_summary$mean))

  # 2. Fallback youden records infeasible_selection with fallback_used = TRUE
  res_fb <- cutoff_stability_roc(
    data = df, outcome = "y", candidate_set = c("q1", "q2"),
    bootstrap_reps = 6L, cutoff_method = "clinical_constraint",
    sensitivity_min = 1.0, specificity_min = 1.0,
    no_feasible = "fallback_youden", seed = 10L, progress = FALSE
  )
  expect_equal(res_fb$cutoff_summary$n_fallback, 6L)
  expect_equal(res_fb$cutoff_summary$n_valid, 6L)

  # 3. Single-class bootstrap replicate -> undefined_score
  single_class_idx <- list(rep(1L, n), seq_len(n))
  res_sc <- cutoff_stability_roc(
    data = df, outcome = "y", candidate_set = c("q1", "q2"),
    bootstrap_reps = 2L, bootstrap_indices = single_class_idx,
    cutoff_method = "youden", progress = FALSE
  )
  expect_equal(res_sc$replicate_summary$status[1L], "undefined_score")
  expect_equal(res_sc$replicate_summary$status[2L], "valid")
  expect_true(all(res_sc$subject_summary$valid_replicates == 1L))

  # 4. Stratified bootstrap preserves class counts in every replicate
  boot_strat <- NCVROC:::.cs_generate_bootstrap_indices(df$y, n, 5L, "stratified", 77L)
  expect_true(all(vapply(boot_strat, function(idx) sum(df$y[idx] == 1L) == 10L, logical(1))))
})

test_that("Full-procedure R vs Rcpp parity, thread invariance, seed reproducibility, and resource guard", {
  set.seed(603)
  n <- 30L
  df <- data.frame(
    q1 = c(sample(0:2, 15, replace = TRUE), sample(2:4, 15, replace = TRUE)),
    q2 = c(sample(0:2, 15, replace = TRUE), sample(2:4, 15, replace = TRUE)),
    q3 = sample(0:4, n, replace = TRUE),
    q4 = sample(0:4, n, replace = TRUE),
    y  = c(rep(0L, 15L), rep(1L, 15L))
  )
  items <- paste0("q", 1:4)

  res_r <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:2,
    resampling = "stratified", bootstrap_reps = 8L,
    selection_metric = "auc", cutoff_method = "youden",
    engine = "R", parallel = "none", seed = 123L, progress = FALSE
  )
  res_cpp_1 <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:2,
    resampling = "stratified", bootstrap_reps = 8L,
    selection_metric = "auc", cutoff_method = "youden",
    engine = "Rcpp", parallel = "none", seed = 123L, progress = FALSE
  )
  res_cpp_2 <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:2,
    resampling = "stratified", bootstrap_reps = 8L,
    selection_metric = "auc", cutoff_method = "youden",
    engine = "Rcpp", parallel = "threads", n_workers = 2L, seed = 123L, progress = FALSE
  )
  res_diff_seed <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = items, model_sizes = 1:2,
    resampling = "stratified", bootstrap_reps = 8L,
    selection_metric = "auc", cutoff_method = "youden",
    engine = "Rcpp", parallel = "none", seed = 124L, progress = FALSE
  )

  expect_identical(res_r$provenance$bootstrap_indices_digest, res_cpp_1$provenance$bootstrap_indices_digest)
  expect_identical(res_cpp_1$provenance$bootstrap_indices_digest, res_cpp_2$provenance$bootstrap_indices_digest)
  expect_false(identical(res_cpp_1$provenance$bootstrap_indices_digest, res_diff_seed$provenance$bootstrap_indices_digest))

  expect_equal(res_r$subject_summary, res_cpp_1$subject_summary, tolerance = 1e-12)
  expect_equal(res_cpp_1$subject_summary, res_cpp_2$subject_summary, tolerance = 1e-12)
  expect_equal(res_r$replicate_summary$cutoff, res_cpp_1$replicate_summary$cutoff, tolerance = 1e-12)
  expect_equal(res_r$score_instability$status, "not_comparable")

  # Resource guard: max_candidates exceeded must stop without silent pruning
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = items, model_sizes = 1:3, max_candidates = 5L,
      bootstrap_reps = 2L, progress = FALSE
    ),
    "max_candidates"
  )

  # Unreconstructable selection_result must stop cleanly
  expect_error(
    classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      selection_result = structure(list(), class = "ncvroc_result"),
      bootstrap_reps = 2L, progress = FALSE
    ),
    "not_reconstructable"
  )
})

test_that("Mode comparison, plots (including empty), and checkpoint/resume parity work", {
  set.seed(604)
  n <- 24L
  df <- data.frame(
    q1 = c(rnorm(12, 0), rnorm(12, 1.5)),
    q2 = c(rnorm(12, 0), rnorm(12, 1.0)),
    q3 = rnorm(n),
    y  = c(rep(0L, 12L), rep(1L, 12L))
  )

  r_fix <- classification_stability_roc(
    data = df, outcome = "y", mode = "fixed_candidate",
    candidate_set = c("q1", "q2"), bootstrap_reps = 6L,
    seed = 55L, progress = FALSE
  )
  r_full <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, seed = 55L, progress = FALSE
  )
  r_full_diff <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, seed = 56L, progress = FALSE
  )

  cmp_ok <- compare_classification_stability(r_fix, r_full)
  expect_true(cmp_ok$comparable)
  expect_equal(cmp_ok$n_common_valid, 6L)

  cmp_bad <- compare_classification_stability(r_fix, r_full_diff)
  expect_false(cmp_bad$comparable)
  expect_equal(cmp_bad$status, "incompatible_resampling_indices")

  # Differing failed replicates: refuses without `data`, recomputes on common valid set with `data`
  r_fix_part <- r_fix
  r_fix_part$replicate_summary$cutoff[1L] <- NA_real_
  r_fix_part$replicate_summary$status[1L] <- "infeasible_selection"
  cmp_need_data <- compare_classification_stability(r_fix_part, r_full)
  expect_false(cmp_need_data$comparable)
  expect_equal(cmp_need_data$status, "common_valid_recomputation_requires_data")
  cmp_with_data <- compare_classification_stability(r_fix_part, r_full, data = df)
  expect_true(cmp_with_data$comparable)
  expect_equal(cmp_with_data$n_common_valid, 5L)
  # Verify common_fixed_cii matches manual recomputation over replicates 2:6
  manual_flips_26 <- integer(n)
  for (b in 2:6) {
    pr <- as.integer(rowSums(as.matrix(df[, c("q1", "q2")])) >= r_fix_part$replicate_summary$cutoff[b])
    manual_flips_26 <- manual_flips_26 + as.integer(pr != r_fix_part$subject_summary$ref_class)
  }
  expect_equal(cmp_with_data$common_fixed_cii, manual_flips_26 / 5, tolerance = 1e-12)
  expect_equal(cmp_with_data$mean_cii_diff, mean(cmp_with_data$common_full_cii - cmp_with_data$common_fixed_cii), tolerance = 1e-12)

  # Different predictors with identical outcome and bootstrap indices -> refused
  df_mod <- df
  df_mod$q1 <- df_mod$q1 + 1.0
  r_full_mod <- classification_stability_roc(
    data = df_mod, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, seed = 55L, progress = FALSE
  )
  cmp_mod <- compare_classification_stability(r_fix, r_full_mod)
  expect_false(cmp_mod$comparable)
  expect_equal(cmp_mod$status, "incompatible_original_data")

  # Missing provenance on both results -> refused (never identical(NULL, NULL))
  r_fix_noprov <- r_fix
  r_full_noprov <- r_full
  r_fix_noprov$provenance$full_data_digest <- NULL
  r_full_noprov$provenance$full_data_digest <- NULL
  cmp_noprov <- compare_classification_stability(r_fix_noprov, r_full_noprov)
  expect_false(cmp_noprov$comparable)
  expect_equal(cmp_noprov$status, "unverifiable_provenance")

  # Malformed digest on both results -> refused
  r_fix_bad <- r_fix
  r_full_bad <- r_full
  r_fix_bad$provenance$full_data_digest <- "invalid"
  r_full_bad$provenance$full_data_digest <- "invalid"
  cmp_bad_digest <- compare_classification_stability(r_fix_bad, r_full_bad)
  expect_false(cmp_bad_digest$comparable)
  expect_equal(cmp_bad_digest$status, "unverifiable_provenance")

  # Plots render cleanly to null pdf device & reject mismatched mode_comparison
  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(r_fix, type = "cutoff"))
  expect_invisible(plot(r_fix, type = "instability"))
  expect_invisible(plot(r_fix, type = "frequency"))
  expect_invisible(plot(r_fix, type = "mode_comparison", compare_to = r_full))
  expect_error(plot(r_fix, type = "mode_comparison", compare_to = r_full_diff), "refused")

  # Checkpoint / resume parity
  chk_rds <- tempfile(fileext = ".rds")
  r_uninterrupted <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, checkpoint_every = 2L, seed = 55L, progress = FALSE
  )
  # Simulate partial checkpoint with first 2 replicates completed
  r_full_chk <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, result_file = chk_rds, checkpoint_every = 2L,
    seed = 55L, progress = FALSE
  )
  saved <- readRDS(chk_rds)
  saved$completed_reps <- 2L
  saved$rep_rows <- saved$rep_rows[1:2]
  # Recompute counters for first 2 reps
  x_m <- as.matrix(df[, c("q1", "q2", "q3")])
  ref_c <- r_uninterrupted$subject_summary$ref_class
  vc <- integer(n); pc <- integer(n); fc <- integer(n)
  for (b in 1:2) {
    it <- trimws(strsplit(saved$rep_rows[[b]]$selected_items, "\\+")[[1L]])
    pr <- as.integer(rowSums(x_m[, it, drop = FALSE]) >= saved$rep_rows[[b]]$cutoff)
    vc <- vc + 1L; pc <- pc + pr; fc <- fc + as.integer(pr != ref_c)
  }
  saved$valid_count <- vc; saved$pos_count <- pc; saved$flip_count <- fc
  saved$final_result <- NULL
  saveRDS(saved, chk_rds)

  r_resumed <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = c("q1", "q2", "q3"), model_sizes = 1:2,
    bootstrap_reps = 6L, result_file = chk_rds, checkpoint_every = 2L,
    seed = 55L, progress = FALSE
  )
  expect_equal(r_resumed$subject_summary, r_uninterrupted$subject_summary, tolerance = 1e-12)
  expect_equal(r_resumed$replicate_summary, r_uninterrupted$replicate_summary, tolerance = 1e-12)
})
