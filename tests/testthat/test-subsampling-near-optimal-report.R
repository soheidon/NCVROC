test_that("subsampling_stability enforces non-replacement, stratification, MSF truncation, and checkpointing", {
  set.seed(808)
  dat <- data.frame(
    y  = rep(c(1, 0), each = 12L),
    x1 = c(rnorm(12, 1.5), rnorm(12, 0)),
    x2 = c(rnorm(12, 1.0), rnorm(12, 0)),
    x3 = rnorm(24),
    x4 = rnorm(24)
  )

  cp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cp_file), add = TRUE)

  sub_res <- subsampling_stability(
    data                = dat,
    outcome             = "y",
    items               = c("x1", "x2", "x3", "x4"),
    model_sizes         = 1:2,
    selection_procedure = "apparent",
    B                   = 8L,
    sample_fraction     = 0.5,
    stratified          = TRUE,
    top_k_models        = 2L,
    seed                = 123L,
    result_file         = cp_file,
    checkpoint_every    = 4L,
    progress            = FALSE
  )
  expect_s3_class(sub_res, "subsampling_stability_result")
  expect_equal(sub_res$metadata$valid_iterations, 8L)
  # Every subsample has zero duplicates and exact m = 12
  expect_true(all(vapply(sub_res$resampling_indices, function(v) length(v) == 12L && !anyDuplicated(v), logical(1))))
  expect_equal(sum(sub_res$msf_table$count) <= 8L, TRUE)
  expect_equal(nrow(sub_res$msf_table) <= 2L, TRUE)

  # Resume from partial checkpoint matches bit-for-bit
  chk <- readRDS(cp_file)
  chk$completed_n <- 4L
  chk$selected_sets[5:8] <- list(NULL)
  saveRDS(chk, cp_file)
  sub_resumed <- subsampling_stability(
    data = dat, outcome = "y", items = c("x1", "x2", "x3", "x4"), model_sizes = 1:2,
    selection_procedure = "apparent", B = 8L, sample_fraction = 0.5, stratified = TRUE,
    top_k_models = 2L, seed = 123L, result_file = cp_file, checkpoint_every = 3L, progress = FALSE
  )
  expect_equal(sub_resumed$vif_table, sub_res$vif_table)
  expect_equal(sub_resumed$nogueira$stability_index, sub_res$nogueira$stability_index)

  # Stale checkpoint rejected when data changes
  dat_mod <- dat
  dat_mod$x1[1L] <- dat_mod$x1[1L] + 10
  expect_error(
    subsampling_stability(
      data = dat_mod, outcome = "y", items = c("x1", "x2", "x3", "x4"), model_sizes = 1:2,
      selection_procedure = "apparent", B = 8L, sample_fraction = 0.5, stratified = TRUE,
      seed = 123L, result_file = cp_file, progress = FALSE
    ),
    "does not match"
  )

  # Compare procedures on identical resampling indices
  sub_cv <- subsampling_stability(
    data = dat, outcome = "y", items = c("x1", "x2", "x3", "x4"), model_sizes = 1:2,
    selection_procedure = "cv", folds = 3L, resampling_indices = sub_res$resampling_indices,
    stratified = TRUE, seed = 123L, progress = FALSE
  )
  cmp <- compare_selection_stability(sub_res, sub_cv)
  expect_equal(cmp$comparability, "paired_identical_resamples")

  # Different fraction -> descriptive_only_mismatched_design
  sub_08 <- subsampling_stability(
    data = dat, outcome = "y", items = c("x1", "x2", "x3", "x4"), model_sizes = 1:2,
    selection_procedure = "apparent", B = 8L, sample_fraction = 0.8, stratified = TRUE,
    seed = 123L, progress = FALSE
  )
  cmp_diff <- compare_selection_stability(sub_res, sub_08)
  expect_equal(cmp_diff$comparability, "descriptive_only_mismatched_design")
  expect_true("different_sample_fraction" %in% cmp_diff$mismatch_reasons)
  expect_output(print(cmp_diff), "descriptive_only_mismatched_design")
})

test_that("near_optimal_candidates matches independent exhaustive enumeration in 2-pass streaming", {
  set.seed(909)
  dat <- data.frame(
    y  = rep(c(1, 0), each = 15L),
    x1 = c(rnorm(15, 1.4), rnorm(15, 0)),
    x2 = c(rnorm(15, 1.1), rnorm(15, 0)),
    x3 = rnorm(30),
    x4 = rnorm(30)
  )
  items <- c("x1", "x2", "x3", "x4")

  # Independent full-enumeration reference
  ex_ref <- exhaustive_sum_roc(dat, "y", items, min_items = 1, max_items = 3, progress = FALSE)
  t_max <- max(ex_ref$auc)
  delta <- 0.03
  in_zone_ref <- ex_ref[t_max - ex_ref$auc <= delta + 1e-12, , drop = FALSE]

  no_res <- near_optimal_candidates(
    data                       = dat,
    outcome                    = "y",
    items                      = items,
    model_sizes                = 1:3,
    evaluation_mode            = "apparent",
    metric                     = "auc",
    delta                      = delta,
    batch_size                 = 4L,
    max_stored_candidates      = 3L,
    max_pair_detail_candidates = 3L,
    engine                     = "Rcpp",
    progress                   = FALSE
  )
  expect_s3_class(no_res, "near_optimal_candidates_result")
  expect_equal(no_res$best_metric, t_max, tolerance = 1e-12)
  expect_equal(no_res$exact_near_optimal_count, nrow(in_zone_ref))
  expect_equal(no_res$stored_truncated, nrow(in_zone_ref) > 3L)
  expect_equal(sum(no_res$size_distribution$count), nrow(in_zone_ref))
  expect_equal(no_res$cost_estimate$passes, 2L)

  # delta = 0 yields exact ties only
  no_ties <- near_optimal_candidates(
    dat, "y", items = items, model_sizes = 1:3, delta = 0, batch_size = 5L, progress = FALSE
  )
  expect_equal(no_ties$exact_near_optimal_count, sum(abs(ex_ref$auc - t_max) <= 1e-12))

  # CV evaluation mode
  no_cv <- near_optimal_candidates(
    dat, "y", items = items, model_sizes = 1:2, evaluation_mode = "cv",
    folds = 3L, delta = 0.05, seed = 42L, batch_size = 4L, progress = FALSE
  )
  expect_equal(no_cv$status, "valid")
  expect_true(no_cv$exact_near_optimal_count >= 1L)

  # Common report separates targets and marks coefficient_stability not_applicable
  sub_r <- subsampling_stability(
    dat, "y", items = items, model_sizes = 1:2, selection_procedure = "apparent",
    B = 4L, sample_fraction = 0.5, seed = 11L, progress = FALSE
  )
  rep_obj <- model_selection_stability_report(subsampling = sub_r, near_optimal = no_res)
  expect_s3_class(rep_obj, "model_selection_stability_report")
  expect_equal(rep_obj$targets$coefficient_stability$status, "not_applicable")
  expect_output(print(rep_obj), "not_applicable")
})
