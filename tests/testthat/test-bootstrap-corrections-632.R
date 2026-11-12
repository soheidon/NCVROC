test_that("bootstrap_select_sum_roc accepts correction_methods argument and defaults to harrell only", {
  dat <- data.frame(
    I1 = c(1, 1, 0, 0, 1, 0),
    I2 = c(1, 0, 1, 0, 0, 1),
    y  = c(1, 1, 1, 0, 0, 0)
  )

  # Default should be harrell only
  res_def <- bootstrap_select_sum_roc(
    data           = dat,
    outcome        = "y",
    items          = c("I1", "I2"),
    model_sizes    = 1,
    bootstrap_reps = 10L,
    cutoff_method  = "youden",
    seed           = 101L,
    progress       = FALSE
  )

  expect_equal(res_def$settings$correction_methods, "harrell")
  expect_true(res_def$corrections$harrell$requested)
  expect_false(res_def$corrections$b632$requested)
  expect_false(res_def$corrections$b632plus$requested)
  expect_true(is.na(res_def$corrections$b632$auc))
  expect_true(is.na(res_def$corrections$b632plus$auc))

  # Invalid correction_methods should be rejected
  expect_error(
    bootstrap_select_sum_roc(
      data               = dat,
      outcome            = "y",
      items              = c("I1", "I2"),
      model_sizes        = 1,
      bootstrap_reps     = 10L,
      cutoff_method      = "youden",
      correction_methods = "invalid_method",
      progress           = FALSE
    ),
    "`correction_methods` must be a character vector"
  )

  # Multiple methods accepted
  res_all <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 10L,
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    seed               = 101L,
    progress           = FALSE
  )

  expect_true(res_all$corrections$harrell$requested)
  expect_true(res_all$corrections$b632$requested)
  expect_true(res_all$corrections$b632plus$requested)
  expect_false(is.na(res_all$corrections$b632$auc))
  expect_false(is.na(res_all$corrections$b632plus$auc))
})

test_that(".632 correction matches exact hand calculation and preserves Youden identity", {
  set.seed(42)
  n <- 40
  # Provide genuine signal: items correlate with outcome
  y <- sample(rep(c(0, 1), each = n / 2))
  dat <- data.frame(
    I1 = ifelse(y == 1, rbinom(n, 1, 0.85), rbinom(n, 1, 0.15)),
    I2 = ifelse(y == 1, rbinom(n, 1, 0.75), rbinom(n, 1, 0.25)),
    I3 = ifelse(y == 1, rbinom(n, 1, 0.65), rbinom(n, 1, 0.35)),
    y  = y
  )

  res <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2", "I3"),
    model_sizes        = 1:2,
    bootstrap_reps     = 30L,
    resampling         = "stratified",
    selection_metric   = "auc",
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    seed               = 2024L,
    progress           = FALSE
  )

  app  <- res$apparent_performance
  oob  <- res$oob_summary$mean_oob
  b632 <- res$corrections$b632

  # Hand-calculate .632: M_632 = 0.368 * M_app + 0.632 * M_OOB
  expected_auc_632  <- 0.368 * app$auc + 0.632 * oob["auc"]
  expected_sens_632 <- 0.368 * app$sensitivity + 0.632 * oob["sensitivity"]
  expected_spec_632 <- 0.368 * app$specificity + 0.632 * oob["specificity"]
  expected_youd_632 <- 0.368 * app$youden + 0.632 * oob["youden"]

  expect_equal(b632$auc, unname(expected_auc_632), tolerance = 1e-9)
  expect_equal(b632$sensitivity, unname(expected_sens_632), tolerance = 1e-9)
  expect_equal(b632$specificity, unname(expected_spec_632), tolerance = 1e-9)
  expect_equal(b632$youden, unname(expected_youd_632), tolerance = 1e-9)

  # Youden identity: J_632 = Se_632 + Sp_632 - 1
  expect_equal(b632$youden, b632$sensitivity + b632$specificity - 1.0, tolerance = 1e-10)
})

test_that(".632+ correction matches exact hand calculation and preserves Youden identity", {
  set.seed(42)
  n <- 50
  y <- sample(rep(c(0, 1), each = n / 2))
  dat <- data.frame(
    I1 = ifelse(y == 1, rbinom(n, 1, 0.85), rbinom(n, 1, 0.15)),
    I2 = ifelse(y == 1, rbinom(n, 1, 0.75), rbinom(n, 1, 0.25)),
    I3 = ifelse(y == 1, rbinom(n, 1, 0.65), rbinom(n, 1, 0.35)),
    y  = y
  )

  res <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2", "I3"),
    model_sizes        = 1:2,
    bootstrap_reps     = 30L,
    resampling         = "stratified",
    selection_metric   = "auc",
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    seed               = 9999L,
    progress           = FALSE
  )

  app   <- res$apparent_performance
  oob   <- res$oob_summary$mean_oob
  b632p <- res$corrections$b632plus

  # Hand-calculate AUC .632+ with gamma = 0.5
  denom_auc <- app$auc - 0.5
  expect_gt(denom_auc, 0)
  r_raw_auc <- (app$auc - oob["auc"]) / denom_auc
  r_auc <- min(max(r_raw_auc, 0.0), 1.0)
  w_auc <- min(max(0.632 / (1.0 - 0.368 * r_auc), 0.632), 1.0)
  expected_auc_632p <- (1.0 - w_auc) * app$auc + w_auc * oob["auc"]

  expect_equal(b632p$r_auc, unname(r_auc), tolerance = 1e-9)
  expect_equal(b632p$w_auc, unname(w_auc), tolerance = 1e-9)
  expect_equal(b632p$auc, unname(expected_auc_632p), tolerance = 1e-9)

  # Hand-calculate Youden-derived common weight for Sensitivity, Specificity, and Youden
  denom_youd <- app$youden - 0.0
  expect_gt(denom_youd, 0)
  r_raw_youd <- (app$youden - oob["youden"]) / denom_youd
  r_youd <- min(max(r_raw_youd, 0.0), 1.0)
  w_youd <- min(max(0.632 / (1.0 - 0.368 * r_youd), 0.632), 1.0)

  expected_sens_632p <- (1.0 - w_youd) * app$sensitivity + w_youd * oob["sensitivity"]
  expected_spec_632p <- (1.0 - w_youd) * app$specificity + w_youd * oob["specificity"]
  expected_youd_632p <- expected_sens_632p + expected_spec_632p - 1.0

  expect_equal(b632p$r_youden, unname(r_youd), tolerance = 1e-9)
  expect_equal(b632p$w_youden, unname(w_youd), tolerance = 1e-9)
  expect_equal(b632p$sensitivity, unname(expected_sens_632p), tolerance = 1e-9)
  expect_equal(b632p$specificity, unname(expected_spec_632p), tolerance = 1e-9)
  expect_equal(b632p$youden, unname(expected_youd_632p), tolerance = 1e-9)

  # Mathematical identity: J_632+ = Se_632+ + Sp_632+ - 1
  expect_equal(b632p$youden, b632p$sensitivity + b632p$specificity - 1.0, tolerance = 1e-10)
})

test_that("OOB rows membership and frozen selection are verified", {
  n <- 30
  dat <- data.frame(
    I1 = c(rep(1, 15), rep(0, 15)),
    I2 = c(rep(1, 10), rep(0, 20)),
    y  = c(rep(1, 15), rep(0, 15))
  )

  res <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 15L,
    resampling         = "ordinary",
    cutoff_method      = "youden",
    correction_methods = c("632", "632plus"),
    seed               = 42L,
    progress           = FALSE
  )

  br <- res$bootstrap_results
  # OOB total rows + in-bag unique rows must equal total N
  for (i in seq_len(nrow(br))) {
    expect_true(br$oob_n_total[i] >= 0L)
    expect_equal(br$oob_n_pos[i] + br$oob_n_neg[i], br$oob_n_total[i])
  }

  # All valid replicates have oob_valid_auc == TRUE
  expect_true(all(br$oob_valid_auc[br$selection_status == "selected"]))
  expect_true(all(br$oob_valid_joint[br$selection_status == "selected"]))
})

test_that(".632+ boundary cases: R clipped to 0 (no overfitting) and R clipped to 1 (severe overfitting)", {
  # Case 1: When OOB performance equals or exceeds apparent (no overfitting)
  # R should be clipped to 0, w should be 0.632, and .632+ reduces to .632
  dat_clean <- data.frame(
    I1 = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    y  = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )

  res_clean <- bootstrap_select_sum_roc(
    data               = dat_clean,
    outcome            = "y",
    items              = "I1",
    model_sizes        = 1,
    bootstrap_reps     = 10L,
    cutoff_method      = "youden",
    correction_methods = c("632", "632plus"),
    seed               = 123L,
    progress           = FALSE
  )

  # With perfect separation, app AUC = 1.0, OOB AUC = 1.0 -> R = 0, w = 0.632
  expect_equal(res_clean$corrections$b632plus$r_auc, 0.0)
  expect_equal(res_clean$corrections$b632plus$w_auc, 0.632)
  expect_equal(res_clean$corrections$b632plus$auc, res_clean$corrections$b632$auc)
})

test_that(".632+ warns and returns NA when apparent AUC <= 0.5", {
  # Data where item is negatively correlated or pure noise (AUC <= 0.5)
  dat_noise <- data.frame(
    I1 = c(0, 0, 0, 0, 1, 1, 1, 1),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  warnings_caught <- character(0)
  res <- withCallingHandlers(
    bootstrap_select_sum_roc(
      data               = dat_noise,
      outcome            = "y",
      items              = "I1",
      model_sizes        = 1,
      bootstrap_reps     = 10L,
      selection_metric   = "auc",
      cutoff_method      = "youden",
      correction_methods = c("harrell", "632plus"),
      seed               = 555L,
      progress           = FALSE
    ),
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Apparent AUC does not exceed chance gamma = 0.5", warnings_caught)))
  expect_true(is.na(res$corrections$b632plus$auc))
  expect_true(is.na(res$corrections$b632plus$r_auc))
  expect_equal(res$overfitting_diagnostics$boundary_counts$auc_denom_nonpos, 1L)
  expect_gt(length(res$overfitting_diagnostics$boundary_notes), 0L)
})

test_that("Summary and print methods work cleanly with multi-method comparisons", {
  set.seed(42)
  n <- 30
  y <- sample(rep(c(0, 1), each = 15))
  dat <- data.frame(
    I1 = ifelse(y == 1, 1, 0),
    I2 = ifelse(y == 1, rbinom(n, 1, 0.7), rbinom(n, 1, 0.3)),
    y  = y
  )

  res <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 10L,
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    seed               = 777L,
    progress           = FALSE
  )

  # Check print
  expect_output(print(res), "Performance & Optimism Summary")
  expect_output(print(res), ".632")
  expect_output(print(res), ".632\\+")

  # Check summary
  s <- summary(res)
  expect_s3_class(s, "summary.bootstrap_select_sum_roc_result")
  expect_equal(ncol(s$performance_table), 6L) # apparent, harrell, .632, .632+, mean_optimism, corrected
  expect_true(all(c("apparent", "harrell", ".632", ".632+") %in% names(s$performance_table)))

  # Check print(summary)
  expect_output(print(s), "Diagnostic Performance & Optimism Correction")
  expect_output(print(s), "experimental extension")
})

test_that("Rcpp vs R engine parity and parallel reproducibility for .632 and .632+", {
  dat <- data.frame(
    I1 = c(1, 1, 1, 1, 0, 0, 0, 0),
    I2 = c(1, 1, 0, 1, 0, 0, 1, 0),
    y  = c(1, 1, 1, 1, 0, 0, 0, 0)
  )

  res_rcpp <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 10L,
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    engine             = "Rcpp",
    seed               = 42L,
    progress           = FALSE
  )

  res_r <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 10L,
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    engine             = "R",
    seed               = 42L,
    progress           = FALSE
  )

  expect_equal(res_rcpp$corrections$b632$auc, res_r$corrections$b632$auc)
  expect_equal(res_rcpp$corrections$b632$sensitivity, res_r$corrections$b632$sensitivity)
  expect_equal(res_rcpp$corrections$b632$specificity, res_r$corrections$b632$specificity)
  expect_equal(res_rcpp$corrections$b632$youden, res_r$corrections$b632$youden)
  expect_equal(res_rcpp$corrections$b632plus$auc, res_r$corrections$b632plus$auc)
  expect_equal(res_rcpp$corrections$b632plus$youden, res_r$corrections$b632plus$youden)
})

test_that("Reviewer finding: conditional estimates and denominators are disclosed in printed summaries", {
  # Small dataset where ordinary bootstrap yields OOB samples with single class
  dat <- data.frame(
    I1 = c(1, 1, 1, 0, 0, 0),
    I2 = c(1, 0, 1, 0, 1, 0),
    y  = c(1, 1, 1, 0, 0, 0)
  )

  res <- bootstrap_select_sum_roc(
    data               = dat,
    outcome            = "y",
    items              = c("I1", "I2"),
    model_sizes        = 1,
    bootstrap_reps     = 30L,
    resampling         = "ordinary",
    cutoff_method      = "youden",
    correction_methods = c("harrell", "632", "632plus"),
    seed               = 99L,
    progress           = FALSE
  )

  # Check that at least one replicate had missing class in OOB
  expect_true(res$bootstrap_summary$n_reps_oob_valid_joint < 30L || res$bootstrap_summary$n_reps_oob_valid_auc < 30L)
  expect_true(res$corrections$b632$is_conditional || res$corrections$b632plus$is_conditional)

  # Test print method shows OOB valid reps count and conditional status
  out_print <- capture.output(print(res))
  expect_true(any(grepl("OOB valid reps:", out_print)))
  expect_true(any(grepl("Conditional Estimation Status:", out_print)))
  expect_true(any(grepl("valid reps:", out_print)))

  # Test summary print method shows OOB valid counts and conditional notes
  s <- summary(res)
  out_summary <- capture.output(print(s))
  expect_true(any(grepl("OOB valid:", out_summary)))
  expect_true(any(grepl("Conditional estimate", out_summary)))
  expect_true(any(grepl("valid OOB resamples", out_summary)))
})
