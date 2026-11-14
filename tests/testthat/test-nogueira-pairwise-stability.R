test_that("feature_selection_stability matches Nogueira (2018) formula and boundary conditions", {
  elig <- c("x1", "x2", "x3", "x4", "x5")

  # 1. Identical sets -> Phi = 1
  sets_same <- list(c("x2", "x1"), c("x1", "x2"), c("x1", "x2"))
  res_same <- feature_selection_stability(sets_same, eligible_features = elig)
  expect_equal(res_same$status, "valid")
  expect_equal(res_same$stability_index, 1.0, tolerance = 1e-12)

  # 2. Independent hand calculation on varying-size sets
  sets_var <- list(c("x1", "x2"), c("x1", "x3"), c("x2", "x3", "x4"), c("x1"))
  res_var <- feature_selection_stability(sets_var, eligible_features = elig)
  z <- matrix(c(
    1, 1, 0, 0, 0,
    1, 0, 1, 0, 0,
    0, 1, 1, 1, 0,
    1, 0, 0, 0, 0
  ), nrow = 4, byrow = TRUE)
  M <- nrow(z)
  d <- ncol(z)
  p_hat <- colMeans(z)
  k_bar <- mean(rowSums(z))
  s2 <- (M / (M - 1)) * p_hat * (1 - p_hat)
  phi_manual <- 1 - mean(s2) / ((k_bar / d) * (1 - k_bar / d))
  expect_equal(res_var$stability_index, phi_manual, tolerance = 1e-12)

  # Feature order invariance
  res_rev <- feature_selection_stability(sets_var, eligible_features = rev(elig))
  expect_equal(res_rev$stability_index, res_var$stability_index, tolerance = 1e-12)

  # 3. Finite-sample negative estimate (unclipped, >= -1/(M-1))
  sets_neg <- list("x1", "x2", "x3", "x4", "x5")
  res_neg <- feature_selection_stability(sets_neg, eligible_features = elig)
  expect_equal(res_neg$status, "valid")
  expect_lt(res_neg$stability_index, 0)
  expect_gte(res_neg$stability_index, -1 / (5 - 1) - 1e-12)

  # 4. Degenerate / undefined boundaries
  expect_equal(feature_selection_stability(list("x1"), elig)$status, "undefined")
  expect_true(is.na(feature_selection_stability(list("x1"), elig)$stability_index))
  expect_equal(feature_selection_stability(list(character(0), character(0)), elig)$undefined_reason,
               "all_sets_empty_denominator_zero")
  expect_equal(feature_selection_stability(list(elig, elig), elig)$undefined_reason,
               "all_sets_full_denominator_zero")
  expect_equal(feature_selection_stability(list(character(0), character(0)), character(0))$undefined_reason,
               "empty_eligible_universe")

  # 5. Rejects duplicate or unknown features
  expect_error(feature_selection_stability(list(c("x1", "x1"), "x2"), elig), "Duplicate")
  expect_error(feature_selection_stability(list("x1", "x99"), elig), "Unknown")

  # 6. CI blocked on dependent resamples by default, allowed with assume_independent = TRUE
  expect_warning(
    res_ci_dep <- feature_selection_stability(sets_var, elig, ci = TRUE),
    "overlapping subjects"
  )
  expect_equal(res_ci_dep$ci_status, "not_applicable_dependent_resamples")
  expect_true(all(is.na(res_ci_dep$ci)))

  res_ci_ind <- feature_selection_stability(sets_var, elig, ci = TRUE, assume_independent = TRUE)
  expect_equal(res_ci_ind$ci_status, "valid_asymptotic_independent")
  expect_true(all(is.finite(res_ci_ind$ci)))
  expect_output(print(summary(res_ci_ind)), "Nogueira")
})

test_that("pairwise_selection_stability computes exact counts, symmetry, XOR, and handles M=0", {
  elig <- c("x1", "x2", "x3", "x4")
  sets <- list(c("x1", "x2"), c("x1", "x3"), c("x2", "x3"), c("x1", "x2"))
  pw <- pairwise_selection_stability(sets, eligible_features = elig)

  expect_equal(pw$status, "valid")
  expect_equal(pw$M, 4L)
  expect_equal(pw$both_count + pw$neither_count + pw$i_only_count + pw$j_only_count,
               matrix(4L, nrow = 4, ncol = 4, dimnames = list(elig, elig)))
  expect_true(isSymmetric(pw$pif_matrix))
  expect_true(isSymmetric(pw$xor_matrix))
  expect_equal(diag(pw$pif_matrix), pw$vif)
  expect_true(is.na(pw$conditional_substitution_matrix["x4", "x4"]))

  # M = 0 -> NA frequencies, not 0
  pw0 <- pairwise_selection_stability(list(character(0), character(0)), elig, valid_mask = c(FALSE, FALSE))
  expect_equal(pw0$status, "undefined_no_valid_sets")
  expect_true(all(is.na(pw0$vif)))
  expect_true(all(is.na(pw0$pif_matrix)))

  # Print, summary, plot
  expect_output(print(summary(pw)), "Pairwise Inclusion")
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf)
  on.exit(unlink(tf), add = TRUE)
  expect_silent(plot(pw, type = "pif", display_features = c("x1", "x2", "x3")))
  expect_silent(plot(pw, type = "xor"))
  expect_silent(plot(pw, type = "vif"))
  grDevices::dev.off()
})
