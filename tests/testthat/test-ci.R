# test-ci.R — Tests for Confidence Interval (CI) calculations

# ---- Clopper-Pearson Exact Binomial CI ----

test_that("compute_clopper_pearson_ci matches stats::binom.test", {
  test_cases <- list(
    list(k = 10, n = 10),  # 10/10 sensitivity
    list(k = 0, n = 10),   # 0/10
    list(k = 5, n = 10),   # 50%
    list(k = 1, n = 20),   # 5%
    list(k = 18, n = 20),  # 90%
    list(k = 37, n = 42)   # general case
  )

  for (tc in test_cases) {
    res <- compute_clopper_pearson_ci(tc$k, tc$n, conf_level = 0.95)
    expected <- stats::binom.test(tc$k, tc$n, conf.level = 0.95)$conf.int

    expect_equal(res$lower, expected[1], tolerance = 1e-6)
    expect_equal(res$upper, expected[2], tolerance = 1e-6)
  }
})

test_that("compute_clopper_pearson_ci handles 10/10 correctly", {
  res <- compute_clopper_pearson_ci(10, 10, conf_level = 0.95)
  expect_equal(res$upper, 1.0)
  expect_equal(res$lower, 0.025^(1 / 10), tolerance = 1e-6)
  expect_true(res$lower > 0.69 && res$lower < 0.70)
})

test_that("compute_clopper_pearson_ci handles vectorized inputs", {
  k <- c(0, 5, 10)
  n <- c(10, 10, 10)
  res <- compute_clopper_pearson_ci(k, n, conf_level = 0.95)

  expect_equal(nrow(res), 3)
  expect_equal(res$lower[1], 0.0)
  expect_equal(res$upper[3], 1.0)
})

test_that("compute_clopper_pearson_ci handles different conf_level", {
  res_90 <- compute_clopper_pearson_ci(5, 10, conf_level = 0.90)
  res_99 <- compute_clopper_pearson_ci(5, 10, conf_level = 0.99)
  expected_90 <- stats::binom.test(5, 10, conf.level = 0.90)$conf.int
  expected_99 <- stats::binom.test(5, 10, conf.level = 0.99)$conf.int

  expect_equal(res_90$lower, expected_90[1], tolerance = 1e-6)
  expect_equal(res_90$upper, expected_90[2], tolerance = 1e-6)
  expect_equal(res_99$lower, expected_99[1], tolerance = 1e-6)
  expect_equal(res_99$upper, expected_99[2], tolerance = 1e-6)
})

test_that("compute_clopper_pearson_ci validates conf_level", {
  expect_error(compute_clopper_pearson_ci(5, 10, conf_level = 0), "conf_level")
  expect_error(compute_clopper_pearson_ci(5, 10, conf_level = 1), "conf_level")
  expect_error(compute_clopper_pearson_ci(5, 10, conf_level = "0.95"), "conf_level")
})

# ---- DeLong AUC CI ----

test_that("compute_delong_auc_ci gives exact bounds for perfect separation", {
  # Positives: all 5, Negatives: all 1
  pos <- setNames(c(10L), "5")
  neg <- setNames(c(10L), "1")
  ci <- compute_delong_auc_ci(pos, neg, conf_level = 0.95)

  expect_equal(ci$auc, 1.0)
  expect_equal(ci$auc_lower, 1.0)
  expect_equal(ci$auc_upper, 1.0)
  expect_equal(ci$se, 0.0)
})

test_that("compute_delong_auc_ci gives symmetric bounds for identical distributions", {
  # Positives: 10 at 2, Negatives: 10 at 2
  pos <- setNames(c(10L), "2")
  neg <- setNames(c(10L), "2")
  ci <- compute_delong_auc_ci(pos, neg, conf_level = 0.95)

  expect_equal(ci$auc, 0.5)
  expect_equal(ci$se, 0.0)
  expect_equal(ci$auc_lower, 0.5)
  expect_equal(ci$auc_upper, 0.5)
})

test_that("compute_delong_auc_ci handles tied and untied score distributions", {
  scores_pos <- c(1, 2, 3, 4, 5, 5, 6, 7, 8, 9)
  scores_neg <- c(0, 1, 2, 2, 3, 4, 4, 5, 6, 7)

  all_scores <- sort(unique(c(scores_pos, scores_neg)))
  pos_counts <- setNames(
    vapply(all_scores, function(s) sum(scores_pos == s), integer(1)),
    as.character(all_scores)
  )
  neg_counts <- setNames(
    vapply(all_scores, function(s) sum(scores_neg == s), integer(1)),
    as.character(all_scores)
  )

  ci <- compute_delong_auc_ci(pos_counts, neg_counts, conf_level = 0.95)

  expect_true(!is.na(ci$auc))
  expect_true(!is.na(ci$auc_lower))
  expect_true(!is.na(ci$auc_upper))
  expect_true(ci$auc_lower >= 0 && ci$auc_lower <= ci$auc)
  expect_true(ci$auc_upper >= ci$auc && ci$auc_upper <= 1)
  expect_true(ci$se > 0)
})

test_that("compute_delong_auc_ci matches pROC::ci.auc when pROC is available", {
  skip_if_not_installed("pROC")

  set.seed(123)
  y <- sample(0:1, 50, replace = TRUE)
  x <- rnorm(50) + y * 0.8
  # discretize to simulate integer sum scores
  x_disc <- round(x * 2)

  freq <- compute_score_frequencies(x_disc, y)
  delong_ncvroc <- compute_delong_auc_ci(freq$pos_counts, freq$neg_counts, conf_level = 0.95)

  proc_res <- pROC::ci.auc(pROC::roc(y, x_disc, direction = "<", quiet = TRUE), method = "delong")

  expect_equal(delong_ncvroc$auc, as.numeric(proc_res[2]), tolerance = 1e-4)
  expect_equal(delong_ncvroc$auc_lower, as.numeric(proc_res[1]), tolerance = 1e-4)
  expect_equal(delong_ncvroc$auc_upper, as.numeric(proc_res[3]), tolerance = 1e-4)
})

test_that("compute_delong_auc_ci handles severe class imbalance", {
  scores_pos <- c(3, 4)
  scores_neg <- rep(1:3, each = 20)

  all_scores <- sort(unique(c(scores_pos, scores_neg)))
  pos_counts <- setNames(
    vapply(all_scores, function(s) sum(scores_pos == s), integer(1)),
    as.character(all_scores)
  )
  neg_counts <- setNames(
    vapply(all_scores, function(s) sum(scores_neg == s), integer(1)),
    as.character(all_scores)
  )

  ci <- compute_delong_auc_ci(pos_counts, neg_counts, conf_level = 0.95)
  expect_true(!is.na(ci$auc))
  expect_true(!is.na(ci$se))
  expect_true(ci$auc_lower >= 0 && ci$auc_upper <= 1)
})

# ---- Integration in fit_final_sum_scale and exhaustive_sum_roc ----

test_that("fit_final_sum_scale includes CI columns by default", {
  d <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1)
  )

  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2, progress = FALSE)

  expected_ci_cols <- c(
    "auc_lower", "auc_upper",
    "sensitivity_lower", "sensitivity_upper",
    "specificity_lower", "specificity_upper",
    "accuracy_lower", "accuracy_upper",
    "ppv_lower", "ppv_upper",
    "npv_lower", "npv_upper"
  )

  for (col in expected_ci_cols) {
    expect_true(col %in% names(res), info = paste("Missing CI col:", col))
    expect_true(all(!is.na(res[[col]])))
    expect_true(all(res[[col]] >= 0 & res[[col]] <= 1))
  }

  # Check ordering lower <= estimate <= upper
  expect_true(all(res$sensitivity_lower <= res$sensitivity + 1e-8))
  expect_true(all(res$sensitivity <= res$sensitivity_upper + 1e-8))
  expect_true(all(res$specificity_lower <= res$specificity + 1e-8))
  expect_true(all(res$specificity <= res$specificity_upper + 1e-8))
})

test_that("fit_final_sum_scale respects ci = FALSE", {
  d <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1)
  )

  res <- fit_final_sum_scale(d, "y", c("q1", "q2", "q3"), max_items = 2, ci = FALSE, progress = FALSE)
  expect_false("auc_lower" %in% names(res))
  expect_false("sensitivity_lower" %in% names(res))
})

test_that("exhaustive_sum_roc ci = TRUE adds CIs without breaking ranking", {
  d <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1)
  )

  res_noci <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2, ci = FALSE, progress = FALSE)
  res_ci   <- exhaustive_sum_roc(d, "y", c("q1", "q2", "q3"), max_items = 2, ci = TRUE, progress = FALSE)

  expect_equal(res_noci$items, res_ci$items)
  expect_equal(res_noci$auc, res_ci$auc)
  expect_true("auc_lower" %in% names(res_ci))
  expect_false("auc_lower" %in% names(res_noci))
})

test_that("ncvroc final_model and final_candidates include CIs", {
  d <- data.frame(
    y  = c(1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0),
    q1 = c(2, 1, 2, 1, 0, 1, 2, 2, 0, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0, 0),
    q2 = c(1, 2, 1, 0, 1, 0, 2, 1, 1, 0, 1, 2, 0, 1, 0, 2, 1, 1, 0, 0),
    q3 = c(2, 2, 1, 1, 0, 1, 2, 2, 0, 1, 2, 2, 0, 0, 1, 1, 0, 2, 1, 0)
  )

  res <- ncvroc(d, y, q1:q3, max_items = 2, mode = "quick",
                outer_k = 2, inner_k = 2, outer_repeats = 1, engine = "R",
                seed = 42, final_search = TRUE, ci = TRUE, progress = FALSE, verbose = FALSE)

  expect_true("auc_lower" %in% names(res$final_model))
  expect_true("sensitivity_lower" %in% names(res$final_model))
  expect_true("specificity_lower" %in% names(res$final_model))

  expect_true("auc_lower" %in% names(res$final_candidates))
  expect_true("sensitivity_lower" %in% names(res$final_candidates))
})
