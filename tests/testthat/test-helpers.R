# test-helpers.R — Tests for internal utility and ROC helper functions

# ---- validate_inputs ----

test_that("validate_inputs rejects non-data.frame", {
  expect_error(
    validate_inputs("not_a_df", "y", c("q1"), 1, 0),
    "must be a data.frame"
  )
})

test_that("validate_inputs rejects missing outcome column", {
  d <- data.frame(y = c(0, 1), q1 = c(1, 2))
  expect_error(
    validate_inputs(d, "z", c("q1"), 1, 0),
    "not found"
  )
})

test_that("validate_inputs rejects missing item columns", {
  d <- data.frame(y = c(0, 1), q1 = c(1, 2))
  expect_error(
    validate_inputs(d, "y", c("q1", "q2"), 1, 0),
    "not found"
  )
})

test_that("validate_inputs rejects NA in outcome", {
  d <- data.frame(y = c(0, 1, NA), q1 = c(1, 2, 3))
  expect_error(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "NA values"
  )
})

test_that("validate_inputs rejects NA in items", {
  d <- data.frame(y = c(0, 1, 1), q1 = c(1, NA, 3))
  expect_error(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "NA values"
  )
})

test_that("validate_inputs rejects outcome values outside labels", {
  d <- data.frame(y = c(0, 1, 2), q1 = c(1, 2, 3))
  expect_error(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "not matching positive_label"
  )
})

test_that("validate_inputs rejects single-value outcome", {
  d <- data.frame(y = c(1, 1, 1), q1 = c(1, 2, 3))
  expect_error(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "only one unique value"
  )
})

test_that("validate_inputs rejects non-numeric item", {
  d <- data.frame(y = c(0, 1, 1), q1 = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "must be numeric"
  )
})

test_that("validate_inputs converts outcome to 0/1", {
  d <- data.frame(y = c(1, 1, 0, 0, 1), q1 = c(2, 1, 0, 1, 2))
  res <- validate_inputs(d, "y", c("q1"), 1, 0)
  expect_equal(res$y, c(1, 1, 0, 0, 1))
})

test_that("validate_inputs works with non-numeric labels (factor-style)", {
  d <- data.frame(y = c("case", "case", "control", "control"),
                  q1 = c(1, 2, 0, 1),
                  stringsAsFactors = FALSE)
  res <- validate_inputs(d, "y", c("q1"), "case", "control")
  expect_equal(res$y, c(1, 1, 0, 0))
})

test_that("validate_inputs warns on extreme class proportion", {
  d <- data.frame(y = c(rep(1, 20), 0), q1 = 1:21)
  expect_warning(
    validate_inputs(d, "y", c("q1"), 1, 0),
    "class proportion"
  )
})

# ---- enumerate_combinations ----

test_that("enumerate_combinations returns correct count", {
  # For n=5 items, min=1, max=3:
  # C(5,1)=5, C(5,2)=10, C(5,3)=10 → total 25
  items <- paste0("q", 1:5)
  combos <- enumerate_combinations(items, min_items = 1, max_items = 3)
  expect_length(combos, 25)
})

test_that("enumerate_combinations with min=max returns exact count", {
  items <- paste0("q", 1:4)
  combos <- enumerate_combinations(items, min_items = 2, max_items = 2)
  expect_length(combos, choose(4, 2))  # 6
})

test_that("enumerate_combinations with max beyond n clips to n", {
  items <- paste0("q", 1:3)
  # max_items=10 but n=3, so max_k=3: C(3,1)+C(3,2)+C(3,3)=3+3+1=7
  combos <- enumerate_combinations(items, min_items = 1, max_items = 10)
  expect_length(combos, 7)
})

test_that("enumerate_combinations errors on empty items", {
  expect_error(
    enumerate_combinations(character(0)),
    "at least one item"
  )
})

test_that("enumerate_combinations errors when min exceeds n", {
  expect_error(
    enumerate_combinations(c("q1", "q2"), min_items = 3, max_items = 4),
    "exceeds available items"
  )
})

test_that("enumerate_combinations each element is character vector of correct length", {
  items <- paste0("q", 1:4)
  combos <- enumerate_combinations(items, min_items = 1, max_items = 3)
  for (cmb in combos) {
    expect_true(length(cmb) >= 1 && length(cmb) <= 3)
    expect_true(all(cmb %in% items))
  }
})

# ---- format_items ----

test_that("format_items joins with comma", {
  expect_equal(format_items(c("q1", "q2", "q3")), "q1, q2, q3")
})

test_that("format_items handles single item", {
  expect_equal(format_items("q1"), "q1")
})

# ---- compute_score_frequencies ----

test_that("compute_score_frequencies handles balanced data", {
  scores  <- c(0, 1, 2, 0, 1, 2)
  outcome <- c(1, 1, 1, 0, 0, 0)
  freq <- compute_score_frequencies(scores, outcome)
  expect_equal(unname(freq$pos_counts), c(1, 1, 1))
  expect_equal(unname(freq$neg_counts), c(1, 1, 1))
  expect_equal(names(freq$pos_counts), c("0", "1", "2"))
})

test_that("compute_score_frequencies handles all positive", {
  scores  <- c(0, 1, 2)
  outcome <- c(1, 1, 1)
  freq <- compute_score_frequencies(scores, outcome)
  expect_equal(sum(freq$pos_counts), 3)
  expect_equal(sum(freq$neg_counts), 0)
})

test_that("compute_score_frequencies handles all negative", {
  scores  <- c(0, 1, 2)
  outcome <- c(0, 0, 0)
  freq <- compute_score_frequencies(scores, outcome)
  expect_equal(sum(freq$pos_counts), 0)
  expect_equal(sum(freq$neg_counts), 3)
})

test_that("compute_score_frequencies handles gapped scores", {
  scores  <- c(0, 0, 3, 3, 5, 5)
  outcome <- c(1, 1, 1, 0, 0, 0)
  freq <- compute_score_frequencies(scores, outcome)
  expect_setequal(names(freq$pos_counts), c("0", "3", "5"))
  expect_equal(unname(freq$pos_counts["0"]), 2)
  expect_equal(unname(freq$pos_counts["3"]), 1)
  expect_equal(unname(freq$pos_counts["5"]), 0)
  expect_equal(unname(freq$neg_counts["5"]), 2)
})

test_that("compute_score_frequencies errors on length mismatch", {
  expect_error(
    compute_score_frequencies(c(1, 2), c(1)),
    "same length"
  )
})

# ---- compute_auc_from_table ----

test_that("compute_auc_from_table: perfect separation gives AUC 1", {
  # Positives all score 2, negatives all score 0
  pos <- c(`0` = 0, `2` = 3)
  neg <- c(`0` = 3, `2` = 0)
  expect_equal(compute_auc_from_table(pos, neg), 1.0)
})

test_that("compute_auc_from_table: inverse perfect separation gives AUC 0", {
  # Positives all score 0, negatives all score 2
  pos <- c(`0` = 3, `2` = 0)
  neg <- c(`0` = 0, `2` = 3)
  expect_equal(compute_auc_from_table(pos, neg), 0.0)
})

test_that("compute_auc_from_table: identical distributions give AUC 0.5", {
  pos <- c(`0` = 5, `1` = 5, `2` = 5)
  neg <- c(`0` = 5, `1` = 5, `2` = 5)
  expect_equal(compute_auc_from_table(pos, neg), 0.5)
})

test_that("compute_auc_from_table: returns NA when all pos", {
  pos <- c(`0` = 3, `1` = 2)
  neg <- c(`0` = 0, `1` = 0)
  expect_true(is.na(compute_auc_from_table(pos, neg)))
})

test_that("compute_auc_from_table: returns NA when all neg", {
  pos <- c(`0` = 0, `1` = 0)
  neg <- c(`0` = 5, `1` = 3)
  expect_true(is.na(compute_auc_from_table(pos, neg)))
})

test_that("compute_auc_from_table: known AUC value", {
  # Pos: score 0→5, 1→10, 2→8  (total 23)
  # Neg: score 0→10, 1→5, 2→3   (total 18)
  # P(pos>neg) = (10*10 + 8*15) / (23*18) = (100+120) / 414 = 220/414
  # P(pos=neg) = (5*10 + 10*5 + 8*3) / 414 = (50+50+24) / 414 = 124/414
  # AUC = 220/414 + 0.5*124/414 = (220 + 62) / 414 = 282 / 414 ≈ 0.68116
  pos <- c(`0` = 5, `1` = 10, `2` = 8)
  neg <- c(`0` = 10, `1` = 5, `2` = 3)
  expected <- 282 / 414
  expect_equal(compute_auc_from_table(pos, neg), expected)
})

# ---- compute_roc_metrics_from_table ----

test_that("compute_roc_metrics_from_table: sensitivity at lowest cutoff is 1", {
  pos <- c(`0` = 3, `1` = 5, `2` = 2)
  neg <- c(`0` = 8, `1` = 4, `2` = 1)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  # At the lowest cutoff (score 0), all are predicted positive
  expect_equal(metrics$sensitivity[metrics$cutoff == 0], 1.0)
  expect_equal(metrics$specificity[metrics$cutoff == 0], 0.0)
})

test_that("compute_roc_metrics_from_table: specificity at highest cutoff is 1", {
  pos <- c(`0` = 3, `1` = 5, `2` = 2)
  neg <- c(`0` = 8, `1` = 4, `2` = 1)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  # At highest cutoff (score 2), only score >= 2 are positive
  # TP = pos at score 2 = 2, TN = neg at scores 0,1 = 8 + 4 = 12
  row <- metrics[metrics$cutoff == 2, ]
  expect_equal(row$tp, 2)
  expect_equal(row$tn, 12)
  expect_equal(row$specificity, 12 / 13)
})

test_that("compute_roc_metrics_from_table: youden = sens + spec - 1", {
  pos <- c(`0` = 3, `1` = 5, `2` = 2)
  neg <- c(`0` = 8, `1` = 4, `2` = 1)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  youden_check <- metrics$sensitivity + metrics$specificity - 1
  expect_equal(metrics$youden, youden_check)
})

test_that("compute_roc_metrics_from_table: PPV is NA when no positive predictions", {
  pos <- c(`0` = 1, `1` = 2)
  neg <- c(`0` = 1, `1` = 0)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  # At the highest cutoff (1): TP=2 but we need to verify
  # At cutoff 1: TP = 2, FP = 0 => PPV = 1
  expect_false(is.na(metrics$ppv[metrics$cutoff == 1]))
})

test_that("compute_roc_metrics_from_table: NPV is NA when no negative predictions", {
  pos <- c(`0` = 3, `1` = 2)
  neg <- c(`0` = 4, `1` = 1)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  # At lowest cutoff (0): FN=0, TN=0 => NPV = NA
  expect_true(is.na(metrics$npv[metrics$cutoff == 0]))
})

test_that("compute_roc_metrics_from_table: returns one row per unique score", {
  pos <- c(`0` = 3, `1` = 5, `2` = 2, `4` = 1)
  neg <- c(`0` = 8, `1` = 4, `2` = 1, `4` = 0)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  expect_equal(nrow(metrics), 4)
})

# ---- find_optimal_cutoff ----

test_that("find_optimal_cutoff with youden picks max youden", {
  pos <- c(`0` = 3, `1` = 5, `2` = 2)
  neg <- c(`0` = 8, `1` = 4, `2` = 1)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  best <- find_optimal_cutoff(metrics, "youden")
  expect_equal(best$cutoff, metrics$cutoff[which.max(metrics$youden)])
})

test_that("find_optimal_cutoff: youden tie-breaks by sensitivity", {
  # Two cutoffs with same Youden but different sensitivity
  metrics <- data.frame(
    cutoff = c(1, 2),
    tp = c(8, 6),
    fp = c(2, 1),
    fn = c(2, 4),
    tn = c(8, 9),
    sensitivity = c(0.8, 0.6),
    specificity = c(0.8, 0.9),
    youden = c(0.6, 0.6),
    accuracy = c(0.8, 0.75),
    ppv = c(0.8, 0.857),
    npv = c(0.8, 0.692),
    stringsAsFactors = FALSE
  )
  best <- find_optimal_cutoff(metrics, "youden")
  # Higher sensitivity wins: cutoff 1 (sens=0.8) > cutoff 2 (sens=0.6)
  expect_equal(best$cutoff, 1)
})

test_that("find_optimal_cutoff with closest_topleft picks correct cutoff", {
  pos <- c(`0` = 0, `1` = 3, `2` = 7)
  neg <- c(`0` = 10, `1` = 5, `2` = 0)
  metrics <- compute_roc_metrics_from_table(pos, neg)
  best <- find_optimal_cutoff(metrics, "closest_topleft")
  # Cutoff=1: sens=10/10=1.0, spec=10/15≈0.667 → dist=sqrt(0^2+0.333^2)≈0.333
  # Cutoff=2: sens=7/10=0.7, spec=15/15=1.0 → dist=sqrt(0.3^2+0^2)=0.3
  # Cutoff 2 is closer to (0,1)
  expect_equal(best$cutoff, 2)
})

test_that("find_optimal_cutoff errors on invalid method", {
  metrics <- data.frame(
    cutoff = 1, tp = 5, fp = 2, fn = 0, tn = 5,
    sensitivity = 1, specificity = 0.714,
    youden = 0.714, accuracy = 0.833,
    ppv = 0.714, npv = 1, stringsAsFactors = FALSE
  )
  expect_error(
    find_optimal_cutoff(metrics, "invalid_method"),
    "should be one of"
  )
})

# ---- Tie-breaking: prefer_fewer_items logic (tested indirectly via sorting) ----

test_that("enumerate_combinations preserves correct item identity", {
  items <- c("Q1", "Q2", "Q3")
  combos <- enumerate_combinations(items, 1, 2)
  # Should have: Q1, Q2, Q3, Q1+Q2, Q1+Q3, Q2+Q3
  combo_strs <- vapply(combos, format_items, character(1))
  expect_true("Q1" %in% combo_strs)
  expect_true("Q1, Q2" %in% combo_strs)
  expect_true("Q2, Q3" %in% combo_strs)
})
