# test-large-candidate-space.R — Tests for combination counting and ranking > 2^31

test_that(".count_total_combos and .count_total_combos_cross_size handle spaces > 2^31 without integer overflow", {
  # choose(35, 17) = 4,537,567,650 (> 2^31 - 1 = 2,147,483,647)
  c35_17 <- NCVROC:::.count_total_combos(35, 17, 17)
  expect_type(c35_17, "double")
  expect_equal(c35_17, 4537567650)
  expect_false(is.na(c35_17))

  c35_17_cs <- NCVROC:::.count_total_combos_cross_size(35, 17)
  expect_type(c35_17_cs, "double")
  expect_equal(c35_17_cs, 4537567650)

  # choose(103, 7) = 19,813,501,785 (> 2^31 - 1)
  c103_7 <- NCVROC:::.count_total_combos_cross_size(103, 7)
  expect_type(c103_7, "double")
  expect_equal(c103_7, 19813501785)
})

test_that(".combination_unrank and .combination_rank roundtrip on rank > 2^31", {
  large_rank <- 3000000000.0 # 3 billion > 2^31 - 1
  combo <- NCVROC:::.combination_unrank(35, 17, large_rank)

  expect_length(combo, 17)
  expect_true(all(combo >= 0 & combo < 35))
  expect_true(!is.unsorted(combo))

  rank_recovered <- NCVROC:::.combination_rank(35, 17, combo)
  expect_type(rank_recovered, "double")
  expect_equal(rank_recovered, large_rank)

  # choose(103, 7): rank 15 billion
  large_rank_103 <- 15000000000.0
  combo103 <- NCVROC:::.combination_unrank(103, 7, large_rank_103)
  expect_length(combo103, 7)
  expect_true(all(combo103 >= 0 & combo103 < 103))
  expect_true(!is.unsorted(combo103))

  rank103_rec <- NCVROC:::.combination_rank(103, 7, combo103)
  expect_type(rank103_rec, "double")
  expect_equal(rank103_rec, large_rank_103)
})

test_that("resolve_global_combination_rank handles large ranks across sizes", {
  # choose(30, 10) = 30,045,015
  # choose(30, 15) = 155,117,520
  # choose(35, 17) = 4,537,567,650
  res <- NCVROC:::.resolve_global_combination_rank(35, 16, 17, 4000000000.0)
  expect_true(res$k %in% c(16, 17))
  expect_type(res$rank_within_k, "double")
  expect_true(res$rank_within_k >= 0)
})

test_that("outer_fold_results schema maintains numeric double for n_candidates_total", {
  set.seed(42)
  d <- data.frame(
    y  = sample(0:1, 40, replace = TRUE),
    Q1 = sample(0:2, 40, replace = TRUE),
    Q2 = sample(0:2, 40, replace = TRUE),
    Q3 = sample(0:2, 40, replace = TRUE),
    Q4 = sample(0:2, 40, replace = TRUE)
  )
  res <- cross_size_nested_cv(
    data               = d,
    outcome            = "y",
    items              = paste0("Q", 1:4),
    model_sizes        = 1:2,
    outer_folds        = 2,
    inner_folds        = 2,
    outer_repeats      = 1,
    selection_metric   = "auc",
    cutoff_method      = "youden",
    progress           = FALSE
  )

  expect_equal(ncol(res$outer_fold_results), 16L)
  expect_type(res$outer_fold_results$n_candidates_total, "double")
  expect_type(res$outer_fold_results$n_feasible_candidates, "integer")
  expect_type(res$outer_fold_results$selection_status, "character")
  expect_equal(res$outer_fold_results$n_candidates_total[1], choose(4, 1) + choose(4, 2))
})
