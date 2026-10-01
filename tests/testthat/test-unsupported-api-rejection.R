# test-unsupported-api-rejection.R — Contract C4 tests

test_that("C4: unsupported entry points reject new cutoff methods before match.arg", {
  df <- make_test_data()
  methods <- c("sensitivity_target", "clinical_constraint")

  for (m in methods) {
    expected_msg <- sprintf('"%s" is supported only by cross_size_cv() and cross_size_nested_cv() in NCVROC v0.21.0.', m)

    # 1. exhaustive_sum_roc
    expect_error(
      exhaustive_sum_roc(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 2. candidate_stability_roc
    expect_error(
      candidate_stability_roc(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 3. fit_final_sum_scale
    expect_error(
      fit_final_sum_scale(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 4. nested_sum_roc
    expect_error(
      nested_sum_roc(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 5. compare_cv_selection
    expect_error(
      compare_cv_selection(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 6. run_ncvroc
    expect_error(
      run_ncvroc(df, items = c("q1", "q2"), config = structure(list(cutoff_method = m), class = "ncvroc_config"), progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 7. ncvroc_config
    expect_error(
      ncvroc_config(outcome = "y", items = c("q1", "q2"), cutoff_method = m),
      regexp = expected_msg, fixed = TRUE
    )

    # 8. cv_sum_roc / loocv_sum_roc
    expect_error(
      cv_sum_roc(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m),
      regexp = expected_msg, fixed = TRUE
    )
    expect_error(
      loocv_sum_roc(df, outcome = "y", items = c("q1", "q2"), cutoff_method = m),
      regexp = expected_msg, fixed = TRUE
    )

    # 9. cross_size_loocv
    expect_error(
      cross_size_loocv(df, outcome = "y", items = c("q1", "q2"), model_sizes = 1:2, cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 10. cv_select_sum_roc
    expect_error(
      cv_select_sum_roc(df, outcome = "y", items = c("q1", "q2"), item_count = 2, cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )

    # 11. loocv_select_sum_roc
    expect_error(
      loocv_select_sum_roc(df, outcome = "y", items = c("q1", "q2"), item_count = 2, cutoff_method = m, progress = FALSE),
      regexp = expected_msg, fixed = TRUE
    )
  }
})
