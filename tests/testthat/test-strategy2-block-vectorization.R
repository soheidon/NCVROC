# tests/testthat/test-strategy2-block-vectorization.R
# Verification of Strategy 2 block-level R vectorization (Opportunity 1)

test_that("block-vectorized Top-N reduction is identical to the former scalar semantics", {

  scalar_reference_reducer <- function(blocks, metric_col, top_n, prefer_fewer_items) {
    running_buffer <- NULL
    for (desc in blocks) {
      res_cpp <- desc$res_cpp
      valid_idx <- which(res_cpp$valid == 1L)
      if (length(valid_idx) == 0L) next

      for (vi in valid_idx) {
        cand_row <- data.frame(
          n_items                  = desc$s,
          combination_index_0based = desc$start_0based + vi - 1L,
          cv_auc                   = res_cpp$cv_auc[vi],
          cv_sensitivity           = res_cpp$cv_sensitivity[vi],
          cv_specificity           = res_cpp$cv_specificity[vi],
          cv_youden                = res_cpp$cv_youden[vi],
          cv_accuracy              = res_cpp$cv_accuracy[vi],
          cv_ppv                   = res_cpp$cv_ppv[vi],
          cv_npv                   = res_cpp$cv_npv[vi],
          cv_cutoff_mean           = res_cpp$cv_cutoff_mean[vi],
          cv_cutoff_sd             = res_cpp$cv_cutoff_sd[vi],
          final_full_data_cutoff   = res_cpp$final_full_data_cutoff[vi],
          .global_combo_index      = desc$cum_offset + desc$start_0based + vi,
          stringsAsFactors         = FALSE
        )
        running_buffer <- NCVROC:::.update_running_top_n(
          buffer = running_buffer,
          candidate_row = cand_row,
          metric_col = metric_col,
          top_n = top_n,
          prefer_fewer_items = prefer_fewer_items
        )
      }
    }
    if (!is.null(running_buffer)) rownames(running_buffer) <- NULL
    running_buffer
  }

  block_vectorized_reducer <- function(blocks, metric_col, top_n, prefer_fewer_items) {
    running_buffer <- NULL
    for (desc in blocks) {
      res_cpp <- desc$res_cpp
      valid_idx <- which(res_cpp$valid == 1L)
      if (length(valid_idx) == 0L) next

      block_df <- data.frame(
        n_items                  = rep.int(desc$s, length(valid_idx)),
        combination_index_0based = desc$start_0based + valid_idx - 1L,
        cv_auc                   = res_cpp$cv_auc[valid_idx],
        cv_sensitivity           = res_cpp$cv_sensitivity[valid_idx],
        cv_specificity           = res_cpp$cv_specificity[valid_idx],
        cv_youden                = res_cpp$cv_youden[valid_idx],
        cv_accuracy              = res_cpp$cv_accuracy[valid_idx],
        cv_ppv                   = res_cpp$cv_ppv[valid_idx],
        cv_npv                   = res_cpp$cv_npv[valid_idx],
        cv_cutoff_mean           = res_cpp$cv_cutoff_mean[valid_idx],
        cv_cutoff_sd             = res_cpp$cv_cutoff_sd[valid_idx],
        final_full_data_cutoff   = res_cpp$final_full_data_cutoff[valid_idx],
        .global_combo_index      = desc$cum_offset + desc$start_0based + valid_idx,
        stringsAsFactors         = FALSE
      )

      ranked_block <- NCVROC:::.order_and_rank_candidates(
        df = block_df,
        rank_by = metric_col,
        prefer_fewer_items = prefer_fewer_items
      )
      block_top <- utils::head(ranked_block, top_n)
      combined <- if (is.null(running_buffer)) block_top else rbind(running_buffer, block_top)
      running_buffer <- utils::head(
        NCVROC:::.order_and_rank_candidates(
          df = combined,
          rank_by = metric_col,
          prefer_fewer_items = prefer_fewer_items
        ),
        top_n
      )
    }
    if (!is.null(running_buffer)) rownames(running_buffer) <- NULL
    running_buffer
  }

  b1 <- list(
    s = 1L, start_0based = 0L, len = 6L, cum_offset = 0L,
    res_cpp = list(
      valid = c(1L,1L,0L,1L,1L,1L),
      cv_auc = c(.80,.85,.50,.85,.80,.70),
      cv_sensitivity = c(.70,.80,.40,.80,.70,.60),
      cv_specificity = c(.80,.75,.50,.75,.80,.70),
      cv_youden = c(.50,.55,.00,.55,.50,.30),
      cv_accuracy = c(.75,.77,.45,.77,.75,.65),
      cv_ppv = c(.70,.72,.40,.72,.70,.60),
      cv_npv = c(.80,.82,.50,.82,.80,.70),
      cv_cutoff_mean = rep(1,6),
      cv_cutoff_sd = rep(.1,6),
      final_full_data_cutoff = rep(1,6)
    )
  )

  b2 <- list(
    s = 2L, start_0based = 0L, len = 8L, cum_offset = 6L,
    res_cpp = list(
      valid = rep(1L,8),
      cv_auc = c(.85,.90,.85,.90,.85,.85,.60,.70),
      cv_sensitivity = c(.80,.85,.80,.85,.80,.80,.50,.60),
      cv_specificity = c(.75,.80,.75,.80,.75,.75,.60,.70),
      cv_youden = c(.55,.65,.55,.65,.55,.55,.10,.30),
      cv_accuracy = rep(.80,8),
      cv_ppv = rep(.75,8),
      cv_npv = rep(.85,8),
      cv_cutoff_mean = rep(2,8),
      cv_cutoff_sd = rep(.2,8),
      final_full_data_cutoff = rep(2,8)
    )
  )

  b3 <- list(
    s = 2L, start_0based = 8L, len = 5L, cum_offset = 6L,
    res_cpp = list(
      valid = c(1L,0L,1L,1L,1L),
      cv_auc = c(.90,.10,.90,.85,.90),
      cv_sensitivity = c(.85,.20,.85,.80,.85),
      cv_specificity = c(.80,.20,.80,.75,.80),
      cv_youden = c(.65,-.60,.65,.55,.65),
      cv_accuracy = c(.82,.20,.82,.80,.82),
      cv_ppv = c(.76,.20,.76,.75,.76),
      cv_npv = c(.86,.20,.86,.85,.86),
      cv_cutoff_mean = rep(2,5),
      cv_cutoff_sd = rep(.2,5),
      final_full_data_cutoff = rep(2,5)
    )
  )

  blocks <- list(b1,b2,b3)

  for (metric in c("cv_youden","cv_auc","cv_accuracy")) {
    for (pref in c(TRUE,FALSE)) {
      for (k in c(1L,3L,5L,10L,30L)) {
        expect_identical(
          block_vectorized_reducer(blocks, metric, k, pref),
          scalar_reference_reducer(blocks, metric, k, pref)
        )
      }
    }
  }
})

test_that("Strategy 2 serial, threads, and chunks remain production-equivalent", {
  skip_if_not_installed("Rcpp")
  skip_if_not_installed("RcppParallel")

  set.seed(1234)
  n <- 60L
  p <- 8L
  dat <- as.data.frame(matrix(sample(0:2, n*p, replace = TRUE), nrow=n, ncol=p))
  names(dat) <- paste0("Q", seq_len(p))
  dat$y <- rep(c(0L,1L), length.out=n)
  items <- paste0("Q", seq_len(p))

  common <- list(
    data = dat, outcome = "y", items = items,
    model_sizes = 1:3, selection_metric = "youden",
    folds = 3L, repeats = 1L, engine = "Rcpp",
    top_n = 10L, seed = 999L, progress = FALSE
  )

  res_serial <- do.call(cross_size_cv, c(common, list(parallel = "none")))
  res_threads <- do.call(cross_size_cv, c(common, list(parallel = "threads", n_workers = 2L)))
  res_chunks <- do.call(cross_size_cv, c(common, list(parallel = "chunks", n_workers = 2L)))

  expect_identical(res_threads$final_selected_model, res_serial$final_selected_model)
  expect_identical(res_chunks$final_selected_model, res_serial$final_selected_model)
  expect_identical(res_threads$candidate_ranking, res_serial$candidate_ranking)
  expect_identical(res_chunks$candidate_ranking, res_serial$candidate_ranking)
})

test_that("Strategy 2 constraints remain production-equivalent after block vectorization", {
  skip_if_not_installed("Rcpp")
  skip_if_not_installed("RcppParallel")

  set.seed(5678)
  n <- 72L
  p <- 8L
  dat <- as.data.frame(matrix(sample(0:2, n*p, replace = TRUE), nrow=n, ncol=p))
  names(dat) <- paste0("Q", seq_len(p))
  dat$y <- rep(c(0L,1L), length.out=n)
  items <- paste0("Q", seq_len(p))

  common <- list(
    data = dat, outcome = "y", items = items,
    model_sizes = 1:3, selection_metric = "youden",
    sensitivity_min = .20, specificity_min = .20,
    folds = 3L, repeats = 1L, engine = "Rcpp",
    top_n = 5L, seed = 999L, progress = FALSE
  )

  res_serial <- do.call(cross_size_cv, c(common, list(parallel = "none")))
  res_threads <- do.call(cross_size_cv, c(common, list(parallel = "threads", n_workers = 2L)))
  res_chunks <- do.call(cross_size_cv, c(common, list(parallel = "chunks", n_workers = 2L)))

  expect_identical(res_threads$final_selected_model, res_serial$final_selected_model)
  expect_identical(res_chunks$final_selected_model, res_serial$final_selected_model)
  expect_identical(res_threads$candidate_ranking, res_serial$candidate_ranking)
  expect_identical(res_chunks$candidate_ranking, res_serial$candidate_ranking)
})
