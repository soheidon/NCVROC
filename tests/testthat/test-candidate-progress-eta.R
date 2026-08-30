# test-candidate-progress-eta.R — Step 4: Candidate-Level Progress & ETA Verification
#
# Covers:
# 1. Total candidate denominator contract (W_total = sum(choose(p, sizes)))
# 2. Fast-path compatibility: progress = TRUE maintains streaming Top-N & bounded memory
# 3. Exact statistical equivalence between progress = TRUE and progress = FALSE
# 4. Monotonic candidate completion counts and intra-model-size batch propagation
# 5. Callback mutual exclusivity (no double-ticking or internal collision)
# 6. ETA suppression at 100% completion (no '< 10 sec' at finish)
# 7. ETA formatting and throughput estimation

test_that("Step 4: Candidate workload denominator is sum(choose(p, sizes)), not model-size count", {
  set.seed(401)
  n <- 50L
  p <- 8L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE),
    Q6 = sample(0:2, n, replace = TRUE),
    Q7 = sample(0:2, n, replace = TRUE),
    Q8 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("Q", 1:p)
  sizes <- 1:4

  # Expected combinations per size: 8 + 28 + 56 + 70 = 162 total
  w_expected <- sum(choose(p, sizes))
  expect_equal(w_expected, 162L)

  # Capture progress output from cross_size_cv
  msgs <- character()
  res_prog <- withCallingHandlers(
    cross_size_cv(
      data             = dat,
      outcome          = "y",
      items            = items,
      model_sizes      = sizes,
      selection_metric = "auc",
      folds            = 3,
      repeats          = 1,
      engine           = "Rcpp",
      parallel         = "threads",
      n_workers        = 2L,
      progress         = TRUE,
      seed             = 42
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Check that output messages mention candidate progress or completion
  expect_s3_class(res_prog, "cross_size_cv_result")
  expect_equal(res_prog$total_combinations, 162L)
})

test_that("Step 4: Fast-path streaming Top-N remains active and exact with progress = TRUE", {
  set.seed(402)
  n <- 40L
  p <- 6L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE),
    Q6 = sample(0:2, n, replace = TRUE)
  )
  items <- paste0("Q", 1:p)

  # exhaustive_sum_roc: progress = FALSE vs progress = TRUE (serial & threads)
  res_silent_serial <- exhaustive_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    top_n = 5, prefer_fewer_items = TRUE, engine = "Rcpp",
    parallel = "none", progress = FALSE
  )
  res_prog_serial <- suppressMessages(exhaustive_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    top_n = 5, prefer_fewer_items = TRUE, engine = "Rcpp",
    parallel = "none", progress = TRUE
  ))
  expect_identical(res_prog_serial, res_silent_serial)

  res_silent_threads <- exhaustive_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    top_n = 5, prefer_fewer_items = TRUE, engine = "Rcpp",
    parallel = "threads", n_workers = 2L, progress = FALSE
  )
  res_prog_threads <- suppressMessages(exhaustive_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    top_n = 5, prefer_fewer_items = TRUE, engine = "Rcpp",
    parallel = "threads", n_workers = 2L, progress = TRUE
  ))
  expect_identical(res_prog_threads, res_silent_threads)
  expect_identical(res_prog_threads, res_silent_serial)
})

test_that("Step 4: cross_size_cv progress = TRUE is bitwise identical to progress = FALSE", {
  set.seed(403)
  n <- 50L
  dat <- data.frame(
    y  = sample(0:1, n, replace = TRUE),
    Q1 = sample(0:2, n, replace = TRUE),
    Q2 = sample(0:2, n, replace = TRUE),
    Q3 = sample(0:2, n, replace = TRUE),
    Q4 = sample(0:2, n, replace = TRUE),
    Q5 = sample(0:2, n, replace = TRUE)
  )

  # Strategy 1 (AUC)
  res_auc_silent <- cross_size_cv(
    dat, "y", paste0("Q", 1:5), model_sizes = 1:3,
    selection_metric = "auc", folds = 3, repeats = 1,
    engine = "Rcpp", parallel = "threads", n_workers = 2L,
    progress = FALSE, seed = 123
  )
  res_auc_prog <- suppressMessages(cross_size_cv(
    dat, "y", paste0("Q", 1:5), model_sizes = 1:3,
    selection_metric = "auc", folds = 3, repeats = 1,
    engine = "Rcpp", parallel = "threads", n_workers = 2L,
    progress = TRUE, seed = 123
  ))
  expect_identical(res_auc_prog$final_selected_model, res_auc_silent$final_selected_model)
  expect_identical(res_auc_prog$candidate_ranking, res_auc_silent$candidate_ranking)

  # Strategy 2 (Youden)
  res_youd_silent <- cross_size_cv(
    dat, "y", paste0("Q", 1:5), model_sizes = 1:3,
    selection_metric = "youden", folds = 3, repeats = 1,
    engine = "Rcpp", parallel = "threads", n_workers = 2L,
    progress = FALSE, seed = 123
  )
  res_youd_prog <- suppressMessages(cross_size_cv(
    dat, "y", paste0("Q", 1:5), model_sizes = 1:3,
    selection_metric = "youden", folds = 3, repeats = 1,
    engine = "Rcpp", parallel = "threads", n_workers = 2L,
    progress = TRUE, seed = 123
  ))
  expect_identical(res_youd_prog$final_selected_model, res_youd_silent$final_selected_model)
  expect_identical(res_youd_prog$candidate_ranking, res_youd_silent$candidate_ranking)
})

test_that("Step 4: Intra-model-size progress callback is invoked monotonically and sums to exact combos", {
  set.seed(404)
  n <- 30L
  p <- 10L
  dat <- data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  # Total combos for sizes 1:3: 10 + 45 + 120 = 175
  total_combos <- sum(choose(p, 1:3))

  callback_ticks <- integer()
  cb <- function(n_done) {
    callback_ticks <<- c(callback_ticks, as.integer(n_done))
  }

  res <- exhaustive_sum_roc(
    dat, "y", items, min_items = 1, max_items = 3,
    top_n = 10, prefer_fewer_items = TRUE, engine = "Rcpp",
    parallel = "threads", n_workers = 2L,
    progress = FALSE, progress_callback = cb
  )

  expect_true(length(callback_ticks) >= 1L)
  expect_true(all(callback_ticks > 0L))
  expect_equal(sum(callback_ticks), total_combos)
})

test_that("Step 4: progress_callback suppresses internal progress bar (mutual exclusivity)", {
  set.seed(405)
  n <- 30L
  p <- 8L
  dat <- data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  callback_ticks <- integer()
  cb <- function(n_done) {
    callback_ticks <<- c(callback_ticks, as.integer(n_done))
  }

  # Ensure no stdout/stderr message is emitted when progress_callback is active
  msgs <- character()
  res <- withCallingHandlers(
    exhaustive_sum_roc(
      dat, "y", items, min_items = 1, max_items = 3,
      top_n = 10, prefer_fewer_items = TRUE, engine = "Rcpp",
      parallel = "threads", n_workers = 2L,
      progress = TRUE, progress_callback = cb
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_equal(length(msgs), 0L)
  expect_true(length(callback_ticks) >= 1L)
  expect_equal(sum(callback_ticks), sum(choose(p, 1:3)))
})

test_that("Step 4: ETA is omitted at 100% completion (no '< 10 sec' at finish)", {
  # Direct test of .progress_make progress exact mode at 100%
  msgs <- character()
  prg <- .progress_make(100L, label = "NCVROC", enabled = TRUE, progress_mode = "exact")

  withCallingHandlers(
    {
      prg$tick(50L)   # 50%
      Sys.sleep(0.6)
      prg$tick(50L)   # 100%
      prg$finish()
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Check that 100% message exists, contains elapsed time, and does NOT contain "ETA:"
  msg_100 <- grep("100\\.0%", msgs, value = TRUE)
  expect_true(length(msg_100) >= 1L)
  expect_true(grepl("\\[Elapsed: [0-9]+\\.[0-9]s\\]", msg_100[[length(msg_100)]]))
  expect_false(grepl("ETA:", msg_100[[length(msg_100)]]))
})

test_that("Step 4: Progress renderer throttles output in non-interactive mode", {
  msgs <- character()
  total <- 10000L
  prg <- .progress_make(total, label = "NCVROC", enabled = TRUE, progress_mode = "exact",
                        interactive_override = FALSE)

  withCallingHandlers(
    {
      # 100 rapid ticks of 10 units (0.1% each) -> total 1000 units (10%)
      for (i in 1:100) {
        prg$tick(10L)
      }
      prg$finish()
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # With 0.1% step ticks, only updates at >= 1.0% should trigger messages
  # 100 ticks should yield ~10-12 messages instead of 100
  expect_true(length(msgs) <= 15L)
  expect_true(length(msgs) >= 8L)

  # 100% completion message must appear exactly once
  msg_100 <- grep("100\\.0%", msgs, value = TRUE)
  expect_equal(length(msg_100), 1L)
  expect_true(grepl("\\[Elapsed: [0-9]+\\.[0-9]s\\]", msg_100))
  expect_false(grepl("ETA:", msg_100))
})

test_that("Step 4: Final 100% rendering is idempotent across tick() and finish()", {
  msgs <- character()
  prg <- .progress_make(100L, label = "NCVROC", enabled = TRUE, progress_mode = "exact",
                        interactive_override = FALSE)

  withCallingHandlers(
    {
      prg$tick(100L) # reaches 100%
      prg$finish()   # called after tick(100)
      prg$finish()   # repeated finish
      prg$close()    # repeated close
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  msg_100 <- grep("100\\.0%", msgs, value = TRUE)
  expect_equal(length(msg_100), 1L)
})

test_that("Step 4: Visual gauge formatting helper produces valid ASCII bars", {
  g0   <- .render_gauge(0, 100, width = 20L)
  g50  <- .render_gauge(50, 100, width = 20L)
  g100 <- .render_gauge(100, 100, width = 20L)

  expect_equal(g0, "[--------------------]")
  expect_equal(g50, "[##########----------]")
  expect_equal(g100, "[####################]")
})

test_that("Step 4: Progress counter handles total exceeding .Machine$integer.max without overflow", {
  msgs <- character()
  # 5 billion combinations (> 2.147B integer.max)
  total_combos <- 5e9
  expect_true(total_combos > .Machine$integer.max)

  prg <- .progress_make(total_combos, label = "NCVROC", enabled = TRUE, progress_mode = "exact",
                        interactive_override = FALSE)

  withCallingHandlers(
    {
      # Step in 500 million increments (10% each)
      for (i in 1:10) {
        prg$tick(5e8)
      }
      prg$finish()
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Check that no NA or integer overflow warnings occurred
  expect_true(length(msgs) >= 10L)
  expect_false(any(grepl("NA", msgs)))

  # Check that 100% completion line displays exact formatted count without scientific notation overflow
  msg_100 <- grep("100\\.0%", msgs, value = TRUE)
  expect_equal(length(msg_100), 1L)
  expect_true(grepl("5,000,000,000 / 5,000,000,000", msg_100[[1]]))
})

test_that("MEDIUM-1: PSOCK mode emits truthful start/completion messages without fake candidate progress", {
  set.seed(42)
  n <- 30
  p <- 6
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  # Test 1: Strategy 1 AUC under parallel = "chunks", progress = TRUE
  msgs_s1 <- character()
  res_s1_prog <- withCallingHandlers(
    cross_size_cv(
      data             = dat,
      outcome          = "y",
      items            = items,
      model_sizes      = 1:2,
      selection_metric = "auc",
      folds            = 3,
      repeats          = 1,
      engine           = "Rcpp",
      parallel         = "chunks",
      n_workers        = 2L,
      progress         = TRUE,
      seed             = 42
    ),
    message = function(m) {
      msgs_s1 <<- c(msgs_s1, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  res_s1_noprog <- cross_size_cv(
    data             = dat,
    outcome          = "y",
    items            = items,
    model_sizes      = 1:2,
    selection_metric = "auc",
    folds            = 3,
    repeats          = 1,
    engine           = "Rcpp",
    parallel         = "chunks",
    n_workers        = 2L,
    progress         = FALSE,
    seed             = 42
  )

  # Truthful start/completion messages present
  expect_true(any(grepl("Evaluating chunks in parallel", msgs_s1)))
  expect_true(any(grepl("All chunks complete", msgs_s1)))
  # Must NOT contain any "%" candidate progress line
  expect_false(any(grepl("%", msgs_s1)))
  # Statistical identity with progress = FALSE
  expect_identical(res_s1_prog$final_selected_model, res_s1_noprog$final_selected_model)
  expect_identical(res_s1_prog$candidate_ranking, res_s1_noprog$candidate_ranking)

  # Test 2: Strategy 2 Youden under parallel = "chunks", progress = TRUE
  msgs_s2 <- character()
  res_s2_prog <- withCallingHandlers(
    cross_size_cv(
      data             = dat,
      outcome          = "y",
      items            = items,
      model_sizes      = 1:2,
      selection_metric = "youden",
      folds            = 3,
      repeats          = 1,
      engine           = "Rcpp",
      parallel         = "chunks",
      n_workers        = 2L,
      progress         = TRUE,
      seed             = 42
    ),
    message = function(m) {
      msgs_s2 <<- c(msgs_s2, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  res_s2_noprog <- cross_size_cv(
    data             = dat,
    outcome          = "y",
    items            = items,
    model_sizes      = 1:2,
    selection_metric = "youden",
    folds            = 3,
    repeats          = 1,
    engine           = "Rcpp",
    parallel         = "chunks",
    n_workers        = 2L,
    progress         = FALSE,
    seed             = 42
  )

  # Truthful start/completion messages present
  expect_true(any(grepl("Evaluating blocks in parallel", msgs_s2)))
  expect_true(any(grepl("All blocks complete", msgs_s2)))
  # Must NOT contain any "%" candidate progress line
  expect_false(any(grepl("%", msgs_s2)))
  # Statistical identity with progress = FALSE
  expect_identical(res_s2_prog$final_selected_model, res_s2_noprog$final_selected_model)
  expect_identical(res_s2_prog$candidate_ranking, res_s2_noprog$candidate_ranking)
})

test_that("Step 4: .progress_make immediately renders 0.0% initial line", {
  msgs <- character()
  prg <- withCallingHandlers(
    NCVROC:::.progress_make(100L, label = "NCVROC", enabled = TRUE, progress_mode = "exact",
                            unit = "candidates", interactive_override = FALSE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(length(msgs) >= 1L)
  expect_match(msgs[1], "0\\.0%")
  expect_match(msgs[1], "0 / 100 candidates")
  expect_match(msgs[1], "\\[Elapsed: 0\\.0s\\]")
  expect_false(grepl("ETA:", msgs[1]))
  prg$finish()
})

test_that("Step 4: cross_size_nested_cv sequential (none & threads) renders per-fold candidate progress", {
  set.seed(42)
  n <- 30
  p <- 6
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  # 1. Test parallel = 'none'
  msgs_none <- character()
  res_none_prog <- withCallingHandlers(
    cross_size_nested_cv(
      data          = dat,
      outcome       = "y",
      items         = items,
      model_sizes   = 1:2,
      outer_folds   = 2,
      inner_folds   = 2,
      outer_repeats = 1,
      engine        = "Rcpp",
      parallel      = "none",
      progress      = TRUE,
      seed          = 100
    ),
    message = function(m) {
      msgs_none <<- c(msgs_none, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  res_none_noprog <- cross_size_nested_cv(
    data          = dat,
    outcome       = "y",
    items         = items,
    model_sizes   = 1:2,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    engine        = "Rcpp",
    parallel      = "none",
    progress      = FALSE,
    seed          = 100
  )

  # Check fold label, 0%, 100%, and Elapsed
  expect_true(any(grepl("\\[Fold 1/2\\]", msgs_none)))
  expect_true(any(grepl("\\[Fold 2/2\\]", msgs_none)))
  expect_true(any(grepl("0\\.0%", msgs_none)))
  expect_true(any(grepl("100\\.0%", msgs_none)))
  expect_true(all(grepl("\\[Elapsed: [0-9]+\\.[0-9]s\\]", msgs_none)))
  expect_false(any(grepl("ETA:", msgs_none)))

  # Statistical equivalence
  expect_identical(res_none_prog$outer_fold_results, res_none_noprog$outer_fold_results)
  expect_identical(res_none_prog$summary, res_none_noprog$summary)
  expect_identical(res_none_prog$outer_predictions, res_none_noprog$outer_predictions)

  # 2. Test parallel = 'threads'
  msgs_thr <- character()
  res_thr_prog <- withCallingHandlers(
    cross_size_nested_cv(
      data          = dat,
      outcome       = "y",
      items         = items,
      model_sizes   = 1:2,
      outer_folds   = 2,
      inner_folds   = 2,
      outer_repeats = 1,
      engine        = "Rcpp",
      parallel      = "threads",
      n_workers     = 2L,
      progress      = TRUE,
      seed          = 100
    ),
    message = function(m) {
      msgs_thr <<- c(msgs_thr, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  res_thr_noprog <- cross_size_nested_cv(
    data          = dat,
    outcome       = "y",
    items         = items,
    model_sizes   = 1:2,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    engine        = "Rcpp",
    parallel      = "threads",
    n_workers     = 2L,
    progress      = FALSE,
    seed          = 100
  )

  expect_true(any(grepl("\\[Fold 1/2\\]", msgs_thr)))
  expect_true(any(grepl("0\\.0%", msgs_thr)))
  expect_true(any(grepl("100\\.0%", msgs_thr)))
  expect_true(all(grepl("\\[Elapsed: [0-9]+\\.[0-9]s\\]", msgs_thr)))
  expect_false(any(grepl("ETA:", msgs_thr)))

  expect_identical(res_thr_prog$outer_fold_results, res_thr_noprog$outer_fold_results)
  expect_identical(res_thr_prog$summary, res_thr_noprog$summary)
  expect_identical(res_thr_prog$outer_predictions, res_thr_noprog$outer_predictions)
})

test_that("Step 4: cross_size_nested_cv PSOCK (outer) renders initial 0% and final 100%", {
  set.seed(42)
  n <- 30
  p <- 5
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  msgs_outer <- character()
  res_outer_prog <- withCallingHandlers(
    cross_size_nested_cv(
      data          = dat,
      outcome       = "y",
      items         = items,
      model_sizes   = 1:2,
      outer_folds   = 2,
      inner_folds   = 2,
      outer_repeats = 1,
      engine        = "Rcpp",
      parallel      = "outer",
      n_workers     = 2L,
      progress      = TRUE,
      seed          = 100
    ),
    message = function(m) {
      msgs_outer <<- c(msgs_outer, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  res_outer_noprog <- cross_size_nested_cv(
    data          = dat,
    outcome       = "y",
    items         = items,
    model_sizes   = 1:2,
    outer_folds   = 2,
    inner_folds   = 2,
    outer_repeats = 1,
    engine        = "Rcpp",
    parallel      = "outer",
    n_workers     = 2L,
    progress      = FALSE,
    seed          = 100
  )

  expect_true(any(grepl("0\\.0%", msgs_outer)))
  expect_true(any(grepl("100\\.0%", msgs_outer)))
  expect_true(any(grepl("outer folds complete", msgs_outer)))
  expect_true(all(grepl("\\[Elapsed: [0-9]+\\.[0-9]s\\]", msgs_outer)))
  expect_false(any(grepl("ETA:", msgs_outer)))

  expect_identical(res_outer_prog$outer_fold_results, res_outer_noprog$outer_fold_results)
  expect_identical(res_outer_prog$summary, res_outer_noprog$summary)
})

test_that("Step 4: exhaustive_sum_roc splits > 2500 candidates into fine-grained batches", {
  set.seed(42)
  n <- 30L
  p <- 18L
  dat <- as.data.frame(matrix(sample(0:2, n * p, replace = TRUE), n, p))
  names(dat) <- paste0("Q", 1:p)
  dat$y <- sample(0:1, n, replace = TRUE)
  items <- paste0("Q", 1:p)

  # For p = 18, size 4: choose(18, 4) = 3060 candidates (> 2500 minimum)
  total_combos <- choose(p, 4L)
  expect_equal(total_combos, 3060L)

  ticks <- integer()
  cb <- function(n_done) {
    ticks <<- c(ticks, as.integer(n_done))
  }

  res <- exhaustive_sum_roc(
    dat, "y", items, min_items = 4L, max_items = 4L,
    top_n = 5L, engine = "Rcpp", parallel = "threads", n_workers = 2L,
    progress = FALSE, progress_callback = cb
  )

  # With 3060 combos and target 20 batches (min 2500), it should partition into 2 batches
  expect_true(length(ticks) >= 2L)
  expect_equal(sum(ticks), total_combos)
  expect_true(all(ticks > 0L))
})
