# cross_size_cv.R — Cross-size ordinary cross-validation for item-set scores
#
# Provides:
#   - cross_size_cv()
#   - .open_block_stream_writer()
#   - .open_block_stream_reader()
#   - .create_initial_block_streams()
#   - .merge_block_streams_hierarchical()
#   - .create_cross_size_block_stream_iterator()
#   - print.cross_size_cv_result()
#   - summary.cross_size_cv_result()

#' Routing instrumentation environment for test verification
#' @keywords internal
.NCVROC_ROUTING_COUNTERS <- new.env(parent = emptyenv())
.NCVROC_ROUTING_COUNTERS$outer_psock_count <- 0L
.NCVROC_ROUTING_COUNTERS$chunk_psock_count <- 0L
.NCVROC_ROUTING_COUNTERS$threads_count <- 0L
.NCVROC_ROUTING_COUNTERS$inner_psock_count <- 0L
.NCVROC_ROUTING_COUNTERS$strategy2_threads_count <- 0L
.NCVROC_ROUTING_COUNTERS$strategy2_serial_count <- 0L
.NCVROC_ROUTING_COUNTERS$strategy2_chunks_count <- 0L

#' Reset routing instrumentation counters
#' @keywords internal
.reset_routing_counters <- function() {
  .NCVROC_ROUTING_COUNTERS$outer_psock_count <- 0L
  .NCVROC_ROUTING_COUNTERS$chunk_psock_count <- 0L
  .NCVROC_ROUTING_COUNTERS$threads_count <- 0L
  .NCVROC_ROUTING_COUNTERS$inner_psock_count <- 0L
  .NCVROC_ROUTING_COUNTERS$strategy2_threads_count <- 0L
  .NCVROC_ROUTING_COUNTERS$strategy2_serial_count <- 0L
  .NCVROC_ROUTING_COUNTERS$strategy2_chunks_count <- 0L
}

#' Open a streaming block writer for a logical block stream
#'
#' Buffers rows up to `block_size` and flushes them as individual RDS block files
#' (`block_00001.rds`, `block_00002.rds`, ...). Peak memory: exactly `block_size` rows.
#'
#' @keywords internal
.open_block_stream_writer <- function(stream_dir, block_size = 2000L) {
  dir.create(stream_dir, recursive = TRUE, showWarnings = FALSE)
  output_buffer <- list()
  block_count <- 0L
  total_rows  <- 0L
  block_files <- character()

  write_row <- function(candidate_row) {
    output_buffer[[length(output_buffer) + 1L]] <<- candidate_row
    total_rows <<- total_rows + 1L

    if (length(output_buffer) >= block_size) {
      block_count <<- block_count + 1L
      blk_df <- do.call(rbind, output_buffer)
      blk_path <- file.path(stream_dir, sprintf("block_%05d.rds", block_count))
      saveRDS(blk_df, blk_path)
      block_files <<- c(block_files, blk_path)
      output_buffer <<- list()
      rm(blk_df)
    }
  }

  finish <- function() {
    if (length(output_buffer) > 0L) {
      block_count <<- block_count + 1L
      blk_df <- do.call(rbind, output_buffer)
      blk_path <- file.path(stream_dir, sprintf("block_%05d.rds", block_count))
      saveRDS(blk_df, blk_path)
      block_files <<- c(block_files, blk_path)
      output_buffer <<- list()
      rm(blk_df)
    }

    structure(
      list(
        stream_dir  = stream_dir,
        block_files = block_files,
        n_blocks    = block_count,
        total_rows  = total_rows,
        block_size  = block_size,
        cleanup     = function() {
          if (dir.exists(stream_dir)) {
            unlink(stream_dir, recursive = TRUE)
          }
        }
      ),
      class = "logical_block_stream"
    )
  }

  list(
    write_row = write_row,
    finish    = finish
  )
}

#' Open a streaming block reader for a logical block stream
#'
#' Reads only ONE block file (at most `block_size` rows) into R memory at any time.
#'
#' @keywords internal
.open_block_stream_reader <- function(stream_obj, size_cum_offset = 0L) {
  n_blocks <- stream_obj$n_blocks
  block_files <- stream_obj$block_files
  current_block_idx <- 0L
  current_block_df  <- data.frame()
  current_ptr       <- 1L

  .load_next_block <- function() {
    current_block_idx <<- current_block_idx + 1L
    if (current_block_idx > n_blocks) {
      current_block_df <<- data.frame()
      current_ptr      <<- 1L
      return(FALSE)
    }
    df <- readRDS(block_files[current_block_idx])
    if (size_cum_offset > 0L) {
      df$.global_combo_index <- size_cum_offset + df$.global_combo_index
    }
    current_block_df <<- df
    current_ptr      <<- 1L
    TRUE
  }

  if (n_blocks > 0L) {
    .load_next_block()
  }

  peek <- function() {
    if (nrow(current_block_df) == 0L || current_ptr > nrow(current_block_df)) {
      if (!.load_next_block()) {
        return(NULL)
      }
    }
    if (nrow(current_block_df) == 0L || current_ptr > nrow(current_block_df)) {
      return(NULL)
    }
    current_block_df[current_ptr, , drop = FALSE]
  }

  pop <- function() {
    cand <- peek()
    if (is.null(cand)) return(NULL)
    current_ptr <<- current_ptr + 1L
    cand
  }

  close <- function() {
    current_block_df <<- data.frame()
  }

  list(
    peek  = peek,
    pop   = pop,
    close = close
  )
}

#' Create initial sorted block streams directly from combinatorial evaluation
#'
#' Evaluates combinations in bounded chunks of `block_size` and saves each as a
#' 1-block logical block stream. Peak memory: strictly `block_size` rows.
#'
#' @keywords internal
.create_initial_block_streams <- function(x_mat,
                                          y,
                                          item_names,
                                          size,
                                          cutoff_method,
                                          prefer_fewer_items,
                                          engine,
                                          parallel_mode,
                                          n_workers_res,
                                          block_size = 2000L,
                                          temp_base_dir = NULL) {
  n_items_total <- length(item_names)
  n_total_s <- choose(n_items_total, size)

  if (is.null(temp_base_dir)) {
    temp_base_dir <- tempdir()
  }
  unique_stub <- gsub("[^A-Za-z0-9]+", "", basename(tempfile(pattern = "")))
  base_dir <- file.path(temp_base_dir, paste0("ncvroc_streams_s", size, "_", unique_stub))
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  n_chunks <- ceiling(n_total_s / block_size)
  num_threads <- if (parallel_mode == "threads") n_workers_res else 1L
  if (parallel_mode == "threads") {
    .NCVROC_ROUTING_COUNTERS$threads_count <- .NCVROC_ROUTING_COUNTERS$threads_count + 1L
  }

  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  initial_streams <- vector("list", n_chunks)

  if (n_chunks > 0L) {
    if (parallel_mode == "chunks" && n_chunks > 1L && n_workers_res > 1L) {
      .NCVROC_ROUTING_COUNTERS$chunk_psock_count <- .NCVROC_ROUTING_COUNTERS$chunk_psock_count + 1L
      actual_workers <- min(as.integer(n_workers_res), as.integer(n_chunks))
      cl <- parallel::makePSOCKcluster(actual_workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      lib_paths <- .libPaths()
      parallel::clusterExport(cl, "lib_paths", envir = environment())
      parallel::clusterEvalQ(cl, {
        .libPaths(lib_paths)
        if (requireNamespace("NCVROC", quietly = TRUE)) {
          try(library(NCVROC), silent = TRUE)
        }
        NULL
      })

      ns <- asNamespace("NCVROC")
      available_symbols <- intersect(.CROSS_SIZE_OUTER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
      parallel::clusterExport(cl, varlist = available_symbols, envir = ns)

      parallel::clusterExport(
        cl,
        varlist = c("x_mat", "y", "item_names", "size", "cutoff_method", "engine", "n_pos", "n_neg",
                    "block_size", "n_total_s", "prefer_fewer_items"),
        envir = environment()
      )

      eval_chunk_worker <- function(k) {
        c_start <- as.double((k - 1L) * block_size)
        c_size  <- min(as.double(block_size), as.double(n_total_s) - c_start)

        chunk_res <- .evaluate_chunk_serial(
          x_mat         = x_mat,
          y             = y,
          items         = item_names,
          min_items     = size,
          max_items     = size,
          cutoff_method = cutoff_method,
          chunk_start   = c_start,
          chunk_size    = c_size,
          engine        = engine,
          n_pos         = n_pos,
          n_neg         = n_neg,
          num_threads   = 1L
        )

        .order_and_rank_candidates(
          df                 = chunk_res,
          rank_by            = "auc",
          prefer_fewer_items = prefer_fewer_items
        )
      }

      chunk_results_list <- parallel::parLapply(cl, seq_len(n_chunks), eval_chunk_worker)

      for (k in seq_len(n_chunks)) {
        ord <- chunk_results_list[[k]]
        stream_k_dir <- file.path(base_dir, sprintf("init_stream_%05d", k))
        writer <- .open_block_stream_writer(stream_k_dir, block_size = block_size)
        for (row_i in seq_len(nrow(ord))) {
          writer$write_row(ord[row_i, , drop = FALSE])
        }
        initial_streams[[k]] <- writer$finish()
        rm(ord)
      }
      rm(chunk_results_list)
    } else {
      # Serial or Threads mode
      for (k in seq_len(n_chunks)) {
        c_start <- as.double((k - 1L) * block_size)
        c_size  <- min(as.double(block_size), as.double(n_total_s) - c_start)

        chunk_res <- .evaluate_chunk_serial(
          x_mat         = x_mat,
          y             = y,
          items         = item_names,
          min_items     = size,
          max_items     = size,
          cutoff_method = cutoff_method,
          chunk_start   = c_start,
          chunk_size    = c_size,
          engine        = engine,
          n_pos         = n_pos,
          n_neg         = n_neg,
          num_threads   = num_threads
        )

        ord <- .order_and_rank_candidates(
          df                 = chunk_res,
          rank_by            = "auc",
          prefer_fewer_items = prefer_fewer_items
        )

        stream_k_dir <- file.path(base_dir, sprintf("init_stream_%05d", k))
        writer <- .open_block_stream_writer(stream_k_dir, block_size = block_size)

        for (row_i in seq_len(nrow(ord))) {
          writer$write_row(ord[row_i, , drop = FALSE])
        }

        initial_streams[[k]] <- writer$finish()
        rm(chunk_res, ord)
      }
    }
  }

  list(
    size            = size,
    base_dir        = base_dir,
    initial_streams = initial_streams,
    cleanup         = function() {
      if (dir.exists(base_dir)) {
        unlink(base_dir, recursive = TRUE)
      }
    }
  )
}

#' Hierarchical merge of logical block streams with bounded fan-in
#'
#' Merges multiple block streams into a single final block stream using bounded fan-in.
#' At NO point during any pass are more than `fan_in` input blocks or 1 output block in memory.
#'
#' @keywords internal
.merge_block_streams_hierarchical <- function(initial_streams,
                                              base_dir,
                                              fan_in = 16L,
                                              block_size = 2000L,
                                              prefer_fewer_items = TRUE) {
  if (length(initial_streams) == 0L) {
    empty_dir <- file.path(base_dir, "final_stream")
    writer <- .open_block_stream_writer(empty_dir, block_size = block_size)
    return(writer$finish())
  }

  current_streams <- initial_streams
  pass <- 1L

  while (length(current_streams) > 1L) {
    n_batches <- ceiling(length(current_streams) / fan_in)
    next_streams <- vector("list", n_batches)

    for (b in seq_len(n_batches)) {
      start_idx <- (b - 1L) * fan_in + 1L
      end_idx   <- min(length(current_streams), b * fan_in)
      batch_streams <- current_streams[start_idx:end_idx]
      n_in_batch    <- length(batch_streams)

      out_stream_dir <- file.path(base_dir, sprintf("pass%d_stream_%05d", pass, b))
      writer <- .open_block_stream_writer(out_stream_dir, block_size = block_size)

      # Open 1 block reader per stream in this batch (at most fan_in blocks in memory)
      readers <- lapply(batch_streams, function(s) .open_block_stream_reader(s, size_cum_offset = 0L))

      while (TRUE) {
        best_r <- NULL
        best_cand <- NULL

        for (r_i in seq_len(n_in_batch)) {
          cand_i <- readers[[r_i]]$peek()
          if (is.null(cand_i) || nrow(cand_i) == 0L) next

          if (is.null(best_cand)) {
            best_r <- r_i
            best_cand <- cand_i
          } else {
            if (cand_i$auc > best_cand$auc) {
              best_r <- r_i
              best_cand <- cand_i
            } else if (cand_i$auc == best_cand$auc) {
              if (prefer_fewer_items) {
                if (cand_i$n_items < best_cand$n_items) {
                  best_r <- r_i
                  best_cand <- cand_i
                } else if (cand_i$n_items == best_cand$n_items) {
                  if (cand_i$.global_combo_index < best_cand$.global_combo_index) {
                    best_r <- r_i
                    best_cand <- cand_i
                  }
                }
              } else {
                if (cand_i$.global_combo_index < best_cand$.global_combo_index) {
                  best_r <- r_i
                  best_cand <- cand_i
                }
              }
            }
          }
        }

        if (is.null(best_r)) break # All readers exhausted

        winner_cand <- readers[[best_r]]$pop()
        writer$write_row(winner_cand)
      }

      # Close readers and finish writer
      for (r in readers) r$close()
      next_streams[[b]] <- writer$finish()

      # Delete input streams in this batch to reclaim disk space immediately
      for (s in batch_streams) {
        if (!is.null(s$cleanup)) s$cleanup()
      }
    }

    current_streams <- next_streams
    pass <- pass + 1L
  }

  current_streams[[1]]
}

#' Create a cross-size global merge iterator across model size block streams
#'
#' @keywords internal
.create_cross_size_block_stream_iterator <- function(final_streams,
                                                     sizes,
                                                     size_cum_offsets,
                                                     prefer_fewer_items = TRUE) {
  n_sizes <- length(final_streams)
  readers <- vector("list", n_sizes)
  for (si in seq_len(n_sizes)) {
    readers[[si]] <- .open_block_stream_reader(
      stream_obj      = final_streams[[si]],
      size_cum_offset = size_cum_offsets[si]
    )
  }

  .find_best_size <- function() {
    best_s <- NULL
    best_cand <- NULL

    for (s_i in seq_len(n_sizes)) {
      cand_i <- readers[[s_i]]$peek()
      if (is.null(cand_i) || nrow(cand_i) == 0L) next

      if (is.null(best_cand)) {
        best_s <- s_i
        best_cand <- cand_i
      } else {
        if (cand_i$auc > best_cand$auc) {
          best_s <- s_i
          best_cand <- cand_i
        } else if (cand_i$auc == best_cand$auc) {
          if (prefer_fewer_items) {
            if (cand_i$n_items < best_cand$n_items) {
              best_s <- s_i
              best_cand <- cand_i
            } else if (cand_i$n_items == best_cand$n_items) {
              if (cand_i$.global_combo_index < best_cand$.global_combo_index) {
                best_s <- s_i
                best_cand <- cand_i
              }
            }
          } else {
            if (cand_i$.global_combo_index < best_cand$.global_combo_index) {
              best_s <- s_i
              best_cand <- cand_i
            }
          }
        }
      }
    }
    best_s
  }

  peek <- function() {
    best_s <- .find_best_size()
    if (is.null(best_s)) return(NULL)
    readers[[best_s]]$peek()
  }

  pop <- function() {
    best_s <- .find_best_size()
    if (is.null(best_s)) return(NULL)
    readers[[best_s]]$pop()
  }

  cleanup <- function() {
    for (r in readers) r$close()
    for (s in final_streams) {
      if (!is.null(s$cleanup)) s$cleanup()
    }
  }

  list(
    peek    = peek,
    pop     = pop,
    cleanup = cleanup
  )
}

#' Update running Top-N buffer for memory-safe streaming candidate ranking
#'
#' @param buffer Current data.frame buffer of top candidates (or NULL).
#' @param candidate_row 1-row data.frame of candidate summary.
#' @param metric_col Character, column name to rank by (e.g. "cv_youden", "cv_accuracy", "auc").
#' @param top_n Integer, maximum size of buffer.
#' @param prefer_fewer_items Logical, prefer smaller models on ties.
#' @return Updated data.frame buffer with at most `top_n` rows.
#' @keywords internal
.update_running_top_n <- function(buffer, candidate_row, metric_col, top_n, prefer_fewer_items = TRUE) {
  if (is.null(buffer) || nrow(buffer) == 0L) {
    return(candidate_row)
  }

  cur_len <- nrow(buffer)

  if (cur_len < top_n) {
    merged <- rbind(buffer, candidate_row)
    ord <- .order_and_rank_candidates(merged, rank_by = metric_col, prefer_fewer_items = prefer_fewer_items)
    return(ord)
  }

  # Buffer is full (nrow == top_n)
  worst_metric <- buffer[[metric_col]][cur_len]
  cand_metric  <- candidate_row[[metric_col]][1]

  if (cand_metric < worst_metric) {
    return(buffer)
  }

  if (cand_metric == worst_metric) {
    if (prefer_fewer_items) {
      if (candidate_row$n_items[1] > buffer$n_items[cur_len]) {
        return(buffer)
      }
      if (candidate_row$n_items[1] == buffer$n_items[cur_len] &&
          candidate_row$.global_combo_index[1] >= buffer$.global_combo_index[cur_len]) {
        return(buffer)
      }
    } else {
      if (candidate_row$.global_combo_index[1] >= buffer$.global_combo_index[cur_len]) {
        return(buffer)
      }
    }
  }

  # Candidate is better than worst; insert and prune
  merged <- rbind(buffer, candidate_row)
  ord <- .order_and_rank_candidates(merged, rank_by = metric_col, prefer_fewer_items = prefer_fewer_items)
  ord[seq_len(top_n), , drop = FALSE]
}

#' Memory-safe exact cross-size AUC model selector
#'
#' @keywords internal
.select_cross_size_auc_exact <- function(data,
                                         outcome_name,
                                         item_names,
                                         sizes,
                                         top_n,
                                         cutoff_method,
                                         prefer_fewer_items,
                                         engine,
                                         parallel_mode,
                                         n_workers_res,
                                         sensitivity_min,
                                         specificity_min,
                                         cv_folds,
                                         repeats,
                                         y) {
  n_items_total <- length(item_names)
  has_constraints <- (!is.null(sensitivity_min) || !is.null(specificity_min))

  # -------------------------------------------------------------------------
  # Case A: Unconstrained AUC search (Top-N per size exact merge)
  # -------------------------------------------------------------------------
  if (!has_constraints) {
    candidate_list <- vector("list", length(sizes))
    cum_offset <- 0L

    for (si in seq_along(sizes)) {
      s <- sizes[si]
      res_s <- exhaustive_sum_roc(
        data               = data,
        outcome            = outcome_name,
        items              = item_names,
        min_items          = s,
        max_items          = s,
        cutoff_method      = cutoff_method,
        rank_by            = "auc",
        top_n              = top_n,
        prefer_fewer_items = prefer_fewer_items,
        engine             = engine,
        parallel           = parallel_mode,
        n_workers          = n_workers_res,
        progress           = FALSE
      )
      idx_s <- if (".global_combo_index" %in% names(res_s)) res_s$.global_combo_index else seq_len(nrow(res_s))
      res_s$.global_combo_index <- cum_offset + idx_s
      cum_offset <- cum_offset + choose(n_items_total, s)
      candidate_list[[si]] <- res_s
    }

    merged_top <- do.call(rbind, candidate_list)
    ranked_top <- .order_and_rank_candidates(
      df                 = merged_top,
      rank_by            = "auc",
      prefer_fewer_items = prefer_fewer_items
    )
    ranked_top$rank <- seq_len(nrow(ranked_top))
    keep_n <- min(top_n, nrow(ranked_top))
    final_ranking_df <- utils::head(ranked_top, keep_n)

    best_candidate <- ranked_top[1, , drop = FALSE]
    best_items_vec <- .parse_itemset(best_candidate$items)

    best_cv <- .run_fixed_model_cv(
      itemset       = best_items_vec,
      data          = data,
      y             = y,
      cv_folds      = cv_folds,
      cutoff_method = cutoff_method
    )
    best_agg <- .aggregate_oof_metrics(
      oof_df       = best_cv$oof_predictions,
      repeats      = repeats,
      fold_results = best_cv$fold_results
    )

    return(list(
      best_candidate   = best_candidate,
      best_cv          = best_cv,
      best_agg         = best_agg,
      ranking_df       = final_ranking_df
    ))
  }

  # -------------------------------------------------------------------------
  # Case B: Constrained AUC search (Universal Logical Block Streams)
  # -------------------------------------------------------------------------
  n_total <- length(y)
  n_folds_total <- length(cv_folds)
  fold_subsets <- lapply(cv_folds, function(test_idx) {
    train_idx <- setdiff(seq_len(n_total), test_idx)
    list(
      train_idx = train_idx,
      test_idx  = test_idx,
      train_y   = y[train_idx],
      test_y    = y[test_idx]
    )
  })

  x_mat <- as.matrix(data[, item_names, drop = FALSE])

  # Build per-size initial block streams and merge hierarchically with fan_in = 16
  final_streams <- vector("list", length(sizes))
  cum_offsets   <- integer(length(sizes))
  cum_off       <- 0L

  for (si in seq_along(sizes)) {
    s <- sizes[si]
    init_obj <- .create_initial_block_streams(
      x_mat              = x_mat,
      y                  = y,
      item_names         = item_names,
      size               = s,
      cutoff_method      = cutoff_method,
      prefer_fewer_items = prefer_fewer_items,
      engine             = engine,
      parallel_mode      = parallel_mode,
      n_workers_res      = n_workers_res,
      block_size         = 2000L
    )

    final_streams[[si]] <- .merge_block_streams_hierarchical(
      initial_streams    = init_obj$initial_streams,
      base_dir           = init_obj$base_dir,
      fan_in             = 16L,
      block_size         = 2000L,
      prefer_fewer_items = prefer_fewer_items
    )

    cum_offsets[si] <- cum_off
    cum_off <- cum_off + choose(n_items_total, s)
  }

  # Global cross-size merge iterator across all model size final block streams
  global_iter <- .create_cross_size_block_stream_iterator(
    final_streams      = final_streams,
    sizes              = sizes,
    size_cum_offsets   = cum_offsets,
    prefer_fewer_items = prefer_fewer_items
  )
  on.exit(global_iter$cleanup(), add = TRUE)

  best_candidate <- NULL
  feasible_ranking_list <- list()

  while (TRUE) {
    cand_i <- global_iter$pop()
    if (is.null(cand_i) || nrow(cand_i) == 0L) {
      break # All candidates across all model sizes exhausted
    }

    cand_items_vec <- .parse_itemset(cand_i$items)

    # Evaluate lightweight OOF sensitivity and specificity across folds
    tps <- numeric(n_folds_total)
    tns <- numeric(n_folds_total)
    fps <- numeric(n_folds_total)
    fns <- numeric(n_folds_total)
    cutoffs_vec <- numeric(n_folds_total)

    for (f in seq_len(n_folds_total)) {
      sub_f <- fold_subsets[[f]]
      train_scores <- rowSums(data[sub_f$train_idx, cand_items_vec, drop = FALSE])
      train_freq <- compute_score_frequencies(train_scores, sub_f$train_y)
      train_metrics <- compute_roc_metrics_from_table(train_freq$pos_counts, train_freq$neg_counts)
      best_cut <- find_optimal_cutoff(train_metrics, method = cutoff_method)
      cutoffs_vec[f] <- best_cut$cutoff

      test_scores <- rowSums(data[sub_f$test_idx, cand_items_vec, drop = FALSE])
      test_preds <- ifelse(test_scores >= best_cut$cutoff, 1L, 0L)
      test_y <- sub_f$test_y

      tps[f] <- sum(test_preds == 1L & test_y == 1L)
      tns[f] <- sum(test_preds == 0L & test_y == 0L)
      fps[f] <- sum(test_preds == 1L & test_y == 0L)
      fns[f] <- sum(test_preds == 0L & test_y == 1L)
    }

    folds_per_rep <- n_folds_total %/% repeats
    rep_sens <- numeric(repeats)
    rep_spec <- numeric(repeats)
    rep_youd <- numeric(repeats)
    rep_acc  <- numeric(repeats)

    for (r in seq_len(repeats)) {
      r_idx <- ((r - 1L) * folds_per_rep + 1L):(r * folds_per_rep)
      tot_tp <- sum(tps[r_idx])
      tot_tn <- sum(tns[r_idx])
      tot_fp <- sum(fps[r_idx])
      tot_fn <- sum(fns[r_idx])

      sens <- if (tot_tp + tot_fn > 0) tot_tp / (tot_tp + tot_fn) else NA_real_
      spec <- if (tot_tn + tot_fp > 0) tot_tn / (tot_tn + tot_fp) else NA_real_
      acc  <- if (tot_tp + tot_tn + tot_fp + tot_fn > 0) (tot_tp + tot_tn) / (tot_tp + tot_tn + tot_fp + tot_fn) else NA_real_
      youd <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1

      rep_sens[r] <- sens
      rep_spec[r] <- spec
      rep_youd[r] <- youd
      rep_acc[r]  <- acc
    }

    mean_sens <- mean(rep_sens, na.rm = TRUE)
    mean_spec <- mean(rep_spec, na.rm = TRUE)
    mean_youd <- mean(rep_youd, na.rm = TRUE)
    mean_acc  <- mean(rep_acc, na.rm = TRUE)

    sens_pass <- if (is.null(sensitivity_min)) TRUE else (!is.na(mean_sens) && mean_sens >= sensitivity_min)
    spec_pass <- if (is.null(specificity_min)) TRUE else (!is.na(mean_spec) && mean_spec >= specificity_min)

    if (sens_pass && spec_pass) {
      feasible_row <- data.frame(
        items               = cand_i$items,
        n_items             = cand_i$n_items,
        auc                 = cand_i$auc,
        cv_sensitivity      = mean_sens,
        cv_specificity      = mean_spec,
        cv_youden           = mean_youd,
        cv_accuracy         = mean_acc,
        cv_cutoff_mean      = mean(cutoffs_vec),
        cv_cutoff_sd        = if (length(cutoffs_vec) > 1) stats::sd(cutoffs_vec) else 0,
        cutoff              = cand_i$cutoff,
        constraint_pass     = TRUE,
        .global_combo_index = cand_i$.global_combo_index,
        stringsAsFactors    = FALSE
      )

      if (is.null(best_candidate)) {
        best_candidate <- cand_i
      }

      feasible_ranking_list[[length(feasible_ranking_list) + 1L]] <- feasible_row

      if (length(feasible_ranking_list) >= top_n) {
        break
      }
    }
  }

  if (is.null(best_candidate)) {
    stop(sprintf(
      "No candidate models satisfied the specified OOF constraints (sensitivity_min = %s, specificity_min = %s).",
      if (is.null(sensitivity_min)) "NULL" else as.character(sensitivity_min),
      if (is.null(specificity_min)) "NULL" else as.character(specificity_min)
    ), call. = FALSE)
  }

  best_items_vec <- .parse_itemset(best_candidate$items)

  # Run CV ONCE on the single final selected best model
  best_cv <- .run_fixed_model_cv(
    itemset       = best_items_vec,
    data          = data,
    y             = y,
    cv_folds      = cv_folds,
    cutoff_method = cutoff_method
  )
  best_agg <- .aggregate_oof_metrics(
    oof_df       = best_cv$oof_predictions,
    repeats      = repeats,
    fold_results = best_cv$fold_results
  )

  final_ranking_df <- do.call(rbind, feasible_ranking_list)
  final_ranking_df$rank <- seq_len(nrow(final_ranking_df))

  list(
    best_candidate = best_candidate,
    best_cv        = best_cv,
    best_agg       = best_agg,
    ranking_df     = final_ranking_df
  )
}

#' Cutoff-dependent candidate evaluation helper across CV folds
#' @keywords internal
.eval_single_combo_cv <- function(combo_items,
                                  s,
                                  gi,
                                  cum_offset,
                                  dat_prep,
                                  y,
                                  fold_subsets,
                                  n_folds_total,
                                  repeats,
                                  cutoff_method,
                                  sensitivity_min,
                                  specificity_min) {
  combo_str <- format_items(combo_items)
  cutoffs_vec <- numeric(n_folds_total)
  tps <- numeric(n_folds_total)
  tns <- numeric(n_folds_total)
  fps <- numeric(n_folds_total)
  fns <- numeric(n_folds_total)

  for (f in seq_len(n_folds_total)) {
    sub_f <- fold_subsets[[f]]
    train_scores <- rowSums(dat_prep[sub_f$train_idx, combo_items, drop = FALSE])
    train_freq <- compute_score_frequencies(train_scores, sub_f$train_y)
    train_metrics <- compute_roc_metrics_from_table(train_freq$pos_counts, train_freq$neg_counts)
    best_cut <- find_optimal_cutoff(train_metrics, method = cutoff_method)
    cutoffs_vec[f] <- best_cut$cutoff

    test_scores <- rowSums(dat_prep[sub_f$test_idx, combo_items, drop = FALSE])
    test_preds <- ifelse(test_scores >= best_cut$cutoff, 1L, 0L)
    test_y <- sub_f$test_y

    tps[f] <- sum(test_preds == 1L & test_y == 1L)
    tns[f] <- sum(test_preds == 0L & test_y == 0L)
    fps[f] <- sum(test_preds == 1L & test_y == 0L)
    fns[f] <- sum(test_preds == 0L & test_y == 1L)
  }

  folds_per_rep <- n_folds_total %/% repeats
  rep_sens <- numeric(repeats)
  rep_spec <- numeric(repeats)
  rep_youd <- numeric(repeats)
  rep_acc  <- numeric(repeats)
  rep_ppv  <- numeric(repeats)
  rep_npv  <- numeric(repeats)

  for (r in seq_len(repeats)) {
    r_idx <- ((r - 1L) * folds_per_rep + 1L):(r * folds_per_rep)
    tot_tp <- sum(tps[r_idx])
    tot_tn <- sum(tns[r_idx])
    tot_fp <- sum(fps[r_idx])
    tot_fn <- sum(fns[r_idx])

    sens <- if (tot_tp + tot_fn > 0) tot_tp / (tot_tp + tot_fn) else NA_real_
    spec <- if (tot_tn + tot_fp > 0) tot_tn / (tot_tn + tot_fp) else NA_real_
    ppv  <- if (tot_tp + tot_fp > 0) tot_tp / (tot_tp + tot_fp) else NA_real_
    npv  <- if (tot_tn + tot_fn > 0) tot_tn / (tot_tn + tot_fn) else NA_real_
    acc  <- if (tot_tp + tot_tn + tot_fp + tot_fn > 0) (tot_tp + tot_tn) / (tot_tp + tot_tn + tot_fp + tot_fn) else NA_real_
    youd <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1

    rep_sens[r] <- sens
    rep_spec[r] <- spec
    rep_youd[r] <- youd
    rep_acc[r]  <- acc
    rep_ppv[r]  <- ppv
    rep_npv[r]  <- npv
  }

  mean_sens <- mean(rep_sens, na.rm = TRUE)
  mean_spec <- mean(rep_spec, na.rm = TRUE)
  mean_youd <- mean(rep_youd, na.rm = TRUE)
  mean_acc  <- mean(rep_acc, na.rm = TRUE)
  mean_ppv  <- mean(rep_ppv, na.rm = TRUE)
  mean_npv  <- mean(rep_npv, na.rm = TRUE)

  if (!is.null(sensitivity_min) && (is.na(mean_sens) || mean_sens < sensitivity_min)) {
    return(NULL)
  }
  if (!is.null(specificity_min) && (is.na(mean_spec) || mean_spec < specificity_min)) {
    return(NULL)
  }

  full_scores  <- rowSums(dat_prep[, combo_items, drop = FALSE])
  full_freq    <- compute_score_frequencies(full_scores, y)
  full_metrics <- compute_roc_metrics_from_table(full_freq$pos_counts, full_freq$neg_counts)
  best_full    <- find_optimal_cutoff(full_metrics, method = cutoff_method)
  full_auc     <- compute_auc_from_table(full_freq$pos_counts, full_freq$neg_counts)

  data.frame(
    items                  = combo_str,
    n_items                = s,
    cv_auc                 = full_auc,
    cv_sensitivity         = mean_sens,
    cv_specificity         = mean_spec,
    cv_youden              = mean_youd,
    cv_accuracy            = mean_acc,
    cv_ppv                 = mean_ppv,
    cv_npv                 = mean_npv,
    cv_cutoff_mean         = mean(cutoffs_vec),
    cv_cutoff_sd           = if (length(cutoffs_vec) > 1) stats::sd(cutoffs_vec) else 0,
    final_full_data_cutoff = best_full$cutoff,
    .global_combo_index    = cum_offset + gi,
    stringsAsFactors       = FALSE
  )
}

#' Cross-size ordinary cross-validation and model selection
#'
#' Evaluates candidate item-set sum score models across multiple model sizes
#' using ordinary cross-validation (\eqn{k}-fold or LOOCV), ranks candidates deterministically,
#' and selects a single final best model.
#'
#' @details
#' \code{cross_size_cv()} performs combinatorial model selection over unweighted sum scores
#' \eqn{\sum X_j}.
#'
#' \bold{Cross-Validation Methods}:
#' \itemize{
#'   \item \code{"kfold"}: \eqn{k}-fold cross-validation with \eqn{2 \le k < N} folds. In repeated
#'     \eqn{k}-fold CV (\code{repeats > 1}), metrics are evaluated independently on each repeat's
#'     \eqn{N} out-of-fold predictions, and summarized as the mean and sample standard deviation across
#'     repeats (no \eqn{N \times R} pooling).
#'   \item \code{"loocv"}: Exact Leave-One-Out Cross-Validation (\eqn{k = N}, \code{repeats = 1}).
#' }
#'
#' \bold{Fixed Sum-Score AUC Identity}:
#' For a fixed unweighted sum-score candidate, the raw score has no fitted parameters.
#' Therefore its pooled out-of-fold score vector is identical to the full-data score vector,
#' and the corresponding AUC is identical to the apparent full-data AUC (theoretical repeat \eqn{\text{SD} = 0}).
#'
#' \bold{Candidate Ranking and Tie-Breaking}:
#' Candidate models are ranked deterministically:
#' \enumerate{
#'   \item Primary selection metric (e.g. mean \code{cv_youden} or \code{cv_auc}) descending.
#'   \item Smaller model size (\code{n_items}) ascending if \code{prefer_fewer_items = TRUE}.
#'   \item Global candidate index (\code{.global_combo_index}) ascending.
#' }
#'
#' @param data A data.frame containing the outcome and item columns.
#' @param outcome Name of the binary outcome column (bare symbol or character string).
#' @param items Item columns to evaluate (bare range e.g. `Q1:Q10`, bare names in `c()`,
#'   character vector, or numeric positions).
#' @param model_sizes Integer vector of model sizes to evaluate (e.g. `1:5` or `c(1, 3, 5)`).
#'   If `NULL`, resolved from `min_items:max_items` or `item_count`.
#' @param min_items Integer, minimum items per model (default 1, used if `model_sizes` is `NULL`).
#' @param max_items Integer, maximum items per model (default 4, used if `model_sizes` is `NULL`).
#' @param item_count String specification (e.g. `"==4"`, `"<=3"`, `"2:4"`, used if `model_sizes` is `NULL`).
#' @param cv_method Character, `"kfold"` (default) or `"loocv"`.
#' @param folds Integer, number of folds for k-fold CV (default 5, \eqn{2 \le \text{folds} < N}). Ignored when `cv_method = "loocv"`.
#' @param repeats Integer, number of independent repeats for k-fold CV (default 1).
#'   Must be 1 for LOOCV.
#' @param stratified Logical, maintain class balance across folds (default `TRUE`).
#' @param selection_metric Metric for candidate ranking and model selection:
#'   `"auc"` (default), `"youden"`, `"sensitivity"`, `"specificity"`, or `"accuracy"`.
#' @param cutoff_method Cutoff selection rule: `"youden"` (default) or `"closest_topleft"`.
#' @param sensitivity_min Optional minimum OOF sensitivity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param specificity_min Optional minimum OOF specificity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param top_n Integer, number of top candidates to return in `candidate_ranking` (default 20).
#' @param prefer_fewer_items Logical, prefer smaller models on ties (default `TRUE`).
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param engine Combinatorial computation engine: `"Rcpp"` (default) or `"R"`.
#' @param parallel Parallel mode: `FALSE` / `"none"` (default), `"threads"` (multi-threaded C++),
#'   or `"chunks"` (PSOCK cluster).
#' @param n_workers Integer, number of workers or threads (default `NULL` = auto).
#' @param tuning Automatic execution-planning mode: `"off"` (default, manual execution configuration is authoritative),
#'   `"auto"` (benchmarks legal backends only when predicted runtime exceeds threshold), or
#'   `"always"` (always benchmarks legal backends for non-degenerate workloads).
#' @param ci Logical, compute confidence intervals for full-data apparent metrics of final model (default `FALSE`).
#' @param conf_level Numeric, confidence level (default 0.95).
#' @param seed Integer, random seed for reproducible fold generation.
#' @param progress Logical, show progress bar (default `interactive()`).
#'
#' @return An S3 object of class `"cross_size_cv_result"`, containing:
#' \describe{
#'   \item{final_selected_model}{A one-row data.frame of performance metrics for the best model.}
#'   \item{candidate_ranking}{A data.frame ranking top candidate models.}
#'   \item{model_size_summary}{Descriptive summary of candidates evaluated per model size.}
#'   \item{oof_predictions}{Out-of-fold predictions data.frame (\eqn{N \times R} rows).}
#'   \item{fold_results}{Fold-level training cutoffs and evaluations.}
#'   \item{repeat_metrics}{Repeat-level performance metrics table (\eqn{R} rows).}
#'   \item{cv_performance}{Summary metrics table with mean and SD across repeats.}
#'   \item{cv_cutoff_distribution}{List with mean and SD of fold cutoffs.}
#'   \item{final_full_data_cutoff}{Deployment cutoff estimated on the full dataset.}
#'   \item{model_sizes}{Evaluated model sizes vector.}
#'   \item{total_combinations}{Total number of evaluated candidate models.}
#'   \item{cv_method}{Cross-validation method used (\code{"kfold"} or \code{"loocv"}).}
#'   \item{settings}{List of execution settings and parameters.}
#' }
#'
#' @seealso \code{\link{cross_size_loocv}}, \code{\link{cv_select_sum_roc}},
#'   \code{\link{cross_size_nested_cv}}, \code{\link{compare_cv_selection}}.
#'
#' @examples
#' \dontrun{
#' data(sample_data_bin)
#' res <- cross_size_cv(
#'   data             = sample_data_bin,
#'   outcome          = y,
#'   items            = paste0("Q", 1:5),
#'   model_sizes      = 1:3,
#'   selection_metric = "youden",
#'   folds            = 5,
#'   seed             = 42
#' )
#' print(res)
#' }
#'
#' @export
cross_size_cv <- function(data,
                          outcome,
                          items,
                          model_sizes        = NULL,
                          min_items          = 1,
                          max_items          = 4,
                          item_count         = NULL,
                          cv_method          = c("kfold", "loocv"),
                          folds              = 5,
                          repeats            = 1,
                          stratified         = TRUE,
                          selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                          cutoff_method      = c("youden", "closest_topleft"),
                          sensitivity_min    = NULL,
                          specificity_min    = NULL,
                          top_n              = 20,
                          prefer_fewer_items = TRUE,
                          positive_label     = 1,
                          negative_label     = 0,
                          engine             = c("Rcpp", "R"),
                          parallel           = FALSE,
                          n_workers          = NULL,
                          tuning             = c("off", "auto", "always"),
                          ci                 = FALSE,
                          conf_level         = 0.95,
                          seed               = NULL,
                          progress           = interactive()) {
  # ---- NSE Column Resolution ----
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)

  cv_method        <- match.arg(cv_method)
  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  engine           <- match.arg(engine)
  tuning           <- match.arg(tuning)

  # ---- Validate top_n ----
  if (is.null(top_n) || is.na(top_n) || !is.numeric(top_n) || top_n <= 0 || is.infinite(top_n)) {
    stop("`top_n` must be a positive integer.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  # ---- Validate constraints ----
  if (!is.null(sensitivity_min)) {
    if (!is.numeric(sensitivity_min) || length(sensitivity_min) != 1 ||
        is.na(sensitivity_min) || sensitivity_min < 0 || sensitivity_min > 1) {
      stop("`sensitivity_min` must be a numeric value between 0 and 1.", call. = FALSE)
    }
  }
  if (!is.null(specificity_min)) {
    if (!is.numeric(specificity_min) || length(specificity_min) != 1 ||
        is.na(specificity_min) || specificity_min < 0 || specificity_min > 1) {
      stop("`specificity_min` must be a numeric value between 0 and 1.", call. = FALSE)
    }
  }

  if (identical(positive_label, negative_label)) {
    stop("`positive_label` and `negative_label` must be distinct.", call. = FALSE)
  }

  # ---- Prepare Data ----
  dat_prep <- .prepare_ncvroc_data(data, outcome_name, item_names)
  y <- dat_prep[[outcome_name]]

  if (!all(y %in% c(positive_label, negative_label))) {
    stop("Outcome contains values other than positive_label and negative_label.", call. = FALSE)
  }
  y <- ifelse(y == positive_label, 1L, 0L)
  if (sum(y == 1L) == 0L || sum(y == 0L) == 0L) {
    stop("Outcome must contain both positive and negative cases.", call. = FALSE)
  }
  n_total <- length(y)
  n_items_total <- length(item_names)

  # ---- Validate cv_method, repeats, and folds ----
  if (cv_method == "loocv") {
    if (!is.null(repeats) && repeats > 1) {
      stop("LOOCV is deterministic and unique; repeats > 1 is not supported.", call. = FALSE)
    }
    repeats <- 1L
    stratified <- FALSE
    n_pos <- sum(y == 1L)
    n_neg <- sum(y == 0L)
    if (min(n_pos, n_neg) < 2L) {
      stop("LOOCV requires at least 2 positive and 2 negative cases so each training fold contains both classes for cutoff optimization.", call. = FALSE)
    }
    requested_folds <- folds
    effective_folds <- n_total
  } else {
    if (is.null(folds) || !is.numeric(folds) || length(folds) != 1L || is.na(folds) || !is.finite(folds) || folds != as.integer(folds)) {
      stop("`folds` must be an integer-valued scalar.", call. = FALSE)
    }
    folds <- as.integer(folds)

    if (folds < 2L || folds >= n_total) {
      stop("Under cv_method = 'kfold', folds must satisfy 2 <= folds < n. Use cv_method = 'loocv' for leave-one-out cross-validation.", call. = FALSE)
    }
    requested_folds <- folds
    effective_folds <- folds
  }

  # ---- Resolve model_sizes ----
  sizes <- .resolve_model_sizes(
    model_sizes = model_sizes,
    min_items   = min_items,
    max_items   = max_items,
    item_count  = item_count,
    n_available = n_items_total
  )

  # ---- Parallel Mode Resolution ----
  parallel_mode <- .resolve_parallel_mode(
    parallel,
    context = "ordinary_cv",
    allowed = c("none", "threads", "chunks")
  )

  if (!is.null(n_workers)) {
    if (!is.numeric(n_workers) || length(n_workers) != 1 ||
        is.na(n_workers) || n_workers <= 0 || n_workers != as.integer(n_workers)) {
      stop("`n_workers` must be a positive integer or NULL.", call. = FALSE)
    }
    n_workers <- as.integer(n_workers)
  }

  if (parallel_mode == "threads" && engine != "Rcpp") {
    stop("`parallel = 'threads'` requires `engine = 'Rcpp'`.", call. = FALSE)
  }

  n_workers_res <- if (parallel_mode != "none") {
    .resolve_n_workers(parallel = parallel_mode, n_workers = n_workers)
  } else {
    1L
  }

  # ---- Build CV Folds ----
  cv_folds <- .build_cv_folds(
    y          = y,
    cv_method  = cv_method,
    folds      = folds,
    repeats    = repeats,
    stratified = stratified,
    seed       = seed
  )

  n_folds_total   <- length(cv_folds)
  effective_folds <- n_folds_total %/% repeats
  requested_folds <- folds

  # Total combinations
  total_combos <- .count_total_combos_cross_size(n_items_total, sizes)

  # ---- Prepare Data Matrix for Evaluators & Planner ----
  dat_mat <- as.matrix(dat_prep[, item_names, drop = FALSE])
  mode(dat_mat) <- "double"
  y_int <- as.integer(y)

  # ---- Execution Planner (Phase 1.2) ----
  execution_plan_metadata <- NULL
  if (tuning != "off") {
    planner_outcome <- .planner_cross_size_cv_controller(
      data_matrix          = dat_mat,
      y                    = y_int,
      item_names           = item_names,
      sizes                = sizes,
      cv_folds             = cv_folds,
      folds                = effective_folds,
      repeats              = repeats,
      stratified           = stratified,
      cv_method            = cv_method,
      selection_metric     = selection_metric,
      cutoff_method        = cutoff_method,
      sensitivity_min      = sensitivity_min,
      specificity_min      = specificity_min,
      engine               = engine,
      tuning               = tuning,
      manual_parallel_mode = parallel_mode,
      manual_n_workers     = n_workers
    )
    if (isTRUE(planner_outcome$warn)) {
      warning("Automatic execution planning failed; falling back to manual execution configuration.", call. = FALSE)
    }
    parallel_mode           <- planner_outcome$plan$parallel[[1L]]
    n_workers_res           <- as.integer(planner_outcome$plan$n_workers[[1L]])
    execution_plan_metadata <- planner_outcome$metadata
  }

  # =========================================================================
  # STRATEGY 1: Exact AUC Search & Fast Path
  # =========================================================================
  if (selection_metric == "auc") {
    auc_res <- .select_cross_size_auc_exact(
      data               = dat_prep,
      outcome_name       = outcome_name,
      item_names         = item_names,
      sizes              = sizes,
      top_n              = top_n,
      cutoff_method      = cutoff_method,
      prefer_fewer_items = prefer_fewer_items,
      engine             = engine,
      parallel_mode      = parallel_mode,
      n_workers_res      = n_workers_res,
      sensitivity_min    = sensitivity_min,
      specificity_min    = specificity_min,
      cv_folds           = cv_folds,
      repeats            = repeats,
      y                  = y
    )

    best_candidate <- auc_res$best_candidate
    best_cv        <- auc_res$best_cv
    best_agg       <- auc_res$best_agg
    ranking_df     <- auc_res$ranking_df

    final_model_df <- data.frame(
      items                  = best_candidate$items,
      n_items                = best_candidate$n_items,
      cv_auc                 = best_agg$summary$mean[best_agg$summary$metric == "auc"],
      cv_sensitivity         = best_agg$summary$mean[best_agg$summary$metric == "sensitivity"],
      cv_specificity         = best_agg$summary$mean[best_agg$summary$metric == "specificity"],
      cv_youden              = best_agg$summary$mean[best_agg$summary$metric == "youden"],
      cv_accuracy            = best_agg$summary$mean[best_agg$summary$metric == "accuracy"],
      cv_ppv                 = best_agg$summary$mean[best_agg$summary$metric == "ppv"],
      cv_npv                 = best_agg$summary$mean[best_agg$summary$metric == "npv"],
      cv_cutoff_mean         = best_agg$cv_cutoff_distribution$mean,
      cv_cutoff_sd           = best_agg$cv_cutoff_distribution$sd,
      final_full_data_cutoff = best_candidate$cutoff,
      selection_metric       = "auc",
      sensitivity            = best_agg$summary$mean[best_agg$summary$metric == "sensitivity"],
      specificity            = best_agg$summary$mean[best_agg$summary$metric == "specificity"],
      accuracy               = best_agg$summary$mean[best_agg$summary$metric == "accuracy"],
      ppv                    = best_agg$summary$mean[best_agg$summary$metric == "ppv"],
      npv                    = best_agg$summary$mean[best_agg$summary$metric == "npv"],
      n_positive             = sum(y == 1L),
      n_negative             = sum(y == 0L),
      stringsAsFactors       = FALSE
    )

    if (ci) {
      final_model_df <- add_performance_cis(
        results    = final_model_df,
        data       = dat_prep,
        outcome    = outcome_name,
        conf_level = conf_level
      )
    }

    final_rank_df <- data.frame(
      rank            = ranking_df$rank,
      items           = ranking_df$items,
      n_items         = ranking_df$n_items,
      auc             = ranking_df$auc,
      cutoff          = ranking_df$cutoff,
      stringsAsFactors = FALSE
    )
    if (!is.null(sensitivity_min) || !is.null(specificity_min)) {
      final_rank_df$cv_sensitivity  <- ranking_df$cv_sensitivity
      final_rank_df$cv_specificity  <- ranking_df$cv_specificity
      final_rank_df$cv_youden       <- ranking_df$cv_youden
      final_rank_df$cv_accuracy     <- ranking_df$cv_accuracy
      final_rank_df$constraint_pass <- ranking_df$constraint_pass
    } else {
      final_rank_df$sensitivity <- ranking_df$sensitivity
      final_rank_df$specificity <- ranking_df$specificity
      final_rank_df$youden      <- ranking_df$youden
      final_rank_df$accuracy    <- ranking_df$accuracy
      final_rank_df$ppv         <- ranking_df$ppv
      final_rank_df$npv         <- ranking_df$npv
    }

    size_summary_df <- do.call(rbind, lapply(sizes, function(s) {
      sub_df <- final_rank_df[final_rank_df$n_items == s, , drop = FALSE]
      n_cand_total <- choose(n_items_total, s)
      if (nrow(sub_df) > 0L) {
        best_row <- sub_df[1, , drop = FALSE]
        data.frame(
          n_items             = s,
          n_candidates_total  = n_cand_total,
          n_evaluated_in_top  = nrow(sub_df),
          best_items          = best_row$items,
          best_metric_value   = best_row$auc,
          stringsAsFactors    = FALSE
        )
      } else {
        data.frame(
          n_items             = s,
          n_candidates_total  = n_cand_total,
          n_evaluated_in_top  = 0L,
          best_items          = NA_character_,
          best_metric_value   = NA_real_,
          stringsAsFactors    = FALSE
        )
      }
    }))

    settings_obj <- list(
      outcome_name       = outcome_name,
      item_names         = item_names,
      model_sizes        = sizes,
      cv_method          = cv_method,
      selection_metric   = selection_metric,
      cutoff_method      = cutoff_method,
      sensitivity_min    = sensitivity_min,
      specificity_min    = specificity_min,
      requested_folds    = requested_folds,
      folds_requested    = requested_folds,
      effective_folds    = effective_folds,
      repeats            = repeats,
      stratified         = stratified,
      seed               = seed,
      prefer_fewer_items = prefer_fewer_items,
      engine             = engine,
      parallel           = parallel_mode,
      n_workers          = n_workers_res,
      threads_per_worker = 1L,
      ci                 = ci,
      conf_level         = conf_level
    )
    if (!is.null(execution_plan_metadata)) {
      settings_obj$execution_plan <- execution_plan_metadata
    }

    return(structure(
      list(
        final_selected_model   = final_model_df,
        candidate_ranking      = final_rank_df,
        model_size_summary     = size_summary_df,
        oof_predictions        = best_cv$oof_predictions,
        fold_results           = best_cv$fold_results,
        repeat_metrics         = best_agg$repeat_metrics,
        cv_performance         = best_agg$summary,
        cv_cutoff_distribution = best_agg$cv_cutoff_distribution,
        final_full_data_cutoff = best_candidate$cutoff,
        model_sizes            = sizes,
        total_combinations     = total_combos,
        cv_method              = cv_method,
        settings               = settings_obj
      ),
      class = "cross_size_cv_result"
    ))
  }

  # =========================================================================
  # STRATEGY 2: Cutoff-dependent Streaming Exact Evaluation (C++ accelerated)
  # =========================================================================
  running_buffer <- NULL
  metric_col <- paste0("cv_", selection_metric)

  test_indices_0based <- lapply(cv_folds, function(f_idx) as.integer(f_idx - 1L))
  sens_min <- if (is.null(sensitivity_min)) -1.0 else as.numeric(sensitivity_min)
  spec_min <- if (is.null(specificity_min)) -1.0 else as.numeric(specificity_min)

  # Build combo plan
  combo_plan <- list()
  cum_offset <- 0L
  for (si in seq_along(sizes)) {
    s <- sizes[si]
    n_combos_s <- choose(n_items_total, s)
    for (gi in seq_len(n_combos_s)) {
      combo_0based <- .combination_unrank(n_items_total, s, gi - 1L)
      combo_plan[[length(combo_plan) + 1L]] <- list(
        items_0based = combo_0based,
        items_names  = item_names[combo_0based + 1L],
        s            = s,
        gi           = gi,
        cum_offset   = cum_offset
      )
    }
    cum_offset <- cum_offset + n_combos_s
  }

  n_plan <- length(combo_plan)

  if (parallel_mode == "chunks" && n_plan > 1L && n_workers_res > 1L) {
    .NCVROC_ROUTING_COUNTERS$chunk_psock_count <- .NCVROC_ROUTING_COUNTERS$chunk_psock_count + 1L
    .NCVROC_ROUTING_COUNTERS$strategy2_chunks_count <- .NCVROC_ROUTING_COUNTERS$strategy2_chunks_count + 1L

    chunk_batch_size <- max(1L, ceiling(n_plan / (n_workers_res * 4L)))
    n_batches <- ceiling(n_plan / chunk_batch_size)
    blocks <- vector("list", n_batches)
    for (b in seq_len(n_batches)) {
      b_start <- (b - 1L) * chunk_batch_size + 1L
      b_end   <- min(n_plan, b * chunk_batch_size)
      blocks[[b]] <- combo_plan[b_start:b_end]
    }

    actual_workers <- min(as.integer(n_workers_res), as.integer(n_batches))
    cl <- parallel::makePSOCKcluster(actual_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    lib_paths <- .libPaths()
    parallel::clusterExport(cl, "lib_paths", envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(lib_paths)
      if (requireNamespace("NCVROC", quietly = TRUE)) {
        try(library(NCVROC), silent = TRUE)
      }
      NULL
    })

    ns <- asNamespace("NCVROC")
    available_symbols <- intersect(.CROSS_SIZE_OUTER_EXPORT_SYMBOLS, ls(ns, all.names = TRUE))
    parallel::clusterExport(cl, varlist = available_symbols, envir = ns)

    parallel::clusterExport(
      cl,
      varlist = c("dat_mat", "y_int", "test_indices_0based", "n_folds_total", "repeats", "cutoff_method",
                  "sens_min", "spec_min", "metric_col", "top_n", "prefer_fewer_items"),
      envir = environment()
    )

    eval_block_psock <- function(block_items) {
      indices_list <- lapply(block_items, function(x) x$items_0based)
      res_cpp <- evaluate_combos_cv_cpp(
        x               = dat_mat,
        y               = y_int,
        combo_indices   = indices_list,
        test_indices    = test_indices_0based,
        n_folds         = n_folds_total,
        repeats         = repeats,
        cutoff_method   = cutoff_method,
        sensitivity_min = sens_min,
        specificity_min = spec_min,
        num_threads     = 1L
      )
      local_buf <- NULL
      valid_idx <- which(res_cpp$valid == 1L)
      if (length(valid_idx) > 0L) {
        for (vi in valid_idx) {
          info <- block_items[[vi]]
          cand_row <- data.frame(
            items                  = format_items(info$items_names),
            n_items                = info$s,
            cv_auc                 = res_cpp$cv_auc[vi],
            cv_sensitivity         = res_cpp$cv_sensitivity[vi],
            cv_specificity         = res_cpp$cv_specificity[vi],
            cv_youden              = res_cpp$cv_youden[vi],
            cv_accuracy            = res_cpp$cv_accuracy[vi],
            cv_ppv                 = res_cpp$cv_ppv[vi],
            cv_npv                 = res_cpp$cv_npv[vi],
            cv_cutoff_mean         = res_cpp$cv_cutoff_mean[vi],
            cv_cutoff_sd           = res_cpp$cv_cutoff_sd[vi],
            final_full_data_cutoff = res_cpp$final_full_data_cutoff[vi],
            .global_combo_index    = info$cum_offset + info$gi,
            stringsAsFactors       = FALSE
          )
          local_buf <- .update_running_top_n(
            buffer             = local_buf,
            candidate_row      = cand_row,
            metric_col         = metric_col,
            top_n              = top_n,
            prefer_fewer_items = prefer_fewer_items
          )
        }
      }
      local_buf
    }

    block_results <- parallel::parLapply(cl, blocks, eval_block_psock)
    for (b_res in block_results) {
      if (!is.null(b_res) && nrow(b_res) > 0L) {
        for (r_i in seq_len(nrow(b_res))) {
          running_buffer <- .update_running_top_n(
            buffer             = running_buffer,
            candidate_row      = b_res[r_i, , drop = FALSE],
            metric_col         = metric_col,
            top_n              = top_n,
            prefer_fewer_items = prefer_fewer_items
          )
        }
      }
    }
  } else {
    # Threads / Serial C++ batch processing
    block_size <- 2000L
    n_blocks <- ceiling(n_plan / block_size)
    blocks <- vector("list", n_blocks)
    for (b in seq_len(n_blocks)) {
      b_start <- (b - 1L) * block_size + 1L
      b_end   <- min(n_plan, b * block_size)
      blocks[[b]] <- combo_plan[b_start:b_end]
    }

    if (parallel_mode == "threads") {
      .NCVROC_ROUTING_COUNTERS$threads_count <- .NCVROC_ROUTING_COUNTERS$threads_count + 1L
      .NCVROC_ROUTING_COUNTERS$strategy2_threads_count <- .NCVROC_ROUTING_COUNTERS$strategy2_threads_count + 1L
      cpp_threads <- as.integer(n_workers_res)
    } else {
      .NCVROC_ROUTING_COUNTERS$strategy2_serial_count <- .NCVROC_ROUTING_COUNTERS$strategy2_serial_count + 1L
      cpp_threads <- 1L
    }

    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = n_blocks, style = 3)
      on.exit(close(pb), add = TRUE)
    }

    for (b in seq_len(n_blocks)) {
      if (progress) utils::setTxtProgressBar(pb, b)
      block_items <- blocks[[b]]
      indices_list <- lapply(block_items, function(x) x$items_0based)
      res_cpp <- evaluate_combos_cv_cpp(
        x               = dat_mat,
        y               = y_int,
        combo_indices   = indices_list,
        test_indices    = test_indices_0based,
        n_folds         = n_folds_total,
        repeats         = repeats,
        cutoff_method   = cutoff_method,
        sensitivity_min = sens_min,
        specificity_min = spec_min,
        num_threads     = cpp_threads
      )
      valid_idx <- which(res_cpp$valid == 1L)
      if (length(valid_idx) > 0L) {
        for (vi in valid_idx) {
          info <- block_items[[vi]]
          cand_row <- data.frame(
            items                  = format_items(info$items_names),
            n_items                = info$s,
            cv_auc                 = res_cpp$cv_auc[vi],
            cv_sensitivity         = res_cpp$cv_sensitivity[vi],
            cv_specificity         = res_cpp$cv_specificity[vi],
            cv_youden              = res_cpp$cv_youden[vi],
            cv_accuracy            = res_cpp$cv_accuracy[vi],
            cv_ppv                 = res_cpp$cv_ppv[vi],
            cv_npv                 = res_cpp$cv_npv[vi],
            cv_cutoff_mean         = res_cpp$cv_cutoff_mean[vi],
            cv_cutoff_sd           = res_cpp$cv_cutoff_sd[vi],
            final_full_data_cutoff = res_cpp$final_full_data_cutoff[vi],
            .global_combo_index    = info$cum_offset + info$gi,
            stringsAsFactors       = FALSE
          )
          running_buffer <- .update_running_top_n(
            buffer             = running_buffer,
            candidate_row      = cand_row,
            metric_col         = metric_col,
            top_n              = top_n,
            prefer_fewer_items = prefer_fewer_items
          )
        }
      }
    }
  }

  if (is.null(running_buffer) || nrow(running_buffer) == 0L) {
    stop(sprintf(
      "No candidate models satisfied the specified OOF constraints (sensitivity_min = %s, specificity_min = %s).",
      if (is.null(sensitivity_min)) "NULL" else as.character(sensitivity_min),
      if (is.null(specificity_min)) "NULL" else as.character(specificity_min)
    ), call. = FALSE)
  }

  best_candidate <- running_buffer[1, , drop = FALSE]
  best_items_vec <- .parse_itemset(best_candidate$items)

  # Run CV on selected best model only to generate complete oof_predictions and fold_results
  best_cv <- .run_fixed_model_cv(
    itemset       = best_items_vec,
    data          = dat_prep,
    y             = y,
    cv_folds      = cv_folds,
    cutoff_method = cutoff_method
  )
  best_agg <- .aggregate_oof_metrics(
    oof_df       = best_cv$oof_predictions,
    repeats      = repeats,
    fold_results = best_cv$fold_results
  )

  final_model_df <- data.frame(
    items                  = best_candidate$items,
    n_items                = best_candidate$n_items,
    cv_auc                 = best_agg$summary$mean[best_agg$summary$metric == "auc"],
    cv_sensitivity         = best_agg$summary$mean[best_agg$summary$metric == "sensitivity"],
    cv_specificity         = best_agg$summary$mean[best_agg$summary$metric == "specificity"],
    cv_youden              = best_agg$summary$mean[best_agg$summary$metric == "youden"],
    cv_accuracy            = best_agg$summary$mean[best_agg$summary$metric == "accuracy"],
    cv_ppv                 = best_agg$summary$mean[best_agg$summary$metric == "ppv"],
    cv_npv                 = best_agg$summary$mean[best_agg$summary$metric == "npv"],
    cv_cutoff_mean         = best_agg$cv_cutoff_distribution$mean,
    cv_cutoff_sd           = best_agg$cv_cutoff_distribution$sd,
    final_full_data_cutoff = best_candidate$final_full_data_cutoff,
    selection_metric       = selection_metric,
    sensitivity            = best_agg$summary$mean[best_agg$summary$metric == "sensitivity"],
    specificity            = best_agg$summary$mean[best_agg$summary$metric == "specificity"],
    accuracy               = best_agg$summary$mean[best_agg$summary$metric == "accuracy"],
    ppv                    = best_agg$summary$mean[best_agg$summary$metric == "ppv"],
    npv                    = best_agg$summary$mean[best_agg$summary$metric == "npv"],
    n_positive             = sum(y == 1L),
    n_negative             = sum(y == 0L),
    stringsAsFactors       = FALSE
  )

  if (ci) {
    final_model_df <- add_performance_cis(
      results    = final_model_df,
      data       = dat_prep,
      outcome    = outcome_name,
      conf_level = conf_level
    )
  }

  running_buffer$rank <- seq_len(nrow(running_buffer))
  keep_n <- min(top_n, nrow(running_buffer))
  ranking_df <- utils::head(running_buffer, keep_n)

  final_rank_df <- data.frame(
    rank           = ranking_df$rank,
    items          = ranking_df$items,
    n_items        = ranking_df$n_items,
    cv_auc         = ranking_df$cv_auc,
    cv_sensitivity = ranking_df$cv_sensitivity,
    cv_specificity = ranking_df$cv_specificity,
    cv_youden      = ranking_df$cv_youden,
    cv_accuracy    = ranking_df$cv_accuracy,
    cv_ppv         = ranking_df$cv_ppv,
    cv_npv         = ranking_df$cv_npv,
    cv_cutoff_mean = ranking_df$cv_cutoff_mean,
    cv_cutoff_sd   = ranking_df$cv_cutoff_sd,
    stringsAsFactors = FALSE
  )

  size_summary_df <- do.call(rbind, lapply(sizes, function(s) {
    sub_df <- final_rank_df[final_rank_df$n_items == s, , drop = FALSE]
    n_cand_total <- choose(n_items_total, s)
    if (nrow(sub_df) > 0L) {
      best_row <- sub_df[1, , drop = FALSE]
      metric_val <- best_row[[paste0("cv_", selection_metric)]]
      data.frame(
        n_items             = s,
        n_candidates_total  = n_cand_total,
        n_evaluated_in_top  = nrow(sub_df),
        best_items          = best_row$items,
        best_metric_value   = metric_val,
        stringsAsFactors    = FALSE
      )
    } else {
      data.frame(
        n_items             = s,
        n_candidates_total  = n_cand_total,
        n_evaluated_in_top  = 0L,
        best_items          = NA_character_,
        best_metric_value   = NA_real_,
        stringsAsFactors    = FALSE
      )
    }
  }))

  settings_obj <- list(
    outcome_name       = outcome_name,
    item_names         = item_names,
    model_sizes        = sizes,
    cv_method          = cv_method,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    requested_folds    = requested_folds,
    folds_requested    = requested_folds,
    effective_folds    = effective_folds,
    repeats            = repeats,
    stratified         = stratified,
    seed               = seed,
    prefer_fewer_items = prefer_fewer_items,
    engine             = engine,
    parallel           = parallel_mode,
    n_workers          = n_workers_res,
    threads_per_worker = 1L,
    ci                 = ci,
    conf_level         = conf_level
  )
  if (!is.null(execution_plan_metadata)) {
    settings_obj$execution_plan <- execution_plan_metadata
  }

  structure(
    list(
      final_selected_model   = final_model_df,
      candidate_ranking      = final_rank_df,
      model_size_summary     = size_summary_df,
      oof_predictions        = best_cv$oof_predictions,
      fold_results           = best_cv$fold_results,
      repeat_metrics         = best_agg$repeat_metrics,
      cv_performance         = best_agg$summary,
      cv_cutoff_distribution = best_agg$cv_cutoff_distribution,
      final_full_data_cutoff = best_candidate$final_full_data_cutoff,
      model_sizes            = sizes,
      total_combinations     = total_combos,
      cv_method              = cv_method,
      settings               = settings_obj
    ),
    class = "cross_size_cv_result"
  )
}

#' Print method for cross_size_cv_result
#'
#' @param x A `cross_size_cv_result` object.
#' @param ... Additional arguments.
#' @export
print.cross_size_cv_result <- function(x, ...) {
  cat("\n=== Cross-Size Ordinary Cross-Validation (NCVROC) ===\n")
  cat("Method:           ", if (x$cv_method == "loocv") "Leave-One-Out (LOOCV)" else paste0(x$settings$repeats, "x repeated ", x$settings$effective_folds, "-fold CV"), "\n")
  cat("Model sizes:      ", paste(x$model_sizes, collapse = ", "), " (Total candidates: ", format(x$total_combinations, big.mark = ","), ")\n", sep = "")
  cat("Selection metric: ", x$settings$selection_metric, "\n")
  cat("Cutoff rule:      ", x$settings$cutoff_method, "\n")
  if (!is.null(x$settings$sensitivity_min)) {
    cat(sprintf("Constraint:        OOF sensitivity >= %.2f\n", x$settings$sensitivity_min))
  }
  if (!is.null(x$settings$specificity_min)) {
    cat(sprintf("Constraint:        OOF specificity >= %.2f\n", x$settings$specificity_min))
  }

  cat("\n--- Final Selected Model ---\n")
  m <- x$final_selected_model
  cat("  Items (", m$n_items, "): ", m$items, "\n", sep = "")
  cat(sprintf("  OOF AUC:        %.4f\n", m$cv_auc))
  cat(sprintf("  OOF Sensitivity:%.4f\n", m$cv_sensitivity))
  cat(sprintf("  OOF Specificity:%.4f\n", m$cv_specificity))
  cat(sprintf("  OOF Youden:     %.4f\n", m$cv_youden))
  cat(sprintf("  OOF Accuracy:   %.4f\n", m$cv_accuracy))
  cat(sprintf("  CV Cutoff:      mean = %.2f (SD = %.2f)\n", m$cv_cutoff_mean, m$cv_cutoff_sd))
  cat(sprintf("  Final Cutoff:   %.2f (Full-data deployment)\n", x$final_full_data_cutoff))

  cat("\n--- Candidate Ranking (Top ", nrow(x$candidate_ranking), ") ---\n", sep = "")
  print(utils::head(x$candidate_ranking, 10), row.names = FALSE)
  if (nrow(x$candidate_ranking) > 10) {
    cat("... (", nrow(x$candidate_ranking) - 10, " more candidates in ranking table)\n", sep = "")
  }
  cat("\n")

  invisible(x)
}

#' Summary method for cross_size_cv_result
#'
#' @param object A `cross_size_cv_result` object.
#' @param ... Additional arguments.
#' @export
summary.cross_size_cv_result <- function(object, ...) {
  print(object, ...)
}

#' Cross-size leave-one-out cross-validation and model selection
#'
#' Dedicated wrapper for combinatorial model selection using leave-one-out
#' cross-validation (LOOCV) across multiple candidate model sizes.
#'
#' @inheritParams cross_size_cv
#'
#' @return An S3 object of class \code{"cross_size_cv_result"}.
#'
#' @examples
#' \dontrun{
#' data(sample_data_bin)
#' res <- cross_size_loocv(
#'   data       = sample_data_bin,
#'   outcome    = "y",
#'   items      = paste0("Q", 1:5),
#'   model_sizes= 1:3,
#'   selection_metric = "auc"
#' )
#' print(res)
#' }
#'
#' @export
cross_size_loocv <- function(data,
                             outcome,
                             items,
                             model_sizes        = NULL,
                             min_items          = 1,
                             max_items          = 4,
                             item_count         = NULL,
                             selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                             cutoff_method      = c("youden", "closest_topleft"),
                             sensitivity_min    = NULL,
                             specificity_min    = NULL,
                             top_n              = 20,
                             prefer_fewer_items = TRUE,
                             positive_label     = 1,
                             negative_label     = 0,
                             engine             = c("Rcpp", "R"),
                             parallel           = FALSE,
                             n_workers          = NULL,
                             ci                 = FALSE,
                             conf_level         = 0.95,
                             progress           = interactive()) {
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)
  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  engine           <- match.arg(engine)

  cross_size_cv(
    data               = data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = model_sizes,
    min_items          = min_items,
    max_items          = max_items,
    item_count         = item_count,
    cv_method          = "loocv",
    folds              = nrow(data),
    repeats            = 1,
    stratified         = FALSE,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    top_n              = top_n,
    prefer_fewer_items = prefer_fewer_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = parallel,
    n_workers          = n_workers,
    ci                 = ci,
    conf_level         = conf_level,
    seed               = NULL,
    progress           = progress
  )
}

#' Within-size cross-validated item-set sum score model selection
#'
#' Evaluates all combinations of a single specified model size (\code{item_count})
#' using ordinary \eqn{k}-fold cross-validation and selects the best model.
#'
#' @details
#' Unlike \code{\link{cv_sum_roc}()}, which evaluates the cross-validated performance
#' of a single fixed set of items, \code{cv_select_sum_roc()} performs combinatorial
#' model selection across all \eqn{\binom{P}{K}} subsets of size \code{item_count}.
#'
#' @param data A data.frame containing the outcome and item columns.
#' @param outcome Name of the binary outcome column (bare symbol or character string).
#' @param items Item columns to evaluate (bare range e.g. `Q1:Q10`, bare names in `c()`,
#'   character vector, or numeric positions).
#' @param item_count Integer, exact number of items in each candidate model (e.g. `3`).
#' @param folds Integer, number of folds for k-fold CV (default 5).
#' @param repeats Integer, number of independent repeats for k-fold CV (default 1).
#' @param stratified Logical, maintain class balance across folds (default `TRUE`).
#' @param selection_metric Metric for candidate ranking and model selection:
#'   `"auc"` (default), `"youden"`, `"sensitivity"`, `"specificity"`, or `"accuracy"`.
#' @param cutoff_method Cutoff selection rule: `"youden"` (default) or `"closest_topleft"`.
#' @param sensitivity_min Optional minimum OOF sensitivity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param specificity_min Optional minimum OOF specificity threshold (numeric in `[0, 1]`, default `NULL`).
#' @param top_n Integer, number of top candidates to return in `candidate_ranking` (default 20).
#' @param prefer_fewer_items Logical, prefer smaller models on ties (default `TRUE`).
#' @param positive_label Value indicating positive class (default 1).
#' @param negative_label Value indicating negative class (default 0).
#' @param engine Combinatorial computation engine: `"Rcpp"` (default) or `"R"`.
#' @param parallel Parallel mode: `FALSE` / `"none"` (default), `"threads"`, or `"chunks"`.
#' @param n_workers Integer, number of workers or threads (default `NULL` = auto).
#' @param ci Logical, compute confidence intervals for full-data apparent metrics of final model (default `FALSE`).
#' @param conf_level Numeric, confidence level (default 0.95).
#' @param seed Integer, random seed for reproducible fold generation.
#' @param progress Logical, show progress bar (default `interactive()`).
#'
#' @return An S3 object of class \code{c("cv_select_sum_roc_result", "cross_size_cv_result")}.
#'
#' @seealso \code{\link{cv_sum_roc}} for fixed-model evaluation,
#'   \code{\link{loocv_select_sum_roc}} for leave-one-out selection,
#'   \code{\link{cross_size_cv}} for multi-size selection.
#'
#' @examples
#' \dontrun{
#' data(sample_data_bin)
#' res <- cv_select_sum_roc(
#'   data       = sample_data_bin,
#'   outcome    = "y",
#'   items      = paste0("Q", 1:5),
#'   item_count = 3,
#'   folds      = 5,
#'   selection_metric = "auc"
#' )
#' print(res)
#' }
#'
#' @export
cv_select_sum_roc <- function(data,
                              outcome,
                              items,
                              item_count         = 3,
                              folds              = 5,
                              repeats            = 1,
                              stratified         = TRUE,
                              selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                              cutoff_method      = c("youden", "closest_topleft"),
                              sensitivity_min    = NULL,
                              specificity_min    = NULL,
                              top_n              = 20,
                              prefer_fewer_items = TRUE,
                              positive_label     = 1,
                              negative_label     = 0,
                              engine             = c("Rcpp", "R"),
                              parallel           = FALSE,
                              n_workers          = NULL,
                              ci                 = FALSE,
                              conf_level         = 0.95,
                              seed               = NULL,
                              progress           = interactive()) {
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)
  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  engine           <- match.arg(engine)

  if (is.null(item_count) || !is.numeric(item_count) || length(item_count) != 1L ||
      is.na(item_count) || !is.finite(item_count) || item_count != as.integer(item_count) || item_count < 1L) {
    stop("`item_count` must be a positive integer-valued scalar.", call. = FALSE)
  }
  item_count <- as.integer(item_count)

  if (item_count > length(item_names)) {
    stop(sprintf("`item_count` (%d) cannot exceed the number of available items (%d).",
                 item_count, length(item_names)), call. = FALSE)
  }

  res <- cross_size_cv(
    data               = data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = item_count,
    cv_method          = "kfold",
    folds              = folds,
    repeats            = repeats,
    stratified         = stratified,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    top_n              = top_n,
    prefer_fewer_items = prefer_fewer_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = parallel,
    n_workers          = n_workers,
    ci                 = ci,
    conf_level         = conf_level,
    seed               = seed,
    progress           = progress
  )

  res$settings$selection_scope <- "within_size"
  res$settings$item_count      <- item_count
  class(res) <- c("cv_select_sum_roc_result", "cross_size_cv_result")

  res
}

#' Within-size leave-one-out cross-validated item-set sum score model selection
#'
#' Evaluates all combinations of a single specified model size (\code{item_count})
#' using leave-one-out cross-validation (LOOCV) and selects the best model.
#'
#' @details
#' Unlike \code{\link{loocv_sum_roc}()}, which evaluates the leave-one-out performance
#' of a single fixed set of items, \code{loocv_select_sum_roc()} performs combinatorial
#' model selection across all \eqn{\binom{P}{K}} subsets of size \code{item_count}.
#'
#' @inheritParams cv_select_sum_roc
#'
#' @return An S3 object of class \code{c("loocv_select_sum_roc_result", "cv_select_sum_roc_result", "cross_size_cv_result")}.
#'
#' @seealso \code{\link{loocv_sum_roc}} for fixed-model evaluation,
#'   \code{\link{cv_select_sum_roc}} for k-fold selection,
#'   \code{\link{cross_size_loocv}} for multi-size LOOCV selection.
#'
#' @examples
#' \dontrun{
#' data(sample_data_bin)
#' res <- loocv_select_sum_roc(
#'   data       = sample_data_bin,
#'   outcome    = "y",
#'   items      = paste0("Q", 1:5),
#'   item_count = 3,
#'   selection_metric = "auc"
#' )
#' print(res)
#' }
#'
#' @export
loocv_select_sum_roc <- function(data,
                                 outcome,
                                 items,
                                 item_count         = 3,
                                 selection_metric   = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
                                 cutoff_method      = c("youden", "closest_topleft"),
                                 sensitivity_min    = NULL,
                                 specificity_min    = NULL,
                                 top_n              = 20,
                                 prefer_fewer_items = TRUE,
                                 positive_label     = 1,
                                 negative_label     = 0,
                                 engine             = c("Rcpp", "R"),
                                 parallel           = FALSE,
                                 n_workers          = NULL,
                                 ci                 = FALSE,
                                 conf_level         = 0.95,
                                 progress           = interactive()) {
  env <- parent.frame()
  outcome_name <- .resolve_outcome(substitute(outcome), env)
  item_names   <- .resolve_items(data, substitute(items), env)
  selection_metric <- match.arg(selection_metric)
  cutoff_method    <- match.arg(cutoff_method)
  engine           <- match.arg(engine)

  if (is.null(item_count) || !is.numeric(item_count) || length(item_count) != 1L ||
      is.na(item_count) || !is.finite(item_count) || item_count != as.integer(item_count) || item_count < 1L) {
    stop("`item_count` must be a positive integer-valued scalar.", call. = FALSE)
  }
  item_count <- as.integer(item_count)

  if (item_count > length(item_names)) {
    stop(sprintf("`item_count` (%d) cannot exceed the number of available items (%d).",
                 item_count, length(item_names)), call. = FALSE)
  }

  res <- cross_size_cv(
    data               = data,
    outcome            = outcome_name,
    items              = item_names,
    model_sizes        = item_count,
    cv_method          = "loocv",
    folds              = nrow(data),
    repeats            = 1,
    stratified         = FALSE,
    selection_metric   = selection_metric,
    cutoff_method      = cutoff_method,
    sensitivity_min    = sensitivity_min,
    specificity_min    = specificity_min,
    top_n              = top_n,
    prefer_fewer_items = prefer_fewer_items,
    positive_label     = positive_label,
    negative_label     = negative_label,
    engine             = engine,
    parallel           = parallel,
    n_workers          = n_workers,
    ci                 = ci,
    conf_level         = conf_level,
    seed               = NULL,
    progress           = progress
  )

  res$settings$selection_scope <- "within_size"
  res$settings$item_count      <- item_count
  class(res) <- c("loocv_select_sum_roc_result", "cv_select_sum_roc_result", "cross_size_cv_result")

  res
}

#' Print method for cv_select_sum_roc_result
#'
#' @param x A `cv_select_sum_roc_result` object.
#' @param ... Additional arguments.
#' @export
print.cv_select_sum_roc_result <- function(x, ...) {
  is_loocv <- inherits(x, "loocv_select_sum_roc_result") || (x$cv_method == "loocv")
  cat("\n=== Within-Size CV Sum-Score Selection (NCVROC) ===\n")
  cat("Selection scope:  Within-size (", x$settings$item_count, " items)\n", sep = "")
  cat("Method:           ", if (is_loocv) "Leave-One-Out (LOOCV)" else paste0(x$settings$repeats, "x repeated ", x$settings$effective_folds, "-fold CV"), "\n")
  cat("Selection metric: ", x$settings$selection_metric, "\n")
  cat("Cutoff rule:      ", x$settings$cutoff_method, "\n")
  if (!is.null(x$settings$sensitivity_min)) {
    cat(sprintf("Constraint:        OOF sensitivity >= %.2f\n", x$settings$sensitivity_min))
  }
  if (!is.null(x$settings$specificity_min)) {
    cat(sprintf("Constraint:        OOF specificity >= %.2f\n", x$settings$specificity_min))
  }

  cat("\n--- Selected Best Model (Size ", x$settings$item_count, ") ---\n", sep = "")
  m <- x$final_selected_model
  cat("  Items:          ", m$items, "\n", sep = "")
  cat(sprintf("  OOF AUC:        %.4f\n", m$cv_auc))
  cat(sprintf("  OOF Sensitivity:%.4f\n", m$cv_sensitivity))
  cat(sprintf("  OOF Specificity:%.4f\n", m$cv_specificity))
  cat(sprintf("  OOF Youden:     %.4f\n", m$cv_youden))
  cat(sprintf("  OOF Accuracy:   %.4f\n", m$cv_accuracy))
  cat(sprintf("  CV Cutoff:      mean = %.2f (SD = %.2f)\n", m$cv_cutoff_mean, m$cv_cutoff_sd))
  cat(sprintf("  Final Cutoff:   %.2f (Full-data deployment)\n", x$final_full_data_cutoff))

  cat("\n--- Top Candidate Models (Size ", x$settings$item_count, ") ---\n", sep = "")
  print(utils::head(x$candidate_ranking, 10), row.names = FALSE)
  if (nrow(x$candidate_ranking) > 10) {
    cat("... (", nrow(x$candidate_ranking) - 10, " more candidates in ranking table)\n", sep = "")
  }
  cat("\n")

  invisible(x)
}

#' Summary method for cv_select_sum_roc_result
#'
#' @param object A `cv_select_sum_roc_result` object.
#' @param ... Additional arguments.
#' @export
summary.cv_select_sum_roc_result <- function(object, ...) {
  print(object, ...)
}
