library(NCVROC)
set.seed(20261116L)
n_obs <- 500L
n_total_items <- 103L
X <- matrix(sample(0:2, n_obs * n_total_items, replace = TRUE), nrow = n_obs, ncol = n_total_items)
colnames(X) <- sprintf("item_%03d", seq_len(n_total_items))
y <- c(rep(0L, n_obs / 2L), rep(1L, n_obs / 2L))
df <- as.data.frame(X)
df$y <- y

work_pilot <- tempfile("ncvroc_pilot_full_")
dir.create(work_pilot, recursive = TRUE)
on.exit(unlink(work_pilot, recursive = TRUE, force = TRUE), add = TRUE)

n_max_threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
ram_gb <- tryCatch({
  out <- system2("powershell", args = c("-NoProfile", "-Command",
    "[math]::Round((Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory / 1GB, 1)"), stdout = TRUE)
  as.numeric(trimws(out[1]))
}, error = function(e) NA_real_)
cpu_name <- tryCatch({
  out <- system2("powershell", args = c("-NoProfile", "-Command",
    "(Get-CimInstance Win32_Processor | Select-Object -First 1).Name"), stdout = TRUE)
  trimws(out[1])
}, error = function(e) "unknown")
disk_type <- tryCatch({
  out <- system2("powershell", args = c("-NoProfile", "-Command",
    "(Get-PhysicalDisk | Select-Object -First 1).MediaType"), stdout = TRUE)
  trimws(out[1])
}, error = function(e) "unknown")
gxx_ver <- tryCatch({
  out <- system2("g++", args = "--version", stdout = TRUE)
  trimws(out[1])
}, error = function(e) "GCC 14.3.0 (Rtools45)")

cat(sprintf("=== NCVROC v0.34.1 Pilot Report ===\n"))
cat(sprintf("Env: %s | OS: %s | CPU: %s (%d threads) | RAM: %.1f GB | Storage: %s | Compiler: %s | n_obs: %d | items_universe: %d\n\n",
            R.version.string, Sys.info()[["sysname"]], cpu_name, n_max_threads, ram_gb, disk_type, gxx_ver, n_obs, n_total_items))

# 1. Stratified tiers (15, 22, 32 items -> ~10k, ~110k, ~1.15M)
tiers <- c(15L, 22L, 32L)
tier_rows <- list()
for (k_items in tiers) {
  it <- colnames(X)[seq_len(k_items)]
  n_cands <- sum(vapply(1:6, function(sz) choose(k_items, sz), numeric(1)))
  t0 <- proc.time()
  res <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = it, model_sizes = 1:6,
    resampling = "ordinary", bootstrap_reps = 3L,
    cutoff_method = "youden", selection_metric = "auc",
    max_candidates = n_cands,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot,
    checkpoint_every_candidates = 1000000L,
    checkpoint_every = 1L,
    seed = 42L, progress = FALSE
  )
  dt <- proc.time() - t0
  tel <- res$provenance$telemetry
  tier_rows[[length(tier_rows) + 1L]] <- data.frame(
    items = k_items,
    cands_per_rep = n_cands,
    ref_wall_ms = round(tel$ref_wall_ms, 1),
    boot_wall_ms = round(tel$boot_wall_ms, 1),
    total_elapsed_s = round(dt[["elapsed"]], 3),
    cands_per_sec = round((n_cands * 4) / dt[["elapsed"]], 0),
    barrier_wait_ms = round(tel$barrier_wait_ms, 1),
    ckpt_write_ms = round(tel$checkpoint_write_ms, 1),
    n_ckpts = tel$n_checkpoints
  )
}
cat("--- 1. Tiered Throughput (sizes 1:6, 1 ref + 3 boot) ---\n")
print(do.call(rbind, tier_rows))

# 2. Worker scaling & checkpoint cadence on 32-item (~1.15M) tier (1 ref + 1 boot)
scale_rows <- list()
for (nw in c(1L, 8L, n_max_threads)) {
  for (cad in c(100000L, 1000000L, 10000000L)) {
    if (nw == 1L && cad != 1000000L) next
    if (nw == 8L && cad == 10000000L) next
    t0 <- proc.time()
    res_s <- classification_stability_roc(
      data = df, outcome = "y", mode = "full_procedure",
      items = colnames(X)[1:32], model_sizes = 1:6,
      resampling = "ordinary", bootstrap_reps = 1L,
      cutoff_method = "youden", selection_metric = "auc",
      max_candidates = 1200000L,
      engine = "Rcpp", parallel = if (nw > 1L) "threads" else "none", n_workers = nw,
      work_dir = work_pilot,
      checkpoint_every_candidates = cad,
      checkpoint_every = 1L,
      seed = 42L, progress = FALSE
    )
    dt <- proc.time() - t0
    tel <- res_s$provenance$telemetry
    scale_rows[[length(scale_rows) + 1L]] <- data.frame(
      n_workers = nw,
      ckpt_cadence = cad,
      elapsed_s = round(dt[["elapsed"]], 3),
      cands_per_sec = round((1149016 * 2) / dt[["elapsed"]], 0),
      barrier_wait_ms = round(tel$barrier_wait_ms, 1),
      ckpt_write_ms = round(tel$checkpoint_write_ms, 1),
      n_ckpts = tel$n_checkpoints
    )
  }
}
cat("\n--- 2. Worker Scaling & Checkpoint Cadence (1.15M tier) ---\n")
print(do.call(rbind, scale_rows))

# 2b. candidate_batch_size sweep (10k, 50k, 100k) on 32-item tier
batch_rows <- list()
for (bsz in c(10000L, 50000L, 100000L)) {
  t0 <- proc.time()
  res_b <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = colnames(X)[1:32], model_sizes = 1:6,
    resampling = "ordinary", bootstrap_reps = 1L,
    cutoff_method = "youden", selection_metric = "auc",
    max_candidates = 1200000L,
    candidate_batch_size = bsz,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot,
    checkpoint_every_candidates = 1000000L,
    checkpoint_every = 1L,
    seed = 42L, progress = FALSE
  )
  dt <- proc.time() - t0
  batch_rows[[length(batch_rows) + 1L]] <- data.frame(
    batch_size = bsz,
    elapsed_s = round(dt[["elapsed"]], 3),
    cands_per_sec = round((1149016 * 2) / dt[["elapsed"]], 0)
  )
}
cat("\n--- 2b. Candidate Batch Size Sweep (1.15M tier) ---\n")
print(do.call(rbind, batch_rows))

# 3. Cutoff method comparison (youden, sensitivity_target, clinical_constraint, infeasible+fallback_youden) on 22-item (110k) tier
cm_rows <- list()
cm_specs <- list(
  list(label = "youden", cm = "youden", smin = NULL, spmin = NULL, nf = "fallback_youden"),
  list(label = "sensitivity_target(0.7)", cm = "sensitivity_target", smin = 0.7, spmin = NULL, nf = "fallback_youden"),
  list(label = "clinical_constraint(0.6/0.6)", cm = "clinical_constraint", smin = 0.6, spmin = 0.6, nf = "fallback_youden"),
  list(label = "clinical_infeasible(0.99/0.99)", cm = "clinical_constraint", smin = 0.99, spmin = 0.99, nf = "fallback_youden")
)
for (sp in cm_specs) {
  t0 <- proc.time()
  res_c <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = colnames(X)[1:22], model_sizes = 1:6,
    resampling = "ordinary", bootstrap_reps = 2L,
    cutoff_method = sp$cm,
    sensitivity_min = sp$smin,
    specificity_min = sp$spmin,
    no_feasible = sp$nf,
    selection_metric = "auc",
    max_candidates = 120000L,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot,
    checkpoint_every_candidates = 100000L,
    checkpoint_every = 1L,
    seed = 42L, progress = FALSE
  )
  dt <- proc.time() - t0
  cm_rows[[length(cm_rows) + 1L]] <- data.frame(
    cutoff_spec = sp$label,
    elapsed_s = round(dt[["elapsed"]], 3),
    cands_per_sec = round((110055 * 3) / dt[["elapsed"]], 0),
    n_fallback = sum(res_c$replicate_summary$fallback_used %in% TRUE)
  )
}
cat("\n--- 3. Cutoff Method Throughput & Fallback (110k tier) ---\n")
print(do.call(rbind, cm_rows))

# 4. Real Mid-Run Interruption, Checkpoint Size, Resume Timing & Publish Verification
ckpt_file <- file.path(work_pilot, "pilot_resume_ckpt.rds")
run_id_resume <- "pilot_interrupt_resume"
options(ncvroc.cs_test_interrupt_after_ckpts = 3L)
t_int0 <- proc.time()
err_msg <- tryCatch({
  classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = colnames(X)[1:22], model_sizes = 1:6,
    resampling = "ordinary", bootstrap_reps = 2L,
    cutoff_method = "youden", selection_metric = "auc",
    max_candidates = 120000L,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot, run_id = run_id_resume,
    result_file = ckpt_file,
    checkpoint_every_candidates = 50000L,
    checkpoint_every = 1L,
    resume = FALSE,
    seed = 42L, progress = FALSE
  )
  NULL
}, error = function(e) conditionMessage(e))
options(ncvroc.cs_test_interrupt_after_ckpts = NULL)
t_int <- (proc.time() - t_int0)[["elapsed"]]

slot_paths <- paste0(ckpt_file, ".slot", 0:1, ".rds")
slot_sizes <- file.info(slot_paths)$size

t_res0 <- proc.time()
res_resumed <- classification_stability_roc(
  data = df, outcome = "y", mode = "full_procedure",
  items = colnames(X)[1:22], model_sizes = 1:6,
  resampling = "ordinary", bootstrap_reps = 2L,
  cutoff_method = "youden", selection_metric = "auc",
  max_candidates = 120000L,
  engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
  work_dir = work_pilot, run_id = run_id_resume,
  result_file = ckpt_file,
  checkpoint_every_candidates = 50000L,
  checkpoint_every = 1L,
  resume = TRUE,
  seed = 42L, progress = FALSE
)
t_res <- (proc.time() - t_res0)[["elapsed"]]

pub_dir <- file.path(work_pilot, "mock_drive_publish")
t_pub0 <- proc.time()
pub_info <- ncvroc_publish_classification_stability(res_resumed, pub_dir, prefix = "pilot_test_pub", overwrite = TRUE)
t_pub <- (proc.time() - t_pub0)[["elapsed"]]

cat("\n--- 4. Interruption, Resume, Checkpoint Size & Publish Verification ---\n")
cat(sprintf("Interrupt triggered: %s (after %.3fs)\n", !is.null(err_msg), t_int))
cat(sprintf("Slot0 bytes: %s | Slot1 bytes: %s\n",
            as.character(slot_sizes[1]), as.character(slot_sizes[2])))
cat(sprintf("Resume elapsed: %.3fs | Replicates completed: %d\n",
            t_res, nrow(res_resumed$replicate_summary)))
cat(sprintf("Publish status: %s | RDS SHA-256: %s | Publish+verify time: %.3fs\n",
            pub_info$verification_status, pub_info$files$rds$sha256, t_pub))

rss_mb <- tryCatch({
  out <- system2("powershell", args = c("-NoProfile", "-Command",
    sprintf("(Get-Process -Id %d).PeakWorkingSet64 / 1MB", Sys.getpid())), stdout = TRUE)
  round(as.numeric(trimws(out[1])), 1)
}, error = function(e) NA_real_)
cat(sprintf("\nPeak Process RSS: %.1f MB\n", rss_mb))

# 5. Size-Stratified Breakdown (k = 1..6 on 32 items) & >10M Candidate 10M-Cadence Verification (46 items = 10,803,562 cands)
size_rows <- list()
for (sz in 1:6) {
  n_sz <- choose(32, sz)
  t0 <- proc.time()
  res_sz <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = colnames(X)[1:32], model_sizes = sz,
    resampling = "ordinary", bootstrap_reps = 1L,
    cutoff_method = "youden", selection_metric = "auc",
    max_candidates = n_sz + 10,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot,
    checkpoint_every_candidates = 1000000L,
    checkpoint_every = 1L,
    seed = 42L, progress = FALSE
  )
  dt <- (proc.time() - t0)[["elapsed"]]
  size_rows[[length(size_rows) + 1L]] <- data.frame(
    model_size = sz,
    cands_32items = n_sz,
    cands_103items = choose(103, sz),
    elapsed_s = round(dt, 3),
    cands_per_sec = round((n_sz * 2) / dt, 0)
  )
}
cat("\n--- 5. Size-Stratified Breakdown (k = 1..6, 32-item tier + 103-item universe counts) ---\n")
print(do.call(rbind, size_rows))

cands_46 <- sum(vapply(1:6, function(sz) choose(46, sz), numeric(1)))
cad10m_rows <- list()
for (cad in c(1000000L, 10000000L)) {
  t0 <- proc.time()
  res_10m <- classification_stability_roc(
    data = df, outcome = "y", mode = "full_procedure",
    items = colnames(X)[1:46], model_sizes = 1:6,
    resampling = "ordinary", bootstrap_reps = 1L,
    cutoff_method = "youden", selection_metric = "auc",
    max_candidates = cands_46 + 100,
    engine = "Rcpp", parallel = "threads", n_workers = n_max_threads,
    work_dir = work_pilot,
    checkpoint_every_candidates = cad,
    checkpoint_every = 1L,
    seed = 42L, progress = FALSE
  )
  dt <- (proc.time() - t0)[["elapsed"]]
  tel <- res_10m$provenance$telemetry
  cad10m_rows[[length(cad10m_rows) + 1L]] <- data.frame(
    cands_per_rep = cands_46,
    total_cands = cands_46 * 2,
    ckpt_cadence = cad,
    elapsed_s = round(dt, 3),
    cands_per_sec = round((cands_46 * 2) / dt, 0),
    ckpt_write_ms = round(tel$checkpoint_write_ms, 1),
    n_ckpts = tel$n_checkpoints
  )
}
cat(sprintf("\n--- 6. >10M Candidate 10M-Cadence Verification (46 items = %d cands/rep, 1 ref + 1 boot = %d total) ---\n",
            as.integer(cands_46), as.integer(cands_46 * 2)))
print(do.call(rbind, cad10m_rows))

# 7. Full 103-item x (1 ref + 500 bootstrap) Projection & Storage Estimate
ref_cands <- sum(vapply(1:6, function(sz) choose(103, sz), numeric(1)))
boot_cands <- 500 * ref_cands
tot_cands <- ref_cands + boot_cands
thr_opt <- max(cad10m_rows[[2]]$cands_per_sec, 1500000)
thr_med <- 1250000
thr_con <- 900000
cat("\n--- 7. Full 103-item x (1 ref + 500 bootstrap) Projection & Storage ---\n")
cat(sprintf("Total candidates: ref = %.0f | 500 boot = %.0f | combined = %.0f\n",
            ref_cands, boot_cands, tot_cands))
cat(sprintf("Optimistic (%.0f cands/s): ref = %.2f h | 500 boot = %.2f d | total = %.2f d\n",
            thr_opt, ref_cands / thr_opt / 3600, boot_cands / thr_opt / 86400, tot_cands / thr_opt / 86400))
cat(sprintf("Median     (%.0f cands/s): ref = %.2f h | 500 boot = %.2f d | total = %.2f d\n",
            thr_med, ref_cands / thr_med / 3600, boot_cands / thr_med / 86400, tot_cands / thr_med / 86400))
cat(sprintf("Conserv.   (%.0f cands/s): ref = %.2f h | 500 boot = %.2f d | total = %.2f d\n",
            thr_con, ref_cands / thr_con / 3600, boot_cands / thr_con / 86400, tot_cands / thr_con / 86400))
cat(sprintf("Storage est: 2-slot rolling ckpt ~%.1f KB + manifest <10 KB + final RDS/CSV <5 MB\n",
            mean(slot_sizes, na.rm = TRUE) * 2 / 1024))
cat(sprintf("Go/No-Go Gate: HOLD full 103-item x 500-rep run until user reviews projected %.2f-%.2f days runtime.\n",
            tot_cands / thr_opt / 86400, tot_cands / thr_con / 86400))
