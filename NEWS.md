# NCVROC 0.20.0

## Execution planning

* Added automatic execution planning for computationally intensive exhaustive
  ROC workflows.
* The planner benchmarks legal execution configurations and selects an
  execution backend from serial, multithreaded, outer-fold parallel, and
  hybrid strategies.
* Backend benchmarking is triggered by a deterministic workload threshold,
  rather than by a predicted total runtime.
* Small workloads below the threshold skip backend benchmarking and use the
  safe baseline execution path.
* Nested-CV planner pilots use a bounded, proportionally allocated candidate
  sample across requested model sizes, preserving the original candidate
  space and model-size structure.
* Planner decisions affect execution strategy only. Candidate generation,
  fold construction, ranking, tie-breaking, cutoff selection, predictions,
  and final model selection are unchanged.

## Parallel execution

* Improved multithreaded exhaustive candidate evaluation using RcppParallel.
* Added bounded resource-plan benchmarking for nested cross-validation,
  including multithreaded, outer-fold PSOCK, and hybrid execution modes.
* Exact statistical equivalence is preserved across supported execution
  backends.
* Production PSOCK execution continues to use standard R parallel
  infrastructure; no custom worker protocol or package-namespace
  modification is used.

## Progress reporting

* Added immediate progress initialization for long-running nested
  cross-validation jobs.
* Serial and multithreaded nested workflows now report fine-grained
  candidate-level progress within each outer fold.
* Candidate evaluation is processed in bounded batches to provide responsive
  progress updates without changing candidate identity or statistical
  results.
* Outer-fold and hybrid PSOCK execution report truthful outer-task progress
  without artificial interpolation.
* Progress output reports observed elapsed time only; ETA estimates are not
  shown.

## Exhaustive search engine

* Added streaming Top-N support to the C++ exhaustive-search path, reducing
  unnecessary result materialization during large combinatorial searches.
* Added rank-bounded nested candidate evaluation while preserving canonical
  combination-index identity and exact unranking behavior.
* Candidate-space enumeration remains exhaustive. No statistical screening or
  implicit pruning of candidate models is introduced.

## Reliability and validation

* Expanded regression coverage for candidate identity, streaming Top-N
  selection, nested execution planning, progress reporting, and
  serial/parallel statistical invariance.
* Verified agreement of selected models, outer-fold results, summaries, and
  outer predictions between serial and multithreaded nested-CV execution.
* Clean installed-package tests confirm that outer and hybrid PSOCK workflows
  remain statistically equivalent to serial execution.
* Repository and generated R/Rcpp interfaces were normalized and checked for
  accidental line-ending-only changes.

---

# NCVROC 0.19.0

## Execution planning and truthful progress reporting

- **Measured execution planning for flat workloads**:
  - `exhaustive_sum_roc()` and `cross_size_cv()` can use deterministic pilot
    measurements to evaluate legal `none`, `threads`, and `chunks` plans.
  - `tuning = "auto"` considers a resource sweep only at an empirical
    180-second serial-runtime gate. `tuning = "always"` requests the same safe
    planning process; `tuning = "off"` preserves the manual configuration.
  - Benchmark overhead is capped at 5% of the estimated runtime. Suitable
    two-point pilot measurements use an empirical affine runtime estimate.
  - The selected plan is a measured near-best configuration, not a claim of
    universally best performance. Metadata is stored canonically in
    `execution_plan`.

- **Safe nested-workflow fallback**:
  - `nested_sum_roc()` and `cross_size_nested_cv()` perform only
    candidate-bounded runtime probing in v0.19.0.
  - A full nested resource sweep is not performed when a rank-bounded nested
    evaluator is unavailable; the manual/default configuration is retained and
    the reason is recorded in `execution_plan`. This capability is deferred.

- **Observation-only progress**:
  - Compiled serial and C++-threaded exhaustive evaluation, and the blocked
    cutoff-dependent `cross_size_cv()` path, report exact completed candidate
    counts at observable block boundaries.
  - Opaque PSOCK backends use `start_completion` metadata with
    `progress_unit = "none"`: they do not advertise percentage progress or ETA.
  - Approximate ETA is emitted only from observed completed work. `progress = FALSE`
    is silent. Progress does not alter candidate ordering, fold/repeat structure,
    statistics, final refitting, or RNG state.

---

# NCVROC 0.18.0

## Automatic execution planning

- **Adaptive hardware- and workload-aware execution planning**:
  - Automatically selects a measured near-best execution plan from pilot micro-benchmarking across all core combinatorial workflows:
    - `exhaustive_sum_roc()`
    - `cross_size_cv()` (and related wrappers `cross_size_loocv()`, `cv_select_sum_roc()`, `loocv_select_sum_roc()`)
    - `nested_sum_roc()`
    - `cross_size_nested_cv()`
  - **Tuning modes** (`tuning = c("off", "auto", "always")`):
    - `"off"`: Preserves user-specified manual execution configuration (`parallel`, `n_workers`, `threads_per_worker`) without runtime probing.
    - `"auto"`: Probes and benchmarks candidate execution plans only when estimated serial runtime exceeds 30 seconds.
    - `"always"`: Actively benchmarks candidate execution plans for all non-degenerate workloads.
  - **Micro-pilot benchmark design**:
    - Flat combinatorial searches: Evaluates an evenly-spaced candidate subset using an active CPU budget cap (`min(detected_cores, 8L)`).
    - Nested cross-validation searches: Strict candidate-count reduction only; preserves the complete outer fold, inner fold, and repeat structure intact to prevent structural or class-balance extrapolation bias.
  - **Multi-stage decision rule**:
    - Identifies the fastest observed benchmarked plan (`min(median_elapsed)`).
    - Qualifies all candidate plans within a 5% near-best threshold (`median_elapsed <= fastest * 1.05`).
    - Breaks ties by resource economy (`resource_count`: fewer total worker/thread allocations).
    - Breaks further ties by backend simplicity priority:
      - Flat workloads: `none` (serial) > `threads` (C++ multi-threading) > `chunks` (PSOCK socket workers).
      - Nested workloads: `none` (serial) > `threads` (inner C++ threads) > `outer` (PSOCK outer folds) > `chunks` (PSOCK chunks) > `hybrid` (PSOCK outer folds + inner C++ threads).
    - Resolves remaining ties deterministically by `median_elapsed` and lexicographical `plan_id`.
  - **Fault tolerance**:
    - Automatic error-handling fallback hierarchy: PSOCK cluster setup failure gracefully falls back to multi-threaded C++ or serial evaluation without aborting the workload.
  - **Unified planning metadata schema**:
    - Compact, serializable `execution_plan` metadata list attached as an attribute or result list element detailing the selected backend, worker allocations, benchmark timings, and decision rationale.
    - Dedicated formatter `format(plan)` / `print(plan)` for inspection.

## Progress reporting & approximate ETA

- **Observation-only progress reporting**:
  - Enhanced `progress` reporting with lightweight, approximate remaining-time (ETA) estimation during long-running combinatorial evaluations.
  - Displays progress bar and periodic ETA messages (at most once every 30 seconds after $\ge 3$ completed units and $\ge 30$ seconds elapsed) on main-process observable iteration loops.
  - Opaque parallel dispatch boundaries (PSOCK cluster calls, C++ multi-threading) report concise start and completion messages.
  - **Zero statistical or execution impact**:
    - Progress reporting is strictly observation-only; does not modify candidate evaluation order, fold partitioning, cutoff selection, ranking, OOF predictions, final model refitting, or RNG determinism (`.Random.seed` is identical).
    - `progress = FALSE` retains minimal overhead with no `proc.time()` calls or console output.
  - **Safe cleanup contract**:
    - Idempotent `finish()` vs. `close()` separation: normal completion advances progress to 100% and closes, while error or user-interrupt handlers (`on.exit`) safely close the display at the current progress position without falsely reporting 100% completion.

---

# NCVROC 0.17.0

## Candidate-level stability & optimism analysis

- **New `candidate_stability_roc()` function**:
  - Comprehensive stability and optimism audit for unweighted sum-score candidate models across data perturbations.
  - **Mode 1 (Fixed Candidates)**: Evaluates a user-supplied named list of candidate item sets (`candidate_sets`).
  - **Mode 2 (Combinatorial Screening)**: Exact Stage-1 apparent performance screening across full combinatorial model sizes (`items`, `model_sizes`) with Stage-2 resampling evaluation of the top `screen_top_n` candidates.
  - **Resampling engines**:
    - Repeated $K$-fold cross-validation (`resampling = "repeated_cv"`).
    - Non-parametric bootstrap with training-vs-original test evaluation for Efron-style optimism estimation (`bootstrap_test = "original"`).
    - Non-parametric bootstrap with out-of-bag test evaluation (`bootstrap_test = "oob"`).
  - **Comprehensive stability metrics**:
    - Apparent vs. resampled test performance distributions (mean, SD, median, IQR, min, max).
    - Candidate-level apparent-minus-resampled performance gap and bootstrap optimism correction.
    - Rank stability across resamples (mean rank, rank SD, rank IQR).
    - Selection frequency across resamples partitioning into winner selection, `no_feasible_candidate`, and `invalid_evaluation`.
    - Clinical constraint feasibility pass rates (`sensitivity_min`, `specificity_min`, joint pass rate).
  - **High-performance computing**: Pure C++ multithreading via `RcppParallel` (`parallel = "threads"`) and multi-process PSOCK parallelism (`parallel = "chunks"`).
  - **Dedicated S3 visualization**: `plot.candidate_stability_result()` supporting `"rank_stability"`, `"performance"`, `"selection_frequency"`, and `"constraint_stability"` with pure base R graphics.

---

# NCVROC 0.16.0

## New cross-validation APIs

- **Explicit ordinary K-fold and LOOCV model-selection APIs**:
  - `cross_size_loocv()`: Dedicated wrapper for cross-size model selection using Leave-One-Out Cross-Validation (LOOCV).
  - `cv_select_sum_roc()`: Dedicated wrapper for within-size combinatorial model selection using $K$-fold cross-validation.
  - `loocv_select_sum_roc()`: Dedicated wrapper for within-size combinatorial model selection using LOOCV.

## Generalized cross-validation

- **Arbitrary $K$-fold and deterministic LOOCV**:
  - Generalized $K$-fold CV engine to support arbitrary $2 \le K < N$ folds.
  - Strict input validation: $K \ge N$ is rejected with an informative error directing users to `cv_method = "loocv"`.
  - Formalized LOOCV execution with single-repeat enforcement (`repeats = 1`), minority class guard ($\ge 2$ positive and negative cases), and fold-level AUC handling.
  - Full support for arbitrary and discontinuous `model_sizes` (e.g. `c(1, 3, 5)`).

## Exact cutoff-dependent search & C++ acceleration

- **C++ accelerated CV evaluator (`evaluate_combos_cv_cpp`)**:
  - Pure C++ multi-threaded evaluation engine powered by `RcppParallel::parallelFor` for cutoff-dependent model selection (`selection_metric = "youden"`, `"accuracy"`, `"sensitivity"`, `"specificity"`).
  - Bounded-memory design combining combinatorial block unranking with thread-local frequency buffers and exact frequency table updates.
  - Full support for both `parallel = "threads"` and `parallel = "chunks"` with zero silent serial fallback.

## Statistical semantics & aggregation policies

- **Repeat-level metric aggregation (No $N \times R$ pooling)**:
  - In repeated $K$-fold CV, performance metrics are computed independently on each repeat's $N$ out-of-fold predictions.
  - Summary metrics report the arithmetic mean and sample standard deviation across the $R$ repeat-level metrics; pooling $N \times R$ observations into a single artificial sample is strictly avoided.
  - S3 result objects include `$repeat_metrics` (per-repeat table) and `$cv_performance` (mean and SD summary).
- **Fixed sum-score AUC mathematical identity**:
  - For a fixed unweighted sum-score candidate, the raw score has no fitted parameters. Therefore its pooled out-of-fold score vector is identical to the full-data score vector, and the corresponding AUC is identical to the apparent full-data AUC (theoretical repeat $\text{SD} = 0$).

## Validation and correctness

- **Deterministic candidate ranking**:
  - Strict tie-breaking chain: (1) primary selection metric, (2) `prefer_fewer_items = TRUE` (fewer items), (3) `.global_combo_index` (deterministic combination order).
  - Mathematical exactness verified across `none`, `threads`, and `chunks` backends against independent brute-force reference loops.

---

# NCVROC 0.15.0

## New features

- **Unified cross-validation & model selection framework**:
  - Added `cv_sum_roc()` for fixed-model k-fold and repeated k-fold cross-validation.
  - Added `loocv_sum_roc()` for fixed-model leave-one-out cross-validation (LOOCV).
  - Added `cross_size_cv()` for ordinary cross-validation model selection across multiple model sizes.
  - Added `cross_size_nested_cv()` for cross-size nested cross-validation evaluating the generalization performance of the model-selection procedure.
  - Added `compare_cv_selection()` for comparing non-nested selected-model performance with nested selection-procedure performance, quantifying empirical `selection_optimism = ordinary - nested`.

- **Exact AUC mathematical identity optimization**:
  - Leverages the exact mathematical identity for fixed unweighted sum scores: the pooled out-of-fold score vector is identical to the full-data score vector, hence pooled OOF AUC equals full-data apparent AUC.
  - Uses this exact identity to avoid redundant fold-wise AUC recomputation across combinations, while strictly evaluating fold-dependent classification metrics with training-derived cutoffs.

- **Universal logical block streams & external merge architecture**:
  - Implemented bounded block streams and hierarchical $K$-way external merge sort (`fan_in = 16L`, `block_size = 2000L`) for constrained AUC search (`sensitivity_min` / `specificity_min`).
  - Guarantees strict bounded memory (proportional to block size) across any number of candidate models or merge passes, with zero full-space R memory materialization.

- **Comprehensive parallel routing**:
  - `cross_size_cv()` supports `parallel = "none"`, `"threads"`, and `"chunks"`.
  - `cross_size_nested_cv()` supports `parallel = "none"`, `"outer"`, `"threads"`, `"chunks"`, and `"hybrid"`.
  - `compare_cv_selection()` automatically maps parallel modes safely between ordinary and nested CV without nested cluster oversubscription.

---

# NCVROC 0.14.0

## New features

- **Hybrid nested-CV parallelism**:
  - Added `parallel = "hybrid"` to `ncvroc()`, `nested_sum_roc()`, and `ncvroc_config()`.
  - Added `threads_per_worker`, defining the requested C++ thread count inside each outer-fold PSOCK worker.
  - Hybrid execution is restricted to nested CV and requires `engine = "Rcpp"`; exhaustive-only APIs reject it.
  - In hybrid mode, `n_workers` is the requested outer PSOCK worker count. When it is `NULL`, the outer worker count is resolved first; `threads_per_worker` is then capped to the remaining physical CPU budget and `_R_CHECK_LIMIT_CORES_` (maximum total 2 during CRAN checks).
  - Hybrid results record requested and effective outer-worker, per-worker-thread, and total-parallelism values in `settings`.
  - Final exhaustive search in `ncvroc()` runs after outer workers finish and uses `threads_per_worker` C++ threads in the main process.
  - Exactness tests cover both cutoff methods, high-tie data, selected models, predictions, metrics, and deterministic seeds.

---

# NCVROC 0.13.0

## New features

- **C++ multi-threading for in-memory combinatorial exhaustive search**:
  - Added support for `parallel = "threads"` across `exhaustive_sum_roc()`, `roc_bruteforce()`, `roc_bf()`, `ncvroc()`, `nested_sum_roc()`, `ncvroc_config()`, and `fit_final_sum_scale()`.
  - Evaluates combinations in parallel within the main R process using native C++ threads via `RcppParallel::parallelFor` with call-local task arenas.
  - Features zero socket IPC overhead, no process-level duplication of input data, and low thread-startup overhead.
  - Guarantees deterministic exact results identical to serial execution across all metrics and cutoff methods.
  - Enforces single-level concurrency guarantees, eliminating oversubscription and thread explosion under nested cross-validation.
  - Built with `RcppParallel` for broad cross-platform portability.

---

# NCVROC 0.12.0

## New features

- **Chunk-level parallelization for combinatorial exhaustive search**:
  - Added support for `parallel = "chunks"` across `exhaustive_sum_roc()`, `roc_bruteforce()`, `roc_bf()`, `ncvroc()`, `nested_sum_roc()`, `ncvroc_config()`, and `run_ncvroc()`.
  - Enables evaluating large combinatorial spaces ($O(\binom{M}{K})$ candidate models) in parallel across socket workers (`parallel::makePSOCKcluster`).
  - **Persistent cluster reuse**: In `nested_sum_roc(..., parallel = "chunks")`, a single PSOCK cluster is created once outside the outer CV loop and reused across all outer folds, eliminating repeated cluster startup/teardown overhead.
  - **One-time per-worker serialization**: Large immutable datasets and search parameters are exported to workers once during cluster setup rather than re-serialized on every chunk task.
  - **Deterministic exact tie-breaking**: Candidates track their 1-based global combination enumeration index (`.global_combo_index`), guaranteeing identical ranking and selection to serial execution even across chunk boundaries.
  - **Atomic RDS writing**: Chunked RDS files are written to temporary files and atomically renamed within the same directory.
  - **Context-aware parallel mode resolution**: `parallel = TRUE` resolves to `"outer"` in nested CV functions (`ncvroc`, `nested_sum_roc`) and `"chunks"` in exhaustive search functions (`roc_bruteforce`, `exhaustive_sum_roc`), while `parallel = FALSE` remains the default.

---

# NCVROC 0.11.1

## New features

- **Outer fold parallelization**:
  - Added `parallel` and `n_workers` arguments to `nested_sum_roc()`, `ncvroc()`, `ncvroc_config()`, and `run_ncvroc()`.
  - Enables concurrent evaluation of outer cross-validation folds across multi-core CPUs using socket clusters (`parallel::makePSOCKcluster`).
  - Automatic core detection (`n_workers = NULL`) respecting system cores, outer fold count, and CRAN check limits (`_R_CHECK_LIMIT_CORES_`).
  - Strict statistical equivalence to serial execution guaranteed by stratified fold generation and deterministic per-fold seed offsetting.

---

# NCVROC 0.11.0

## New features

- **Confidence interval (CI) estimation**:
  - **AUC 95% CI**: Non-parametric asymptotic method by DeLong et al. (1988), computed efficiently from score frequency distributions in $O(K)$ time.
  - **Classification metrics 95% CI**: Exact binomial confidence intervals by Clopper and Pearson (1934) for sensitivity, specificity, accuracy, positive predictive value (PPV), and negative predictive value (NPV) via Beta distribution quantiles (`stats::qbeta`).
  - Added `ci` (logical) and `conf_level` (default `0.95`) arguments across `fit_final_sum_scale()`, `ncvroc()`, and `exhaustive_sum_roc()`.
  - Added output columns: `auc_lower`, `auc_upper`, `sensitivity_lower`, `sensitivity_upper`, `specificity_lower`, `specificity_upper`, `accuracy_lower`, `accuracy_upper`, `ppv_lower`, `ppv_upper`, `npv_lower`, `npv_upper`.
  - In `exhaustive_sum_roc()`, CIs are calculated only after ranking and top-$N$ candidate selection to preserve high combinatorial search speed.
  - Added clear documentation distinguishing apparent full-data sampling uncertainty CIs for fixed models from nested cross-validation out-of-fold performance variability.

---

# NCVROC 0.10.2

## Bug fixes

- Fixed `ncvroc_results()` on chunked RDS storage returning per-chunk `rank`
  values and rownames instead of globally sequential numbering. The `rank`
  column is now reassigned to `seq_len(nrow(dat))` and rownames are reset
  after filtering, sorting, and top-N selection. This affects all three
  chunked retrieval paths: streaming top-N, full load, and empty result.

---

# NCVROC 0.10.1

## Bug fixes

- Fixed `results_dir` being silently ignored in the chunked RDS code path of
  `.evaluate_final_exhaustive()`. When `results_storage = "rds"` and
  `results_dir` was explicitly set, chunk files were always written to
  `tempdir()` instead of the user-specified directory. Now respects
  `results_dir` for both single RDS and chunked RDS paths.
- Fixed incorrect `AUTO_MEMORY_LIMIT` reference in test comment
  (`5,000,000` → `100,000`).

## New tests

- Added 6 tests verifying `results_dir` handling across storage backends:
  chunked_rds + explicit results_dir, chunked_rds + NULL results_dir,
  single_rds + explicit results_dir, and cache-enabled chunked mode.
- Tests cover both `ncvroc()` and `roc_bruteforce()`.

---

# NCVROC 0.10.0

## New features

- New `results_storage = "auto"` (default): automatically selects RAM for
  small searches and chunked disk storage for large searches, preventing
  out-of-memory errors. Explicit `"memory"`, `"rds"`, and `"none"` modes
  remain available.
- Chunked evaluation: for search spaces exceeding 100,000 combinations,
  candidates are evaluated in chunks of `chunk_size` (default 200,000) and
  written to individual RDS files, with peak memory independent of total
  combinations.
- New `cache = "reuse"` parameter for `ncvroc()` and `roc_bruteforce()`:
  caches complete results to `cache_dir` by content hash. Subsequent runs
  with identical inputs and `cache = "reuse"` return the cached result
  instantly. `cache = "refresh"` forces re-computation.
- Lexicographic combination unranking: direct 0-based rank → column-index
  mapping matching `combn()` column order, used by both the R and C++
  chunked evaluation engines.
- New C++ chunk evaluator (`evaluate_combos_cpp_chunk`) that unranks and
  evaluates combinations directly without building the full index list.
- `ncvroc_results()` gains `allow_full_load` parameter. When storage is
  chunked and `top_n` is finite, results are streamed chunk-by-chunk with
  constant memory. `top_n = NULL` with chunked storage requires
  `allow_full_load = TRUE`.
- `results_dir` default changed from `getwd()` to `tempdir()` (CRAN-safe).
- All default writes go to `tempdir()`.

## Internal changes

- `.resolve_global_combination_rank()`: maps global rank to (k, rank_within_k).
- `.enumerate_combinations_chunk()`: enumerates a slice via combinadic.
- `.compute_cache_key()`: deterministic hash of normalized data + all
  analysis parameters using `serialize()` + `tools::md5sum()`.
- `.save_cache()` / `.load_cache()`: atomic cache writes via
  `.building-<pid>/` staging directory.
- `.evaluate_final_exhaustive()`: unified final-search engine used by both
  `ncvroc()` and `roc_bruteforce()`.
- `.streaming_top_n_exhaustive()`: streaming top-N for nested CV
  preselection when combinations exceed `AUTO_MEMORY_LIMIT`.

## Breaking changes

- `results_storage` default changed from `"rds"` to `"auto"`.
- `results_dir` default changed from `getwd()` to `tempdir()`.
- `ncvroc_result` objects now include `storage_backend`, `chunk_dir`,
  `chunk_prefix`, `chunk_size`, `cache_key`, and `cache_dir` fields.

---

# NCVROC 0.9.0

## New features

- Added `item_count` argument to `ncvroc()`, `roc_bruteforce()` (and its
  alias `roc_bf()`), and `ncvroc_config()` for concise specification of
  candidate scale size. Three syntaxes: `"==4"` (exactly 4 items), `"<=4"`
  (up to 4 items), `"2:4"` (2 through 4 items). `min_items` and `max_items`
  remain supported for backward compatibility.
- Added `results_storage` parameter to `ncvroc()` and `roc_bruteforce()`
  for controlling where full candidate tables are stored. Three modes:
  `"rds"` (default: save to RDS file on disk), `"memory"` (keep in RAM,
  previous behavior), and `"none"` (discard). This prevents large result
  tables from consuming hundreds of MB of memory indefinitely.
- Added `results_name` parameter for custom filename prefixes on RDS files.
- Added `results_dir` parameter for specifying the RDS file directory
  (defaults to the current working directory, typically the folder
  containing the user's Rmd/Qmd file).
- New internal helpers: `.parse_item_count()` for item_count parsing with
  syntax validation, `.describe_item_count()` for human-readable print
  descriptions, `.make_results_path()` for unique RDS filename generation,
  and `.read_results_from_storage()` for transparent RDS reading.
- All three print methods (`print.ncvroc_config()`,
  `print.ncvroc_analysis()`, `print.roc_bruteforce_result()`) now display
  item_count when set, and storage status for RDS files.
- `run_ncvroc()` revalidates `config$item_count` against actual items when
  the config was created with `items = NULL`.

## Breaking changes

- The `item_count` argument is appended as the **last formal argument** in
  each affected public function. Existing positional calls are unaffected.
- `roc_bruteforce_result$results` is now `NULL` by default (was a
  data.frame). Use `ncvroc_results(result, top_n = NULL)` to retrieve the
  full table, or pass `results_storage = "memory"` for the old behavior.
- `ncvroc_analysis$final_exhaustive_ranked` is now `NULL` by default (was a
  data.frame). Same workarounds apply.
- `roc_bruteforce_result` gains `results_file`, `results_storage`, and
  `n_combinations` fields.
- `ncvroc_analysis` gains `final_exhaustive_file`,
  `final_results_storage`, `final_n_combinations`, and `item_count` fields.

# NCVROC 0.8.0

## New features

- Added `roc_bruteforce()` for exhaustive item-combination ROC analysis
  directly on the full dataset without cross-validation. Supports NSE column
  resolution (bare symbols, bare ranges, character vectors, numeric positions),
  structured S3 return values with `print()` method, and optional CSV output.
  The alias `roc_bf()` provides identical functionality with a shorter name.

## Improvements

- Factor columns are now handled safely in `ncvroc()` via internal
  `.prepare_ncvroc_data()` helper (factor to character to numeric). Previously,
  `as.numeric()` on factors silently converted level codes, producing wrong
  numeric values. The fix applies to both `ncvroc()` and `roc_bruteforce()`.
- `ncvroc_results()` now accepts `roc_bruteforce_result` objects in addition
  to `ncvroc_analysis` objects.

# NCVROC 0.7.0

## New features

- Added `ncvroc_results()` for filtering final exhaustive results by clinical
  constraints. Supports conditions on `sensitivity`, `specificity`, `auc`,
  `youden`, `accuracy`, `ppv`, `npv`, `n_items`, and `cutoff` with six
  operators (`>=`, `>`, `<=`, `<`, `==`, `!=`). Multiple conditions are
  combined with AND logic. Results are ranked by a user-specified metric with
  stable tiebreakers.

# NCVROC 0.6.0

## New features

- Added `plot.ncvroc_analysis()` S3 method. Users can now call
  `plot(result)` directly on an `ncvroc()` return value instead of
  manually extracting `result$nested_result`. Supports `which = "all"`
  (default, shows both selection frequency and per-fold AUC),
  `which = "selection"`, and `which = "auc"`.

# NCVROC 0.5.0

## Improvements

- Added `final_top_n` to `ncvroc()` to control how many final candidate
  models are stored and printed.
- Added `final_rank_by` to `ncvroc()` to control the ranking criterion for
  the full-data final exhaustive search.
- `ncvroc()` now returns `final_candidates` and `final_model` for convenient
  reporting.
- When `save_results = TRUE`, `ncvroc()` now also saves `final_candidates.csv`
  and `final_model.csv`.

# NCVROC 0.4.0

## New features

- Added `ncvroc()` as the primary user-facing entry point. Resolves outcome
  and item columns using base-R style selection (`items = Q1:Q112`, bare
  symbols, character vectors, existing variables, or numeric positions).
  Combines data preparation, nested CV, optional final exhaustive search, and
  optional CSV output into a single function call.
- Added `print.ncvroc_analysis()` S3 method for formatted summary output.

# NCVROC 0.3.0

## New features

- Added `count_item_combinations()` to estimate the number of candidate item
  sets without generating combinations.
- Added `suggest_preselect_top_n()` to choose practical preselection sizes
  using `"quick"`, `"balanced"`, `"thorough"`, and `"exhaustive"` modes.
- Added `ncvroc_config()` to bundle common nested-CV and ROC-analysis
  settings into a reusable configuration object.
- Added `run_ncvroc()` as a convenience wrapper around `nested_sum_roc()`
  using an `ncvroc_config` object.

## Improvements

- Configuration printing now reports item count, total combinations,
  preselection size, CV settings, and engine.
- Large preselection settings now trigger a warning in printed
  configurations.

# NCVROC 0.2.0

- Added single-thread Rcpp backend for `exhaustive_sum_roc()`.
  Use `engine = "Rcpp"` to enable native C++ computation (~7x speedup on typical workloads).
  `engine = "R"` remains the default and produces identical results.

- `nested_sum_roc()` and `fit_final_sum_scale()` propagate `engine` to the inner
  exhaustive search, so the Rcpp engine can be used in nested CV workflows as well.

- Fixed Rcpp namespace initialization: `library(NCVROC)` alone is now sufficient
  for the Rcpp engine; `library(Rcpp)` is no longer required beforehand.

- All existing tests pass with both engines, confirming numerical equivalence
  between the R and Rcpp backends.
