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

