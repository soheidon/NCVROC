[English](README.md) | [日本語 README](README-ja.md) | [日本語詳細リファレンス](docs/reference-ja.md)

# NCVROC 0.14.0

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores

Develops short item-based screening scales through combinatorial item-set selection, ROC-based evaluation, and nested cross-validation. For psychological/clinical questionnaire data, identifies which small subset of items best predicts a binary outcome using simple sum scores.

Assume higher sum scores indicate higher probability of a positive outcome. Users must reverse-code items beforehand.

---

## What's new in NCVROC 0.14.0

- **C++ Shared-Memory Multi-Threading (`parallel = "threads"`)**: Evaluates combinatorial candidate search spaces in parallel directly within the main R process using native C++ threads via `RcppParallel`.
- **Zero Socket Startup Overhead**: Avoids socket process initialization, IPC serialization, and process-level data duplication.
- **Deterministic Exact Results**: Produces identical results and rankings to single-threaded serial execution across all evaluation metrics and cutoff selection methods.
- **Explicit Hybrid Concurrency Budgeting**: Nested CV can safely combine outer PSOCK workers with C++ threads while capping total concurrency to available CPU and CRAN limits.
- **Cross-Platform Portability**: Built on `RcppParallel` and verified with clean compilation and full test suites under Windows.

---

## What's new in NCVROC 0.12.0

- **Chunk-Level Parallelization (`parallel = "chunks"`)**: Evaluate massive combinatorial candidate search spaces ($O(\binom{M}{K})$ candidate models) across multiple CPU socket workers in parallel.
- **Clear Separation of Parallelism Levels**: Distinct parallel modes for `"outer"` (cross-validation folds) and `"chunks"` (combinatorial candidate chunks), preventing nested oversubscription.
- **High-Performance Chunk-Based Streaming Search Engine**: Generates combinations on-the-fly via C++ mathematical unranking (`evaluate_combos_cpp_chunk()`) and performs streaming local Top-N candidate reduction, bypassing large R list allocations in memory.
- **Persistent PSOCK Cluster Reuse**: In `nested_sum_roc(..., parallel = "chunks")`, a single PSOCK cluster is initialized once and reused across all outer cross-validation folds, eliminating repeated cluster startup/teardown overhead.
- **Atomic RDS Writing & Robust Cache Validation**: Chunks are saved to temporary files and atomically renamed within the same directory; invalid or stale files are safely ignored.
- **Exact Acceleration (No Approximation)**: Evaluates the full exhaustive combination space without candidate screening or heuristic pruning.
- **Deterministic Exact Tie-Breaking**: Enforces 1-based `.global_combo_index` tracking to guarantee exact candidate ordering and numerical equivalence with serial execution even across chunk boundaries.
- **100% Backward Compatibility**: `parallel = TRUE` resolves contextually to `"outer"` in nested CV (`ncvroc`, `nested_sum_roc`) and `"chunks"` in exhaustive search (`roc_bruteforce`, `exhaustive_sum_roc`); `parallel = FALSE` remains the default.

---

## Installation

```r
# Install NCVROC from GitHub
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

## Core assumptions

1. **Higher score = more likely positive.** Reverse-code items beforehand if needed.
2. **Cutoff rule:** `predicted_positive = score >= cutoff`.
3. **AUC with ties:** `AUC = P(pos > neg) + 0.5 * P(pos == neg)`.
4. **Missing values:** Empty strings and whitespace-only values are treated as
   missing. Rows with missing values in the outcome or selected item columns are
   removed before analysis.
5. **Strict binary outcome.** Outcome column must contain only `positive_label` and `negative_label` values.

---

## Configuration style

`ncvroc()` has sensible defaults. Users can start with a short call:

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  seed       = 20260705
)
```

`mode` controls the default size of the preselected candidate set:

| mode           | preselect_top_n |
| -------------- | --------------: |
| `"quick"`      |             100 |
| `"balanced"`   |             500 |
| `"thorough"`   |            1000 |
| `"exhaustive"` |  all candidates |

Other arguments keep their own defaults unless explicitly changed.
For example, this changes only the computation engine:

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  engine     = "R",
  seed       = 20260705
)
```

This is equivalent to using `mode = "balanced"` while overriding only `engine`.
Users can override any individual setting:

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  inner_repeats = 5,
  preselect_top_n = 1000,
  engine     = "Rcpp",
  seed       = 20260705
)
```

In general, the rule is:

```text
defaults < mode-based suggestion < explicitly supplied arguments
```

So `mode = "balanced"` suggests `preselect_top_n = 500`, but an explicit
`preselect_top_n` value overrides that suggestion.

---

### Item count syntax

The `item_count` argument provides a concise alternative to `min_items` and
`max_items`. It must not be combined with explicit `min_items` or `max_items`.

| `item_count` | Meaning |
|---|---|
| `"==4"` | Exactly 4 items |
| `"<=4"` | Up to 4 items (1 through 4) |
| `"2:4"` | 2 through 4 items |

```r
# Exactly 4-item scales
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "==4",
  mode    = "balanced",
  seed    = 20260705
)

# Up to 4-item scales (1-4 items)
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "<=4",
  mode    = "balanced",
  seed    = 20260705
)

# 2-to-4-item scales
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "2:4",
  mode    = "balanced",
  seed    = 20260705
)
```

`item_count` is available in `ncvroc()`, `roc_bruteforce()` (and `roc_bf()`),
and `ncvroc_config()`.

### Backward compatibility

`min_items` and `max_items` remain supported. The table below shows equivalent
old and new syntax:

| Old (`min_items` / `max_items`) | New (`item_count`) |
|---|---|
| `min_items = 4, max_items = 4` | `item_count = "==4"` |
| `min_items = 1, max_items = 4` | `item_count = "<=4"` |
| `min_items = 2, max_items = 4` | `item_count = "2:4"` |

Low-level functions (`exhaustive_sum_roc()`, `nested_sum_roc()`,
`fit_final_sum_scale()`, `count_item_combinations()`,
`suggest_preselect_top_n()`) continue to use `min_items` and `max_items`.

---

### Result storage

`ncvroc()` and `roc_bruteforce()` accept a `results_storage` parameter to
control where full candidate tables are stored. Since v0.10.0 the default is
`"auto"`, which selects the storage mode automatically based on search size.

| `results_storage` | Behavior |
|---|---|
| `"auto"` (default) | Small searches use in-memory storage; large searches (> 100,000 combinations) use chunked RDS files on disk. |
| `"memory"` | Keep full table in RAM (pre-v0.9.0 behavior). |
| `"rds"` | Save the full table to a single RDS file. In RStudio or Quarto projects this is typically the project root. The save location is always shown in the printed output. Use `getwd()` to check the current directory, or set `results_dir` to an explicit path if the default is not suitable. `$results` / `$final_exhaustive_ranked` is `NULL`. |
| `"none"` | Discard full table. `ncvroc_results()` will error. |

Use `ncvroc_results()` to retrieve the full table when `results_storage` is not
`"none"`:

```r
ncvroc_results(result, top_n = 20)  # get top 20 candidates
```

For chunked RDS results (produced by `"auto"` on large searches or explicitly
with `"rds"` when chunked), `top_n = NULL` requires
`allow_full_load = TRUE`:

```r
ncvroc_results(result, top_n = NULL, allow_full_load = TRUE)
```

### Caching & atomic storage
 
Large exhaustive searches can take significant time. `ncvroc()` and
`roc_bruteforce()` support result caching and atomic chunked storage to disk:
 
```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "<=4",
  mode    = "balanced",
  cache   = "reuse",     # "off" (default), "reuse", or "refresh"
  seed    = 20260705
)

# Second identical call loads from cache instantly
result2 <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "<=4",
  mode    = "balanced",
  cache   = "reuse",
  seed    = 20260705
)
```

| `cache` | Behavior |
|---|---|
| `"off"` (default) | No caching. |
| `"reuse"` | Use cached result if available (validated against exact hash of data, items, metric, and search parameters); otherwise compute and cache. |
| `"refresh"` | Always recompute and overwrite the cache. |

`cache_dir` controls where cached results are stored (default: `tempdir()`).

**Atomic RDS Storage Guarantee**: Chunk RDS files are written to temporary files (`.tmp`) within the target directory and atomically renamed to `.rds`. Incomplete writes from interrupted processes are never treated as valid final chunk files, and stale temporary files are ignored.

### Parallel computing

NCVROC provides four distinct parallelization modes to suit different analysis scales and workflows:

| Mode | `parallel` | Description | Best For |
|---|---|---|---|
| **C++ Multi-Threading** | `"threads"` | Evaluates combinations in parallel within the main R process using native C++ threads via RcppParallel (shared memory, zero socket startup). | High-speed in-memory exhaustive search (`roc_bruteforce()`, `exhaustive_sum_roc()`, or `ncvroc()` with thorough/exhaustive search). |
| **Outer Fold Parallel** | `"outer"` (or `TRUE` in nested CV) | Evaluates outer cross-validation folds in parallel using PSOCK socket worker processes. | Standard nested CV workflows across multiple folds (`ncvroc()`, `nested_sum_roc()`). |
| **Hybrid Nested CV** | `"hybrid"` | Evaluates outer folds with PSOCK workers and each fold's exhaustive search with C++ threads. | Nested CV workloads where both fold-level and within-fold concurrency are useful. |
| **Chunk Process Parallel** | `"chunks"` (or `TRUE` in exhaustive search) | Evaluates combination chunks across persistent PSOCK socket worker processes. | Massive searches using disk-backed chunked storage, caching, and process isolation. |

```r
# High-speed in-memory C++ multi-threading:
result <- roc_bruteforce(
  data       = d,
  outcome    = y,
  items      = Q1:Q10,
  max_items  = 4,
  parallel   = "threads",
  n_workers  = 4
)

# Parallel outer cross-validation folds:
result <- ncvroc(
  data       = d,
  outcome    = y,
  items      = Q1:Q10,
  max_items  = 3,
  parallel   = "outer",
  n_workers  = 4
)

# Hybrid outer processes x C++ threads (total budget 8):
result <- ncvroc(
  data               = d,
  outcome            = y,
  items              = Q1:Q10,
  max_items          = 3,
  parallel           = "hybrid",
  n_workers          = 4,
  threads_per_worker = 2
)
```

**Backward Compatibility**: Specifying `parallel = TRUE` automatically maps to `"outer"` in nested cross-validation functions (`ncvroc()`, `nested_sum_roc()`) and to `"chunks"` in exhaustive search functions (`roc_bruteforce()`, `exhaustive_sum_roc()`).

### Chunk size

The `chunk_size` parameter (default `200000`) controls how many combinations are
evaluated per chunk in large exhaustive searches. You typically do not need to
change this default.

### Final candidate output

`ncvroc()` runs the final exhaustive search by default and saves the ranked
full-data candidate table to an RDS file (see `results_storage` above).

For convenience, the following are kept in memory:

```r
result$final_candidates       # top N rows (controlled by final_top_n)
result$final_model            # best single model (first row)
result$final_n_combinations   # total combinations evaluated
result$final_results_storage  # storage mode ("auto", "rds", "memory", or "none")
result$final_exhaustive_file  # RDS file path (in "rds" mode)
```

`selection_criterion` controls which candidate is selected during nested CV.

`final_rank_by` controls how the final full-data candidate table is ranked.

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  final_rank_by = "auc",
  final_top_n = 20,
  seed    = 20260705,
  save_results = TRUE
)

result$final_candidates
result$final_model
```

Use `final_rank_by` to choose the ranking criterion:

```r
final_rank_by = "auc"          # default
final_rank_by = "youden"
final_rank_by = "sensitivity"
final_rank_by = "specificity"
final_rank_by = "accuracy"
```

Use `ncvroc_results()` to filter the ranked table by clinical constraints
before choosing a model:

```r
ncvroc_results(
  result,
  sensitivity = ">= 0.90",
  specificity = ">= 0.85",
  rank_by = "youden",
  top_n = 20
)
```

Conditions support six operators (`>=`, `>`, `<=`, `<`, `==`, `!=`) combined
with AND logic. Available columns: `sensitivity`, `specificity`, `auc`,
`youden`, `accuracy`, `ppv`, `npv`, `n_items`, `cutoff`.

---

## Reference

### `ncvroc()`

Primary entry point for a complete NCVROC analysis in a single call. Resolves outcome and item columns using base-R style selection, prepares data, runs nested CV, optionally performs a final exhaustive search, and optionally saves CSV outputs.

```r
ncvroc(
  data,
  outcome,
  items,
  min_items         = 1,
  max_items         = 4,
  mode              = c("balanced", "quick", "thorough", "exhaustive"),
  outer_k           = 5,
  inner_k           = 4,
  outer_repeats     = 5,
  inner_repeats     = 1,
  preselect_top_n   = NULL,
  preselect_by      = "auc",
  selection_criterion = "auc",
  cutoff_method     = "youden",
  positive_label    = 1,
  negative_label    = 0,
  stratified        = TRUE,
  engine            = "Rcpp",
  seed              = NULL,
  final_search      = TRUE,
  final_top_n       = 20,
  final_rank_by     = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  results_storage   = c("auto", "memory", "rds", "none"),
  results_name      = NULL,
  results_dir       = NULL,
  return            = "full",
  item_count        = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL,
  parallel          = FALSE,
  n_workers         = NULL
)
```

`outcome` accepts a bare symbol (`y`) or character string (`"y"`).
`items` accepts bare range (`Q1:Q5`), bare names with `c()`, character vector, existing variable, or numeric positions.

`selection_criterion` controls which candidate is selected during nested CV.
`final_rank_by` controls how the final full-data candidate table is ranked.

**Returns:** S3 object of class `"ncvroc_analysis"`. `print()`, `summary()`, and `plot()` S3 methods are available. Use `ncvroc_results()` to filter the final candidate table by clinical constraints.

---

### `ncvroc_results()`

Filter and rank candidate models from an `ncvroc_analysis` or
`roc_bruteforce_result` object using clinical or practical constraints.

```r
ncvroc_results(
  x,
  sensitivity  = NULL,
  specificity  = NULL,
  auc          = NULL,
  youden       = NULL,
  accuracy     = NULL,
  ppv          = NULL,
  npv          = NULL,
  n_items      = NULL,
  cutoff       = NULL,
  rank_by = c("youden", "auc", "sensitivity", "specificity", "accuracy", "ppv", "npv"),
  top_n  = 20,
  allow_full_load = FALSE
)
```

Each condition is a string like `">= 0.90"` or `"<= 3"`. Six operators are supported: `>=`, `>`, `<=`, `<`, `==`, `!=`. Multiple conditions are combined with AND logic. Results are ranked by `rank_by` with stable tiebreakers. Set `top_n = NULL` to return all matching rows, or `0` for an empty table.

For chunked RDS storage, `top_n = NULL` requires `allow_full_load = TRUE`.

**Returns:** A data.frame containing the filtered and ranked candidate models.

`x` may be either:

- an `ncvroc_analysis` object created with `final_search = TRUE`, or
- a `roc_bruteforce_result` object returned by `roc_bruteforce()` or `roc_bf()`.

---

### `roc_bruteforce()`

Full-data exhaustive item-combination ROC analysis with NSE column resolution.

```r
roc_bruteforce(
  data,
  outcome,
  items,
  min_items        = 1,
  max_items        = 4,
  cutoff_method    = c("youden", "closest_topleft"),
  positive_label   = 1,
  negative_label   = 0,
  engine           = c("Rcpp", "R"),
  rank_by          = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n            = 20,
  progress         = interactive(),
  save_results     = FALSE,
  output_dir       = ".",
  results_storage  = c("auto", "memory", "rds", "none"),
  results_name     = NULL,
  results_dir      = NULL,
  item_count       = NULL,
  chunk_size       = 200000L,
  cache            = c("off", "reuse", "refresh"),
  cache_dir        = NULL,
  parallel         = FALSE,
  n_workers        = NULL
)
```

**Returns:** S3 object of class `"roc_bruteforce_result"` with `$candidates`
(top_n subset), `$best_model` (first row), `$results_storage`, `$results_file`,
and `$n_combinations`. By default `$results` is `NULL` (saved to RDS).
`print()` displays a formatted summary with a warning that performance may be
optimistic. Use `ncvroc_results()` to filter by clinical constraints.

The alias `roc_bf()` takes the same arguments and returns the same result.

---

### `ncvroc_config()`

Bundle all analysis parameters into a single configuration object. Use with `run_ncvroc()` to reduce verbosity in analysis scripts.

```r
ncvroc_config(
  outcome,
  items             = NULL,
  min_items         = 1,
  max_items         = 4,
  mode              = c("balanced", "quick", "thorough", "exhaustive"),
  outer_k           = 5,
  inner_k           = 4,
  outer_repeats     = 5,
  inner_repeats     = 1,
  preselect_top_n   = NULL,
  preselect_by      = "auc",
  selection_criterion = "auc",
  cutoff_method     = c("youden", "closest_topleft"),
  positive_label    = 1,
  negative_label    = 0,
  stratified        = TRUE,
  engine            = c("Rcpp", "R"),
  item_count        = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL
)
```

`mode` controls the default `preselect_top_n`:

| Mode | Preselection | Use case |
|---|---|---|
| `"quick"` | Top 100 | Fast screening, exploration |
| `"balanced"` | Top 500 (default) | Routine analysis |
| `"thorough"` | Top 1000 | Comprehensive search |
| `"exhaustive"` | All candidates | Full enumeration (may be slow) |

**Returns:** S3 object of class `"ncvroc_config"`. `print()` shows a formatted summary with a warning if `preselect_top_n >= 100,000`.

---

### `run_ncvroc()`

Convenience wrapper around `nested_sum_roc()` that reads all parameters from an `ncvroc_config` object.

```r
run_ncvroc(
  data,
  items,
  config,
  seed     = NULL,
  progress = TRUE,
  verbose  = TRUE,
  return   = c("full", "summary")
)
```

**Returns:** `ncvroc_result` object (same as `nested_sum_roc()`).

---

### `nested_sum_roc()`

Nested cross-validation with outer loop for performance estimation, inner loop for model selection.

```r
nested_sum_roc(
  data,
  outcome,
  items,
  min_items          = 1,
  max_items          = 4,
  positive_label     = 1,
  negative_label     = 0,
  cutoff_method      = c("youden", "closest_topleft"),
  preselect_top_n    = 20,
  preselect_by       = "auc",
  selection_criterion = "auc",
  outer_k            = 5,
  inner_k            = 4,
  outer_repeats      = 1,
  inner_repeats      = 1,
  stratified         = TRUE,
  seed               = NULL,
  engine             = c("R", "Rcpp"),
  progress           = TRUE,
  verbose            = TRUE,
  return             = c("full", "summary"),
  output_dir         = NULL,
  file_prefix        = "NCVROC"
)
```

**Returns:** S3 object of class `"ncvroc_result"` with elements:

| Element | Description |
|---|---|
| `summary` | data.frame: one row per outer fold with AUC, sensitivity, specificity, etc. |
| `outer_results` | list: full per-fold details including predictions |
| `selected_models` | character: item-set selected in each fold |
| `selected_model_frequency` | data.frame: selection frequency of each item set |
| `outer_predictions` | data.frame: all out-of-sample predictions with scores |
| `settings` | list: all argument values |

**S3 methods:** `print()`, `summary()`, `plot(which = "selection"|"auc")`.

---

### `exhaustive_sum_roc()`

Enumerate all item combinations, compute simple sum scores, evaluate via ROC.

```r
exhaustive_sum_roc(
  data,
  outcome,
  items,
  min_items          = 1,
  max_items          = 4,
  positive_label     = 1,
  negative_label     = 0,
  cutoff_method      = c("youden", "closest_topleft"),
  rank_by            = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n              = NULL,
  prefer_fewer_items = TRUE,
  ci                 = FALSE,
  conf_level         = 0.95,
  engine             = c("R", "Rcpp"),
  progress           = TRUE
)
```

**Returns:** data.frame with columns `rank`, `items`, `n_items`, `auc`, `cutoff`, `sensitivity`, `specificity`, `youden`, `accuracy`, `ppv`, `npv`, `n_positive`, `n_negative`. When `ci = TRUE`, also includes `auc_lower`, `auc_upper`, `sensitivity_lower`, `sensitivity_upper`, `specificity_lower`, `specificity_upper`, `accuracy_lower`, `accuracy_upper`, `ppv_lower`, `ppv_upper`, `npv_lower`, `npv_upper`. Sorted by `rank_by` descending.

**Performance is apparent (in-sample), not cross-validated.**

Default is `engine = "R"`. For ~7x speedup, use `engine = "Rcpp"`.

---

### `fit_final_sum_scale()`

Thin wrapper around `exhaustive_sum_roc()` for fitting the final scale on the full dataset after cross-validation.

```r
fit_final_sum_scale(
  data,
  outcome,
  items,
  min_items      = 1,
  max_items      = 4,
  positive_label = 1,
  negative_label = 0,
  cutoff_method  = c("youden", "closest_topleft"),
  rank_by        = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n          = 20,
  ci             = TRUE,
  conf_level     = 0.95,
  engine         = c("R", "Rcpp"),
  progress       = TRUE
)
```

**Returns:** data.frame with point estimates and confidence intervals (when `ci = TRUE`), tagged with `attr(result, "performance_type") <- "apparent"`. These are in-sample estimates, not cross-validated. Use `nested_sum_roc()` for validated performance.

Default is `engine = "R"`. For ~7x speedup, use `engine = "Rcpp"`.

---

### `make_stratified_folds()`

Create stratified k-fold cross-validation indices.

```r
make_stratified_folds(y, k = 5, repeats = 1, seed = NULL)
```

**Returns:** named list of integer vectors. Names follow `"Rep1_Fold1"` format. If `k` exceeds the size of the smaller class, `k` is reduced with a warning.

---

### `count_item_combinations()`

Count total k-item combinations without generating them.

```r
count_item_combinations(
  items_or_n,
  min_items = 1,
  max_items = 4,
  detail    = FALSE
)
```

`items_or_n` accepts a character vector of item names or a single integer n.
`detail = TRUE` returns a data.frame with per-k breakdown.

---

### `suggest_preselect_top_n()`

Suggest a practical `preselect_top_n` based on total combinations and analysis mode.

```r
suggest_preselect_top_n(
  items_or_n,
  min_items = 1,
  max_items = 4,
  mode      = c("balanced", "quick", "thorough", "exhaustive")
)
```

**Returns:** single numeric value, capped at the total number of combinations.

---

## Quick example

```r
library(NCVROC)

set.seed(42)
d <- data.frame(
  y  = sample(0:1, 100, replace = TRUE),
  Q1 = sample(0:2, 100, replace = TRUE),
  Q2 = sample(0:2, 100, replace = TRUE),
  Q3 = sample(0:2, 100, replace = TRUE),
  Q4 = sample(0:2, 100, replace = TRUE),
  Q5 = sample(0:2, 100, replace = TRUE)
)

# Single-call analysis with base-R style column selection
result <- ncvroc(d, y, Q1:Q5, item_count = "<=2", mode = "quick",
  outer_k = 3, inner_k = 2, outer_repeats = 1, engine = "R",
  seed = 42, final_search = FALSE)
print(result)
summary(result)
plot(result)
```

### Configuration workflow

```r
# Define the analysis intent once
cfg <- ncvroc_config(
  outcome    = "y",
  items      = paste0("Q", 1:5),
  item_count = "<=2",
  mode       = "quick",
  engine     = "Rcpp"
)

print(cfg)

result <- run_ncvroc(d, paste0("Q", 1:5), cfg, seed = 42)
summary(result)
```

---

## Apparent vs. nested CV performance

| Function | Performance | Use case |
|---|---|---|
| `ncvroc()` | Nested cross-validated | Single-call entry point (recommended) |
| `roc_bruteforce()` | Apparent (in-sample) | Full-data exhaustive search with NSE |
| `exhaustive_sum_roc()` | Apparent (in-sample) | Quick screening, exploration |
| `nested_sum_roc()` | Nested cross-validated | Validated performance estimation |
| `run_ncvroc()` | Nested cross-validated | Convenience wrapper (config-driven) |
| `fit_final_sum_scale()` | Apparent (in-sample) | Final scale on full data |

---

## Confidence intervals

`NCVROC` provides confidence interval (CI) estimation for final scale performance metrics evaluated on the full dataset:

- **AUC CI:** Non-parametric asymptotic method by DeLong et al. (1988), computed directly and efficiently from score frequency distributions in $O(K)$ time.
- **Sensitivity, Specificity, Accuracy, PPV, NPV CI:** Exact binomial confidence intervals by Clopper and Pearson (1934) computed using Beta distribution quantiles (`stats::qbeta`).
- **Confidence level:** Specified via `conf_level` (default `0.95` for 95% CIs).
- **Default behavior:**
  - `fit_final_sum_scale()` and `ncvroc()` compute CIs for final models/candidates by default (`ci = TRUE`).
  - `exhaustive_sum_roc()` defaults to `ci = FALSE` to maintain high combinatorial search speed, computing CIs only after ranking and top-N candidate selection when `ci = TRUE`.

### Example

```r
# Fit final scale with 95% confidence intervals
final <- fit_final_sum_scale(
  data       = d,
  outcome    = "y",
  items      = c("Q1", "Q2", "Q3"),
  max_items  = 2,
  ci         = TRUE,
  conf_level = 0.95
)

# Output includes point estimates alongside lower/upper bounds
final[, c("rank", "items", "auc", "auc_lower", "auc_upper",
          "sensitivity", "sensitivity_lower", "sensitivity_upper",
          "specificity", "specificity_lower", "specificity_upper")]
```

For small samples or perfect classification (e.g. 10/10 true positives), the Clopper–Pearson exact method properly quantifies sample uncertainty:
```text
Sensitivity: 1.000 [0.692, 1.000]
```

### Important note on CI interpretation

> [!IMPORTANT]
> **Confidence intervals for final-model performance quantify sampling uncertainty for the fitted model evaluated on the full dataset. They do not account for uncertainty introduced by model or cutoff selection and should not be interpreted as cross-validated confidence intervals.**
>
> Use `nested_sum_roc()` or `ncvroc()` to assess out-of-sample generalizability across outer cross-validation folds.

---

## Parallel execution

`NCVROC` supports multi-core parallelization across four distinct operational modes:

- **C++ Multi-Threading (`parallel = "threads"`)**: Evaluates combinatorial candidate search spaces in parallel directly within the main R process using native C++ threads via `RcppParallel`. Features zero socket IPC overhead, no process-level duplication of input data, and negligible startup latency.
- **Outer-Fold Parallelization (`parallel = "outer"`)**: Evaluates outer cross-validation folds concurrently across socket worker processes (`parallel::makePSOCKcluster`). Preselection within each fold runs sequentially.
- **Hybrid Nested CV (`parallel = "hybrid"`)**: Evaluates outer folds across PSOCK workers while each worker evaluates exhaustive candidates with `threads_per_worker` C++ threads. This mode is available only for nested CV with `engine = "Rcpp"`.
- **Chunk-Level Parallelization (`parallel = "chunks"`)**: Evaluates large combinatorial search spaces ($O(\binom{M}{K})$ candidate models) concurrently across chunks via persistent socket worker processes.

### Parallel modes and contextual resolution

| Setting | Nested Context (`ncvroc`, `nested_sum_roc`) | Exhaustive Context (`roc_bruteforce`, `exhaustive_sum_roc`) |
|---|---|---|
| `parallel = FALSE` (Default) | Sequential single-process execution | Sequential single-process execution |
| `parallel = TRUE` | Resolves to `"outer"` (outer-fold parallelization) | Resolves to `"chunks"` (chunk parallelization) |
| `parallel = "none"` | Sequential single-process execution | Sequential single-process execution |
| `parallel = "threads"` | Outer folds run sequentially; inner exhaustive searches use C++ multi-threading | Evaluates exhaustive combinations in parallel using C++ multi-threading |
| `parallel = "outer"` | Evaluates outer cross-validation folds in parallel | Unsupported (raises error) |
| `parallel = "chunks"` | Outer folds run sequentially; inner preselection chunks are evaluated in parallel (reusing a single cluster) | Evaluates candidate combination chunks in parallel across socket workers |
| `parallel = "hybrid"` | Outer folds use PSOCK workers; each worker uses C++ threads for exhaustive evaluation | Unsupported (raises error) |

> [!NOTE]
> `parallel = "auto"` is reserved for a future release. Specifying `"auto"` raises an informative error prompting you to choose `"none"`, `"threads"`, `"outer"`, `"chunks"`, or `"hybrid"`.

### Worker count (`n_workers`)

- **`n_workers = NULL` (Default)**: Automatically detects available physical CPU cores (`max(1L, parallel::detectCores(logical = FALSE) - 1L)`).
- **`n_workers = 4`**: Uses up to 4 worker processes or threads.
- **Automatic Capping**: The effective worker count is safely capped by available CPU cores, task count (for outer folds), and CRAN environment limits (`_R_CHECK_LIMIT_CORES_`).
- **Hybrid meaning**: In `parallel = "hybrid"`, `n_workers` is the requested number of outer PSOCK workers. When it is `NULL`, NCVROC resolves the outer worker count first, then caps `threads_per_worker` to the remaining CPU budget.

### Threads per hybrid worker (`threads_per_worker`)

`threads_per_worker` is the requested number of C++ threads used inside each outer PSOCK worker in hybrid mode. Its effective value may be reduced according to the CPU budget, the resolved outer worker count, and `_R_CHECK_LIMIT_CORES_`. The effective product of outer workers and threads per worker is kept within the available budget; CRAN checks are limited to at most 2.

### Controlled concurrency and oversubscription protection

Except for the explicitly budgeted `"hybrid"` mode, outer parallelization, chunk parallelization, and C++ multi-threading are never nested concurrently. Hybrid combines only outer PSOCK workers with call-local C++ threads and caps their product to prevent oversubscription. Chunk PSOCK workers are never used inside outer workers.

### Persistent cluster reuse in nested CV

When running nested CV with `parallel = "chunks"`, `nested_sum_roc()` initializes the PSOCK cluster **once** before the outer fold loop, exports package code and DLLs once, and reuses the cluster across all outer folds by updating only fold-specific training matrices. The cluster is cleanly closed on exit.

### High-performance chunk/streaming search engine & exactness

In v0.12.0 and v0.13.0, the exhaustive search engine generates combinations on-the-fly via C++ mathematical unranking (`evaluate_combos_cpp_chunk()`) and maintains streaming local Top-N candidate pools.
- **Exact exhaustive search**: Evaluates the full exhaustive candidate space without heuristics or screening approximations.
- **Exact tie-breaking**: Candidates track their 1-based `.global_combo_index` from serial combinatorial enumeration, ensuring deterministic identical candidate ordering and numerical metrics between serial and parallel runs.
- **Streaming Top-N invariant**: Each chunk retains at least as many candidates as required for the final global Top-N (`top_n_local >= global_top_n`), so no candidate belonging to the global Top-N can be lost during chunk reduction.

### Usage examples

#### Hybrid nested CV

```r
res_hybrid <- ncvroc(
  data               = analysis_dat,
  outcome            = y,
  items              = Q1:Q30,
  max_items          = 4,
  parallel           = "hybrid",
  n_workers          = 4,
  threads_per_worker = 2
)
```

Hybrid performance depends on fold count, candidate-space size, data size, and hardware; it is not always faster than outer-only or threads-only execution.

#### Example 1: In-memory C++ multi-threading (fastest for exhaustive search)

```r
res_threads <- roc_bruteforce(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q30,
  item_count = "<=4",
  parallel   = "threads",
  n_workers  = 4
)
```

#### Example 2: Outer-fold parallelization (recommended for nested CV with moderate M)

```r
res_outer <- ncvroc(
  data          = analysis_dat,
  outcome       = y,
  items         = Q1:Q14,
  max_items     = 4,
  outer_k       = 5,
  inner_k       = 4,
  outer_repeats = 5,
  parallel      = "outer",   # or parallel = TRUE
  n_workers     = 4,
  seed          = 42
)
```

#### Example 3: Chunk-level parallel search (recommended for disk-backed/cached massive search)

```r
res_chunks <- roc_bruteforce(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q40,
  item_count = "<=4",
  parallel   = "chunks",  # or parallel = TRUE
  n_workers  = 4
)
```

---

## Brute-force ROC search without cross-validation

Use `roc_bruteforce()` (or its alias `roc_bf()`) for exhaustive item-combination
ROC analysis directly on the full dataset. It shares the same NSE column
resolution as `ncvroc()`.

> Performance is calculated on the same data used for item and cutoff
> selection. These estimates may be optimistic. Use `ncvroc()` for nested
> cross-validated performance estimation.

```r
result <- roc_bruteforce(
  data       = d,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=3",
  rank_by    = "youden",
  engine     = "Rcpp",
  top_n      = 20
)

result
result$best_model
result$candidates

# To retrieve the full candidate table (saved to RDS by default):
ncvroc_results(result, top_n = NULL)
```

Filter with `ncvroc_results()`, just like `ncvroc()` output:

```r
ncvroc_results(result, sensitivity = ">= 0.90", specificity = ">= 0.85")
```

The alias `roc_bf()` is equivalent:

```r
result <- roc_bf(d, y, Q1:Q5, item_count = "<=3", engine = "Rcpp")
```

## Rcpp engine

Specify `engine = "Rcpp"` in `ncvroc()`, `roc_bruteforce()`,
`exhaustive_sum_roc()`, `nested_sum_roc()`, or `fit_final_sum_scale()` to use
the native C++ backend. Results are numerically identical to the R engine;
typical speedup is ~7x on moderate workloads.

```r
exhaustive_sum_roc(d, "y", paste0("Q", 1:5), max_items = 2, engine = "Rcpp")
```

## License

MIT
