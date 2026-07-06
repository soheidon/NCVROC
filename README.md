# NCVROC 0.5.0

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores

Develops short item-based screening scales through combinatorial item-set selection, ROC-based evaluation, and nested cross-validation. For psychological/clinical questionnaire data, identifies which small subset of items best predicts a binary outcome using simple sum scores.

Assume higher sum scores indicate higher probability of a positive outcome. Users must reverse-code items beforehand.

---

## Installation

```r
# v0.1.0 pre-release (GitHub)
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

## Core assumptions

1. **Higher score = more likely positive.** Reverse-code items beforehand if needed.
2. **Cutoff rule:** `predicted_positive = score >= cutoff`.
3. **AUC with ties:** `AUC = P(pos > neg) + 0.5 * P(pos == neg)`.
4. **No missing values in v0.1.** Any `NA` causes an immediate error.
5. **Strict binary outcome.** Outcome column must contain only `positive_label` and `negative_label` values.

---

## Configuration style

`ncvroc()` has sensible defaults. Users can start with a short call:

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = OCD_C,
  items   = Q1:Q112,
  max_items = 4,
  mode    = "balanced",
  seed    = 20260705
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
  outcome = OCD_C,
  items   = Q1:Q112,
  max_items = 4,
  mode    = "balanced",
  engine  = "R",
  seed    = 20260705
)
```

This is equivalent to using `mode = "balanced"` while overriding only `engine`.
Users can override any individual setting:

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = OCD_C,
  items   = Q1:Q112,
  max_items = 4,
  mode    = "balanced",
  inner_repeats = 5,
  preselect_top_n = 1000,
  engine  = "Rcpp",
  seed    = 20260705
)
```

In general, the rule is:

```text
defaults < mode-based suggestion < explicitly supplied arguments
```

So `mode = "balanced"` suggests `preselect_top_n = 500`, but an explicit
`preselect_top_n` value overrides that suggestion.

---

### Final candidate output

`ncvroc()` runs the final exhaustive search by default and stores the ranked
full-data candidate table in `result$final_exhaustive_ranked`.

For convenience, it also stores:

```r
result$final_candidates   # top N rows (controlled by final_top_n)
result$final_model        # best single model (first row)
```

`selection_criterion` controls which candidate is selected during nested CV.

`final_rank_by` controls how the final full-data candidate table is ranked.

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = OCD_C,
  items   = Q1:Q112,
  max_items = 4,
  mode    = "balanced",
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

---

## Reference

### `exhaustive_sum_roc()`

Enumerate all item combinations, compute simple sum scores, evaluate via ROC.

```r
exhaustive_sum_roc(
  data,
  outcome,
  items,
  min_items         = 1,
  max_items         = 4,
  positive_label    = 1,
  negative_label    = 0,
  cutoff_method     = c("youden", "closest_topleft"),
  rank_by           = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n             = NULL,
  prefer_fewer_items = TRUE,
  engine            = c("R", "Rcpp"),
  progress          = TRUE
)
```

**Returns:** data.frame with columns `rank`, `items`, `n_items`, `auc`, `cutoff`, `sensitivity`, `specificity`, `youden`, `accuracy`, `ppv`, `npv`, `n_positive`, `n_negative`. Sorted by `rank_by` descending.

**Performance is apparent (in-sample), not cross-validated.**

Default is `engine = "R"`. For ~7x speedup, use `engine = "Rcpp"`.

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
  engine         = c("R", "Rcpp"),
  progress       = TRUE
)
```

**Returns:** data.frame with `attr(result, "performance_type") <- "apparent"`. These are in-sample estimates, not cross-validated. Use `nested_sum_roc()` for validated performance.

Default is `engine = "R"`. For ~7x speedup, use `engine = "Rcpp"`.

---

### `make_stratified_folds()`

Create stratified k-fold cross-validation indices.

```r
make_stratified_folds(y, k = 5, repeats = 1, seed = NULL)
```

**Returns:** named list of integer vectors. Names follow `"Rep1_Fold1"` format. If `k` exceeds the size of the smaller class, `k` is reduced with a warning.

---

### `ncvroc()` (v0.4.0)

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
  save_results      = FALSE,
  output_dir        = ".",
  progress          = TRUE,
  verbose           = TRUE,
  return            = "full"
)
```

`outcome` accepts a bare symbol (`OCD_C`) or character string (`"OCD_C"`).
`items` accepts bare range (`Q1:Q112`), bare names with `c()`, character vector, existing variable, or numeric positions.

`selection_criterion` controls which candidate is selected during nested CV.
`final_rank_by` controls how the final full-data candidate table is ranked.

**Returns:** S3 object of class `"ncvroc_analysis"`. `print()`, `summary()`, and `plot()` S3 methods are available.

---

### `ncvroc_config()` (v0.3.0)

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
  engine            = c("Rcpp", "R")
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

### `run_ncvroc()` (v0.3.0)

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

### `count_item_combinations()` (v0.3.0)

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

### `suggest_preselect_top_n()` (v0.3.0)

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
result <- ncvroc(d, y, Q1:Q5, max_items = 2, mode = "quick",
  outer_k = 3, inner_k = 2, outer_repeats = 1, engine = "R",
  seed = 42, final_search = FALSE)
print(result)
summary(result$nested_result)
plot(result)
```

### Configuration workflow (v0.3.0)

```r
# Define the analysis intent once
cfg <- ncvroc_config(
  outcome   = "y",
  items     = paste0("Q", 1:5),
  max_items = 2,
  mode      = "quick",
  engine    = "Rcpp"
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
| `exhaustive_sum_roc()` | Apparent (in-sample) | Quick screening, exploration |
| `nested_sum_roc()` | Nested cross-validated | Validated performance estimation |
| `run_ncvroc()` | Nested cross-validated | Convenience wrapper (config-driven) |
| `fit_final_sum_scale()` | Apparent (in-sample) | Final scale on full data |

---

## Rcpp engine

Specify `engine = "Rcpp"` in `exhaustive_sum_roc()`, `nested_sum_roc()`, or
`fit_final_sum_scale()` to use the native C++ backend. Results are numerically
identical to the R engine; typical speedup is ~7x on moderate workloads.

```r
exhaustive_sum_roc(d, "y", paste0("q", 1:5), max_items = 2, engine = "Rcpp")
```

## License

MIT — see [LICENSE](LICENSE).
