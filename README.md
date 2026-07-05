# NCVROC 0.2.0

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

## Apparent vs. nested CV performance

| Function | Performance | Use case |
|---|---|---|
| `exhaustive_sum_roc()` | Apparent (in-sample) | Quick screening, exploration |
| `nested_sum_roc()` | Nested cross-validated | Validated performance estimation |
| `fit_final_sum_scale()` | Apparent (in-sample) | Final scale on full data |

---

## Quick example

```r
library(NCVROC)

set.seed(42)
d <- data.frame(
  y  = sample(0:1, 100, replace = TRUE),
  q1 = sample(0:2, 100, replace = TRUE),
  q2 = sample(0:2, 100, replace = TRUE),
  q3 = sample(0:2, 100, replace = TRUE),
  q4 = sample(0:2, 100, replace = TRUE),
  q5 = sample(0:2, 100, replace = TRUE)
)

# Exhaustive search
exhaustive_sum_roc(d, "y", paste0("q", 1:5), max_items = 2)

# Nested CV
result <- nested_sum_roc(d, "y", paste0("q", 1:5),
  max_items = 2, outer_k = 3, inner_k = 2, seed = 42, progress = FALSE)
summary(result)
plot(result, which = "selection")

# Final scale
fit_final_sum_scale(d, "y", paste0("q", 1:5), max_items = 2)
```

## Rcpp engine

Specify `engine = "Rcpp"` in `exhaustive_sum_roc()`, `nested_sum_roc()`, or
`fit_final_sum_scale()` to use the native C++ backend. Results are numerically
identical to the R engine; typical speedup is ~7x on moderate workloads.

```r
exhaustive_sum_roc(d, "y", paste0("q", 1:5), max_items = 2, engine = "Rcpp")
```

## License

MIT — see [LICENSE](LICENSE).
