[English](README.md) | [日本語 README](README-ja.md) | [日本語詳細リファレンス](docs/reference-ja.md)

# NCVROC 0.20.0

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores

NCVROC develops short item-based screening scales through combinatorial item-set selection, Receiver Operating Characteristic (ROC) curve evaluation, ordinary and nested cross-validation, and selection optimism assessment. For psychological and clinical questionnaire data, NCVROC identifies which small subset of items best predicts a binary outcome using unweighted sum scores.

Assume higher sum scores indicate higher probability of a positive outcome. Users must reverse-code items beforehand.

---

## What's new in NCVROC 0.20.0

- **Automatic execution planning**:
  - Benchmarks legal execution configurations for large exhaustive searches
    and chooses a measured execution backend from serial, multithreaded,
    outer-fold parallel, and hybrid strategies.
  - The benchmark trigger is a deterministic workload threshold. Workloads
    below that threshold use the safe baseline path without a backend sweep.
  - Nested-CV planning uses a bounded pilot sample allocated across requested
    model sizes. The pilot does not replace, screen, or prune the exhaustive
    candidate search.
  - Planning changes scheduling only; candidate generation, folds, ranking,
    tie-breaking, cutoffs, predictions, and final model selection remain
    unchanged.
- **Parallel execution**:
  - Improved C++ multithreaded exhaustive evaluation with `RcppParallel`.
  - Added bounded nested resource-plan benchmarking for multithreaded,
    outer-fold PSOCK, and hybrid execution modes.
  - Production PSOCK paths continue to use standard R parallel infrastructure.
- **Progress reporting**:
  - Serial and multithreaded nested execution reports completed candidates
    within each outer fold.
  - Outer-fold parallel execution reports observable outer-task progress.
  - Progress shows observed elapsed time only; NCVROC does not display ETA
    estimates.
- **Large-search reliability**:
  - Added streaming C++ Top-N evaluation and rank-bounded nested evaluation
    while preserving canonical combination identity and exact exhaustive
    semantics.
  - Expanded regression coverage for serial/parallel statistical and RNG
    invariance.

---

## What's new in NCVROC 0.19.0

- **Public Execution Preview API (`plan_ncvroc_execution()`)**:
  - Previews candidate combinatorial workloads, evaluates scaling across all legal resource configurations, and returns a dedicated S3 `"ncvroc_execution_plan"` object.
  - Dedicated S3 methods: `print()`, `format()`, and base R diagnostic visualization `plot(plan, type = c("runtime", "speedup", "efficiency", "all"))`.
- **Empirical Setup-Aware Affine Runtime Estimation**:
  - Models execution scaling via an affine formulation $T(n) = a + b \cdot n$, distinguishing fixed cluster startup/export overhead ($a$) from candidate-dependent throughput ($b$) so cluster initialization time is not multiplied linearly by massive candidate counts.
  - Invalid, negative, or unstable fits safely fall back to conservative linear estimates without claiming exact or guaranteed runtimes.
- **Two-Gate Benchmark Trigger**:
  - **180-Second Primary Gate**: In `tuning = "auto"` mode, full multi-configuration benchmarking is triggered only when estimated serial runtime meets or exceeds 180 seconds (3 minutes); smaller workloads proceed directly with default/manual execution.
  - **5% Predicted Overhead Gate**: Caps benchmark sweep time at 5% of estimated production runtime; safely falls back to manual/default plans if the benchmark budget is insufficient.
- **Bounded Exhaustive Resource Sweep (Flat & CV Workflows)**:
  - Exhaustively evaluates all legal integer worker allocations (threads, socket chunks) up to the system/user CPU cap for `exhaustive_sum_roc()` and `cross_size_cv()`, applying the formal 5% near-best resource-efficient selection rule.
- **Explicit Nested Workflow Limitation**:
  - Nested cross-validation workflows (`nested_sum_roc()`, `cross_size_nested_cv()`) use safe, candidate-bounded runtime probing where supported.
  - Full nested resource sweep benchmarking is not performed in v0.19.0 when the evaluator cannot safely consume rank-bounded candidate subsets; in that case NCVROC preserves the manual/default execution plan and records the rationale in metadata. Full nested multi-configuration resource sweep is deferred to v0.20.0.
- **Observable Long-Running Execution & Progress UX**:
  - **Exact Completed Counts**: Reports exact completed-candidate counts at observable C++ batch boundaries for compiled evaluation loops.
  - **Observed-Only ETA**: Displays stable approximate remaining-time updates derived strictly from completed work batches.
  - **Truthful PSOCK Observability**: Opaque PSOCK socket backends report concise start and completion messages (`progress_unit = "none"`, `progress_mode = "start_completion"`) without advertising unverified progress percentages, ETAs, or fake heartbeats.
  - **Silence Contract**: `progress = FALSE` guarantees complete console silence and zero timing overhead.
- **Canonical Execution Metadata**:
  - Canonical `execution_plan` metadata includes `$progress_mode`, `$progress_unit`, `$benchmark_table`, and `$saturation_summary`.

---

## What's new in NCVROC 0.18.0

- **Automatic Execution Planning Foundation (`tuning = c("off", "auto", "always")`)**:
  - Automatically identifies a measured near-best execution configuration (serial, C++ multithreading, or PSOCK worker processes) from deterministic pilot micro-benchmarking across core combinatorial workflows (`exhaustive_sum_roc()`, `cross_size_cv()`, `nested_sum_roc()`, `cross_size_nested_cv()`).
  - **`tuning = "off"`**: Retains user-specified manual execution configuration (`parallel`, `n_workers`, `threads_per_worker`) without runtime probing overhead.
  - **`tuning = "auto"`**: Probes and benchmarks legal execution configurations only when estimated serial runtime meets or exceeds the trigger threshold.
  - **`tuning = "always"`**: Actively benchmarks legal execution configurations for all non-degenerate workloads.
- **Deterministic Micro-Pilot Benchmarking**:
  - Measures throughput over an evenly-spaced candidate subset while preserving the full sample size $N$, class proportions, and fold/repeat structure intact.
- **Resource-Efficient Selection Rule**:
  - Selects the fastest observed configuration, qualifies all plans within a 5% near-best envelope (`median_elapsed <= fastest * 1.05`), prioritizes the lowest resource allocation (`resource_count`), and applies deterministic backend priorities (`none` > `threads` > `outer` > `chunks` > `hybrid`).
- **Canonical Execution Metadata**:
  - Attached to result objects (`$settings$execution_plan` or `attr(..., "execution_plan")`) with dedicated S3 formatters (`format()`, `print()`) detailing benchmark timings, candidate allocations, and decision rationale.
- **Observation-Only Progress Reporting with Approximate ETA**:
  - Displays lightweight progress bars and approximate remaining time estimates on observable loops without altering candidate ordering, statistics, or RNG determinism (`.Random.seed`). `progress = FALSE` guarantees complete silence.
- **Strict Non-Statistical Invariance**:
  - Execution planning changes execution strategy only and never alters candidate spaces, fold/repeat partitions, classification cutoffs, candidate rankings, clinical constraints (`sens_min`, `spec_min`), out-of-fold predictions, final selected models, or final refits.

---

## Installation

```r
# Install NCVROC from GitHub
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

---

## Unified Cross-Validation & Selection API Hierarchy

NCVROC provides a structured hierarchy of functions covering execution preview, fixed-model evaluation, within-size selection, cross-size selection, nested cross-validation, and selection optimism assessment:

| Analysis Goal | $K$-Fold Cross-Validation | Leave-One-Out CV (LOOCV) |
| :--- | :--- | :--- |
| **Execution planning & preview** | `plan_ncvroc_execution()` | — |
| **Fixed model evaluation** (no selection) | `cv_sum_roc()` | `loocv_sum_roc()` |
| **Within-size model selection** (single size $K$) | `cv_select_sum_roc()` | `loocv_select_sum_roc()` |
| **Cross-size ordinary model selection** (multiple sizes) | `cross_size_cv()` | `cross_size_loocv()` |
| **Selection-procedure validation** (nested CV) | `cross_size_nested_cv()` | — *(Not supported)* |
| **Ordinary vs. Nested comparison** (selection optimism) | `compare_cv_selection()` | *(Ordinary LOOCV vs. Nested $K$-fold)* |
| **Candidate stability & optimism audit** | `candidate_stability_roc()` *(Repeated $K$-fold & Bootstrap)* | — |
| **Full automated analysis (single call)** | `ncvroc()` *(Nested CV + final exhaustive search)* | — |
| **Flat exhaustive combinatorial search** | `exhaustive_sum_roc()` / `roc_bruteforce()` | — |

---

## Execution Planning, Preview & Tuning

Large combinatorial searches across multiple item sizes can generate tens or hundreds of thousands of candidate models. Finding the most efficient execution configuration depends on dataset size, fold counts, CPU core availability, and operating system overhead.

### 1. Previewing Execution Scaling (`plan_ncvroc_execution`)

Before launching a long-running analysis, `plan_ncvroc_execution()` allows you to inspect candidate counts, compare legal execution configurations on a bounded pilot workload, and visualize observed scaling curves:

```r
library(NCVROC)

# Preview execution scaling for 1 to 4 item models
plan <- plan_ncvroc_execution(
  data        = analysis_dat,
  outcome     = y,
  items       = paste0("Q", 1:10),
  workflow    = "cross_size_cv",
  model_sizes = 1:4,
  folds       = 5
)

# Print execution plan summary and benchmark table
print(plan)

# Plot runtime, speedup, and parallel efficiency curves
plot(plan, type = "all")
```

### 2. Tuning Modes & Selection Hierarchy

Execution planning is integrated directly into `exhaustive_sum_roc()`, `cross_size_cv()`, `nested_sum_roc()`, and `cross_size_nested_cv()` via the `tuning` parameter:

```r
# 1. Manual mode (no planner overhead)
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "off")

# 2. Automatic planning (deterministic workload threshold)
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "auto")

# 3. Explicit benchmarking (always sweep legal configurations)
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "always")
```

When benchmarking is performed, NCVROC evaluates legal execution configurations and selects a measured near-best plan using the following deterministic hierarchy:

1. **Fastest observed time**: Finds the configuration with the lowest median benchmark elapsed time (`T_min`).
2. **5% near-best qualifying envelope**: Identifies all candidate plans where `median_elapsed <= T_min * 1.05`.
3. **Resource economy**: Chooses the plan requiring the fewest total CPU cores/workers (`min(resource_count)`).
4. **Backend simplicity priority**:
   - **Flat workloads**: `none` (serial) > `threads` (C++ multithreading) > `chunks` (PSOCK socket workers).
   - **Nested workloads**: `none` > `threads` > `outer` (PSOCK outer folds) > `chunks` > `hybrid`.
5. **Deterministic tie-breakers**: Lowest `median_elapsed` followed by lexicographical `plan_id`.

> [!NOTE]
> **Nested workflows in v0.20.0**: Nested cross-validation workflows use a bounded pilot workload when rank-bounded evaluation is available. The pilot preserves the full fold/repeat structure and does not remove candidates from the final exhaustive search.

---

## Parallel Execution

NCVROC supports four distinct parallel execution modes. `parallel` (manual execution mode) and `tuning` (automatic planning) are separate parameters:

| Mode | `parallel` | Description | Best For |
|---|---|---|---|
| **C++ Multi-Threading** | `"threads"` | Parallel combination evaluation within the main R process using native C++ threads via `RcppParallel` (shared memory, zero socket startup). | High-speed in-memory searches (`exhaustive_sum_roc()`, `cross_size_cv()`). |
| **Outer Fold Parallel** | `"outer"` (or `TRUE` in nested CV) | Parallel outer cross-validation folds using PSOCK worker processes. | Standard nested CV workflows across multiple folds (`nested_sum_roc()`, `cross_size_nested_cv()`, `ncvroc()`). |
| **Hybrid Nested CV** | `"hybrid"` | Evaluates outer folds with PSOCK workers and each fold's inner search with C++ threads. | High-core systems with large nested CV candidate spaces. |
| **Chunk Process Parallel** | `"chunks"` (or `TRUE` in exhaustive search) | Evaluates combination chunks across persistent PSOCK socket worker processes. | Massive searches using disk-backed chunked storage and process isolation. |

```r
# Manual C++ multi-threading with 4 threads:
res <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, parallel = "threads", n_workers = 4)

# Manual outer PSOCK fold parallel:
res <- cross_size_nested_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:3, parallel = "outer", n_workers = 4)

# Manual hybrid outer processes x inner C++ threads:
res <- cross_size_nested_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:3, parallel = "hybrid", n_workers = 2, threads_per_worker = 2)
```

---

## Progress Reporting & Observability

NCVROC includes observation-only progress reporting controlled by `progress`:

- **Exact Completed Counts**: On compiled serial and multithreaded loops, progress reports exact completed candidate counts at batch boundaries (e.g. `Evaluating 25,000 / 100,000 combinations (25.0%)`).
- **Observed elapsed time**: Progress reports elapsed time from completed work; estimated time remaining is not displayed.
- **Truthful PSOCK Boundaries**: PSOCK worker processes emit concise start and completion notifications (`progress_unit = "none"`) without generating unverified progress percentages.
- **Silence Contract**: `progress = FALSE` guarantees complete console silence and minimal execution overhead.

---

## Concise Item-Count Syntax

The `item_count` argument provides a concise alternative to `min_items` and `max_items` across `ncvroc()`, `cross_size_cv()`, `cross_size_nested_cv()`, `roc_bruteforce()`, and `cv_select_sum_roc()`:

| Syntax | Meaning | Equivalent Parameters |
|---|---|---|
| `item_count = "<=4"` | Scales with 1 through 4 items | `min_items = 1, max_items = 4` |
| `item_count = "==3"` | Scales with exactly 3 items | `min_items = 3, max_items = 3` (or `item_count = 3`) |
| `item_count = "2:4"` | Scales with 2 through 4 items | `min_items = 2, max_items = 4` |

---

## Result Storage & Caching

For large combinatorial searches, `ncvroc()` and `roc_bruteforce()` provide disk-backed storage and caching to manage memory and enable rapid re-runs:

- **`results_storage = c("auto", "memory", "rds", "none")`**: In `"auto"` mode, small searches stay in RAM while large searches (> 100,000 combinations) are saved to disk as chunked RDS files.
- **`cache = c("off", "reuse", "refresh")`**: In `"reuse"` mode, validated cached results are loaded instantly if the exact dataset, items, and search parameters match.

---

## Examples

### 1. Execution Preview & Scaling Diagnostics (`plan_ncvroc_execution`)

```r
library(NCVROC)

# Synthetic questionnaire data
set.seed(42)
n <- 120
analysis_dat <- data.frame(
  matrix(rbinom(n * 10, 1, 0.4), nrow = n, ncol = 10),
  y = rbinom(n, 1, 0.5)
)
names(analysis_dat)[1:10] <- paste0("Q", 1:10)

# Preview execution plans and scaling metrics
plan <- plan_ncvroc_execution(
  data        = analysis_dat,
  outcome     = y,
  items       = paste0("Q", 1:10),
  workflow    = "cross_size_cv",
  model_sizes = 1:4,
  folds       = 5
)
print(plan)
```

### 2. Fixed-Model Cross-Validation (`cv_sum_roc`)

```r
# Evaluate 5-fold CV for fixed scale Q1 + Q2 + Q3
cv_fit <- cv_sum_roc(
  data          = analysis_dat,
  outcome       = y,
  items         = c("Q1", "Q2", "Q3"),
  folds         = 5,
  repeats       = 3,
  cutoff_method = "youden",
  seed          = 42
)
print(cv_fit)
```

### 3. Cross-Size Ordinary Model Selection (`cross_size_cv`)

```r
# Select the best model across 1 to 4 items via 5-fold CV with automatic tuning
ord_selection <- cross_size_cv(
  data             = analysis_dat,
  outcome          = y,
  items            = paste0("Q", 1:10),
  model_sizes      = 1:4,
  selection_metric = "youden",
  folds            = 5,
  repeats          = 2,
  tuning           = "auto",
  seed             = 42
)
print(ord_selection)
```

### 4. Selection-Procedure Generalization Validation (`cross_size_nested_cv`)

```r
# Evaluate model-selection procedure across 1 to 3 items
nested_val <- cross_size_nested_cv(
  data             = analysis_dat,
  outcome          = y,
  items            = paste0("Q", 1:10),
  model_sizes      = 1:3,
  selection_metric = "auc",
  outer_folds      = 5,
  inner_folds      = 4,
  outer_repeats    = 1,
  seed             = 42
)
print(nested_val)
```

### 5. Comparing Ordinary Selection vs. Nested Validation (`compare_cv_selection`)

```r
comp <- compare_cv_selection(
  data             = analysis_dat,
  outcome          = y,
  items            = paste0("Q", 1:10),
  model_sizes      = 1:3,
  selection_metric = "auc",
  folds            = 5,
  outer_folds      = 5,
  inner_folds      = 4,
  outer_repeats    = 1,
  seed             = 42
)
print(comp)
```

### 6. Candidate Stability & Optimism Audit (`candidate_stability_roc`)

```r
# Audit stability of specific candidate scales
cand_audit <- candidate_stability_roc(
  data            = analysis_dat,
  outcome         = y,
  candidate_sets  = list(
    "Scale_A" = c("Q1", "Q2", "Q3"),
    "Scale_B" = c("Q1", "Q4", "Q5"),
    "Scale_C" = c("Q2", "Q3", "Q4")
  ),
  resampling      = "repeated_cv",
  folds           = 5,
  repeats         = 10,
  sensitivity_min = 0.70,
  specificity_min = 0.60,
  seed            = 42
)
print(cand_audit)
```

### 7. Full Automated Analysis (`ncvroc`)

```r
result <- ncvroc(
  data                = analysis_dat,
  outcome             = y,
  items               = Q1:Q10,
  item_count          = "<=3",
  mode                = "balanced",
  outer_k             = 5,
  inner_k             = 4,
  outer_repeats       = 1,
  selection_criterion = "auc",
  final_rank_by       = "auc",
  final_top_n         = 10,
  seed                = 42
)
print(result)
```

### 8. Clinical Constraint Filtering (`ncvroc_results`)

```r
# Filter candidate models requiring sensitivity >= 0.70 and specificity >= 0.30
filtered <- ncvroc_results(
  result,
  sensitivity = ">= 0.70",
  specificity = ">= 0.30",
  rank_by     = "youden",
  top_n       = 5
)
print(filtered)
```

---

## Core Assumptions

1. **Higher score = more likely positive.** Reverse-code items beforehand if needed.
2. **Cutoff rule:** `predicted_positive = score >= cutoff`.
3. **AUC with ties:** $\text{AUC} = P(\text{pos} > \text{neg}) + 0.5 \times P(\text{pos} == \text{neg})$.
4. **Missing values:** Empty strings and whitespace-only values are treated as missing. Rows with missing values in the outcome or selected item columns are removed before analysis.
5. **Strict binary outcome:** Outcome column must contain only `positive_label` and `negative_label` values.

---

## References

- Fawcett, T. (2006). An introduction to ROC analysis. *Pattern Recognition Letters*, 27(8), 861–874. [doi:10.1016/j.patrec.2005.10.010](https://doi.org/10.1016/j.patrec.2005.10.010)
- Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91. [doi:10.1186/1471-2105-7-91](https://doi.org/10.1186/1471-2105-7-91)
- Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer*, 3(1), 32–35.
