// src/exhaustive_sum_roc_cpp.cpp
// Rcpp backend for exhaustive_sum_roc() combo-evaluation loop.
// Supports single-thread and RcppParallel multi-threaded evaluation.
// Returns numeric metrics only — items/rank applied in R.

#include <Rcpp.h>
#include <RcppParallel.h>
#include <map>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

enum CutoffMethod {
  CUTOFF_YOUDEN = 1,
  CUTOFF_CLOSEST_TOPLEFT = 2
};

// Compute binomial coefficient (n choose k) using multiplicative formula.
// Returns 0 for invalid inputs (k < 0, k > n, n < 0).
static inline double binom(int n, int k) {
  if (k < 0 || k > n || n < 0) return 0.0;
  if (k == 0 || k == n) return 1.0;
  if (k > n - k) k = n - k;
  double result = 1.0;
  for (int i = 1; i <= k; i++) {
    result = result * (n - k + i) / i;
  }
  return result;
}

// Unrank a single combination: 0-based rank -> 0-based indices into result vector.
// Matches R's combn(0:(n-1), k) lexicographic column order.
// rank must be in [0, choose(n, k) - 1].
static inline void unrank_combination_into(int n, int k, double rank, std::vector<int>& result) {
  result.resize(k);
  int next_min = 0;
  double remaining_rank = rank;

  for (int position = 0; position < k; position++) {
    int remaining_slots = k - position - 1;
    int max_value = n - remaining_slots - 1;

    for (int candidate = next_min; candidate <= max_value; candidate++) {
      double block_size = binom(n - candidate - 1, remaining_slots);

      if (remaining_rank < block_size) {
        result[position] = candidate;
        next_min = candidate + 1;
        break;
      }
      remaining_rank -= block_size;
    }
  }
}

// Working buffers encapsulated for per-range / thread-local evaluation
struct ThreadLocalBuffer {
  std::vector<double> scores;
  std::map<double, int> pos_counts;
  std::map<double, int> neg_counts;
  std::vector<double> unique_scores;
  std::vector<double> cum_pos, cum_neg;
  std::vector<double> tp, fp, fn, tn;
  std::vector<double> sensitivity, specificity, youden_vals, accuracy;
  std::vector<double> ppv_vals, npv_vals;
  std::vector<int> cols;

  explicit ThreadLocalBuffer(int n) : scores(n) {}
};

// Pure C++ single candidate evaluation (zero R/Rcpp API calls)
static inline void evaluate_single_candidate(
    double global_rank,
    const double* x_ptr, // rows = n, cols = n_cols, column-major: x_ptr[i + col * n]
    const int* y_ptr,    // length = n
    int n,
    int n_cols,
    int min_items,
    int n_k,
    const std::vector<double>& level_starts,
    int total_pos,
    int total_neg,
    int total_n,
    CutoffMethod cutoff_method,
    ThreadLocalBuffer& buf,
    // Outputs
    int& out_n_items_val,
    double& out_auc_val,
    double& out_cutoff_val,
    double& out_sensitivity_val,
    double& out_specificity_val,
    double& out_youden_val,
    double& out_accuracy_val,
    double& out_ppv_val,
    double& out_npv_val
) {
  // Find k-level using upper_bound on level_starts
  auto it = std::upper_bound(level_starts.begin(), level_starts.end(), global_rank);
  int ki = std::distance(level_starts.begin(), it) - 1;
  int k = min_items + ki;
  double local_rank = global_rank - level_starts[ki];

  out_n_items_val = k;

  // Unrank combination
  unrank_combination_into(n_cols, k, local_rank, buf.cols);

  // ---- 1. Compute sum scores ----
  for (int i = 0; i < n; i++) {
    double s = 0.0;
    for (int j = 0; j < k; j++) {
      s += x_ptr[i + buf.cols[j] * n];
    }
    buf.scores[i] = s;
  }

  // ---- 2. Frequency table ----
  buf.pos_counts.clear();
  buf.neg_counts.clear();
  for (int i = 0; i < n; i++) {
    if (y_ptr[i] == 1) {
      buf.pos_counts[buf.scores[i]]++;
    } else {
      buf.neg_counts[buf.scores[i]]++;
    }
  }

  // ---- 3. AUC ----
  if (total_pos == 0 || total_neg == 0) {
    out_auc_val = NA_REAL;
    out_cutoff_val = NA_REAL;
    out_sensitivity_val = NA_REAL;
    out_specificity_val = NA_REAL;
    out_youden_val = NA_REAL;
    out_accuracy_val = NA_REAL;
    out_ppv_val = NA_REAL;
    out_npv_val = NA_REAL;
    return;
  }

  double auc_sum = 0.0;
  for (const auto &p : buf.pos_counts) {
    double sp = p.first;
    int pc = p.second;
    for (const auto &neg : buf.neg_counts) {
      double sn = neg.first;
      int nc = neg.second;
      double pair_count = (double)pc * nc;
      if (sp > sn) {
        auc_sum += pair_count;
      } else if (sp == sn) {
        auc_sum += 0.5 * pair_count;
      }
    }
  }
  out_auc_val = auc_sum / ((double)total_pos * total_neg);

  // ---- 4. ROC metrics ----
  buf.unique_scores.clear();
  for (const auto &p : buf.pos_counts) {
    buf.unique_scores.push_back(p.first);
  }
  for (const auto &neg : buf.neg_counts) {
    bool found = false;
    for (double s : buf.unique_scores) {
      if (s == neg.first) { found = true; break; }
    }
    if (!found) buf.unique_scores.push_back(neg.first);
  }
  std::sort(buf.unique_scores.begin(), buf.unique_scores.end(),
            std::greater<double>());

  int n_scores = buf.unique_scores.size();
  buf.cum_pos.resize(n_scores);
  buf.cum_neg.resize(n_scores);

  for (int si = 0; si < n_scores; si++) {
    double sc = buf.unique_scores[si];
    int prev_pos = (si == 0) ? 0 : buf.cum_pos[si - 1];
    int prev_neg = (si == 0) ? 0 : buf.cum_neg[si - 1];
    auto it_pos = buf.pos_counts.find(sc);
    int pos_c = (it_pos != buf.pos_counts.end()) ? it_pos->second : 0;
    auto it_neg = buf.neg_counts.find(sc);
    int neg_c = (it_neg != buf.neg_counts.end()) ? it_neg->second : 0;
    buf.cum_pos[si] = prev_pos + pos_c;
    buf.cum_neg[si] = prev_neg + neg_c;
  }

  buf.tp.resize(n_scores);
  buf.fp.resize(n_scores);
  buf.fn.resize(n_scores);
  buf.tn.resize(n_scores);
  buf.sensitivity.resize(n_scores);
  buf.specificity.resize(n_scores);
  buf.youden_vals.resize(n_scores);
  buf.accuracy.resize(n_scores);
  buf.ppv_vals.resize(n_scores);
  buf.npv_vals.resize(n_scores);

  for (int si = 0; si < n_scores; si++) {
    buf.tp[si] = buf.cum_pos[si];
    buf.fp[si] = buf.cum_neg[si];
    buf.fn[si] = total_pos - buf.tp[si];
    buf.tn[si] = total_neg - buf.fp[si];

    buf.sensitivity[si] = buf.tp[si] / total_pos;
    buf.specificity[si] = buf.tn[si] / total_neg;
    buf.youden_vals[si] = buf.sensitivity[si] + buf.specificity[si] - 1.0;
    buf.accuracy[si] = (buf.tp[si] + buf.tn[si]) / (double)total_n;

    if (buf.tp[si] + buf.fp[si] > 0) {
      buf.ppv_vals[si] = buf.tp[si] / (buf.tp[si] + buf.fp[si]);
    } else {
      buf.ppv_vals[si] = NA_REAL;
    }
    if (buf.tn[si] + buf.fn[si] > 0) {
      buf.npv_vals[si] = buf.tn[si] / (buf.tn[si] + buf.fn[si]);
    } else {
      buf.npv_vals[si] = NA_REAL;
    }
  }

  // ---- 5. Optimal cutoff ----
  int best_idx = 0;

  if (cutoff_method == CUTOFF_YOUDEN) {
    double best_youden = -2.0, best_sens = -1.0, best_spec = -1.0;
    double best_cutoff_val = R_PosInf;
    for (int si = 0; si < n_scores; si++) {
      double yd = buf.youden_vals[si];
      double se = buf.sensitivity[si];
      double sp = buf.specificity[si];
      double co = buf.unique_scores[si];

      if (yd > best_youden ||
          (yd == best_youden && se > best_sens) ||
          (yd == best_youden && se == best_sens && sp > best_spec) ||
          (yd == best_youden && se == best_sens && sp == best_spec && co < best_cutoff_val)) {
        best_youden = yd;
        best_sens = se;
        best_spec = sp;
        best_cutoff_val = co;
        best_idx = si;
      }
    }
  } else if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT) {
    double best_dist = R_PosInf;
    double best_youden = -2.0;
    double best_cutoff_val = R_PosInf;
    for (int si = 0; si < n_scores; si++) {
      double d = std::sqrt(
        (1.0 - buf.sensitivity[si]) * (1.0 - buf.sensitivity[si]) +
        (1.0 - buf.specificity[si]) * (1.0 - buf.specificity[si]));
      double yd = buf.youden_vals[si];
      double co = buf.unique_scores[si];

      if (d < best_dist ||
          (d == best_dist && yd > best_youden) ||
          (d == best_dist && yd == best_youden && co < best_cutoff_val)) {
        best_dist = d;
        best_youden = yd;
        best_cutoff_val = co;
        best_idx = si;
      }
    }
  }

  out_cutoff_val      = buf.unique_scores[best_idx];
  out_sensitivity_val = buf.sensitivity[best_idx];
  out_specificity_val = buf.specificity[best_idx];
  out_youden_val      = buf.youden_vals[best_idx];
  out_accuracy_val    = buf.accuracy[best_idx];
  out_ppv_val         = buf.ppv_vals[best_idx];
  out_npv_val         = buf.npv_vals[best_idx];
}

// [[Rcpp::export]]
DataFrame evaluate_combos_cpp(
    NumericMatrix x,           // rows=subjects, cols=items
    IntegerVector y,           // 0/1 outcome, length = nrow(x)
    List combo_indices,        // list of IntegerVector, each 0-based col indices
    std::string cutoff_method  // "youden" or "closest_topleft"
) {
  int n = x.nrow();
  int n_combos = combo_indices.size();

  // Output column vectors
  IntegerVector out_n_items(n_combos);
  NumericVector out_auc(n_combos);
  NumericVector out_cutoff(n_combos);
  NumericVector out_sensitivity(n_combos);
  NumericVector out_specificity(n_combos);
  NumericVector out_youden(n_combos);
  NumericVector out_accuracy(n_combos);
  NumericVector out_ppv(n_combos);
  NumericVector out_npv(n_combos);
  IntegerVector out_n_positive(n_combos);
  IntegerVector out_n_negative(n_combos);

  // Count total positives and negatives
  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) {
    if (y[i] == 1) total_pos++; else total_neg++;
  }
  int total_n = total_pos + total_neg;

  // Per-combo working buffers
  std::vector<double> scores(n);
  std::map<double, int> pos_counts;
  std::map<double, int> neg_counts;

  // For ROC metrics computation
  std::vector<double> unique_scores;
  std::vector<double> cum_pos, cum_neg;
  std::vector<double> tp, fp, fn, tn;
  std::vector<double> sensitivity, specificity, youden_vals, accuracy;
  std::vector<double> ppv_vals, npv_vals;

  for (int ci = 0; ci < n_combos; ci++) {
    IntegerVector cols = combo_indices[ci];
    int k = cols.size();
    out_n_items[ci] = k;
    out_n_positive[ci] = total_pos;
    out_n_negative[ci] = total_neg;

    // ---- 1. Compute sum scores ----
    for (int i = 0; i < n; i++) {
      double s = 0.0;
      for (int j = 0; j < k; j++) {
        s += x(i, cols[j]);
      }
      scores[i] = s;
    }

    // ---- 2. Frequency table ----
    pos_counts.clear();
    neg_counts.clear();
    for (int i = 0; i < n; i++) {
      if (y[i] == 1) {
        pos_counts[scores[i]]++;
      } else {
        neg_counts[scores[i]]++;
      }
    }

    // ---- 3. AUC ----
    if (total_pos == 0 || total_neg == 0) {
      out_auc[ci] = NA_REAL;
      out_cutoff[ci] = NA_REAL;
      out_sensitivity[ci] = NA_REAL;
      out_specificity[ci] = NA_REAL;
      out_youden[ci] = NA_REAL;
      out_accuracy[ci] = NA_REAL;
      out_ppv[ci] = NA_REAL;
      out_npv[ci] = NA_REAL;
      continue;
    }

    double auc_sum = 0.0;
    for (auto &p : pos_counts) {
      double sp = p.first;
      int pc = p.second;
      for (auto &n : neg_counts) {
        double sn = n.first;
        int nc = n.second;
        double pair_count = (double)pc * nc;
        if (sp > sn) {
          auc_sum += pair_count;
        } else if (sp == sn) {
          auc_sum += 0.5 * pair_count;
        }
      }
    }
    out_auc[ci] = auc_sum / ((double)total_pos * total_neg);

    // ---- 4. ROC metrics: sort scores descending, cumsum ----
    unique_scores.clear();
    for (auto &p : pos_counts) {
      unique_scores.push_back(p.first);
    }
    for (auto &n : neg_counts) {
      bool found = false;
      for (double s : unique_scores) {
        if (s == n.first) { found = true; break; }
      }
      if (!found) unique_scores.push_back(n.first);
    }
    std::sort(unique_scores.begin(), unique_scores.end(),
              std::greater<double>());

    int n_scores = unique_scores.size();
    cum_pos.resize(n_scores);
    cum_neg.resize(n_scores);

    for (int si = 0; si < n_scores; si++) {
      double sc = unique_scores[si];
      int prev_pos = (si == 0) ? 0 : cum_pos[si - 1];
      int prev_neg = (si == 0) ? 0 : cum_neg[si - 1];
      cum_pos[si] = prev_pos + pos_counts[sc];
      cum_neg[si] = prev_neg + neg_counts[sc];
    }

    tp.resize(n_scores);
    fp.resize(n_scores);
    fn.resize(n_scores);
    tn.resize(n_scores);
    sensitivity.resize(n_scores);
    specificity.resize(n_scores);
    youden_vals.resize(n_scores);
    accuracy.resize(n_scores);
    ppv_vals.resize(n_scores);
    npv_vals.resize(n_scores);

    for (int si = 0; si < n_scores; si++) {
      tp[si] = cum_pos[si];
      fp[si] = cum_neg[si];
      fn[si] = total_pos - tp[si];
      tn[si] = total_neg - fp[si];

      sensitivity[si] = tp[si] / total_pos;
      specificity[si] = tn[si] / total_neg;
      youden_vals[si] = sensitivity[si] + specificity[si] - 1.0;
      accuracy[si] = (tp[si] + tn[si]) / (double)total_n;

      if (tp[si] + fp[si] > 0) {
        ppv_vals[si] = tp[si] / (tp[si] + fp[si]);
      } else {
        ppv_vals[si] = NA_REAL;
      }

      if (tn[si] + fn[si] > 0) {
        npv_vals[si] = tn[si] / (tn[si] + fn[si]);
      } else {
        npv_vals[si] = NA_REAL;
      }
    }

    // ---- 5. Optimal cutoff ----
    int best_idx = 0;

    if (cutoff_method == "youden") {
      double best_youden = -2.0, best_sens = -1.0, best_spec = -1.0;
      double best_cutoff_val = R_PosInf;
      for (int si = 0; si < n_scores; si++) {
        double yd = youden_vals[si];
        double se = sensitivity[si];
        double sp = specificity[si];
        double co = unique_scores[si];

        if (yd > best_youden ||
            (yd == best_youden && se > best_sens) ||
            (yd == best_youden && se == best_sens && sp > best_spec) ||
            (yd == best_youden && se == best_sens && sp == best_spec && co < best_cutoff_val)) {
          best_youden = yd;
          best_sens = se;
          best_spec = sp;
          best_cutoff_val = co;
          best_idx = si;
        }
      }
    } else if (cutoff_method == "closest_topleft") {
      double best_dist = R_PosInf;
      double best_youden = -2.0;
      double best_cutoff_val = R_PosInf;
      for (int si = 0; si < n_scores; si++) {
        double d = std::sqrt(
          (1.0 - sensitivity[si]) * (1.0 - sensitivity[si]) +
          (1.0 - specificity[si]) * (1.0 - specificity[si]));
        double yd = youden_vals[si];
        double co = unique_scores[si];

        if (d < best_dist ||
            (d == best_dist && yd > best_youden) ||
            (d == best_dist && yd == best_youden && co < best_cutoff_val)) {
          best_dist = d;
          best_youden = yd;
          best_cutoff_val = co;
          best_idx = si;
        }
      }
    } else {
      stop("Unknown cutoff_method: '%s'", cutoff_method);
    }

    out_cutoff[ci]      = unique_scores[best_idx];
    out_sensitivity[ci] = sensitivity[best_idx];
    out_specificity[ci] = specificity[best_idx];
    out_youden[ci]      = youden_vals[best_idx];
    out_accuracy[ci]    = accuracy[best_idx];
    out_ppv[ci]         = ppv_vals[best_idx];
    out_npv[ci]         = npv_vals[best_idx];
  }

  return DataFrame::create(
    _["n_items"]     = out_n_items,
    _["auc"]         = out_auc,
    _["cutoff"]      = out_cutoff,
    _["sensitivity"] = out_sensitivity,
    _["specificity"] = out_specificity,
    _["youden"]      = out_youden,
    _["accuracy"]    = out_accuracy,
    _["ppv"]         = out_ppv,
    _["npv"]         = out_npv,
    _["n_positive"]  = out_n_positive,
    _["n_negative"]  = out_n_negative
  );
}

// [[Rcpp::export]]
DataFrame evaluate_combos_cpp_chunk(
    NumericMatrix x,           // rows=subjects, cols=items
    IntegerVector y,           // 0/1 outcome, length = nrow(x)
    int min_items,             // minimum items per combination
    int max_items,             // maximum items per combination
    std::string cutoff_method, // "youden" or "closest_topleft"
    double chunk_start,        // zero-based global combination index
    int chunk_size             // number of combos to evaluate (may be < at tail)
) {
  int n = x.nrow();
  int n_cols = x.ncol();

  // Validate cutoff method upfront
  CutoffMethod cm;
  if (cutoff_method == "youden") {
    cm = CUTOFF_YOUDEN;
  } else if (cutoff_method == "closest_topleft") {
    cm = CUTOFF_CLOSEST_TOPLEFT;
  } else {
    stop("Unknown cutoff_method: '%s'", cutoff_method);
  }

  // Cap max_items at n_cols to avoid overflow
  if (max_items > n_cols) max_items = n_cols;
  if (min_items > max_items) min_items = max_items;
  if (min_items < 1) min_items = 1;

  // Compute k-level sizes and total combos
  int n_k = max_items - min_items + 1;
  std::vector<double> level_sizes(n_k);
  std::vector<double> level_starts(n_k);
  double total = 0.0;
  for (int ki = 0; ki < n_k; ki++) {
    int k = min_items + ki;
    level_sizes[ki] = binom(n_cols, k);
    level_starts[ki] = total;
    total += level_sizes[ki];
  }

  if (chunk_start < 0 || chunk_start >= total) {
    stop("chunk_start is outside the combination range.");
  }

  double chunk_end = chunk_start + (double)chunk_size;
  if (chunk_end > total) chunk_end = total;
  int actual_size = (int)(chunk_end - chunk_start);

  IntegerVector out_n_items(actual_size);
  NumericVector out_auc(actual_size);
  NumericVector out_cutoff(actual_size);
  NumericVector out_sensitivity(actual_size);
  NumericVector out_specificity(actual_size);
  NumericVector out_youden(actual_size);
  NumericVector out_accuracy(actual_size);
  NumericVector out_ppv(actual_size);
  NumericVector out_npv(actual_size);
  IntegerVector out_n_positive(actual_size);
  IntegerVector out_n_negative(actual_size);

  // Count total positives and negatives
  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) {
    if (y[i] == 1) total_pos++; else total_neg++;
  }
  int total_n = total_pos + total_neg;

  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];

  ThreadLocalBuffer buf(n);

  for (int gi = 0; gi < actual_size; gi++) {
    double global_rank = chunk_start + (double)gi;
    int n_items_val = 0;
    double auc_val = 0.0, cutoff_val = 0.0, sens_val = 0.0, spec_val = 0.0;
    double youden_val = 0.0, acc_val = 0.0, ppv_val = 0.0, npv_val = 0.0;

    evaluate_single_candidate(
      global_rank,
      x_ptr,
      y_ptr,
      n,
      n_cols,
      min_items,
      n_k,
      level_starts,
      total_pos,
      total_neg,
      total_n,
      cm,
      buf,
      n_items_val,
      auc_val,
      cutoff_val,
      sens_val,
      spec_val,
      youden_val,
      acc_val,
      ppv_val,
      npv_val
    );

    out_n_items[gi]     = n_items_val;
    out_auc[gi]         = auc_val;
    out_cutoff[gi]      = cutoff_val;
    out_sensitivity[gi] = sens_val;
    out_specificity[gi] = spec_val;
    out_youden[gi]      = youden_val;
    out_accuracy[gi]    = acc_val;
    out_ppv[gi]         = ppv_val;
    out_npv[gi]         = npv_val;
    out_n_positive[gi]  = total_pos;
    out_n_negative[gi]  = total_neg;
  }

  return DataFrame::create(
    _["n_items"]     = out_n_items,
    _["auc"]         = out_auc,
    _["cutoff"]      = out_cutoff,
    _["sensitivity"] = out_sensitivity,
    _["specificity"] = out_specificity,
    _["youden"]      = out_youden,
    _["accuracy"]    = out_accuracy,
    _["ppv"]         = out_ppv,
    _["npv"]         = out_npv,
    _["n_positive"]  = out_n_positive,
    _["n_negative"]  = out_n_negative
  );
}

// RcppParallel Worker for evaluate_combos_cpp_chunk_parallel
struct ChunkEvaluatorWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n;
  int n_cols;
  int min_items;
  int n_k;
  const std::vector<double>& level_starts;
  int total_pos;
  int total_neg;
  int total_n;
  CutoffMethod cutoff_method;
  double chunk_start;
  const double* explicit_ranks;

  // Output vectors accessed thread-safely via RVector
  RcppParallel::RVector<int> out_n_items;
  RcppParallel::RVector<double> out_auc;
  RcppParallel::RVector<double> out_cutoff;
  RcppParallel::RVector<double> out_sensitivity;
  RcppParallel::RVector<double> out_specificity;
  RcppParallel::RVector<double> out_youden;
  RcppParallel::RVector<double> out_accuracy;
  RcppParallel::RVector<double> out_ppv;
  RcppParallel::RVector<double> out_npv;
  RcppParallel::RVector<int> out_n_positive;
  RcppParallel::RVector<int> out_n_negative;

  ChunkEvaluatorWorker(
      const double* x_ptr,
      const int* y_ptr,
      int n,
      int n_cols,
      int min_items,
      int n_k,
      const std::vector<double>& level_starts,
      int total_pos,
      int total_neg,
      int total_n,
      CutoffMethod cutoff_method,
      double chunk_start,
      Rcpp::IntegerVector& out_n_items,
      Rcpp::NumericVector& out_auc,
      Rcpp::NumericVector& out_cutoff,
      Rcpp::NumericVector& out_sensitivity,
      Rcpp::NumericVector& out_specificity,
      Rcpp::NumericVector& out_youden,
      Rcpp::NumericVector& out_accuracy,
      Rcpp::NumericVector& out_ppv,
      Rcpp::NumericVector& out_npv,
      Rcpp::IntegerVector& out_n_positive,
      Rcpp::IntegerVector& out_n_negative
  ) : x_ptr(x_ptr), y_ptr(y_ptr), n(n), n_cols(n_cols), min_items(min_items),
      n_k(n_k), level_starts(level_starts), total_pos(total_pos), total_neg(total_neg),
      total_n(total_n), cutoff_method(cutoff_method), chunk_start(chunk_start), explicit_ranks(NULL),
      out_n_items(out_n_items), out_auc(out_auc), out_cutoff(out_cutoff),
      out_sensitivity(out_sensitivity), out_specificity(out_specificity),
      out_youden(out_youden), out_accuracy(out_accuracy), out_ppv(out_ppv),
      out_npv(out_npv), out_n_positive(out_n_positive), out_n_negative(out_n_negative) {}

  void operator()(std::size_t begin, std::size_t end) {
    ThreadLocalBuffer buf(n);
    for (std::size_t gi = begin; gi < end; gi++) {
      double global_rank = explicit_ranks ? explicit_ranks[gi] : chunk_start + (double)gi;
      int n_items_val = 0;
      double auc_val = 0.0, cutoff_val = 0.0, sens_val = 0.0, spec_val = 0.0;
      double youden_val = 0.0, acc_val = 0.0, ppv_val = 0.0, npv_val = 0.0;

      evaluate_single_candidate(
        global_rank,
        x_ptr,
        y_ptr,
        n,
        n_cols,
        min_items,
        n_k,
        level_starts,
        total_pos,
        total_neg,
        total_n,
        cutoff_method,
        buf,
        n_items_val,
        auc_val,
        cutoff_val,
        sens_val,
        spec_val,
        youden_val,
        acc_val,
        ppv_val,
        npv_val
      );

      out_n_items[gi]     = n_items_val;
      out_auc[gi]         = auc_val;
      out_cutoff[gi]      = cutoff_val;
      out_sensitivity[gi] = sens_val;
      out_specificity[gi] = spec_val;
      out_youden[gi]      = youden_val;
      out_accuracy[gi]    = acc_val;
      out_ppv[gi]         = ppv_val;
      out_npv[gi]         = npv_val;
      out_n_positive[gi]  = total_pos;
      out_n_negative[gi]  = total_neg;
    }
  }
};

// [[Rcpp::export]]
DataFrame evaluate_combos_cpp_chunk_parallel(
    NumericMatrix x,           // rows=subjects, cols=items
    IntegerVector y,           // 0/1 outcome, length = nrow(x)
    int min_items,             // minimum items per combination
    int max_items,             // maximum items per combination
    std::string cutoff_method, // "youden" or "closest_topleft"
    double chunk_start,        // zero-based global combination index
    int chunk_size,            // number of combos to evaluate
    int num_threads = -1,      // scoped thread count (-1 = default)
    std::size_t grain_size = 1000 // scheduler grain size
) {
  int n = x.nrow();
  int n_cols = x.ncol();

  // Validate cutoff method upfront
  CutoffMethod cm;
  if (cutoff_method == "youden") {
    cm = CUTOFF_YOUDEN;
  } else if (cutoff_method == "closest_topleft") {
    cm = CUTOFF_CLOSEST_TOPLEFT;
  } else {
    stop("Unknown cutoff_method: '%s'", cutoff_method);
  }

  // Cap max_items at n_cols to avoid overflow
  if (max_items > n_cols) max_items = n_cols;
  if (min_items > max_items) min_items = max_items;
  if (min_items < 1) min_items = 1;

  // Compute k-level sizes and total combos
  int n_k = max_items - min_items + 1;
  std::vector<double> level_sizes(n_k);
  std::vector<double> level_starts(n_k);
  double total = 0.0;
  for (int ki = 0; ki < n_k; ki++) {
    int k = min_items + ki;
    level_sizes[ki] = binom(n_cols, k);
    level_starts[ki] = total;
    total += level_sizes[ki];
  }

  if (chunk_start < 0 || chunk_start >= total) {
    stop("chunk_start is outside the combination range.");
  }

  double chunk_end = chunk_start + (double)chunk_size;
  if (chunk_end > total) chunk_end = total;
  int actual_size = (int)(chunk_end - chunk_start);

  IntegerVector out_n_items(actual_size);
  NumericVector out_auc(actual_size);
  NumericVector out_cutoff(actual_size);
  NumericVector out_sensitivity(actual_size);
  NumericVector out_specificity(actual_size);
  NumericVector out_youden(actual_size);
  NumericVector out_accuracy(actual_size);
  NumericVector out_ppv(actual_size);
  NumericVector out_npv(actual_size);
  IntegerVector out_n_positive(actual_size);
  IntegerVector out_n_negative(actual_size);

  // Count total positives and negatives
  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) {
    if (y[i] == 1) total_pos++; else total_neg++;
  }
  int total_n = total_pos + total_neg;

  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];

  ChunkEvaluatorWorker worker(
    x_ptr,
    y_ptr,
    n,
    n_cols,
    min_items,
    n_k,
    level_starts,
    total_pos,
    total_neg,
    total_n,
    cm,
    chunk_start,
    out_n_items,
    out_auc,
    out_cutoff,
    out_sensitivity,
    out_specificity,
    out_youden,
    out_accuracy,
    out_ppv,
    out_npv,
    out_n_positive,
    out_n_negative
  );

  RcppParallel::parallelFor(0, (std::size_t)actual_size, worker, grain_size, num_threads);

  return DataFrame::create(
    _["n_items"]     = out_n_items,
    _["auc"]         = out_auc,
    _["cutoff"]      = out_cutoff,
    _["sensitivity"] = out_sensitivity,
    _["specificity"] = out_specificity,
    _["youden"]      = out_youden,
    _["accuracy"]    = out_accuracy,
    _["ppv"]         = out_ppv,
    _["npv"]         = out_npv,
    _["n_positive"]  = out_n_positive,
    _["n_negative"]  = out_n_negative
  );
}

// [[Rcpp::export]]
DataFrame evaluate_combos_cpp_sparse_parallel(
    NumericMatrix x,
    IntegerVector y,
    int min_items,
    int max_items,
    std::string cutoff_method,
    NumericVector global_ranks,
    int num_threads = -1
) {
  int n = x.nrow();
  int n_cols = x.ncol();
  CutoffMethod cm;
  if (cutoff_method == "youden") cm = CUTOFF_YOUDEN;
  else if (cutoff_method == "closest_topleft") cm = CUTOFF_CLOSEST_TOPLEFT;
  else stop("Unknown cutoff_method: '%s'", cutoff_method);
  if (max_items > n_cols) max_items = n_cols;
  if (min_items > max_items) min_items = max_items;
  if (min_items < 1) min_items = 1;
  int n_k = max_items - min_items + 1;
  std::vector<double> level_starts(n_k);
  double total = 0.0;
  for (int ki = 0; ki < n_k; ki++) {
    level_starts[ki] = total;
    total += binom(n_cols, min_items + ki);
  }
  int actual_size = global_ranks.size();
  for (int i = 0; i < actual_size; i++) {
    if (!R_finite(global_ranks[i]) || global_ranks[i] < 0 ||
        global_ranks[i] >= total || std::floor(global_ranks[i]) != global_ranks[i]) {
      stop("global_ranks must contain valid integral exhaustive candidate ranks.");
    }
  }
  IntegerVector out_n_items(actual_size), out_n_positive(actual_size), out_n_negative(actual_size);
  NumericVector out_auc(actual_size), out_cutoff(actual_size), out_sensitivity(actual_size),
    out_specificity(actual_size), out_youden(actual_size), out_accuracy(actual_size),
    out_ppv(actual_size), out_npv(actual_size);
  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) { if (y[i] == 1) total_pos++; else total_neg++; }
  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];
  ChunkEvaluatorWorker worker(x_ptr, y_ptr, n, n_cols, min_items, n_k, level_starts,
    total_pos, total_neg, total_pos + total_neg, cm, 0.0, out_n_items, out_auc,
    out_cutoff, out_sensitivity, out_specificity, out_youden, out_accuracy, out_ppv,
    out_npv, out_n_positive, out_n_negative);
  worker.explicit_ranks = &global_ranks[0];
  RcppParallel::parallelFor(0, (std::size_t)actual_size, worker, 1, num_threads);
  return DataFrame::create(
    _["n_items"] = out_n_items, _["auc"] = out_auc, _["cutoff"] = out_cutoff,
    _["sensitivity"] = out_sensitivity, _["specificity"] = out_specificity,
    _["youden"] = out_youden, _["accuracy"] = out_accuracy, _["ppv"] = out_ppv,
    _["npv"] = out_npv, _["n_positive"] = out_n_positive,
    _["n_negative"] = out_n_negative
  );
}

// -----------------------------------------------------------------------------
// CV Candidate Evaluator for Strategy 2 (Cutoff-Dependent Selection)
// -----------------------------------------------------------------------------

struct CvThreadBuffer {
  std::vector<double> scores;
  std::map<double, int> full_pos_counts;
  std::map<double, int> full_neg_counts;
  std::vector<double> unique_scores;
  std::vector<double> cum_pos;
  std::vector<double> cum_neg;
  std::vector<double> tp, fp, fn, tn;
  std::vector<double> sensitivity, specificity, youden_vals, accuracy;
  std::vector<double> ppv_vals, npv_vals;
  std::vector<double> cutoffs_vec;
  std::vector<int> tps, tns, fps, fns;
  std::vector<double> rep_sens, rep_spec, rep_youd, rep_acc, rep_ppv, rep_npv;

  explicit CvThreadBuffer(int n, int n_folds, int repeats)
    : scores(n),
      cutoffs_vec(n_folds),
      tps(n_folds),
      tns(n_folds),
      fps(n_folds),
      fns(n_folds),
      rep_sens(repeats),
      rep_spec(repeats),
      rep_youd(repeats),
      rep_acc(repeats),
      rep_ppv(repeats),
      rep_npv(repeats) {}
};

static inline void evaluate_single_combo_cv_cpp(
    const std::vector<int>& combo_cols,
    const double* x_ptr,
    const int* y_ptr,
    int n,
    int n_cols,
    const std::vector<std::vector<int>>& fold_test_indices,
    int n_folds,
    int repeats,
    CutoffMethod cutoff_method,
    double sensitivity_min,
    double specificity_min,
    CvThreadBuffer& buf,
    double& out_auc,
    double& out_sensitivity,
    double& out_specificity,
    double& out_youden,
    double& out_accuracy,
    double& out_ppv,
    double& out_npv,
    double& out_cutoff_mean,
    double& out_cutoff_sd,
    double& out_final_cutoff,
    bool& out_valid
) {
  int k = combo_cols.size();

  // 1. Compute full scores
  for (int i = 0; i < n; i++) {
    double s = 0.0;
    for (int j = 0; j < k; j++) {
      s += x_ptr[i + combo_cols[j] * n];
    }
    buf.scores[i] = s;
  }

  // 2. Full-data frequency table and full AUC
  buf.full_pos_counts.clear();
  buf.full_neg_counts.clear();
  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) {
    if (y_ptr[i] == 1) {
      buf.full_pos_counts[buf.scores[i]]++;
      total_pos++;
    } else {
      buf.full_neg_counts[buf.scores[i]]++;
      total_neg++;
    }
  }

  if (total_pos == 0 || total_neg == 0) {
    out_auc = NA_REAL;
    out_sensitivity = NA_REAL;
    out_specificity = NA_REAL;
    out_youden = NA_REAL;
    out_accuracy = NA_REAL;
    out_ppv = NA_REAL;
    out_npv = NA_REAL;
    out_cutoff_mean = NA_REAL;
    out_cutoff_sd = NA_REAL;
    out_final_cutoff = NA_REAL;
    out_valid = false;
    return;
  }

  // Compute full AUC
  double auc_sum = 0.0;
  for (const auto& p : buf.full_pos_counts) {
    double sp = p.first;
    int pc = p.second;
    for (const auto& neg : buf.full_neg_counts) {
      double sn = neg.first;
      int nc = neg.second;
      double pair_count = (double)pc * nc;
      if (sp > sn) {
        auc_sum += pair_count;
      } else if (sp == sn) {
        auc_sum += 0.5 * pair_count;
      }
    }
  }
  out_auc = auc_sum / ((double)total_pos * total_neg);

  // Helper lambda to find optimal cutoff from frequency maps
  auto find_cutoff_from_freqs = [&](const std::map<double, int>& pos_map,
                                    const std::map<double, int>& neg_map,
                                    int n_pos,
                                    int n_neg) -> double {
    if (n_pos == 0 || n_neg == 0) return NA_REAL;
    int tot_sub = n_pos + n_neg;

    buf.unique_scores.clear();
    for (const auto& p : pos_map) {
      if (p.second > 0) buf.unique_scores.push_back(p.first);
    }
    for (const auto& neg : neg_map) {
      if (neg.second > 0) {
        bool found = false;
        for (double s : buf.unique_scores) {
          if (s == neg.first) { found = true; break; }
        }
        if (!found) buf.unique_scores.push_back(neg.first);
      }
    }
    std::sort(buf.unique_scores.begin(), buf.unique_scores.end(), std::greater<double>());

    int n_scores = buf.unique_scores.size();
    if (n_scores == 0) return NA_REAL;

    buf.cum_pos.resize(n_scores);
    buf.cum_neg.resize(n_scores);
    buf.tp.resize(n_scores);
    buf.fp.resize(n_scores);
    buf.fn.resize(n_scores);
    buf.tn.resize(n_scores);
    buf.sensitivity.resize(n_scores);
    buf.specificity.resize(n_scores);
    buf.youden_vals.resize(n_scores);
    buf.accuracy.resize(n_scores);

    for (int si = 0; si < n_scores; si++) {
      double sc = buf.unique_scores[si];
      int prev_pos = (si == 0) ? 0 : buf.cum_pos[si - 1];
      int prev_neg = (si == 0) ? 0 : buf.cum_neg[si - 1];
      auto it_pos = pos_map.find(sc);
      int pos_c = (it_pos != pos_map.end()) ? it_pos->second : 0;
      auto it_neg = neg_map.find(sc);
      int neg_c = (it_neg != neg_map.end()) ? it_neg->second : 0;
      buf.cum_pos[si] = prev_pos + pos_c;
      buf.cum_neg[si] = prev_neg + neg_c;

      buf.tp[si] = buf.cum_pos[si];
      buf.fp[si] = buf.cum_neg[si];
      buf.fn[si] = n_pos - buf.tp[si];
      buf.tn[si] = n_neg - buf.fp[si];

      buf.sensitivity[si] = buf.tp[si] / n_pos;
      buf.specificity[si] = buf.tn[si] / n_neg;
      buf.youden_vals[si] = buf.sensitivity[si] + buf.specificity[si] - 1.0;
      buf.accuracy[si] = (buf.tp[si] + buf.tn[si]) / (double)tot_sub;
    }

    int best_idx = 0;
    if (cutoff_method == CUTOFF_YOUDEN) {
      double best_youden = -2.0, best_sens = -1.0, best_spec = -1.0;
      double best_cutoff_val = R_PosInf;
      for (int si = 0; si < n_scores; si++) {
        double yd = buf.youden_vals[si];
        double se = buf.sensitivity[si];
        double sp = buf.specificity[si];
        double co = buf.unique_scores[si];
        if (yd > best_youden ||
            (yd == best_youden && se > best_sens) ||
            (yd == best_youden && se == best_sens && sp > best_spec) ||
            (yd == best_youden && se == best_sens && sp == best_spec && co < best_cutoff_val)) {
          best_youden = yd;
          best_sens = se;
          best_spec = sp;
          best_cutoff_val = co;
          best_idx = si;
        }
      }
    } else if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT) {
      double best_dist = R_PosInf;
      double best_youden = -2.0;
      double best_cutoff_val = R_PosInf;
      for (int si = 0; si < n_scores; si++) {
        double d = std::sqrt(
          (1.0 - buf.sensitivity[si]) * (1.0 - buf.sensitivity[si]) +
          (1.0 - buf.specificity[si]) * (1.0 - buf.specificity[si]));
        double yd = buf.youden_vals[si];
        double co = buf.unique_scores[si];
        if (d < best_dist ||
            (d == best_dist && yd > best_youden) ||
            (d == best_dist && yd == best_youden && co < best_cutoff_val)) {
          best_dist = d;
          best_youden = yd;
          best_cutoff_val = co;
          best_idx = si;
        }
      }
    }
    return buf.unique_scores[best_idx];
  };

  out_final_cutoff = find_cutoff_from_freqs(buf.full_pos_counts, buf.full_neg_counts, total_pos, total_neg);

  // 3. Loop over folds
  bool is_loocv = (n_folds == n && repeats == 1);

  if (is_loocv) {
    for (int i = 0; i < n; i++) {
      double s_i = buf.scores[i];
      int y_i = y_ptr[i];
      int tr_pos = total_pos;
      int tr_neg = total_neg;

      // Decrement observation i from full counts
      if (y_i == 1) {
        buf.full_pos_counts[s_i]--;
        tr_pos--;
      } else {
        buf.full_neg_counts[s_i]--;
        tr_neg--;
      }

      double fold_cutoff = find_cutoff_from_freqs(buf.full_pos_counts, buf.full_neg_counts, tr_pos, tr_neg);
      buf.cutoffs_vec[i] = fold_cutoff;

      // Restore full counts
      if (y_i == 1) {
        buf.full_pos_counts[s_i]++;
      } else {
        buf.full_neg_counts[s_i]++;
      }

      // Test prediction
      int pred_cls = (s_i >= fold_cutoff) ? 1 : 0;
      buf.tps[i] = (pred_cls == 1 && y_i == 1) ? 1 : 0;
      buf.tns[i] = (pred_cls == 0 && y_i == 0) ? 1 : 0;
      buf.fps[i] = (pred_cls == 1 && y_i == 0) ? 1 : 0;
      buf.fns[i] = (pred_cls == 0 && y_i == 1) ? 1 : 0;
    }
  } else {
    // General K-fold / repeated K-fold
    for (int f = 0; f < n_folds; f++) {
      const auto& test_idx = fold_test_indices[f];
      int n_test = test_idx.size();
      int tr_pos = total_pos;
      int tr_neg = total_neg;

      // Subtract test observations from full counts
      for (int ti = 0; ti < n_test; ti++) {
        int idx = test_idx[ti];
        double s_idx = buf.scores[idx];
        if (y_ptr[idx] == 1) {
          buf.full_pos_counts[s_idx]--;
          tr_pos--;
        } else {
          buf.full_neg_counts[s_idx]--;
          tr_neg--;
        }
      }

      double fold_cutoff = find_cutoff_from_freqs(buf.full_pos_counts, buf.full_neg_counts, tr_pos, tr_neg);
      buf.cutoffs_vec[f] = fold_cutoff;

      // Restore full counts
      for (int ti = 0; ti < n_test; ti++) {
        int idx = test_idx[ti];
        double s_idx = buf.scores[idx];
        if (y_ptr[idx] == 1) {
          buf.full_pos_counts[s_idx]++;
        } else {
          buf.full_neg_counts[s_idx]++;
        }
      }

      // Test predictions for fold f
      int fold_tp = 0, fold_tn = 0, fold_fp = 0, fold_fn = 0;
      for (int ti = 0; ti < n_test; ti++) {
        int idx = test_idx[ti];
        int pred_cls = (buf.scores[idx] >= fold_cutoff) ? 1 : 0;
        int y_idx = y_ptr[idx];
        if (pred_cls == 1 && y_idx == 1) fold_tp++;
        else if (pred_cls == 0 && y_idx == 0) fold_tn++;
        else if (pred_cls == 1 && y_idx == 0) fold_fp++;
        else if (pred_cls == 0 && y_idx == 1) fold_fn++;
      }
      buf.tps[f] = fold_tp;
      buf.tns[f] = fold_tn;
      buf.fps[f] = fold_fp;
      buf.fns[f] = fold_fn;
    }
  }

  // 4. Repeat aggregations
  int folds_per_rep = n_folds / repeats;
  for (int r = 0; r < repeats; r++) {
    int r_start = r * folds_per_rep;
    int r_end = (r + 1) * folds_per_rep;
    int tot_tp = 0, tot_tn = 0, tot_fp = 0, tot_fn = 0;
    for (int fi = r_start; fi < r_end; fi++) {
      tot_tp += buf.tps[fi];
      tot_tn += buf.tns[fi];
      tot_fp += buf.fps[fi];
      tot_fn += buf.fns[fi];
    }
    double sens = (tot_tp + tot_fn > 0) ? (double)tot_tp / (tot_tp + tot_fn) : NA_REAL;
    double spec = (tot_tn + tot_fp > 0) ? (double)tot_tn / (tot_tn + tot_fp) : NA_REAL;
    double ppv  = (tot_tp + tot_fp > 0) ? (double)tot_tp / (tot_tp + tot_fp) : NA_REAL;
    double npv  = (tot_tn + tot_fn > 0) ? (double)tot_tn / (tot_tn + tot_fn) : NA_REAL;
    double acc  = (tot_tp + tot_tn + tot_fp + tot_fn > 0) ? (double)(tot_tp + tot_tn) / (tot_tp + tot_tn + tot_fp + tot_fn) : NA_REAL;
    double youd = (std::isnan(sens) || std::isnan(spec)) ? NA_REAL : (sens + spec - 1.0);

    buf.rep_sens[r] = sens;
    buf.rep_spec[r] = spec;
    buf.rep_youd[r] = youd;
    buf.rep_acc[r]  = acc;
    buf.rep_ppv[r]  = ppv;
    buf.rep_npv[r]  = npv;
  }

  auto mean_vec = [](const std::vector<double>& v) -> double {
    double s = 0.0;
    int count = 0;
    for (double val : v) {
      if (!std::isnan(val)) { s += val; count++; }
    }
    return count > 0 ? (s / count) : NA_REAL;
  };

  out_sensitivity = mean_vec(buf.rep_sens);
  out_specificity = mean_vec(buf.rep_spec);
  out_youden      = mean_vec(buf.rep_youd);
  out_accuracy    = mean_vec(buf.rep_acc);
  out_ppv         = mean_vec(buf.rep_ppv);
  out_npv         = mean_vec(buf.rep_npv);

  // Cutoff mean and SD
  double c_sum = 0.0;
  for (int f = 0; f < n_folds; f++) {
    c_sum += buf.cutoffs_vec[f];
  }
  out_cutoff_mean = c_sum / n_folds;
  if (n_folds > 1) {
    double sq_diff = 0.0;
    for (int f = 0; f < n_folds; f++) {
      double diff = buf.cutoffs_vec[f] - out_cutoff_mean;
      sq_diff += diff * diff;
    }
    out_cutoff_sd = std::sqrt(sq_diff / (n_folds - 1));
  } else {
    out_cutoff_sd = 0.0;
  }

  // Constraints check
  if (sensitivity_min >= 0.0 && (std::isnan(out_sensitivity) || out_sensitivity < sensitivity_min)) {
    out_valid = false;
    return;
  }
  if (specificity_min >= 0.0 && (std::isnan(out_specificity) || out_specificity < specificity_min)) {
    out_valid = false;
    return;
  }
  out_valid = true;
}

struct CvComboEvaluatorWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n;
  int n_cols;
  const std::vector<std::vector<int>>& combo_indices_vec;
  const std::vector<std::vector<int>>& fold_test_indices_vec;
  int n_folds;
  int repeats;
  CutoffMethod cm;
  double sensitivity_min;
  double specificity_min;

  RcppParallel::RVector<double> out_auc;
  RcppParallel::RVector<double> out_sensitivity;
  RcppParallel::RVector<double> out_specificity;
  RcppParallel::RVector<double> out_youden;
  RcppParallel::RVector<double> out_accuracy;
  RcppParallel::RVector<double> out_ppv;
  RcppParallel::RVector<double> out_npv;
  RcppParallel::RVector<double> out_cutoff_mean;
  RcppParallel::RVector<double> out_cutoff_sd;
  RcppParallel::RVector<double> out_final_cutoff;
  RcppParallel::RVector<int> out_valid;

  CvComboEvaluatorWorker(
    const double* x_ptr_,
    const int* y_ptr_,
    int n_,
    int n_cols_,
    const std::vector<std::vector<int>>& combo_indices_vec_,
    const std::vector<std::vector<int>>& fold_test_indices_vec_,
    int n_folds_,
    int repeats_,
    CutoffMethod cm_,
    double sensitivity_min_,
    double specificity_min_,
    NumericVector out_auc_,
    NumericVector out_sensitivity_,
    NumericVector out_specificity_,
    NumericVector out_youden_,
    NumericVector out_accuracy_,
    NumericVector out_ppv_,
    NumericVector out_npv_,
    NumericVector out_cutoff_mean_,
    NumericVector out_cutoff_sd_,
    NumericVector out_final_cutoff_,
    IntegerVector out_valid_
  ) : x_ptr(x_ptr_), y_ptr(y_ptr_), n(n_), n_cols(n_cols_),
      combo_indices_vec(combo_indices_vec_),
      fold_test_indices_vec(fold_test_indices_vec_),
      n_folds(n_folds_), repeats(repeats_), cm(cm_),
      sensitivity_min(sensitivity_min_), specificity_min(specificity_min_),
      out_auc(out_auc_), out_sensitivity(out_sensitivity_),
      out_specificity(out_specificity_), out_youden(out_youden_),
      out_accuracy(out_accuracy_), out_ppv(out_ppv_), out_npv(out_npv_),
      out_cutoff_mean(out_cutoff_mean_), out_cutoff_sd(out_cutoff_sd_),
      out_final_cutoff(out_final_cutoff_), out_valid(out_valid_) {}

  void operator()(std::size_t begin, std::size_t end) {
    CvThreadBuffer buf(n, n_folds, repeats);
    for (std::size_t i = begin; i < end; ++i) {
      double auc_v, sens_v, spec_v, youd_v, acc_v, ppv_v, npv_v;
      double cut_mean_v, cut_sd_v, final_cut_v;
      bool valid_v;

      evaluate_single_combo_cv_cpp(
        combo_indices_vec[i],
        x_ptr,
        y_ptr,
        n,
        n_cols,
        fold_test_indices_vec,
        n_folds,
        repeats,
        cm,
        sensitivity_min,
        specificity_min,
        buf,
        auc_v,
        sens_v,
        spec_v,
        youd_v,
        acc_v,
        ppv_v,
        npv_v,
        cut_mean_v,
        cut_sd_v,
        final_cut_v,
        valid_v
      );

      out_auc[i] = auc_v;
      out_sensitivity[i] = sens_v;
      out_specificity[i] = spec_v;
      out_youden[i] = youd_v;
      out_accuracy[i] = acc_v;
      out_ppv[i] = ppv_v;
      out_npv[i] = npv_v;
      out_cutoff_mean[i] = cut_mean_v;
      out_cutoff_sd[i] = cut_sd_v;
      out_final_cutoff[i] = final_cut_v;
      out_valid[i] = valid_v ? 1 : 0;
    }
  }
};

// [[Rcpp::export]]
DataFrame evaluate_combos_cv_cpp(
    NumericMatrix x,
    IntegerVector y,
    List combo_indices,
    List test_indices,
    int n_folds,
    int repeats,
    std::string cutoff_method,
    double sensitivity_min = -1.0,
    double specificity_min = -1.0,
    int num_threads = 1,
    std::size_t grain_size = 64
) {
  int n = x.nrow();
  int n_cols = x.ncol();
  int n_combos = combo_indices.size();

  CutoffMethod cm = (cutoff_method == "closest_topleft") ? CUTOFF_CLOSEST_TOPLEFT : CUTOFF_YOUDEN;

  std::vector<std::vector<int>> combo_indices_vec(n_combos);
  for (int i = 0; i < n_combos; i++) {
    IntegerVector iv = combo_indices[i];
    combo_indices_vec[i].assign(iv.begin(), iv.end());
  }

  std::vector<std::vector<int>> fold_test_indices_vec(n_folds);
  for (int f = 0; f < n_folds; f++) {
    IntegerVector iv = test_indices[f];
    fold_test_indices_vec[f].assign(iv.begin(), iv.end());
  }

  NumericVector out_auc(n_combos);
  NumericVector out_sensitivity(n_combos);
  NumericVector out_specificity(n_combos);
  NumericVector out_youden(n_combos);
  NumericVector out_accuracy(n_combos);
  NumericVector out_ppv(n_combos);
  NumericVector out_npv(n_combos);
  NumericVector out_cutoff_mean(n_combos);
  NumericVector out_cutoff_sd(n_combos);
  NumericVector out_final_cutoff(n_combos);
  IntegerVector out_valid(n_combos);

  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];

  CvComboEvaluatorWorker worker(
    x_ptr,
    y_ptr,
    n,
    n_cols,
    combo_indices_vec,
    fold_test_indices_vec,
    n_folds,
    repeats,
    cm,
    sensitivity_min,
    specificity_min,
    out_auc,
    out_sensitivity,
    out_specificity,
    out_youden,
    out_accuracy,
    out_ppv,
    out_npv,
    out_cutoff_mean,
    out_cutoff_sd,
    out_final_cutoff,
    out_valid
  );

  if (num_threads <= 1) {
    worker(0, n_combos);
  } else {
    RcppParallel::parallelFor(0, (std::size_t)n_combos, worker, grain_size, num_threads);
  }

  return DataFrame::create(
    _["cv_auc"]                 = out_auc,
    _["cv_sensitivity"]         = out_sensitivity,
    _["cv_specificity"]         = out_specificity,
    _["cv_youden"]              = out_youden,
    _["cv_accuracy"]            = out_accuracy,
    _["cv_ppv"]                 = out_ppv,
    _["cv_npv"]                 = out_npv,
    _["cv_cutoff_mean"]         = out_cutoff_mean,
    _["cv_cutoff_sd"]           = out_cutoff_sd,
    _["final_full_data_cutoff"] = out_final_cutoff,
    _["valid"]                  = out_valid
  );
}


// ============================================================================
// evaluate_candidate_stability_cv_cpp
// ============================================================================

struct CandidateStabilityCvWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n;
  int n_cols;
  const std::vector<std::vector<int>>& combo_indices_vec;
  const std::vector<std::vector<int>>& fold_test_indices_vec;
  int n_folds;
  int repeats;
  CutoffMethod cm;

  RcppParallel::RVector<int> out_candidate_id;
  RcppParallel::RVector<int> out_repeat_id;
  RcppParallel::RVector<double> out_auc;
  RcppParallel::RVector<double> out_sensitivity;
  RcppParallel::RVector<double> out_specificity;
  RcppParallel::RVector<double> out_youden;
  RcppParallel::RVector<double> out_accuracy;
  RcppParallel::RVector<double> out_ppv;
  RcppParallel::RVector<double> out_npv;

  CandidateStabilityCvWorker(
    const double* x_ptr_,
    const int* y_ptr_,
    int n_,
    int n_cols_,
    const std::vector<std::vector<int>>& combo_indices_vec_,
    const std::vector<std::vector<int>>& fold_test_indices_vec_,
    int n_folds_,
    int repeats_,
    CutoffMethod cm_,
    IntegerVector out_candidate_id_,
    IntegerVector out_repeat_id_,
    NumericVector out_auc_,
    NumericVector out_sensitivity_,
    NumericVector out_specificity_,
    NumericVector out_youden_,
    NumericVector out_accuracy_,
    NumericVector out_ppv_,
    NumericVector out_npv_
  ) : x_ptr(x_ptr_), y_ptr(y_ptr_), n(n_), n_cols(n_cols_),
      combo_indices_vec(combo_indices_vec_),
      fold_test_indices_vec(fold_test_indices_vec_),
      n_folds(n_folds_), repeats(repeats_), cm(cm_),
      out_candidate_id(out_candidate_id_), out_repeat_id(out_repeat_id_),
      out_auc(out_auc_), out_sensitivity(out_sensitivity_),
      out_specificity(out_specificity_), out_youden(out_youden_),
      out_accuracy(out_accuracy_), out_ppv(out_ppv_), out_npv(out_npv_) {}

  void operator()(std::size_t begin, std::size_t end) {
    CvThreadBuffer buf(n, n_folds, repeats);
    for (std::size_t i = begin; i < end; ++i) {
      double auc_v, sens_v, spec_v, youd_v, acc_v, ppv_v, npv_v;
      double cut_mean_v, cut_sd_v, final_cut_v;
      bool valid_v;

      evaluate_single_combo_cv_cpp(
        combo_indices_vec[i],
        x_ptr,
        y_ptr,
        n,
        n_cols,
        fold_test_indices_vec,
        n_folds,
        repeats,
        cm,
        -1.0,
        -1.0,
        buf,
        auc_v,
        sens_v,
        spec_v,
        youd_v,
        acc_v,
        ppv_v,
        npv_v,
        cut_mean_v,
        cut_sd_v,
        final_cut_v,
        valid_v
      );

      for (int r = 0; r < repeats; r++) {
        std::size_t out_idx = i * (std::size_t)repeats + (std::size_t)r;
        out_candidate_id[out_idx] = (int)(i + 1);
        out_repeat_id[out_idx] = r + 1;
        out_auc[out_idx] = auc_v;
        out_sensitivity[out_idx] = buf.rep_sens[r];
        out_specificity[out_idx] = buf.rep_spec[r];
        out_youden[out_idx] = buf.rep_youd[r];
        out_accuracy[out_idx] = buf.rep_acc[r];
        out_ppv[out_idx] = buf.rep_ppv[r];
        out_npv[out_idx] = buf.rep_npv[r];
      }
    }
  }
};

// [[Rcpp::export]]
DataFrame evaluate_candidate_stability_cv_cpp(
    NumericMatrix x,
    IntegerVector y,
    List combo_indices,
    List test_indices,
    int n_folds,
    int repeats,
    std::string cutoff_method,
    int num_threads = 1
) {
  int n = x.nrow();
  int n_cols = x.ncol();
  int n_combos = combo_indices.size();
  std::size_t total_rows = (std::size_t)n_combos * (std::size_t)repeats;

  CutoffMethod cm = (cutoff_method == "closest_topleft") ?
    CUTOFF_CLOSEST_TOPLEFT : CUTOFF_YOUDEN;

  std::vector<std::vector<int>> combo_indices_vec(n_combos);
  for (int i = 0; i < n_combos; i++) {
    IntegerVector cv = combo_indices[i];
    combo_indices_vec[i].assign(cv.begin(), cv.end());
  }

  int total_test_folds = test_indices.size();
  std::vector<std::vector<int>> fold_test_indices_vec(total_test_folds);
  for (int f = 0; f < total_test_folds; f++) {
    IntegerVector fv = test_indices[f];
    fold_test_indices_vec[f].assign(fv.begin(), fv.end());
  }

  IntegerVector out_candidate_id(total_rows);
  IntegerVector out_repeat_id(total_rows);
  NumericVector out_auc(total_rows);
  NumericVector out_sensitivity(total_rows);
  NumericVector out_specificity(total_rows);
  NumericVector out_youden(total_rows);
  NumericVector out_accuracy(total_rows);
  NumericVector out_ppv(total_rows);
  NumericVector out_npv(total_rows);

  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];

  CandidateStabilityCvWorker worker(
    x_ptr,
    y_ptr,
    n,
    n_cols,
    combo_indices_vec,
    fold_test_indices_vec,
    n_folds,
    repeats,
    cm,
    out_candidate_id,
    out_repeat_id,
    out_auc,
    out_sensitivity,
    out_specificity,
    out_youden,
    out_accuracy,
    out_ppv,
    out_npv
  );

  std::size_t grain_size = 1;
  if (num_threads <= 1) {
    worker(0, n_combos);
  } else {
    RcppParallel::parallelFor(0, (std::size_t)n_combos, worker, grain_size, num_threads);
  }

  return DataFrame::create(
    _["candidate_id"] = out_candidate_id,
    _["repeat_id"]    = out_repeat_id,
    _["auc"]          = out_auc,
    _["sensitivity"]  = out_sensitivity,
    _["specificity"]  = out_specificity,
    _["youden"]       = out_youden,
    _["accuracy"]     = out_accuracy,
    _["ppv"]          = out_ppv,
    _["npv"]          = out_npv
  );
}

// ============================================================================
// evaluate_candidate_stability_bootstrap_cpp
// ============================================================================

struct CandidateStabilityBootstrapWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n;
  int n_cols;
  const std::vector<std::vector<int>>& combo_indices_vec;
  const std::vector<std::vector<int>>& train_indices_vec;
  const std::vector<std::vector<int>>& oob_indices_vec;
  int n_reps;
  bool is_original_test;
  CutoffMethod cm;
  const double* apparent_auc_ptr;

  RcppParallel::RVector<int> out_candidate_id;
  RcppParallel::RVector<int> out_replicate_id;
  RcppParallel::RVector<int> out_training_valid;
  RcppParallel::RVector<int> out_test_valid;

  RcppParallel::RVector<double> out_train_auc;
  RcppParallel::RVector<double> out_train_sensitivity;
  RcppParallel::RVector<double> out_train_specificity;
  RcppParallel::RVector<double> out_train_youden;
  RcppParallel::RVector<double> out_train_accuracy;
  RcppParallel::RVector<double> out_train_ppv;
  RcppParallel::RVector<double> out_train_npv;
  RcppParallel::RVector<double> out_train_cutoff;

  RcppParallel::RVector<double> out_test_auc;
  RcppParallel::RVector<double> out_test_sensitivity;
  RcppParallel::RVector<double> out_test_specificity;
  RcppParallel::RVector<double> out_test_youden;
  RcppParallel::RVector<double> out_test_accuracy;
  RcppParallel::RVector<double> out_test_ppv;
  RcppParallel::RVector<double> out_test_npv;

  CandidateStabilityBootstrapWorker(
    const double* x_ptr_,
    const int* y_ptr_,
    int n_,
    int n_cols_,
    const std::vector<std::vector<int>>& combo_indices_vec_,
    const std::vector<std::vector<int>>& train_indices_vec_,
    const std::vector<std::vector<int>>& oob_indices_vec_,
    int n_reps_,
    bool is_original_test_,
    CutoffMethod cm_,
    const double* apparent_auc_ptr_,
    IntegerVector out_candidate_id_,
    IntegerVector out_replicate_id_,
    IntegerVector out_training_valid_,
    IntegerVector out_test_valid_,
    NumericVector out_train_auc_,
    NumericVector out_train_sensitivity_,
    NumericVector out_train_specificity_,
    NumericVector out_train_youden_,
    NumericVector out_train_accuracy_,
    NumericVector out_train_ppv_,
    NumericVector out_train_npv_,
    NumericVector out_train_cutoff_,
    NumericVector out_test_auc_,
    NumericVector out_test_sensitivity_,
    NumericVector out_test_specificity_,
    NumericVector out_test_youden_,
    NumericVector out_test_accuracy_,
    NumericVector out_test_ppv_,
    NumericVector out_test_npv_
  ) : x_ptr(x_ptr_), y_ptr(y_ptr_), n(n_), n_cols(n_cols_),
      combo_indices_vec(combo_indices_vec_),
      train_indices_vec(train_indices_vec_),
      oob_indices_vec(oob_indices_vec_),
      n_reps(n_reps_),
      is_original_test(is_original_test_),
      cm(cm_),
      apparent_auc_ptr(apparent_auc_ptr_),
      out_candidate_id(out_candidate_id_),
      out_replicate_id(out_replicate_id_),
      out_training_valid(out_training_valid_),
      out_test_valid(out_test_valid_),
      out_train_auc(out_train_auc_),
      out_train_sensitivity(out_train_sensitivity_),
      out_train_specificity(out_train_specificity_),
      out_train_youden(out_train_youden_),
      out_train_accuracy(out_train_accuracy_),
      out_train_ppv(out_train_ppv_),
      out_train_npv(out_train_npv_),
      out_train_cutoff(out_train_cutoff_),
      out_test_auc(out_test_auc_),
      out_test_sensitivity(out_test_sensitivity_),
      out_test_specificity(out_test_specificity_),
      out_test_youden(out_test_youden_),
      out_test_accuracy(out_test_accuracy_),
      out_test_ppv(out_test_ppv_),
      out_test_npv(out_test_npv_) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> scores(n);
    std::vector<double> unique_scores;
    unique_scores.reserve(n);
    std::vector<int> pos_counts;
    std::vector<int> neg_counts;
    std::vector<double> sensitivity;
    std::vector<double> specificity;
    std::vector<double> youden_vals;
    std::vector<double> accuracy;
    std::vector<double> ppv_vals;
    std::vector<double> npv_vals;

    for (std::size_t i = begin; i < end; ++i) {
      const std::vector<int>& items = combo_indices_vec[i];
      int k = items.size();
      double app_auc = apparent_auc_ptr[i];

      // Compute full data sum scores for candidate i
      for (int r = 0; r < n; r++) {
        double sum_val = 0.0;
        for (int c = 0; c < k; c++) {
          sum_val += x_ptr[items[c] * n + r];
        }
        scores[r] = sum_val;
      }

      for (int b = 0; b < n_reps; b++) {
        std::size_t out_idx = i * (std::size_t)n_reps + (std::size_t)b;
        out_candidate_id[out_idx] = (int)(i + 1);
        out_replicate_id[out_idx] = b + 1;

        const std::vector<int>& tr_idx = train_indices_vec[b];
        int tr_size = tr_idx.size();

        // 1. Build frequency table for bootstrap training sample
        std::map<double, std::pair<int, int>> tr_freq;
        int tr_pos = 0;
        int tr_neg = 0;

        for (int idx_pos = 0; idx_pos < tr_size; idx_pos++) {
          int obs = tr_idx[idx_pos];
          double s = scores[obs];
          int y_obs = y_ptr[obs];
          if (y_obs == 1) {
            tr_freq[s].first++;
            tr_pos++;
          } else {
            tr_freq[s].second++;
            tr_neg++;
          }
        }

        if (tr_pos == 0 || tr_neg == 0) {
          out_training_valid[out_idx] = 0;
          out_test_valid[out_idx] = 0;
          out_train_auc[out_idx] = NA_REAL;
          out_train_sensitivity[out_idx] = NA_REAL;
          out_train_specificity[out_idx] = NA_REAL;
          out_train_youden[out_idx] = NA_REAL;
          out_train_accuracy[out_idx] = NA_REAL;
          out_train_ppv[out_idx] = NA_REAL;
          out_train_npv[out_idx] = NA_REAL;
          out_train_cutoff[out_idx] = NA_REAL;

          out_test_auc[out_idx] = NA_REAL;
          out_test_sensitivity[out_idx] = NA_REAL;
          out_test_specificity[out_idx] = NA_REAL;
          out_test_youden[out_idx] = NA_REAL;
          out_test_accuracy[out_idx] = NA_REAL;
          out_test_ppv[out_idx] = NA_REAL;
          out_test_npv[out_idx] = NA_REAL;
          continue;
        }

        out_training_valid[out_idx] = 1;

        // Unpack training frequency table
        int n_scores = tr_freq.size();
        unique_scores.resize(n_scores);
        pos_counts.resize(n_scores);
        neg_counts.resize(n_scores);
        sensitivity.resize(n_scores);
        specificity.resize(n_scores);
        youden_vals.resize(n_scores);
        accuracy.resize(n_scores);
        ppv_vals.resize(n_scores);
        npv_vals.resize(n_scores);

        int si = 0;
        for (std::map<double, std::pair<int, int>>::const_iterator it = tr_freq.begin(); it != tr_freq.end(); ++it, ++si) {
          unique_scores[si] = it->first;
          pos_counts[si] = it->second.first;
          neg_counts[si] = it->second.second;
        }

        // Training AUC via trapezoidal rule
        double cum_neg = 0.0;
        double auc_sum = 0.0;
        for (int s_idx = 0; s_idx < n_scores; s_idx++) {
          double p_c = (double)pos_counts[s_idx];
          double n_c = (double)neg_counts[s_idx];
          auc_sum += p_c * cum_neg + 0.5 * p_c * n_c;
          cum_neg += n_c;
        }
        double tr_auc = auc_sum / ((double)tr_pos * (double)tr_neg);
        out_train_auc[out_idx] = tr_auc;

        // Training ROC cumulative metrics
        double tr_tp = (double)tr_pos;
        double tr_fp = (double)tr_neg;

        for (int s_idx = 0; s_idx < n_scores; s_idx++) {
          sensitivity[s_idx] = tr_tp / (double)tr_pos;
          specificity[s_idx] = 1.0 - (tr_fp / (double)tr_neg);
          youden_vals[s_idx] = sensitivity[s_idx] + specificity[s_idx] - 1.0;
          accuracy[s_idx] = (tr_tp + ((double)tr_neg - tr_fp)) / (double)tr_size;

          double pred_pos = tr_tp + tr_fp;
          ppv_vals[s_idx] = (pred_pos > 0.0) ? (tr_tp / pred_pos) : NA_REAL;

          double pred_neg = (double)tr_size - pred_pos;
          double tr_tn = (double)tr_neg - tr_fp;
          npv_vals[s_idx] = (pred_neg > 0.0) ? (tr_tn / pred_neg) : NA_REAL;

          tr_tp -= (double)pos_counts[s_idx];
          tr_fp -= (double)neg_counts[s_idx];
        }

        // Optimal cutoff on training sample
        int best_idx = 0;
        if (cm == CUTOFF_YOUDEN) {
          double best_youden = -2.0;
          double best_cutoff_val = R_PosInf;
          for (int s_idx = 0; s_idx < n_scores; s_idx++) {
            double yd = youden_vals[s_idx];
            double co = unique_scores[s_idx];
            if (yd > best_youden || (yd == best_youden && co < best_cutoff_val)) {
              best_youden = yd;
              best_cutoff_val = co;
              best_idx = s_idx;
            }
          }
        } else {
          double best_dist = R_PosInf;
          double best_youden = -2.0;
          double best_cutoff_val = R_PosInf;
          for (int s_idx = 0; s_idx < n_scores; s_idx++) {
            double d = std::sqrt((1.0 - sensitivity[s_idx]) * (1.0 - sensitivity[s_idx]) +
                                 (1.0 - specificity[s_idx]) * (1.0 - specificity[s_idx]));
            double yd = youden_vals[s_idx];
            double co = unique_scores[s_idx];
            if (d < best_dist ||
                (d == best_dist && yd > best_youden) ||
                (d == best_dist && yd == best_youden && co < best_cutoff_val)) {
              best_dist = d;
              best_youden = yd;
              best_cutoff_val = co;
              best_idx = s_idx;
            }
          }
        }

        double opt_cutoff = unique_scores[best_idx];
        out_train_cutoff[out_idx] = opt_cutoff;
        out_train_sensitivity[out_idx] = sensitivity[best_idx];
        out_train_specificity[out_idx] = specificity[best_idx];
        out_train_youden[out_idx] = youden_vals[best_idx];
        out_train_accuracy[out_idx] = accuracy[best_idx];
        out_train_ppv[out_idx] = ppv_vals[best_idx];
        out_train_npv[out_idx] = npv_vals[best_idx];

        // 2. Evaluate training-derived cutoff on test set
        if (is_original_test) {
          out_test_valid[out_idx] = 1;
          out_test_auc[out_idx] = app_auc;

          int tp = 0, fp = 0, tn = 0, fn = 0;
          int full_pos = 0, full_neg = 0;
          for (int obs = 0; obs < n; obs++) {
            int y_obs = y_ptr[obs];
            if (y_obs == 1) full_pos++; else full_neg++;
            if (scores[obs] >= opt_cutoff) {
              if (y_obs == 1) tp++; else fp++;
            } else {
              if (y_obs == 0) tn++; else fn++;
            }
          }

          double se = (full_pos > 0) ? ((double)tp / (double)full_pos) : NA_REAL;
          double sp = (full_neg > 0) ? ((double)tn / (double)full_neg) : NA_REAL;
          double yd = (!ISNA(se) && !ISNA(sp)) ? (se + sp - 1.0) : NA_REAL;
          double acc = (double)(tp + tn) / (double)n;
          double ppv = (tp + fp > 0) ? ((double)tp / (double)(tp + fp)) : NA_REAL;
          double npv = (tn + fn > 0) ? ((double)tn / (double)(tn + fn)) : NA_REAL;

          out_test_sensitivity[out_idx] = se;
          out_test_specificity[out_idx] = sp;
          out_test_youden[out_idx] = yd;
          out_test_accuracy[out_idx] = acc;
          out_test_ppv[out_idx] = ppv;
          out_test_npv[out_idx] = npv;
        } else {
          // Out-of-bag (OOB) evaluation
          const std::vector<int>& oob_idx = oob_indices_vec[b];
          int oob_size = oob_idx.size();

          if (oob_size == 0) {
            out_test_valid[out_idx] = 0;
            out_test_auc[out_idx] = NA_REAL;
            out_test_sensitivity[out_idx] = NA_REAL;
            out_test_specificity[out_idx] = NA_REAL;
            out_test_youden[out_idx] = NA_REAL;
            out_test_accuracy[out_idx] = NA_REAL;
            out_test_ppv[out_idx] = NA_REAL;
            out_test_npv[out_idx] = NA_REAL;
            continue;
          }

          out_test_valid[out_idx] = 1;

          std::map<double, std::pair<int, int>> oob_freq;
          int oob_pos = 0, oob_neg = 0;
          int tp = 0, fp = 0, tn = 0, fn = 0;

          for (int idx_pos = 0; idx_pos < oob_size; idx_pos++) {
            int obs = oob_idx[idx_pos];
            double s = scores[obs];
            int y_obs = y_ptr[obs];
            if (y_obs == 1) {
              oob_freq[s].first++;
              oob_pos++;
              if (s >= opt_cutoff) tp++; else fn++;
            } else {
              oob_freq[s].second++;
              oob_neg++;
              if (s >= opt_cutoff) fp++; else tn++;
            }
          }

          if (oob_pos > 0 && oob_neg > 0) {
            double oob_cum_neg = 0.0;
            double oob_auc_sum = 0.0;
            for (std::map<double, std::pair<int, int>>::const_iterator it = oob_freq.begin(); it != oob_freq.end(); ++it) {
              double p_c = (double)it->second.first;
              double n_c = (double)it->second.second;
              oob_auc_sum += p_c * oob_cum_neg + 0.5 * p_c * n_c;
              oob_cum_neg += n_c;
            }
            out_test_auc[out_idx] = oob_auc_sum / ((double)oob_pos * (double)oob_neg);
          } else {
            out_test_auc[out_idx] = NA_REAL;
          }

          double se = (oob_pos > 0) ? ((double)tp / (double)oob_pos) : NA_REAL;
          double sp = (oob_neg > 0) ? ((double)tn / (double)oob_neg) : NA_REAL;
          double yd = (!ISNA(se) && !ISNA(sp)) ? (se + sp - 1.0) : NA_REAL;
          double acc = (double)(tp + tn) / (double)oob_size;
          double ppv = (tp + fp > 0) ? ((double)tp / (double)(tp + fp)) : NA_REAL;
          double npv = (tn + fn > 0) ? ((double)tn / (double)(tn + fn)) : NA_REAL;

          out_test_sensitivity[out_idx] = se;
          out_test_specificity[out_idx] = sp;
          out_test_youden[out_idx] = yd;
          out_test_accuracy[out_idx] = acc;
          out_test_ppv[out_idx] = ppv;
          out_test_npv[out_idx] = npv;
        }
      }
    }
  }
};

// [[Rcpp::export]]
DataFrame evaluate_candidate_stability_bootstrap_cpp(
    NumericMatrix x,
    IntegerVector y,
    List combo_indices,
    List train_indices,
    List oob_indices,
    std::string bootstrap_test,
    std::string cutoff_method,
    NumericVector apparent_auc,
    int num_threads = 1
) {
  int n = x.nrow();
  int n_cols = x.ncol();
  int n_combos = combo_indices.size();
  int n_reps = train_indices.size();
  std::size_t total_rows = (std::size_t)n_combos * (std::size_t)n_reps;

  bool is_original_test = (bootstrap_test == "original");
  CutoffMethod cm = (cutoff_method == "closest_topleft") ?
    CUTOFF_CLOSEST_TOPLEFT : CUTOFF_YOUDEN;

  std::vector<std::vector<int>> combo_indices_vec(n_combos);
  for (int i = 0; i < n_combos; i++) {
    IntegerVector cv = combo_indices[i];
    combo_indices_vec[i].assign(cv.begin(), cv.end());
  }

  std::vector<std::vector<int>> train_indices_vec(n_reps);
  for (int b = 0; b < n_reps; b++) {
    IntegerVector tv = train_indices[b];
    train_indices_vec[b].assign(tv.begin(), tv.end());
  }

  std::vector<std::vector<int>> oob_indices_vec(n_reps);
  for (int b = 0; b < n_reps; b++) {
    IntegerVector ov = oob_indices[b];
    oob_indices_vec[b].assign(ov.begin(), ov.end());
  }

  IntegerVector out_candidate_id(total_rows);
  IntegerVector out_replicate_id(total_rows);
  IntegerVector out_training_valid(total_rows);
  IntegerVector out_test_valid(total_rows);

  NumericVector out_train_auc(total_rows);
  NumericVector out_train_sensitivity(total_rows);
  NumericVector out_train_specificity(total_rows);
  NumericVector out_train_youden(total_rows);
  NumericVector out_train_accuracy(total_rows);
  NumericVector out_train_ppv(total_rows);
  NumericVector out_train_npv(total_rows);
  NumericVector out_train_cutoff(total_rows);

  NumericVector out_test_auc(total_rows);
  NumericVector out_test_sensitivity(total_rows);
  NumericVector out_test_specificity(total_rows);
  NumericVector out_test_youden(total_rows);
  NumericVector out_test_accuracy(total_rows);
  NumericVector out_test_ppv(total_rows);
  NumericVector out_test_npv(total_rows);

  const double* x_ptr = &x[0];
  const int* y_ptr = &y[0];
  const double* app_auc_ptr = &apparent_auc[0];

  CandidateStabilityBootstrapWorker worker(
    x_ptr,
    y_ptr,
    n,
    n_cols,
    combo_indices_vec,
    train_indices_vec,
    oob_indices_vec,
    n_reps,
    is_original_test,
    cm,
    app_auc_ptr,
    out_candidate_id,
    out_replicate_id,
    out_training_valid,
    out_test_valid,
    out_train_auc,
    out_train_sensitivity,
    out_train_specificity,
    out_train_youden,
    out_train_accuracy,
    out_train_ppv,
    out_train_npv,
    out_train_cutoff,
    out_test_auc,
    out_test_sensitivity,
    out_test_specificity,
    out_test_youden,
    out_test_accuracy,
    out_test_ppv,
    out_test_npv
  );

  std::size_t grain_size = 1;
  if (num_threads <= 1) {
    worker(0, n_combos);
  } else {
    RcppParallel::parallelFor(0, (std::size_t)n_combos, worker, grain_size, num_threads);
  }

  return DataFrame::create(
    _["candidate_id"]       = out_candidate_id,
    _["replicate_id"]       = out_replicate_id,
    _["training_valid"]     = out_training_valid,
    _["test_valid"]         = out_test_valid,
    _["train_auc"]          = out_train_auc,
    _["train_sensitivity"]  = out_train_sensitivity,
    _["train_specificity"]  = out_train_specificity,
    _["train_youden"]       = out_train_youden,
    _["train_accuracy"]     = out_train_accuracy,
    _["train_ppv"]          = out_train_ppv,
    _["train_npv"]          = out_train_npv,
    _["train_cutoff"]       = out_train_cutoff,
    _["test_auc"]           = out_test_auc,
    _["test_sensitivity"]   = out_test_sensitivity,
    _["test_specificity"]   = out_test_specificity,
    _["test_youden"]        = out_test_youden,
    _["test_accuracy"]      = out_test_accuracy,
    _["test_ppv"]           = out_test_ppv,
    _["test_npv"]           = out_test_npv
  );
}
