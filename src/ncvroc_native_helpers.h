#ifndef NCVROC_NATIVE_HELPERS_H
#define NCVROC_NATIVE_HELPERS_H

#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <functional>
#include <map>
#include <queue>
#include <vector>

enum CutoffMethod {
  CUTOFF_YOUDEN = 1,
  CUTOFF_CLOSEST_TOPLEFT = 2,
  CUTOFF_SENSITIVITY_TARGET = 3,
  CUTOFF_CLINICAL_CONSTRAINT = 4
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
  // Reused counters for the common bounded-integer sum-score case.  The
  // general map/sort path remains the fallback for arbitrary numeric scores.
  static const int bounded_score_capacity = 257;
  std::array<int, bounded_score_capacity> bounded_pos_counts;
  std::array<int, bounded_score_capacity> bounded_neg_counts;
  int bounded_counts_max;

  explicit ThreadLocalBuffer(int n)
    : scores(n),
      bounded_pos_counts{},
      bounded_neg_counts{},
      bounded_counts_max(-1) {}
};

// Pure C++ ROC & optimal cutoff evaluation from already-computed buf.scores[0..n-1] and y_vals[0..n-1].
static inline void evaluate_roc_from_scores(
    const int* y_vals,
    int n,
    int total_pos,
    int total_neg,
    CutoffMethod cutoff_method,
    ThreadLocalBuffer& buf,
    double& out_auc_val,
    double& out_cutoff_val,
    double& out_sensitivity_val,
    double& out_specificity_val,
    double& out_youden_val,
    double& out_accuracy_val,
    double& out_ppv_val,
    double& out_npv_val
) {
  if (buf.bounded_counts_max >= 0) {
    std::fill_n(buf.bounded_pos_counts.begin(), buf.bounded_counts_max + 1, 0);
    std::fill_n(buf.bounded_neg_counts.begin(), buf.bounded_counts_max + 1, 0);
    buf.bounded_counts_max = -1;
  }

  // Sum scores from bounded nonnegative integer items (including the 0/1/2
  // item coding used by CBCL) have a small discrete score range. Count those
  // values directly instead of allocating map nodes and sorting a temporary
  // score vector for every candidate. Preserve the general implementation
  // below for continuous, negative, non-finite, or larger-range scores.
  bool bounded_integer_scores = true;
  int bounded_max = -1;
  for (int i = 0; i < n; ++i) {
    const double score = buf.scores[i];
    if (!std::isfinite(score) || score < 0.0 ||
        score >= ThreadLocalBuffer::bounded_score_capacity ||
        std::floor(score) != score) {
      bounded_integer_scores = false;
      break;
    }
    const int score_index = static_cast<int>(score);
    if (score_index > bounded_max) bounded_max = score_index;
    if (y_vals[i] == 1) {
      ++buf.bounded_pos_counts[score_index];
    } else {
      ++buf.bounded_neg_counts[score_index];
    }
  }

  if (bounded_integer_scores && bounded_max >= 0) {
    buf.bounded_counts_max = bounded_max;
  } else {
    // Clear any partial counts from a score vector that left the fast path.
    if (bounded_max >= 0) {
      std::fill_n(buf.bounded_pos_counts.begin(), bounded_max + 1, 0);
      std::fill_n(buf.bounded_neg_counts.begin(), bounded_max + 1, 0);
    }
  }

  if (bounded_integer_scores && bounded_max >= 0) {
    // Keep the repeated bounded-score path allocation-free. In particular,
    // do not append threshold values to std::vector for every candidate.
    buf.pos_counts.clear();
    buf.neg_counts.clear();
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
    for (int positive_score = 0; positive_score <= bounded_max; ++positive_score) {
      const int positive_count = buf.bounded_pos_counts[positive_score];
      if (positive_count == 0) continue;
      for (int negative_score = 0; negative_score <= bounded_max; ++negative_score) {
        const int negative_count = buf.bounded_neg_counts[negative_score];
        if (negative_count == 0) continue;
        const double pair_count = static_cast<double>(positive_count) * negative_count;
        if (positive_score > negative_score) auc_sum += pair_count;
        else if (positive_score == negative_score) auc_sum += 0.5 * pair_count;
      }
    }
    out_auc_val = auc_sum / (static_cast<double>(total_pos) * total_neg);

    const int total_n = total_pos + total_neg;
    int cumulative_pos = 0;
    int cumulative_neg = 0;
    bool have_cutoff = false;
    double best_youden = -2.0;
    double best_sensitivity = -1.0;
    double best_specificity = -1.0;
    double best_cutoff = R_PosInf;
    double best_distance = R_PosInf;

    for (int score = bounded_max; score >= 0; --score) {
      const int pos_at_score = buf.bounded_pos_counts[score];
      const int neg_at_score = buf.bounded_neg_counts[score];
      if (pos_at_score == 0 && neg_at_score == 0) continue;

      cumulative_pos += pos_at_score;
      cumulative_neg += neg_at_score;
      const double sensitivity = cumulative_pos / static_cast<double>(total_pos);
      const double specificity = (total_neg - cumulative_neg) / static_cast<double>(total_neg);
      const double youden = sensitivity + specificity - 1.0;
      const double accuracy = (cumulative_pos + total_neg - cumulative_neg) /
        static_cast<double>(total_n);
      const double ppv = cumulative_pos + cumulative_neg > 0
        ? cumulative_pos / static_cast<double>(cumulative_pos + cumulative_neg)
        : NA_REAL;
      const double npv = (total_neg - cumulative_neg) + (total_pos - cumulative_pos) > 0
        ? (total_neg - cumulative_neg) /
          static_cast<double>((total_neg - cumulative_neg) + (total_pos - cumulative_pos))
        : NA_REAL;
      const double cutoff = static_cast<double>(score);

      bool better = false;
      if (cutoff_method == CUTOFF_YOUDEN) {
        better = youden > best_youden ||
          (youden == best_youden && sensitivity > best_sensitivity) ||
          (youden == best_youden && sensitivity == best_sensitivity &&
           specificity > best_specificity) ||
          (youden == best_youden && sensitivity == best_sensitivity &&
           specificity == best_specificity && cutoff < best_cutoff);
      } else if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT) {
        const double distance = std::sqrt(
          (1.0 - sensitivity) * (1.0 - sensitivity) +
          (1.0 - specificity) * (1.0 - specificity));
        better = distance < best_distance ||
          (distance == best_distance && youden > best_youden) ||
          (distance == best_distance && youden == best_youden && cutoff < best_cutoff);
        if (better) best_distance = distance;
      }

      if (better || !have_cutoff) {
        have_cutoff = true;
        best_youden = youden;
        best_sensitivity = sensitivity;
        best_specificity = specificity;
        best_cutoff = cutoff;
        out_cutoff_val = cutoff;
        out_sensitivity_val = sensitivity;
        out_specificity_val = specificity;
        out_youden_val = youden;
        out_accuracy_val = accuracy;
        out_ppv_val = ppv;
        out_npv_val = npv;
      }
    }
    return;
  }

  buf.pos_counts.clear();
  buf.neg_counts.clear();
  for (int i = 0; i < n; i++) {
    if (y_vals[i] == 1) {
      buf.pos_counts[buf.scores[i]]++;
    } else {
      buf.neg_counts[buf.scores[i]]++;
    }
  }

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
  for (const auto& p : buf.pos_counts) {
    const double sp = p.first;
    const int pc = p.second;
    for (const auto& neg : buf.neg_counts) {
      const double sn = neg.first;
      const int nc = neg.second;
      const double pair_count = static_cast<double>(pc) * nc;
      if (sp > sn) {
        auc_sum += pair_count;
      } else if (sp == sn) {
        auc_sum += 0.5 * pair_count;
      }
    }
  }
  out_auc_val = auc_sum / ((double)total_pos * total_neg);

  buf.unique_scores.clear();
  for (const auto& p : buf.pos_counts) {
    buf.unique_scores.push_back(p.first);
  }
  for (const auto& neg : buf.neg_counts) {
    bool found = false;
    for (double s : buf.unique_scores) {
      if (s == neg.first) { found = true; break; }
    }
    if (!found) buf.unique_scores.push_back(neg.first);
  }
  std::sort(buf.unique_scores.begin(), buf.unique_scores.end(),
            std::greater<double>());

  int n_scores = (int)buf.unique_scores.size();
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

  int total_n = total_pos + total_neg;
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
  (void)n_k;
  (void)total_n;
  auto it = std::upper_bound(level_starts.begin(), level_starts.end(), global_rank);
  int ki = (int)std::distance(level_starts.begin(), it) - 1;
  int k = min_items + ki;
  double local_rank = global_rank - level_starts[ki];

  out_n_items_val = k;

  unrank_combination_into(n_cols, k, local_rank, buf.cols);

  for (int i = 0; i < n; i++) {
    double s = 0.0;
    for (int j = 0; j < k; j++) {
      s += x_ptr[i + buf.cols[j] * n];
    }
    buf.scores[i] = s;
  }

  evaluate_roc_from_scores(
    y_ptr, n, total_pos, total_neg, cutoff_method, buf,
    out_auc_val, out_cutoff_val, out_sensitivity_val, out_specificity_val,
    out_youden_val, out_accuracy_val, out_ppv_val, out_npv_val
  );
}

struct CandidateRecord {
  double global_rank;
  int n_items;
  double auc;
  double cutoff;
  double sensitivity;
  double specificity;
  double youden;
  double accuracy;
  double ppv;
  double npv;
  int n_positive;
  int n_negative;
};

enum RankMetric {
  RANK_AUC = 0,
  RANK_YOUDEN = 1,
  RANK_SENSITIVITY = 2,
  RANK_SPECIFICITY = 3,
  RANK_ACCURACY = 4
};

static inline double get_candidate_metric(const CandidateRecord& c, RankMetric rm) {
  switch (rm) {
    case RANK_AUC: return c.auc;
    case RANK_YOUDEN: return c.youden;
    case RANK_SENSITIVITY: return c.sensitivity;
    case RANK_SPECIFICITY: return c.specificity;
    case RANK_ACCURACY: return c.accuracy;
  }
  return c.auc;
}

struct NestedCandidateComparator {
  RankMetric rank_metric;
  bool prefer_fewer_items;

  NestedCandidateComparator(RankMetric rm, bool pfi) : rank_metric(rm), prefer_fewer_items(pfi) {}

  bool operator()(const CandidateRecord& a, const CandidateRecord& b) const {
    double ma = get_candidate_metric(a, rank_metric);
    double mb = get_candidate_metric(b, rank_metric);

    bool a_nan = std::isnan(ma);
    bool b_nan = std::isnan(mb);
    if (a_nan && !b_nan) return false;
    if (!a_nan && b_nan) return true;
    if (!a_nan && !b_nan && ma != mb) {
      return ma > mb;
    }

    bool ya_nan = std::isnan(a.youden);
    bool yb_nan = std::isnan(b.youden);
    if (ya_nan && !yb_nan) return false;
    if (!ya_nan && yb_nan) return true;
    if (!ya_nan && !yb_nan && a.youden != b.youden) {
      return a.youden > b.youden;
    }

    bool sa_nan = std::isnan(a.sensitivity);
    bool sb_nan = std::isnan(b.sensitivity);
    if (sa_nan && !sb_nan) return false;
    if (!sa_nan && sb_nan) return true;
    if (!sa_nan && !sb_nan && a.sensitivity != b.sensitivity) {
      return a.sensitivity > b.sensitivity;
    }

    bool spa_nan = std::isnan(a.specificity);
    bool spb_nan = std::isnan(b.specificity);
    if (spa_nan && !spb_nan) return false;
    if (!spa_nan && spb_nan) return true;
    if (!spa_nan && !spb_nan && a.specificity != b.specificity) {
      return a.specificity > b.specificity;
    }

    if (prefer_fewer_items && a.n_items != b.n_items) {
      return a.n_items < b.n_items;
    }

    return a.global_rank < b.global_rank;
  }
};

// Pure C++ serial candidate evaluation kernel (zero R/Rcpp API calls)
static inline void evaluate_combos_topn_kernel(
    const double* x_ptr,
    const int* y_ptr,
    int n,
    int n_cols,
    int min_items,
    int max_items,
    CutoffMethod cutoff_method,
    RankMetric rank_metric,
    bool prefer_fewer_items,
    int top_n,
    std::vector<CandidateRecord>& out_top
) {
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

  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n; i++) {
    if (y_ptr[i] == 1) total_pos++; else total_neg++;
  }
  int total_n = total_pos + total_neg;

  NestedCandidateComparator comp(rank_metric, prefer_fewer_items);
  std::priority_queue<CandidateRecord, std::vector<CandidateRecord>, NestedCandidateComparator> heap(comp);
  ThreadLocalBuffer buf(n);

  for (double global_rank = 0.0; global_rank < total; global_rank += 1.0) {
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

    CandidateRecord cand;
    cand.global_rank = global_rank;
    cand.n_items = n_items_val;
    cand.auc = auc_val;
    cand.cutoff = cutoff_val;
    cand.sensitivity = sens_val;
    cand.specificity = spec_val;
    cand.youden = youden_val;
    cand.accuracy = acc_val;
    cand.ppv = ppv_val;
    cand.npv = npv_val;
    cand.n_positive = total_pos;
    cand.n_negative = total_neg;

    if ((int)heap.size() < top_n) {
      heap.push(cand);
    } else if (comp(cand, heap.top())) {
      heap.pop();
      heap.push(cand);
    }
  }

  out_top.clear();
  out_top.reserve(heap.size());
  while (!heap.empty()) {
    out_top.push_back(heap.top());
    heap.pop();
  }
  std::reverse(out_top.begin(), out_top.end());
}

#endif // NCVROC_NATIVE_HELPERS_H
