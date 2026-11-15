// src/native_classification_stability.cpp
// Bounded-memory streaming combination evaluator & durable file replacement (v0.34.1)
// Zero R API calls and zero R RNG calls inside worker threads.

#include <Rcpp.h>
#include <RcppParallel.h>
#include "ncvroc_native_helpers.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <string>
#include <vector>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif

using namespace Rcpp;

namespace {

enum CsSelectionMetric {
  CS_METRIC_AUC = 0,
  CS_METRIC_YOUDEN = 1,
  CS_METRIC_SENSITIVITY = 2,
  CS_METRIC_SPECIFICITY = 3,
  CS_METRIC_ACCURACY = 4
};

static inline CsSelectionMetric cs_parse_metric(const std::string& s) {
  if (s == "auc") return CS_METRIC_AUC;
  if (s == "youden") return CS_METRIC_YOUDEN;
  if (s == "sensitivity") return CS_METRIC_SENSITIVITY;
  if (s == "specificity") return CS_METRIC_SPECIFICITY;
  if (s == "accuracy") return CS_METRIC_ACCURACY;
  Rcpp::stop("Unknown selection_metric: %s", s.c_str());
}

static inline CutoffMethod cs_parse_cutoff_method(const std::string& s) {
  if (s == "youden") return CUTOFF_YOUDEN;
  if (s == "closest_topleft") return CUTOFF_CLOSEST_TOPLEFT;
  if (s == "sensitivity_target") return CUTOFF_SENSITIVITY_TARGET;
  if (s == "clinical_constraint") return CUTOFF_CLINICAL_CONSTRAINT;
  Rcpp::stop("Unknown cutoff_method: %s", s.c_str());
}

static inline double cs_extract_metric(
    CsSelectionMetric metric,
    double auc,
    double sens,
    double spec,
    double youden,
    double acc
) {
  switch (metric) {
    case CS_METRIC_AUC: return auc;
    case CS_METRIC_YOUDEN: return youden;
    case CS_METRIC_SENSITIVITY: return sens;
    case CS_METRIC_SPECIFICITY: return spec;
    case CS_METRIC_ACCURACY: return acc;
  }
  return auc;
}

// Evaluate ROC, AUC, and optimal operating point from buf.scores[0..n-1].
// Preserves exact tie-breaking of R's select_operating_point() for all 4 cutoff methods,
// using the allocation-free bounded integer fast path when applicable.
static inline void evaluate_cs_roc_from_scores(
    const int* y_vals,
    int n,
    int total_pos,
    int total_neg,
    CutoffMethod cutoff_method,
    double sensitivity_min,
    double specificity_min,
    ThreadLocalBuffer& buf,
    double& out_auc,
    double& out_cutoff,
    double& out_sens,
    double& out_spec,
    double& out_youden,
    double& out_acc
) {
  out_auc = NA_REAL;
  out_cutoff = NA_REAL;
  out_sens = NA_REAL;
  out_spec = NA_REAL;
  out_youden = NA_REAL;
  out_acc = NA_REAL;

  if (total_pos == 0 || total_neg == 0) {
    return;
  }

  if (buf.bounded_counts_max >= 0) {
    std::fill_n(buf.bounded_pos_counts.begin(), buf.bounded_counts_max + 1, 0);
    std::fill_n(buf.bounded_neg_counts.begin(), buf.bounded_counts_max + 1, 0);
    buf.bounded_counts_max = -1;
  }

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

    double auc_sum = 0.0;
    for (int ps = 0; ps <= bounded_max; ++ps) {
      const int pc = buf.bounded_pos_counts[ps];
      if (pc == 0) continue;
      for (int ns = 0; ns <= bounded_max; ++ns) {
        const int nc = buf.bounded_neg_counts[ns];
        if (nc == 0) continue;
        const double pair_count = static_cast<double>(pc) * nc;
        if (ps > ns) auc_sum += pair_count;
        else if (ps == ns) auc_sum += 0.5 * pair_count;
      }
    }
    out_auc = auc_sum / (static_cast<double>(total_pos) * total_neg);

    const int total_n = total_pos + total_neg;
    int cumulative_pos = 0;
    int cumulative_neg = 0;
    bool have_cutoff = false;
    double best_youden = -2.0;
    double best_sens = -1.0;
    double best_spec = -1.0;
    double best_cutoff = R_PosInf;
    double best_distance = R_PosInf;

    for (int score = bounded_max; score >= 0; --score) {
      const int pos_at_score = buf.bounded_pos_counts[score];
      const int neg_at_score = buf.bounded_neg_counts[score];
      if (pos_at_score == 0 && neg_at_score == 0) continue;

      cumulative_pos += pos_at_score;
      cumulative_neg += neg_at_score;
      const double sens = cumulative_pos / static_cast<double>(total_pos);
      const double spec = (total_neg - cumulative_neg) / static_cast<double>(total_neg);
      const double youd = sens + spec - 1.0;
      const double acc = (cumulative_pos + total_neg - cumulative_neg) / static_cast<double>(total_n);
      const double cutoff = static_cast<double>(score);

      if (cutoff_method == CUTOFF_SENSITIVITY_TARGET && sens < sensitivity_min) {
        continue;
      }
      if (cutoff_method == CUTOFF_CLINICAL_CONSTRAINT &&
          (sens < sensitivity_min || spec < specificity_min)) {
        continue;
      }

      bool better = false;
      if (!have_cutoff) {
        better = true;
      } else if (cutoff_method == CUTOFF_YOUDEN || cutoff_method == CUTOFF_CLINICAL_CONSTRAINT) {
        better = youd > best_youden ||
          (youd == best_youden && sens > best_sens) ||
          (youd == best_youden && sens == best_sens && spec > best_spec) ||
          (youd == best_youden && sens == best_sens && spec == best_spec && cutoff < best_cutoff);
      } else if (cutoff_method == CUTOFF_SENSITIVITY_TARGET) {
        better = spec > best_spec ||
          (spec == best_spec && sens > best_sens) ||
          (spec == best_spec && sens == best_sens && youd > best_youden) ||
          (spec == best_spec && sens == best_sens && youd == best_youden && cutoff < best_cutoff);
      } else if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT) {
        const double dist = std::sqrt((1.0 - sens) * (1.0 - sens) + (1.0 - spec) * (1.0 - spec));
        better = dist < best_distance ||
          (dist == best_distance && youd > best_youden) ||
          (dist == best_distance && youd == best_youden && cutoff < best_cutoff);
        if (better) best_distance = dist;
      }

      if (better) {
        if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT && !have_cutoff) {
          best_distance = std::sqrt((1.0 - sens) * (1.0 - sens) + (1.0 - spec) * (1.0 - spec));
        }
        have_cutoff = true;
        best_youden = youd;
        best_sens = sens;
        best_spec = spec;
        best_cutoff = cutoff;
        out_cutoff = cutoff;
        out_sens = sens;
        out_spec = spec;
        out_youden = youd;
        out_acc = acc;
      }
    }
    return;
  }

  // Clear any partial bounded counts
  if (bounded_max >= 0) {
    std::fill_n(buf.bounded_pos_counts.begin(), bounded_max + 1, 0);
    std::fill_n(buf.bounded_neg_counts.begin(), bounded_max + 1, 0);
  }

  // General fallback for arbitrary floating-point / negative / large scores
  buf.pos_counts.clear();
  buf.neg_counts.clear();
  for (int i = 0; i < n; ++i) {
    const double sc = buf.scores[i];
    if (!std::isfinite(sc)) return;
    if (y_vals[i] == 1) buf.pos_counts[sc]++;
    else buf.neg_counts[sc]++;
  }

  double auc_sum = 0.0;
  for (const auto& p : buf.pos_counts) {
    const double sp = p.first;
    const int pc = p.second;
    for (const auto& neg : buf.neg_counts) {
      const double sn = neg.first;
      const int nc = neg.second;
      const double pair_count = static_cast<double>(pc) * nc;
      if (sp > sn) auc_sum += pair_count;
      else if (sp == sn) auc_sum += 0.5 * pair_count;
    }
  }
  out_auc = auc_sum / (static_cast<double>(total_pos) * total_neg);

  buf.unique_scores.clear();
  for (const auto& p : buf.pos_counts) buf.unique_scores.push_back(p.first);
  for (const auto& neg : buf.neg_counts) {
    if (buf.pos_counts.find(neg.first) == buf.pos_counts.end()) {
      buf.unique_scores.push_back(neg.first);
    }
  }
  std::sort(buf.unique_scores.begin(), buf.unique_scores.end(), std::greater<double>());

  const int total_n = total_pos + total_neg;
  int cumulative_pos = 0;
  int cumulative_neg = 0;
  bool have_cutoff = false;
  double best_youden = -2.0;
  double best_sens = -1.0;
  double best_spec = -1.0;
  double best_cutoff = R_PosInf;
  double best_distance = R_PosInf;

  for (double cutoff : buf.unique_scores) {
    auto it_p = buf.pos_counts.find(cutoff);
    auto it_n = buf.neg_counts.find(cutoff);
    if (it_p != buf.pos_counts.end()) cumulative_pos += it_p->second;
    if (it_n != buf.neg_counts.end()) cumulative_neg += it_n->second;

    const double sens = cumulative_pos / static_cast<double>(total_pos);
    const double spec = (total_neg - cumulative_neg) / static_cast<double>(total_neg);
    const double youd = sens + spec - 1.0;
    const double acc = (cumulative_pos + total_neg - cumulative_neg) / static_cast<double>(total_n);

    if (cutoff_method == CUTOFF_SENSITIVITY_TARGET && sens < sensitivity_min) {
      continue;
    }
    if (cutoff_method == CUTOFF_CLINICAL_CONSTRAINT &&
        (sens < sensitivity_min || spec < specificity_min)) {
      continue;
    }

    bool better = false;
    if (!have_cutoff) {
      better = true;
    } else if (cutoff_method == CUTOFF_YOUDEN || cutoff_method == CUTOFF_CLINICAL_CONSTRAINT) {
      better = youd > best_youden ||
        (youd == best_youden && sens > best_sens) ||
        (youd == best_youden && sens == best_sens && spec > best_spec) ||
        (youd == best_youden && sens == best_sens && spec == best_spec && cutoff < best_cutoff);
    } else if (cutoff_method == CUTOFF_SENSITIVITY_TARGET) {
      better = spec > best_spec ||
        (spec == best_spec && sens > best_sens) ||
        (spec == best_spec && sens == best_sens && youd > best_youden) ||
        (spec == best_spec && sens == best_sens && youd == best_youden && cutoff < best_cutoff);
    } else if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT) {
      const double dist = std::sqrt((1.0 - sens) * (1.0 - sens) + (1.0 - spec) * (1.0 - spec));
      better = dist < best_distance ||
        (dist == best_distance && youd > best_youden) ||
        (dist == best_distance && youd == best_youden && cutoff < best_cutoff);
      if (better) best_distance = dist;
    }

    if (better) {
      if (cutoff_method == CUTOFF_CLOSEST_TOPLEFT && !have_cutoff) {
        best_distance = std::sqrt((1.0 - sens) * (1.0 - sens) + (1.0 - spec) * (1.0 - spec));
      }
      have_cutoff = true;
      best_youden = youd;
      best_sens = sens;
      best_spec = spec;
      best_cutoff = cutoff;
      out_cutoff = cutoff;
      out_sens = sens;
      out_spec = spec;
      out_youden = youd;
      out_acc = acc;
    }
  }
}

struct BestCandidateState {
  bool has_best = false;
  double ordinal = 0.0;
  int n_items = 0;
  double metric_val = -R_PosInf;
  double auc = NA_REAL;
  double youden = NA_REAL;
  double cutoff = NA_REAL;
  double sens = NA_REAL;
  double spec = NA_REAL;
  double n_feasible = 0.0;
  double elapsed_ms = 0.0;
};

static inline bool is_better_cs_candidate(
    double m_val, int k, double auc_v, double yd_v, double ord,
    const BestCandidateState& best,
    bool prefer_fewer_items
) {
  if (!best.has_best) return true;
  if (m_val != best.metric_val) return m_val > best.metric_val;
  if (k != best.n_items) {
    return prefer_fewer_items ? (k < best.n_items) : (k > best.n_items);
  }
  if (auc_v != best.auc) return auc_v > best.auc;
  if (yd_v != best.youden) return yd_v > best.youden;
  return ord < best.ordinal;
}

// Advance combination in-place to next lexicographic combination.
// Returns the lowest index in `cols` that changed (0..k-1), or -1 if already at last combination.
static inline int next_combination_changed_pos(std::vector<int>& cols, int n, int k) {
  for (int i = k - 1; i >= 0; --i) {
    if (cols[i] < n - k + i) {
      cols[i]++;
      for (int j = i + 1; j < k; ++j) {
        cols[j] = cols[j - 1] + 1;
      }
      return i;
    }
  }
  return -1;
}

struct CsShardWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n_sample;
  int n_cols;
  int total_pos;
  int total_neg;
  const std::vector<int>& model_sizes;
  const std::vector<double>& size_offsets;
  const std::vector<double>& shard_starts;
  const std::vector<double>& shard_ends;
  CutoffMethod cutoff_method;
  CsSelectionMetric sel_metric;
  double sensitivity_min;
  double specificity_min;
  bool prefer_fewer_items;
  std::vector<BestCandidateState>& shard_states;

  CsShardWorker(
      const double* x_ptr_,
      const int* y_ptr_,
      int n_sample_,
      int n_cols_,
      int total_pos_,
      int total_neg_,
      const std::vector<int>& model_sizes_,
      const std::vector<double>& size_offsets_,
      const std::vector<double>& shard_starts_,
      const std::vector<double>& shard_ends_,
      CutoffMethod cutoff_method_,
      CsSelectionMetric sel_metric_,
      double sensitivity_min_,
      double specificity_min_,
      bool prefer_fewer_items_,
      std::vector<BestCandidateState>& shard_states_
  ) : x_ptr(x_ptr_), y_ptr(y_ptr_), n_sample(n_sample_), n_cols(n_cols_),
      total_pos(total_pos_), total_neg(total_neg_),
      model_sizes(model_sizes_), size_offsets(size_offsets_),
      shard_starts(shard_starts_), shard_ends(shard_ends_),
      cutoff_method(cutoff_method_), sel_metric(sel_metric_),
      sensitivity_min(sensitivity_min_), specificity_min(specificity_min_),
      prefer_fewer_items(prefer_fewer_items_), shard_states(shard_states_) {}

  void recompute_prefix(const std::vector<int>& cols, int k, std::vector<long double>& prefix_sum) const {
    if (k <= 1) {
      std::fill(prefix_sum.begin(), prefix_sum.end(), 0.0L);
      return;
    }
    const double* c0 = x_ptr + static_cast<std::size_t>(cols[0]) * n_sample;
    for (int i = 0; i < n_sample; ++i) {
      prefix_sum[i] = static_cast<long double>(c0[i]);
    }
    for (int j = 1; j < k - 1; ++j) {
      const double* cj = x_ptr + static_cast<std::size_t>(cols[j]) * n_sample;
      for (int i = 0; i < n_sample; ++i) {
        prefix_sum[i] += static_cast<long double>(cj[i]);
      }
    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    ThreadLocalBuffer buf(n_sample);
    std::vector<long double> prefix_sum(n_sample, 0.0L);
    std::vector<int> cols;

    for (std::size_t s = begin; s < end; ++s) {
      auto t0 = std::chrono::steady_clock::now();
      BestCandidateState st;
      double cur = shard_starts[s];
      const double s_end = shard_ends[s];
      if (cur >= s_end) {
        shard_states[s] = st;
        continue;
      }

      // Find initial size_idx via binary search / linear scan
      int n_sizes = static_cast<int>(model_sizes.size());
      int size_idx = 0;
      while (size_idx + 1 < n_sizes && cur >= size_offsets[size_idx + 1]) {
        size_idx++;
      }
      int k = model_sizes[size_idx];
      double local_rank = cur - size_offsets[size_idx];
      unrank_combination_into(n_cols, k, local_rank, cols);
      recompute_prefix(cols, k, prefix_sum);

      while (cur < s_end) {
        const double* last_col = x_ptr + static_cast<std::size_t>(cols[k - 1]) * n_sample;
        if (k == 1) {
          for (int i = 0; i < n_sample; ++i) {
            buf.scores[i] = last_col[i];
          }
        } else {
          for (int i = 0; i < n_sample; ++i) {
            buf.scores[i] = static_cast<double>(prefix_sum[i] + static_cast<long double>(last_col[i]));
          }
        }

        double auc_v, cut_v, se_v, sp_v, yd_v, acc_v;
        evaluate_cs_roc_from_scores(
          y_ptr, n_sample, total_pos, total_neg,
          cutoff_method, sensitivity_min, specificity_min,
          buf, auc_v, cut_v, se_v, sp_v, yd_v, acc_v
        );

        if (!std::isnan(cut_v)) {
          st.n_feasible += 1.0;
          double m_val = cs_extract_metric(sel_metric, auc_v, se_v, sp_v, yd_v, acc_v);
          if (!std::isnan(m_val) &&
              is_better_cs_candidate(m_val, k, auc_v, yd_v, cur, st, prefer_fewer_items)) {
            st.has_best = true;
            st.ordinal = cur;
            st.n_items = k;
            st.metric_val = m_val;
            st.auc = auc_v;
            st.youden = yd_v;
            st.cutoff = cut_v;
            st.sens = se_v;
            st.spec = sp_v;
          }
        }

        cur += 1.0;
        if (cur >= s_end) break;

        if (cur >= size_offsets[size_idx + 1]) {
          size_idx++;
          k = model_sizes[size_idx];
          cols.resize(k);
          for (int j = 0; j < k; ++j) cols[j] = j;
          recompute_prefix(cols, k, prefix_sum);
        } else {
          int changed = next_combination_changed_pos(cols, n_cols, k);
          if (changed >= 0 && changed < k - 1) {
            recompute_prefix(cols, k, prefix_sum);
          }
        }
      }

      auto t1 = std::chrono::steady_clock::now();
      st.elapsed_ms = std::chrono::duration<double, std::milli>(t1 - t0).count();
      shard_states[s] = st;
    }
  }
};

} // namespace

// [[Rcpp::export]]
List cs_eval_candidate_batch_cpp(
    NumericMatrix x_sample,
    IntegerVector y_sample,
    IntegerVector model_sizes,
    NumericVector size_offsets,
    double ordinal_start,
    double ordinal_end,
    std::string cutoff_method,
    std::string selection_metric,
    double sensitivity_min = -1.0,
    double specificity_min = -1.0,
    bool prefer_fewer_items = true,
    int num_threads = 1
) {
  const int n_sample = x_sample.nrow();
  const int n_cols = x_sample.ncol();
  if (y_sample.size() != n_sample) stop("y_sample length must match nrow(x_sample).");
  if (size_offsets.size() != model_sizes.size() + 1) {
    stop("size_offsets length must equal length(model_sizes) + 1.");
  }
  const double total_candidates = size_offsets[model_sizes.size()];
  if (total_candidates > 9007199254740991.0) {
    stop("Total candidates exceed exact 53-bit integer precision limit.");
  }
  if (ordinal_start < 0.0 || ordinal_end > total_candidates || ordinal_start > ordinal_end) {
    stop("Invalid [ordinal_start, ordinal_end) range.");
  }

  int total_pos = 0, total_neg = 0;
  for (int i = 0; i < n_sample; ++i) {
    if (y_sample[i] == 1) total_pos++;
    else total_neg++;
  }
  if (total_pos == 0 || total_neg == 0) {
    return List::create(
      _["invalid_sample"] = true,
      _["has_best"] = false,
      _["n_feasible"] = 0.0,
      _["wall_ms"] = 0.0,
      _["worker_times_ms"] = NumericVector::create(0.0)
    );
  }

  std::vector<int> ms_vec(model_sizes.begin(), model_sizes.end());
  std::vector<double> off_vec(size_offsets.begin(), size_offsets.end());
  CutoffMethod cm = cs_parse_cutoff_method(cutoff_method);
  CsSelectionMetric sm = cs_parse_metric(selection_metric);

  const double batch_len = ordinal_end - ordinal_start;
  int n_shards = (num_threads > 1 && batch_len >= 64.0) ? std::min(num_threads, 64) : 1;
  if (batch_len < static_cast<double>(n_shards)) {
    n_shards = std::max(1, static_cast<int>(batch_len));
  }

  std::vector<double> shard_starts(n_shards);
  std::vector<double> shard_ends(n_shards);
  const double base_step = std::floor(batch_len / n_shards);
  double rem = batch_len - base_step * n_shards;
  double pos = ordinal_start;
  for (int s = 0; s < n_shards; ++s) {
    double len = base_step + (s < rem ? 1.0 : 0.0);
    shard_starts[s] = pos;
    shard_ends[s] = pos + len;
    pos += len;
  }

  std::vector<BestCandidateState> shard_states(n_shards);
  CsShardWorker worker(
    x_sample.begin(), y_sample.begin(), n_sample, n_cols,
    total_pos, total_neg, ms_vec, off_vec,
    shard_starts, shard_ends, cm, sm,
    sensitivity_min, specificity_min, prefer_fewer_items,
    shard_states
  );

  auto t_wall0 = std::chrono::steady_clock::now();
  if (n_shards == 1) {
    worker(0, 1);
  } else {
    RcppParallel::parallelFor(0, static_cast<std::size_t>(n_shards), worker, 1, num_threads);
  }
  auto t_wall1 = std::chrono::steady_clock::now();
  double wall_ms = std::chrono::duration<double, std::milli>(t_wall1 - t_wall0).count();

  BestCandidateState agg;
  NumericVector worker_times(n_shards);
  for (int s = 0; s < n_shards; ++s) {
    const auto& st = shard_states[s];
    worker_times[s] = st.elapsed_ms;
    agg.n_feasible += st.n_feasible;
    if (st.has_best &&
        is_better_cs_candidate(st.metric_val, st.n_items, st.auc, st.youden, st.ordinal, agg, prefer_fewer_items)) {
      agg.has_best = true;
      agg.ordinal = st.ordinal;
      agg.n_items = st.n_items;
      agg.metric_val = st.metric_val;
      agg.auc = st.auc;
      agg.youden = st.youden;
      agg.cutoff = st.cutoff;
      agg.sens = st.sens;
      agg.spec = st.spec;
    }
  }

  return List::create(
    _["invalid_sample"] = false,
    _["has_best"] = agg.has_best,
    _["best_ordinal"] = agg.has_best ? agg.ordinal : NA_REAL,
    _["best_n_items"] = agg.has_best ? agg.n_items : NA_INTEGER,
    _["best_metric_val"] = agg.has_best ? agg.metric_val : NA_REAL,
    _["best_auc"] = agg.has_best ? agg.auc : NA_REAL,
    _["best_youden"] = agg.has_best ? agg.youden : NA_REAL,
    _["best_cutoff"] = agg.has_best ? agg.cutoff : NA_REAL,
    _["best_sens"] = agg.has_best ? agg.sens : NA_REAL,
    _["best_spec"] = agg.has_best ? agg.spec : NA_REAL,
    _["n_feasible"] = agg.n_feasible,
    _["wall_ms"] = wall_ms,
    _["worker_times_ms"] = worker_times
  );
}

// [[Rcpp::export]]
IntegerVector cs_unrank_combination_cpp(int n_cols, int k, double local_rank_0) {
  std::vector<int> res;
  unrank_combination_into(n_cols, k, local_rank_0, res);
  IntegerVector out(k);
  for (int i = 0; i < k; ++i) {
    out[i] = res[i] + 1; // 1-based for R
  }
  return out;
}

// [[Rcpp::export]]
bool cs_durable_replace_file_cpp(std::string temp_path, std::string target_path) {
#ifdef _WIN32
  auto to_wide = [](const std::string& s) -> std::wstring {
    if (s.empty()) return std::wstring();
    int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, NULL, 0);
    if (len <= 0) Rcpp::stop("Failed UTF-8 to UTF-16 path conversion.");
    std::wstring w(static_cast<std::size_t>(len), L'\0');
    int written = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, &w[0], len);
    if (written <= 0) Rcpp::stop("Failed UTF-8 to UTF-16 path conversion.");
    w.resize(static_cast<std::size_t>(written - 1));
    return w;
  };
  std::wstring w_temp = to_wide(temp_path);
  std::wstring w_target = to_wide(target_path);

  HANDLE hFile = CreateFileW(
    w_temp.c_str(),
    GENERIC_READ | GENERIC_WRITE,
    0,
    NULL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    NULL
  );
  if (hFile == INVALID_HANDLE_VALUE) {
    DWORD err = GetLastError();
    Rcpp::stop("Failed to open temp file for FlushFileBuffers (Win32 error %lu): %s",
               static_cast<unsigned long>(err), temp_path.c_str());
  }
  BOOL flushed = FlushFileBuffers(hFile);
  DWORD flush_err = flushed ? 0 : GetLastError();
  CloseHandle(hFile);
  if (!flushed) {
    Rcpp::stop("FlushFileBuffers failed (Win32 error %lu): %s",
               static_cast<unsigned long>(flush_err), temp_path.c_str());
  }

  BOOL moved = MoveFileExW(
    w_temp.c_str(),
    w_target.c_str(),
    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH
  );
  if (!moved) {
    DWORD err = GetLastError();
    Rcpp::stop("MoveFileExW durable replace failed (Win32 error %lu): %s -> %s",
               static_cast<unsigned long>(err), temp_path.c_str(), target_path.c_str());
  }
  return true;
#else
  if (std::rename(temp_path.c_str(), target_path.c_str()) != 0) {
    Rcpp::stop("rename() failed: %s -> %s", temp_path.c_str(), target_path.c_str());
  }
  return true;
#endif
}

// [[Rcpp::export]]
bool cs_check_durable_replace_cpp(std::string dir_path) {
  std::string probe_tmp = dir_path + "/.ncvroc_durable_probe.tmp";
  std::string probe_dst = dir_path + "/.ncvroc_durable_probe.dst";
  FILE* f = std::fopen(probe_tmp.c_str(), "wb");
  if (!f) {
    Rcpp::stop("Cannot create probe file in directory: %s", dir_path.c_str());
  }
  const char msg[] = "NCVROC_DURABLE_PROBE";
  std::fwrite(msg, 1, sizeof(msg) - 1, f);
  std::fflush(f);
  std::fclose(f);

  bool ok = cs_durable_replace_file_cpp(probe_tmp, probe_dst);
  std::remove(probe_dst.c_str());
  std::remove(probe_tmp.c_str());
  return ok;
}
