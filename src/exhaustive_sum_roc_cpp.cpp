// src/exhaustive_sum_roc_cpp.cpp
// Rcpp backend for exhaustive_sum_roc() combo-evaluation loop.
// Single-thread. Returns numeric metrics only — items/rank applied in R.

#include <Rcpp.h>
#include <map>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

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

  // Count total positives and negatives (constant across combos)
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
      for (int si = 0; si < n_scores; si++) {
        double d = std::sqrt(
          (1.0 - sensitivity[si]) * (1.0 - sensitivity[si]) +
          (1.0 - specificity[si]) * (1.0 - specificity[si]));
        double yd = youden_vals[si];

        if (d < best_dist ||
            (d == best_dist && yd > best_youden)) {
          best_dist = d;
          best_youden = yd;
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
