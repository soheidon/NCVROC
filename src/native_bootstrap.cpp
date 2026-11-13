// src/native_bootstrap.cpp
// Native C++ / RcppParallel Multi-Replicate Bootstrap Evaluation Engine (v0.30.0 R7)
// Zero R API calls and zero R RNG calls inside worker threads.

#include <Rcpp.h>
#include <RcppParallel.h>
#include "ncvroc_native_helpers.h"
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

using namespace Rcpp;

namespace {

enum SelectionMetric {
  METRIC_AUC = 0,
  METRIC_YOUDEN = 1,
  METRIC_SENSITIVITY = 2,
  METRIC_SPECIFICITY = 3,
  METRIC_ACCURACY = 4
};

static inline SelectionMetric parse_selection_metric(const std::string& s) {
  if (s == "auc") return METRIC_AUC;
  if (s == "youden") return METRIC_YOUDEN;
  if (s == "sensitivity") return METRIC_SENSITIVITY;
  if (s == "specificity") return METRIC_SPECIFICITY;
  if (s == "accuracy") return METRIC_ACCURACY;
  return METRIC_AUC;
}

static inline CutoffMethod parse_boot_cutoff_method(const std::string& s) {
  if (s == "youden") return CUTOFF_YOUDEN;
  if (s == "closest_topleft") return CUTOFF_CLOSEST_TOPLEFT;
  if (s == "clinical_constraint") return CUTOFF_CLINICAL_CONSTRAINT;
  return CUTOFF_YOUDEN;
}

static inline double extract_metric(
    SelectionMetric metric,
    double auc,
    double sens,
    double spec,
    double youden,
    double acc
) {
  switch (metric) {
    case METRIC_AUC: return auc;
    case METRIC_YOUDEN: return youden;
    case METRIC_SENSITIVITY: return sens;
    case METRIC_SPECIFICITY: return spec;
    case METRIC_ACCURACY: return acc;
  }
  return auc;
}

// Compute AUC and ROC table + optimal cutoff supporting YOUDEN, CLOSEST_TOPLEFT, and CLINICAL_CONSTRAINT
static inline void evaluate_boot_roc_from_scores(
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
  if (cutoff_method != CUTOFF_CLINICAL_CONSTRAINT) {
    double ppv = NA_REAL, npv = NA_REAL;
    evaluate_roc_from_scores(
      y_vals, n, total_pos, total_neg, cutoff_method, buf,
      out_auc, out_cutoff, out_sens, out_spec, out_youden, out_acc, ppv, npv
    );
    return;
  }

  // First call with CUTOFF_YOUDEN to populate buf.unique_scores, buf.sensitivity, buf.specificity,
  // buf.youden_vals, buf.accuracy, and out_auc cleanly
  double ppv = NA_REAL, npv = NA_REAL;
  evaluate_roc_from_scores(
    y_vals, n, total_pos, total_neg, CUTOFF_YOUDEN, buf,
    out_auc, out_cutoff, out_sens, out_spec, out_youden, out_acc, ppv, npv
  );
  if (total_pos == 0 || total_neg == 0 || buf.unique_scores.empty()) {
    out_cutoff = NA_REAL;
    out_sens = NA_REAL;
    out_spec = NA_REAL;
    out_youden = NA_REAL;
    out_acc = NA_REAL;
    return;
  }

  int n_scores = (int)buf.unique_scores.size();
  int best_idx = -1;
  double best_youden = -2.0, best_sens = -1.0, best_spec = -1.0;
  double best_cutoff_val = R_PosInf;
  for (int si = 0; si < n_scores; si++) {
    double se = buf.sensitivity[si];
    double sp = buf.specificity[si];
    if (se < sensitivity_min || sp < specificity_min) continue;
    double yd = buf.youden_vals[si];
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

  if (best_idx < 0) {
    out_cutoff = NA_REAL;
    out_sens = NA_REAL;
    out_spec = NA_REAL;
    out_youden = NA_REAL;
    out_acc = NA_REAL;
    return;
  }

  out_cutoff = buf.unique_scores[best_idx];
  out_sens   = buf.sensitivity[best_idx];
  out_spec   = buf.specificity[best_idx];
  out_youden = buf.youden_vals[best_idx];
  out_acc    = buf.accuracy[best_idx];
}

// Compute AUC on (scores[0..n-1], y_vals[0..n-1]) using ThreadLocalBuffer maps
static inline double compute_auc_only_from_scores(
    const int* y_vals,
    int n,
    int total_pos,
    int total_neg,
    ThreadLocalBuffer& buf
) {
  if (total_pos == 0 || total_neg == 0) return NA_REAL;
  buf.pos_counts.clear();
  buf.neg_counts.clear();
  for (int i = 0; i < n; i++) {
    if (y_vals[i] == 1) {
      buf.pos_counts[buf.scores[i]]++;
    } else {
      buf.neg_counts[buf.scores[i]]++;
    }
  }
  double auc_sum = 0.0;
  for (const auto& p : buf.pos_counts) {
    double sp = p.first;
    int pc = p.second;
    for (const auto& neg : buf.neg_counts) {
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
  return auc_sum / ((double)total_pos * total_neg);
}

struct BootstrapReplicateResult {
  int selected_combo_idx_1based; // NA_integer_ if failed
  double selected_cutoff;
  int status_code; // 0 = selected, 1 = fallback_youden, 2 = no_feasible_candidate, 3 = invalid_evaluation
  bool fallback_used;
  int n_feasible_candidates;

  double training_auc;
  double training_sensitivity;
  double training_specificity;
  double training_youden;

  double test_auc;
  double test_sensitivity;
  double test_specificity;
  double test_youden;

  double optimism_auc;
  double optimism_sensitivity;
  double optimism_specificity;
  double optimism_youden;

  bool training_feasible;
  bool test_feasible;
  bool exact_match;
  double jaccard_similarity;

  int oob_n_total;
  int oob_n_pos;
  int oob_n_neg;
  bool oob_valid_auc;
  bool oob_valid_joint;
  double oob_auc;
  double oob_sensitivity;
  double oob_specificity;
  double oob_youden;
  int oob_reason_code; // 0 = NA (valid), 1 = not_requested, 2 = empty_oob, 3 = no_oob_samples,
                       // 4 = no_oob_positives, 5 = no_oob_negatives, 6 = selection_failed
};

struct BootstrapWorker : public RcppParallel::Worker {
  const double* x_ptr;
  const int* y_ptr;
  int n_obs;
  int n_pos_orig;
  int n_neg_orig;
  const std::vector<std::vector<int>>& boot_indices;
  const std::vector<std::vector<int>>& combos;
  CutoffMethod cutoff_method;
  SelectionMetric sel_metric;
  double sensitivity_min;
  double specificity_min;
  bool prefer_fewer_items;
  bool fallback_youden;
  bool need_oob;
  const std::vector<int>& ref_items; // sorted 0-based
  std::vector<BootstrapReplicateResult>& results;

  BootstrapWorker(
      const double* x_ptr_,
      const int* y_ptr_,
      int n_obs_,
      int n_pos_orig_,
      int n_neg_orig_,
      const std::vector<std::vector<int>>& boot_indices_,
      const std::vector<std::vector<int>>& combos_,
      CutoffMethod cutoff_method_,
      SelectionMetric sel_metric_,
      double sensitivity_min_,
      double specificity_min_,
      bool prefer_fewer_items_,
      bool fallback_youden_,
      bool need_oob_,
      const std::vector<int>& ref_items_,
      std::vector<BootstrapReplicateResult>& results_
  ) : x_ptr(x_ptr_), y_ptr(y_ptr_), n_obs(n_obs_),
      n_pos_orig(n_pos_orig_), n_neg_orig(n_neg_orig_),
      boot_indices(boot_indices_), combos(combos_),
      cutoff_method(cutoff_method_), sel_metric(sel_metric_),
      sensitivity_min(sensitivity_min_), specificity_min(specificity_min_),
      prefer_fewer_items(prefer_fewer_items_), fallback_youden(fallback_youden_),
      need_oob(need_oob_), ref_items(ref_items_), results(results_) {}

  void operator()(std::size_t begin, std::size_t end) {
    ThreadLocalBuffer buf(n_obs);
    std::vector<int> y_boot(n_obs);
    std::vector<int> oob_rows;
    std::vector<int> y_oob;
    std::vector<unsigned char> in_boot(n_obs);
    std::vector<int> sel_sorted;

    int n_combos = (int)combos.size();

    for (std::size_t r = begin; r < end; r++) {
      const std::vector<int>& b_idx = boot_indices[r];
      int n_boot = (int)b_idx.size();
      if ((int)y_boot.size() < n_boot) y_boot.resize(n_boot);
      if ((int)buf.scores.size() < std::max(n_obs, n_boot)) {
        buf.scores.resize(std::max(n_obs, n_boot));
      }

      std::fill(in_boot.begin(), in_boot.end(), 0);
      int n_pos_boot = 0;
      int n_neg_boot = 0;
      for (int i = 0; i < n_boot; i++) {
        int idx = b_idx[i];
        in_boot[idx] = 1;
        int yi = y_ptr[idx];
        y_boot[i] = yi;
        if (yi == 1) n_pos_boot++;
        else n_neg_boot++;
      }

      oob_rows.clear();
      y_oob.clear();
      int n_pos_oob = 0;
      int n_neg_oob = 0;
      for (int i = 0; i < n_obs; i++) {
        if (!in_boot[i]) {
          oob_rows.push_back(i);
          int yi = y_ptr[i];
          y_oob.push_back(yi);
          if (yi == 1) n_pos_oob++;
          else n_neg_oob++;
        }
      }
      int n_oob = (int)oob_rows.size();

      BootstrapReplicateResult& out = results[r];
      out.selected_combo_idx_1based = NA_INTEGER;
      out.selected_cutoff = NA_REAL;
      out.status_code = 3;
      out.fallback_used = false;
      out.n_feasible_candidates = 0;
      out.training_auc = NA_REAL;
      out.training_sensitivity = NA_REAL;
      out.training_specificity = NA_REAL;
      out.training_youden = NA_REAL;
      out.test_auc = NA_REAL;
      out.test_sensitivity = NA_REAL;
      out.test_specificity = NA_REAL;
      out.test_youden = NA_REAL;
      out.optimism_auc = NA_REAL;
      out.optimism_sensitivity = NA_REAL;
      out.optimism_specificity = NA_REAL;
      out.optimism_youden = NA_REAL;
      out.training_feasible = false;
      out.test_feasible = false;
      out.exact_match = false;
      out.jaccard_similarity = NA_REAL;
      out.oob_n_total = n_oob;
      out.oob_n_pos = n_pos_oob;
      out.oob_n_neg = n_neg_oob;
      out.oob_valid_auc = false;
      out.oob_valid_joint = false;
      out.oob_auc = NA_REAL;
      out.oob_sensitivity = NA_REAL;
      out.oob_specificity = NA_REAL;
      out.oob_youden = NA_REAL;
      out.oob_reason_code = 6; // selection_failed

      if (n_pos_boot == 0 || n_neg_boot == 0) {
        out.status_code = 3; // invalid_evaluation
        continue;
      }

      auto eval_pass = [&](CutoffMethod cm, bool check_constraint,
                           int& best_c, int& n_feas,
                           double& best_auc, double& best_cut,
                           double& best_sens, double& best_spec,
                           double& best_youd) {
        best_c = -1;
        n_feas = 0;
        double best_metric_val = -R_PosInf;
        int best_n_items = 0;

        for (int c = 0; c < n_combos; c++) {
          const std::vector<int>& cols = combos[c];
          int k = (int)cols.size();

          for (int i = 0; i < n_boot; i++) {
            int orig_i = b_idx[i];
            double s = 0.0;
            for (int j = 0; j < k; j++) {
              s += x_ptr[orig_i + cols[j] * n_obs];
            }
            buf.scores[i] = s;
          }

          double auc_v, cut_v, se_v, sp_v, yd_v, acc_v;
          evaluate_boot_roc_from_scores(
            y_boot.data(), n_boot, n_pos_boot, n_neg_boot,
            cm, sensitivity_min, specificity_min, buf,
            auc_v, cut_v, se_v, sp_v, yd_v, acc_v
          );

          bool feas = true;
          if (check_constraint) {
            feas = (!std::isnan(cut_v) && se_v >= sensitivity_min && sp_v >= specificity_min);
          }
          if (!feas) continue;
          n_feas++;

          double m_val = extract_metric(sel_metric, auc_v, se_v, sp_v, yd_v, acc_v);
          if (std::isnan(m_val)) continue;

          bool better = false;
          if (best_c < 0) {
            better = true;
          } else if (m_val > best_metric_val) {
            better = true;
          } else if (m_val == best_metric_val) {
            if (prefer_fewer_items && k != best_n_items) {
              better = (k < best_n_items);
            }
            // Equal metric & equal n_items: lower candidate index c wins (preserve first)
          }

          if (better) {
            best_c = c;
            best_metric_val = m_val;
            best_n_items = k;
            best_auc = auc_v;
            best_cut = cut_v;
            best_sens = se_v;
            best_spec = sp_v;
            best_youd = yd_v;
          }
        }
      };

      int best_c = -1, n_feas = 0;
      double b_auc = NA_REAL, b_cut = NA_REAL, b_sens = NA_REAL, b_spec = NA_REAL, b_youd = NA_REAL;
      bool check_con = (cutoff_method == CUTOFF_CLINICAL_CONSTRAINT);

      eval_pass(cutoff_method, check_con, best_c, n_feas, b_auc, b_cut, b_sens, b_spec, b_youd);
      out.n_feasible_candidates = n_feas;

      if (n_feas == 0 || best_c < 0) {
        if (fallback_youden) {
          out.fallback_used = true;
          out.status_code = 1; // fallback_youden
          int fb_feas = 0;
          if (cutoff_method == CUTOFF_CLINICAL_CONSTRAINT) {
            eval_pass(CUTOFF_YOUDEN, false, best_c, fb_feas, b_auc, b_cut, b_sens, b_spec, b_youd);
          } else {
            eval_pass(cutoff_method, false, best_c, fb_feas, b_auc, b_cut, b_sens, b_spec, b_youd);
          }
          if (best_c < 0) {
            out.status_code = 2;
            continue;
          }
        } else {
          out.status_code = 2; // no_feasible_candidate
          continue;
        }
      } else {
        out.status_code = 0; // selected
      }

      out.selected_combo_idx_1based = best_c + 1;
      out.selected_cutoff = b_cut;
      out.training_auc = b_auc;
      out.training_sensitivity = b_sens;
      out.training_specificity = b_spec;
      out.training_youden = b_youd;

      // Apply selected model & cutoff to original data
      const std::vector<int>& sel_cols = combos[best_c];
      int sel_k = (int)sel_cols.size();
      for (int i = 0; i < n_obs; i++) {
        double s = 0.0;
        for (int j = 0; j < sel_k; j++) {
          s += x_ptr[i + sel_cols[j] * n_obs];
        }
        buf.scores[i] = s;
      }

      double orig_auc = compute_auc_only_from_scores(y_ptr, n_obs, n_pos_orig, n_neg_orig, buf);
      int tp = 0, tn = 0;
      for (int i = 0; i < n_obs; i++) {
        int pred = (buf.scores[i] >= b_cut) ? 1 : 0;
        if (pred == 1 && y_ptr[i] == 1) tp++;
        else if (pred == 0 && y_ptr[i] == 0) tn++;
      }
      double orig_sens = (double)tp / n_pos_orig;
      double orig_spec = (double)tn / n_neg_orig;
      double orig_youd = orig_sens + orig_spec - 1.0;

      out.test_auc = orig_auc;
      out.test_sensitivity = orig_sens;
      out.test_specificity = orig_spec;
      out.test_youden = orig_youd;

      out.optimism_auc = b_auc - orig_auc;
      out.optimism_sensitivity = b_sens - orig_sens;
      out.optimism_specificity = b_spec - orig_spec;
      out.optimism_youden = b_youd - orig_youd;

      if (cutoff_method == CUTOFF_CLINICAL_CONSTRAINT) {
        out.training_feasible = (b_sens >= sensitivity_min && b_spec >= specificity_min);
        out.test_feasible = (orig_sens >= sensitivity_min && orig_spec >= specificity_min);
      } else {
        out.training_feasible = true;
        out.test_feasible = true;
      }

      if (!ref_items.empty()) {
        sel_sorted = sel_cols;
        std::sort(sel_sorted.begin(), sel_sorted.end());
        out.exact_match = (sel_sorted == ref_items);
        int inter = 0;
        std::size_t ia = 0, ib = 0;
        while (ia < sel_sorted.size() && ib < ref_items.size()) {
          if (sel_sorted[ia] == ref_items[ib]) {
            inter++;
            ia++;
            ib++;
          } else if (sel_sorted[ia] < ref_items[ib]) {
            ia++;
          } else {
            ib++;
          }
        }
        int uni = (int)sel_sorted.size() + (int)ref_items.size() - inter;
        out.jaccard_similarity = (uni > 0) ? ((double)inter / uni) : NA_REAL;
      }

      // OOB evaluation (buf.scores[0..n_obs-1] holds orig_scores for all n_obs!)
      if (!need_oob) {
        out.oob_reason_code = 1; // not_requested
      } else if (n_oob == 0) {
        out.oob_reason_code = 2; // empty_oob
      } else if (n_pos_oob == 0 && n_neg_oob == 0) {
        out.oob_reason_code = 3; // no_oob_samples
      } else if (n_pos_oob == 0) {
        out.oob_reason_code = 4; // no_oob_positives
        int tn_oob = 0;
        for (int i = 0; i < n_oob; i++) {
          int row = oob_rows[i];
          if (buf.scores[row] < b_cut && y_ptr[row] == 0) tn_oob++;
        }
        out.oob_specificity = (double)tn_oob / n_neg_oob;
      } else if (n_neg_oob == 0) {
        out.oob_reason_code = 5; // no_oob_negatives
        int tp_oob = 0;
        for (int i = 0; i < n_oob; i++) {
          int row = oob_rows[i];
          if (buf.scores[row] >= b_cut && y_ptr[row] == 1) tp_oob++;
        }
        out.oob_sensitivity = (double)tp_oob / n_pos_oob;
      } else {
        out.oob_reason_code = 0; // NA_character_
        int tp_oob = 0, tn_oob = 0;
        for (int i = 0; i < n_oob; i++) {
          int row = oob_rows[i];
          double sc = buf.scores[row];
          buf.scores[i] = sc; // safe because i <= row (oob_rows is strictly increasing!)
          int yi = y_ptr[row];
          if (sc >= b_cut && yi == 1) tp_oob++;
          else if (sc < b_cut && yi == 0) tn_oob++;
        }
        double oob_auc = compute_auc_only_from_scores(y_oob.data(), n_oob, n_pos_oob, n_neg_oob, buf);
        out.oob_auc = oob_auc;
        out.oob_valid_auc = !std::isnan(oob_auc);
        out.oob_sensitivity = (double)tp_oob / n_pos_oob;
        out.oob_specificity = (double)tn_oob / n_neg_oob;
        out.oob_youden = out.oob_sensitivity + out.oob_specificity - 1.0;
        out.oob_valid_joint = true;
      }
    }
  }
};

} // namespace

// [[Rcpp::export]]
DataFrame evaluate_bootstrap_replicates_native_cpp(
    NumericMatrix x,
    IntegerVector y,
    List boot_indices_list,
    List combo_0based,
    std::string cutoff_method,
    std::string selection_metric,
    double sensitivity_min = -1.0,
    double specificity_min = -1.0,
    bool prefer_fewer_items = true,
    bool fallback_youden = true,
    bool need_oob = true,
    IntegerVector ref_items_0based = IntegerVector(),
    int num_threads = 1
) {
  int n_obs = x.nrow();
  int n_cols = x.ncol();
  if (y.size() != n_obs) stop("y length must match nrow(x).");
  if (num_threads < 1) num_threads = 1;

  int n_reps = boot_indices_list.size();
  int n_combos = combo_0based.size();

  int n_pos_orig = 0, n_neg_orig = 0;
  for (int i = 0; i < n_obs; i++) {
    if (y[i] == 1) n_pos_orig++;
    else n_neg_orig++;
  }

  std::vector<std::vector<int>> boot_indices(n_reps);
  for (int r = 0; r < n_reps; r++) {
    IntegerVector iv = boot_indices_list[r];
    boot_indices[r].assign(iv.begin(), iv.end());
    for (int idx : boot_indices[r]) {
      if (idx < 0 || idx >= n_obs) stop("boot index out of bounds.");
    }
  }

  std::vector<std::vector<int>> combos(n_combos);
  for (int c = 0; c < n_combos; c++) {
    IntegerVector cv = combo_0based[c];
    combos[c].assign(cv.begin(), cv.end());
    for (int col : combos[c]) {
      if (col < 0 || col >= n_cols) stop("combo column index out of bounds.");
    }
  }

  std::vector<int> ref_items(ref_items_0based.begin(), ref_items_0based.end());
  std::sort(ref_items.begin(), ref_items.end());

  std::vector<BootstrapReplicateResult> results(n_reps);
  BootstrapWorker worker(
    x.begin(), y.begin(), n_obs, n_pos_orig, n_neg_orig,
    boot_indices, combos,
    parse_boot_cutoff_method(cutoff_method),
    parse_selection_metric(selection_metric),
    sensitivity_min, specificity_min,
    prefer_fewer_items, fallback_youden, need_oob,
    ref_items, results
  );

  if (num_threads == 1 || n_reps <= 1) {
    worker(0, (std::size_t)n_reps);
  } else {
    RcppParallel::parallelFor(0, (std::size_t)n_reps, worker, 1, num_threads);
  }

  IntegerVector selected_combo_idx(n_reps);
  NumericVector selected_cutoff(n_reps);
  IntegerVector status_code(n_reps);
  LogicalVector fallback_used_vec(n_reps);
  IntegerVector n_feasible_vec(n_reps);

  NumericVector tr_auc(n_reps), tr_sens(n_reps), tr_spec(n_reps), tr_youd(n_reps);
  NumericVector te_auc(n_reps), te_sens(n_reps), te_spec(n_reps), te_youd(n_reps);
  NumericVector op_auc(n_reps), op_sens(n_reps), op_spec(n_reps), op_youd(n_reps);

  LogicalVector tr_feas(n_reps), te_feas(n_reps), exact_match_vec(n_reps);
  NumericVector jaccard_vec(n_reps);

  IntegerVector oob_n_total(n_reps), oob_n_pos(n_reps), oob_n_neg(n_reps);
  LogicalVector oob_valid_auc(n_reps), oob_valid_joint(n_reps);
  NumericVector oob_auc(n_reps), oob_sens(n_reps), oob_spec(n_reps), oob_youd(n_reps);
  IntegerVector oob_reason_code(n_reps);

  for (int r = 0; r < n_reps; r++) {
    const auto& res = results[r];
    selected_combo_idx[r] = res.selected_combo_idx_1based;
    selected_cutoff[r]    = res.selected_cutoff;
    status_code[r]        = res.status_code;
    fallback_used_vec[r]  = res.fallback_used;
    n_feasible_vec[r]     = res.n_feasible_candidates;

    tr_auc[r]  = res.training_auc;
    tr_sens[r] = res.training_sensitivity;
    tr_spec[r] = res.training_specificity;
    tr_youd[r] = res.training_youden;

    te_auc[r]  = res.test_auc;
    te_sens[r] = res.test_sensitivity;
    te_spec[r] = res.test_specificity;
    te_youd[r] = res.test_youden;

    op_auc[r]  = res.optimism_auc;
    op_sens[r] = res.optimism_sensitivity;
    op_spec[r] = res.optimism_specificity;
    op_youd[r] = res.optimism_youden;

    tr_feas[r]         = res.training_feasible;
    te_feas[r]         = res.test_feasible;
    exact_match_vec[r] = res.exact_match;
    jaccard_vec[r]     = res.jaccard_similarity;

    oob_n_total[r]     = res.oob_n_total;
    oob_n_pos[r]       = res.oob_n_pos;
    oob_n_neg[r]       = res.oob_n_neg;
    oob_valid_auc[r]   = res.oob_valid_auc;
    oob_valid_joint[r] = res.oob_valid_joint;
    oob_auc[r]         = res.oob_auc;
    oob_sens[r]        = res.oob_sensitivity;
    oob_spec[r]        = res.oob_specificity;
    oob_youd[r]        = res.oob_youden;
    oob_reason_code[r] = res.oob_reason_code;
  }

  return DataFrame::create(
    _["selected_combo_idx"]    = selected_combo_idx,
    _["selected_cutoff"]       = selected_cutoff,
    _["status_code"]           = status_code,
    _["fallback_used"]         = fallback_used_vec,
    _["n_feasible_candidates"] = n_feasible_vec,
    _["training_auc"]          = tr_auc,
    _["training_sensitivity"]  = tr_sens,
    _["training_specificity"]  = tr_spec,
    _["training_youden"]       = tr_youd,
    _["test_auc"]              = te_auc,
    _["test_sensitivity"]      = te_sens,
    _["test_specificity"]      = te_spec,
    _["test_youden"]           = te_youd,
    _["optimism_auc"]          = op_auc,
    _["optimism_sensitivity"]  = op_sens,
    _["optimism_specificity"]  = op_spec,
    _["optimism_youden"]       = op_youd,
    _["training_feasible"]     = tr_feas,
    _["test_feasible"]         = te_feas,
    _["exact_match"]           = exact_match_vec,
    _["jaccard_similarity"]    = jaccard_vec,
    _["oob_n_total"]           = oob_n_total,
    _["oob_n_pos"]             = oob_n_pos,
    _["oob_n_neg"]             = oob_n_neg,
    _["oob_valid_auc"]         = oob_valid_auc,
    _["oob_valid_joint"]       = oob_valid_joint,
    _["oob_auc"]               = oob_auc,
    _["oob_sensitivity"]       = oob_sens,
    _["oob_specificity"]       = oob_spec,
    _["oob_youden"]            = oob_youd,
    _["oob_reason_code"]       = oob_reason_code
  );
}

// [[Rcpp::export]]
List reduce_candidate_stability_replicates_cpp(
    IntegerVector candidate_id,
    IntegerVector replicate_id,
    IntegerVector cand_n_items,
    NumericVector rank_metric_vals,
    LogicalVector training_valid,
    NumericVector test_sensitivity,
    NumericVector test_specificity,
    bool check_training_valid,
    double sensitivity_min,
    double specificity_min,
    bool prefer_fewer_items,
    int total_resamples
) {
  int N = candidate_id.size();
  IntegerVector rank_out(N, NA_INTEGER);
  LogicalVector feasible_out(N, false);
  IntegerVector status_code(total_resamples, 2); // 0=selected, 1=no_feasible_candidate, 2=invalid_evaluation
  IntegerVector winner_ids(total_resamples, NA_INTEGER);
  IntegerVector n_feasible_vec(total_resamples, 0);

  std::vector<std::vector<int>> rep_rows(total_resamples);
  for (int i = 0; i < N; i++) {
    int r = replicate_id[i];
    if (r >= 1 && r <= total_resamples) {
      rep_rows[r - 1].push_back(i);
    }
  }

  bool has_sens = (sensitivity_min >= 0.0);
  bool has_spec = (specificity_min >= 0.0);

  std::vector<int> valid_rows;
  std::vector<int> feas_rows;

  for (int r = 0; r < total_resamples; r++) {
    const std::vector<int>& rows = rep_rows[r];
    if (rows.empty()) {
      status_code[r] = 2;
      continue;
    }

    if (check_training_valid) {
      bool all_tr = true;
      for (int idx : rows) {
        if (training_valid[idx] != TRUE) {
          all_tr = false;
          break;
        }
      }
      if (!all_tr) {
        status_code[r] = 2;
        continue;
      }
    }

    valid_rows.clear();
    feas_rows.clear();
    int n_feas = 0;

    for (int idx : rows) {
      bool s_ok = !has_sens || (!NumericVector::is_na(test_sensitivity[idx]) && test_sensitivity[idx] >= sensitivity_min);
      bool p_ok = !has_spec || (!NumericVector::is_na(test_specificity[idx]) && test_specificity[idx] >= specificity_min);
      bool feas = s_ok && p_ok;
      feasible_out[idx] = feas;
      if (feas) {
        n_feas++;
        feas_rows.push_back(idx);
      }
      if (!NumericVector::is_na(rank_metric_vals[idx])) {
        valid_rows.push_back(idx);
      }
    }
    n_feasible_vec[r] = n_feas;

    auto cmp_rows = [&](int a, int b) {
      double va = rank_metric_vals[a];
      double vb = rank_metric_vals[b];
      bool na_a = NumericVector::is_na(va);
      bool na_b = NumericVector::is_na(vb);
      if (na_a != na_b) return !na_a; // non-NA first
      if (!na_a && va != vb) return va > vb;
      int ca = candidate_id[a];
      int cb = candidate_id[b];
      if (prefer_fewer_items) {
        int sa = cand_n_items[ca - 1];
        int sb = cand_n_items[cb - 1];
        if (sa != sb) return sa < sb;
      }
      return ca < cb;
    };

    if (!valid_rows.empty()) {
      std::sort(valid_rows.begin(), valid_rows.end(), cmp_rows);
      for (int pos = 0; pos < (int)valid_rows.size(); pos++) {
        rank_out[valid_rows[pos]] = pos + 1;
      }
    }

    if (valid_rows.empty()) {
      status_code[r] = 2;
    } else if (n_feas == 0) {
      status_code[r] = 1;
    } else {
      status_code[r] = 0;
      int best_idx = *std::min_element(feas_rows.begin(), feas_rows.end(), cmp_rows);
      winner_ids[r] = candidate_id[best_idx];
    }
  }

  return List::create(
    _["rank"]           = rank_out,
    _["feasible"]       = feasible_out,
    _["status_code"]    = status_code,
    _["winner_ids"]     = winner_ids,
    _["n_feasible_vec"] = n_feasible_vec
  );
}
