// src/native_outer_fold.cpp
// Scope B: Native End-to-End Outer-Fold Execution Foundation
// Pure C++ / RcppParallel worker execution with explicit input/result contracts.
// Zero R API calls and zero R RNG calls inside worker threads.

#include <Rcpp.h>
#include <RcppParallel.h>
#include "ncvroc_native_helpers.h"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

using namespace Rcpp;

static const int NATIVE_OUTER_FOLD_SCHEMA_VERSION = 1;
static const double NATIVE_OUTER_FOLD_MAX_CANDIDATES = 2000000.0;

struct NativeOuterFoldInput {
  int schema_version;
  int repeat_id;
  int outer_fold_id;
  std::string fold_name;
  std::vector<int> train_indices; // 0-based into x_full/y_full
  std::vector<int> test_indices;  // 0-based into x_full/y_full
  // inner_folds[cand_idx][inner_fold_idx] = 0-based test indices into outer_train
  std::vector<std::vector<std::vector<int>>> inner_folds;
  int min_items;
  int max_items;
  int preselect_top_n;
  CutoffMethod cutoff_method;
  RankMetric preselect_by;
  RankMetric selection_criterion;
  bool prefer_fewer_items;
  bool has_seed;
  int inner_seed_base;
  int num_threads;
  std::string backend_provenance;

  NativeOuterFoldInput()
    : schema_version(0),
      repeat_id(0),
      outer_fold_id(0),
      min_items(0),
      max_items(0),
      preselect_top_n(0),
      cutoff_method(CUTOFF_YOUDEN),
      preselect_by(RANK_AUC),
      selection_criterion(RANK_AUC),
      prefer_fewer_items(true),
      has_seed(false),
      inner_seed_base(0),
      num_threads(1) {}
};

struct NativeOuterFoldResult {
  int schema_version;
  int repeat_id;
  int outer_fold_id;
  std::string outer_fold;
  bool completed;
  int status_code;
  std::string status_message;
  std::string backend_provenance;
  bool has_seed;
  int inner_seed_base;
  int n_candidates_evaluated;
  double selected_global_rank;
  std::vector<int> selected_col_indices; // 0-based column indices
  int n_items;
  double inner_mean_auc;
  double inner_mean_sensitivity;
  double inner_mean_specificity;
  double inner_mean_youden;
  double auc;
  double cutoff;
  double sensitivity;
  double specificity;
  double youden;
  double accuracy;
  double ppv;
  double npv;
  std::vector<int> pred_row_index;    // 1-based original row indices
  std::vector<int> pred_true_outcome; // 0/1
  std::vector<double> pred_score;
  std::vector<int> pred_class;

  NativeOuterFoldResult()
    : schema_version(NATIVE_OUTER_FOLD_SCHEMA_VERSION),
      repeat_id(0),
      outer_fold_id(0),
      completed(false),
      status_code(-1),
      has_seed(false),
      inner_seed_base(0),
      n_candidates_evaluated(0),
      selected_global_rank(0.0),
      n_items(0),
      inner_mean_auc(NA_REAL),
      inner_mean_sensitivity(NA_REAL),
      inner_mean_specificity(NA_REAL),
      inner_mean_youden(NA_REAL),
      auc(NA_REAL),
      cutoff(NA_REAL),
      sensitivity(NA_REAL),
      specificity(NA_REAL),
      youden(NA_REAL),
      accuracy(NA_REAL),
      ppv(NA_REAL),
      npv(NA_REAL) {}
};

// Pure C++ helper: compute optimal cutoff on a subset of rows for given columns
static inline double fit_cutoff_on_subset(
    const double* x_mat,
    const int* y_vec,
    int n_rows_stride,
    const int* row_subset, // nullptr => 0..n_subset-1
    int n_subset,
    const std::vector<int>& col_indices,
    CutoffMethod cutoff_method,
    ThreadLocalBuffer& buf,
    std::vector<int>& y_sub_buf
) {
  int k = (int)col_indices.size();
  if ((int)buf.scores.size() < n_subset) {
    buf.scores.resize(n_subset);
  }
  y_sub_buf.resize(n_subset);

  int total_pos = 0;
  int total_neg = 0;
  for (int i = 0; i < n_subset; ++i) {
    int r = (row_subset != nullptr) ? row_subset[i] : i;
    double s = 0.0;
    for (int j = 0; j < k; ++j) {
      s += x_mat[r + col_indices[j] * n_rows_stride];
    }
    buf.scores[i] = s;
    int yv = y_vec[r];
    y_sub_buf[i] = yv;
    if (yv == 1) total_pos++; else total_neg++;
  }

  double auc_val = NA_REAL, cutoff_val = NA_REAL, sens_val = NA_REAL, spec_val = NA_REAL;
  double youden_val = NA_REAL, acc_val = NA_REAL, ppv_val = NA_REAL, npv_val = NA_REAL;

  evaluate_roc_from_scores(
    y_sub_buf.data(), n_subset, total_pos, total_neg, cutoff_method, buf,
    auc_val, cutoff_val, sens_val, spec_val, youden_val, acc_val, ppv_val, npv_val
  );

  return cutoff_val;
}

// Pure C++ helper: score a test subset against a fixed cutoff and compute AUC & confusion metrics
static inline void score_test_subset(
    const double* x_mat,
    const int* y_vec,
    int n_rows_stride,
    const int* test_indices,
    int n_test,
    const std::vector<int>& col_indices,
    double cutoff_val,
    ThreadLocalBuffer& buf,
    double& out_auc,
    double& out_sens,
    double& out_spec,
    double& out_youden,
    double& out_acc,
    double& out_ppv,
    double& out_npv,
    std::vector<double>* out_scores = nullptr,
    std::vector<int>* out_pred_class = nullptr
) {
  int k = (int)col_indices.size();
  buf.pos_counts.clear();
  buf.neg_counts.clear();

  if (out_scores != nullptr) out_scores->resize(n_test);
  if (out_pred_class != nullptr) out_pred_class->resize(n_test);

  int tp = 0, tn = 0, fp = 0, fn = 0;
  int total_pos = 0, total_neg = 0;

  for (int i = 0; i < n_test; ++i) {
    int r = test_indices[i];
    double s = 0.0;
    for (int j = 0; j < k; ++j) {
      s += x_mat[r + col_indices[j] * n_rows_stride];
    }
    int yv = y_vec[r];
    int pred = (s >= cutoff_val) ? 1 : 0;

    if (out_scores != nullptr) (*out_scores)[i] = s;
    if (out_pred_class != nullptr) (*out_pred_class)[i] = pred;

    if (yv == 1) {
      total_pos++;
      buf.pos_counts[s]++;
      if (pred == 1) tp++; else fn++;
    } else {
      total_neg++;
      buf.neg_counts[s]++;
      if (pred == 1) fp++; else tn++;
    }
  }

  if (total_pos == 0 || total_neg == 0) {
    out_auc = NA_REAL;
  } else {
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
    out_auc = auc_sum / ((double)total_pos * total_neg);
  }

  out_sens   = (tp + fn > 0) ? (double)tp / (double)(tp + fn) : NA_REAL;
  out_spec   = (tn + fp > 0) ? (double)tn / (double)(tn + fp) : NA_REAL;
  out_ppv    = (tp + fp > 0) ? (double)tp / (double)(tp + fp) : NA_REAL;
  out_npv    = (tn + fn > 0) ? (double)tn / (double)(tn + fn) : NA_REAL;
  out_acc    = (tp + tn + fp + fn > 0) ? (double)(tp + tn) / (double)(tp + tn + fp + fn) : NA_REAL;
  out_youden = (std::isnan(out_sens) || std::isnan(out_spec)) ? NA_REAL : (out_sens + out_spec - 1.0);
}

static inline double mean_na_rm(const std::vector<double>& vals) {
  long double sum = 0.0L;
  int count = 0;
  for (double v : vals) {
    if (!std::isnan(v)) {
      sum += (long double)v;
      count++;
    }
  }
  if (count == 0) return NA_REAL;
  long double res = sum / (long double)count;
  long double corr = 0.0L;
  for (double v : vals) {
    if (!std::isnan(v)) {
      corr += ((long double)v - res);
    }
  }
  return (double)(res + corr / (long double)count);
}

static inline void validate_result_payload(const NativeOuterFoldResult& res, int expected_test_size, int min_items, int max_items) {
  if (res.schema_version != NATIVE_OUTER_FOLD_SCHEMA_VERSION) {
    throw std::runtime_error("Result schema_version mismatch.");
  }
  if (res.repeat_id < 1 || res.outer_fold_id < 1 || res.outer_fold.empty()) {
    throw std::runtime_error("Result identity fields are invalid.");
  }
  if (res.n_candidates_evaluated < 1) {
    throw std::runtime_error("Result evaluated zero candidates.");
  }
  if (res.n_items < min_items || res.n_items > max_items || (int)res.selected_col_indices.size() != res.n_items) {
    throw std::runtime_error("Result selected item count is out of bounds.");
  }
  if ((int)res.pred_row_index.size() != expected_test_size ||
      (int)res.pred_true_outcome.size() != expected_test_size ||
      (int)res.pred_score.size() != expected_test_size ||
      (int)res.pred_class.size() != expected_test_size) {
    throw std::runtime_error("Result prediction vector lengths do not match outer test size.");
  }
  if (std::isnan(res.cutoff) || !std::isfinite(res.cutoff)) {
    throw std::runtime_error("Result cutoff is non-finite.");
  }
}

// Pure C++ complete-fold execution kernel (zero R API / R RNG calls)
static inline void execute_single_native_outer_fold(
    const double* x_full,
    const int* y_full,
    int n_full,
    int n_cols,
    const NativeOuterFoldInput& input,
    NativeOuterFoldResult& res
) {
  res = NativeOuterFoldResult();
  res.schema_version = input.schema_version;
  res.repeat_id = input.repeat_id;
  res.outer_fold_id = input.outer_fold_id;
  res.outer_fold = input.fold_name;
  res.backend_provenance = input.backend_provenance;
  res.has_seed = input.has_seed;
  res.inner_seed_base = input.inner_seed_base;
  res.completed = false;
  res.status_code = -1;

  int n_train = (int)input.train_indices.size();
  int n_test = (int)input.test_indices.size();

  // Build contiguous outer-training data
  std::vector<double> x_train((std::size_t)n_train * (std::size_t)n_cols);
  std::vector<int> y_train(n_train);
  int train_pos = 0, train_neg = 0;

  for (int r = 0; r < n_train; ++r) {
    int row_orig = input.train_indices[r];
    int yv = y_full[row_orig];
    y_train[r] = yv;
    if (yv == 1) train_pos++; else train_neg++;
    for (int c = 0; c < n_cols; ++c) {
      x_train[r + c * n_train] = x_full[row_orig + c * n_full];
    }
  }

  if (train_pos == 0 || train_neg == 0) {
    throw std::runtime_error("Outer training fold must contain both positive and negative outcomes.");
  }

  // Step 1 & 2: Candidate search and preselection on outer train
  std::vector<CandidateRecord> top_candidates;
  evaluate_combos_topn_kernel(
    x_train.data(),
    y_train.data(),
    n_train,
    n_cols,
    input.min_items,
    input.max_items,
    input.cutoff_method,
    input.preselect_by,
    input.prefer_fewer_items,
    input.preselect_top_n,
    top_candidates
  );

  int n_cand = (int)top_candidates.size();
  if (n_cand < 1) {
    throw std::runtime_error("Candidate search returned zero candidates.");
  }
  if (n_cand != (int)input.inner_folds.size()) {
    throw std::runtime_error("Inner fold specification count does not match candidate count.");
  }
  res.n_candidates_evaluated = n_cand;

  // Step 3: Inner CV for each preselected candidate
  std::vector<double> mean_aucs(n_cand, NA_REAL);
  std::vector<double> mean_sensitivities(n_cand, NA_REAL);
  std::vector<double> mean_specificities(n_cand, NA_REAL);
  std::vector<double> mean_youdens(n_cand, NA_REAL);

  ThreadLocalBuffer buf(n_train);
  std::vector<int> y_sub_buf;
  std::vector<int> cand_cols;
  std::vector<int> inner_train_idx;
  std::vector<uint8_t> is_inner_test(n_train, 0);

  int n_k = input.max_items - input.min_items + 1;
  std::vector<double> level_starts(n_k);
  {
    double total = 0.0;
    for (int ki = 0; ki < n_k; ++ki) {
      level_starts[ki] = total;
      total += binom(n_cols, input.min_items + ki);
    }
  }

  for (int c_i = 0; c_i < n_cand; ++c_i) {
    const CandidateRecord& cand = top_candidates[c_i];
    auto it = std::upper_bound(level_starts.begin(), level_starts.end(), cand.global_rank);
    int ki = (int)std::distance(level_starts.begin(), it) - 1;
    int k = input.min_items + ki;
    double local_rank = cand.global_rank - level_starts[ki];
    unrank_combination_into(n_cols, k, local_rank, cand_cols);

    const auto& cand_inner_folds = input.inner_folds[c_i];
    int n_inner = (int)cand_inner_folds.size();
    if (n_inner < 1) {
      throw std::runtime_error("Candidate inner fold list is empty.");
    }

    std::vector<double> f_aucs(n_inner);
    std::vector<double> f_sens(n_inner);
    std::vector<double> f_spec(n_inner);
    std::vector<double> f_youden(n_inner);

    for (int f = 0; f < n_inner; ++f) {
      const std::vector<int>& inner_test_idx = cand_inner_folds[f];
      std::fill(is_inner_test.begin(), is_inner_test.end(), 0);
      for (int idx : inner_test_idx) {
        is_inner_test[idx] = 1;
      }
      inner_train_idx.clear();
      inner_train_idx.reserve(n_train - (int)inner_test_idx.size());
      for (int r = 0; r < n_train; ++r) {
        if (!is_inner_test[r]) {
          inner_train_idx.push_back(r);
        }
      }
      if (inner_train_idx.empty()) {
        throw std::runtime_error("Inner training fold is empty.");
      }

      double inner_cutoff = fit_cutoff_on_subset(
        x_train.data(),
        y_train.data(),
        n_train,
        inner_train_idx.data(),
        (int)inner_train_idx.size(),
        cand_cols,
        input.cutoff_method,
        buf,
        y_sub_buf
      );
      if (std::isnan(inner_cutoff) || !std::isfinite(inner_cutoff)) {
        throw std::runtime_error("Inner training fold produced non-finite cutoff.");
      }

      double t_auc = NA_REAL, t_sens = NA_REAL, t_spec = NA_REAL, t_youden = NA_REAL;
      double t_acc = NA_REAL, t_ppv = NA_REAL, t_npv = NA_REAL;
      score_test_subset(
        x_train.data(),
        y_train.data(),
        n_train,
        inner_test_idx.data(),
        (int)inner_test_idx.size(),
        cand_cols,
        inner_cutoff,
        buf,
        t_auc,
        t_sens,
        t_spec,
        t_youden,
        t_acc,
        t_ppv,
        t_npv
      );

      f_aucs[f]   = std::isnan(t_auc) ? 0.5 : t_auc;
      f_sens[f]   = t_sens;
      f_spec[f]   = t_spec;
      f_youden[f] = t_youden;
    }

    mean_aucs[c_i]          = mean_na_rm(f_aucs);
    mean_sensitivities[c_i] = mean_na_rm(f_sens);
    mean_specificities[c_i] = mean_na_rm(f_spec);
    mean_youdens[c_i]       = mean_na_rm(f_youden);
  }

  // Step 4: Model selection matching R's which.max(inner_results[[criterion_col]])
  // Note: In R/nested_sum_roc.R, `best_idx <- which.max(inner_results[[criterion_col]])`
  // always returns a single scalar index (the first maximum, ignoring NAs), so the
  // subsequent `if (length(best_idx) > 1)` tie-break branch in R is unreachable.
  // To preserve exact statistical parity with the R reference, Scope B uses strict `>`
  // (first-in-order wins ties) while skipping NaNs.
  const std::vector<double>* crit_vec = &mean_aucs;
  if (input.selection_criterion == RANK_YOUDEN) crit_vec = &mean_youdens;
  else if (input.selection_criterion == RANK_SENSITIVITY) crit_vec = &mean_sensitivities;
  else if (input.selection_criterion == RANK_SPECIFICITY) crit_vec = &mean_specificities;

  int best_idx = -1;
  double best_val = 0.0;
  for (int c_i = 0; c_i < n_cand; ++c_i) {
    double v = (*crit_vec)[c_i];
    if (std::isnan(v)) continue;
    if (best_idx < 0 || v > best_val) {
      best_idx = c_i;
      best_val = v;
    }
  }
  if (best_idx < 0) {
    throw std::runtime_error("All candidates produced NA for inner CV selection criterion.");
  }

  const CandidateRecord& best_cand = top_candidates[best_idx];
  {
    auto it = std::upper_bound(level_starts.begin(), level_starts.end(), best_cand.global_rank);
    int ki = (int)std::distance(level_starts.begin(), it) - 1;
    int k = input.min_items + ki;
    double local_rank = best_cand.global_rank - level_starts[ki];
    unrank_combination_into(n_cols, k, local_rank, res.selected_col_indices);
    res.n_items = k;
  }
  res.selected_global_rank   = best_cand.global_rank;
  res.inner_mean_auc         = mean_aucs[best_idx];
  res.inner_mean_sensitivity = mean_sensitivities[best_idx];
  res.inner_mean_specificity = mean_specificities[best_idx];
  res.inner_mean_youden      = mean_youdens[best_idx];

  // Step 5: Fit cutoff on complete outer training set and evaluate on outer test set
  double outer_cutoff = fit_cutoff_on_subset(
    x_train.data(),
    y_train.data(),
    n_train,
    nullptr,
    n_train,
    res.selected_col_indices,
    input.cutoff_method,
    buf,
    y_sub_buf
  );
  if (std::isnan(outer_cutoff) || !std::isfinite(outer_cutoff)) {
    throw std::runtime_error("Outer training fold produced non-finite cutoff.");
  }
  res.cutoff = outer_cutoff;

  res.pred_row_index.resize(n_test);
  res.pred_true_outcome.resize(n_test);
  for (int i = 0; i < n_test; ++i) {
    int orig_0 = input.test_indices[i];
    res.pred_row_index[i] = orig_0 + 1;
    res.pred_true_outcome[i] = y_full[orig_0];
  }

  score_test_subset(
    x_full,
    y_full,
    n_full,
    input.test_indices.data(),
    n_test,
    res.selected_col_indices,
    outer_cutoff,
    buf,
    res.auc,
    res.sensitivity,
    res.specificity,
    res.youden,
    res.accuracy,
    res.ppv,
    res.npv,
    &res.pred_score,
    &res.pred_class
  );

  validate_result_payload(res, n_test, input.min_items, input.max_items);

  res.status_code = 0;
  res.status_message = "OK";
  res.completed = true;
}

struct OuterFoldCompleteWorker : public RcppParallel::Worker {
  const double* x_full;
  const int* y_full;
  int n_full;
  int n_cols;
  const std::vector<NativeOuterFoldInput>& inputs;
  std::vector<NativeOuterFoldResult>& results;

  OuterFoldCompleteWorker(
    const double* x_full_,
    const int* y_full_,
    int n_full_,
    int n_cols_,
    const std::vector<NativeOuterFoldInput>& inputs_,
    std::vector<NativeOuterFoldResult>& results_
  ) : x_full(x_full_), y_full(y_full_), n_full(n_full_), n_cols(n_cols_),
      inputs(inputs_), results(results_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t fi = begin; fi < end; ++fi) {
      const NativeOuterFoldInput& in = inputs[fi];
      NativeOuterFoldResult& out = results[fi];
      try {
        execute_single_native_outer_fold(x_full, y_full, n_full, n_cols, in, out);
      } catch (const std::exception& e) {
        out = NativeOuterFoldResult();
        out.schema_version = in.schema_version;
        out.repeat_id = in.repeat_id;
        out.outer_fold_id = in.outer_fold_id;
        out.outer_fold = in.fold_name;
        out.backend_provenance = in.backend_provenance;
        out.has_seed = in.has_seed;
        out.inner_seed_base = in.inner_seed_base;
        out.completed = false;
        out.status_code = 1;
        out.status_message = e.what();
      } catch (...) {
        out = NativeOuterFoldResult();
        out.schema_version = in.schema_version;
        out.repeat_id = in.repeat_id;
        out.outer_fold_id = in.outer_fold_id;
        out.outer_fold = in.fold_name;
        out.backend_provenance = in.backend_provenance;
        out.has_seed = in.has_seed;
        out.inner_seed_base = in.inner_seed_base;
        out.completed = false;
        out.status_code = 2;
        out.status_message = "Unknown C++ exception in native outer fold worker";
      }
    }
  }
};

static inline std::vector<int> validate_and_extract_0based_indices(
    SEXP vec_sexp,
    int upper_bound,
    const char* context_label
) {
  int sexp_type = TYPEOF(vec_sexp);
  if (sexp_type != INTSXP && sexp_type != REALSXP) {
    stop("%s must be an integer or numeric vector.", context_label);
  }
  std::vector<int> out;
  if (sexp_type == REALSXP) {
    NumericVector nv(vec_sexp);
    int n = nv.size();
    if (n < 1) stop("%s must not be empty.", context_label);
    out.resize(n);
    for (int i = 0; i < n; ++i) {
      double val = nv[i];
      if (!R_finite(val) || NumericVector::is_na(val) || std::floor(val) != val) {
        stop("%s contains invalid or non-integer index at position %d.", context_label, i + 1);
      }
      int iv = (int)val;
      if (iv < 0 || iv >= upper_bound) {
        stop("%s index %d out of bounds [0, %d).", context_label, iv, upper_bound);
      }
      out[i] = iv;
    }
  } else {
    IntegerVector iv_vec(vec_sexp);
    int n = iv_vec.size();
    if (n < 1) stop("%s must not be empty.", context_label);
    out.resize(n);
    for (int i = 0; i < n; ++i) {
      if (IntegerVector::is_na(iv_vec[i])) {
        stop("%s contains NA index at position %d.", context_label, i + 1);
      }
      int iv = iv_vec[i];
      if (iv < 0 || iv >= upper_bound) {
        stop("%s index %d out of bounds [0, %d).", context_label, iv, upper_bound);
      }
      out[i] = iv;
    }
  }
  return out;
}

static inline void validate_partition_pair(
    const std::vector<int>& train_idx,
    const std::vector<int>& test_idx,
    int n_full,
    int fold_num
) {
  std::vector<uint8_t> seen(n_full, 0);
  for (int idx : train_idx) {
    if (seen[idx]) {
      stop("Outer fold %d train_indices contains duplicate index %d.", fold_num, idx);
    }
    seen[idx] = 1;
  }
  for (int idx : test_idx) {
    if (seen[idx] == 1) {
      stop("Outer fold %d test_indices overlaps train_indices at index %d.", fold_num, idx);
    }
    if (seen[idx] == 2) {
      stop("Outer fold %d test_indices contains duplicate index %d.", fold_num, idx);
    }
    seen[idx] = 2;
  }
  for (int i = 0; i < n_full; ++i) {
    if (seen[i] == 0) {
      stop("Outer fold %d train/test indices do not partition 0..%d (missing %d).", fold_num, n_full - 1, i);
    }
  }
}

static inline void validate_inner_folds_partition(
    const std::vector<std::vector<int>>& folds,
    int n_train,
    int fold_num,
    int cand_num
) {
  if (folds.empty()) {
    stop("Outer fold %d candidate %d has empty inner_folds.", fold_num, cand_num);
  }
  std::vector<uint8_t> seen(n_train, 0);
  for (std::size_t f = 0; f < folds.size(); ++f) {
    for (int idx : folds[f]) {
      if (seen[idx]) {
        stop("Outer fold %d candidate %d inner fold %d has duplicate/overlapping index %d.",
             fold_num, cand_num, (int)f + 1, idx);
      }
      seen[idx] = 1;
    }
  }
  for (int i = 0; i < n_train; ++i) {
    if (seen[i] == 0) {
      stop("Outer fold %d candidate %d inner_folds do not partition 0..%d (missing %d).",
           fold_num, cand_num, n_train - 1, i);
    }
  }
}

static inline List native_outer_fold_result_to_r_list(const NativeOuterFoldResult& res) {
  IntegerVector col_idx_1based(res.selected_col_indices.size());
  for (std::size_t j = 0; j < res.selected_col_indices.size(); ++j) {
    col_idx_1based[j] = res.selected_col_indices[j] + 1;
  }

  DataFrame predictions = DataFrame::create(
    Named("row_index")       = wrap(res.pred_row_index),
    Named("true_outcome")    = wrap(res.pred_true_outcome),
    Named("predicted_score") = wrap(res.pred_score),
    Named("predicted_class") = wrap(res.pred_class),
    Named("stringsAsFactors") = false
  );

  return List::create(
    Named("schema_version")         = res.schema_version,
    Named("repeat_id")              = res.repeat_id,
    Named("outer_fold_id")          = res.outer_fold_id,
    Named("outer_fold")             = res.outer_fold,
    Named("completed")              = res.completed,
    Named("status_code")            = res.status_code,
    Named("status_message")         = res.status_message,
    Named("backend_provenance")     = res.backend_provenance,
    Named("has_seed")               = res.has_seed,
    Named("inner_seed_base")        = res.inner_seed_base,
    Named("n_candidates_evaluated") = res.n_candidates_evaluated,
    Named("selected_global_rank")   = res.selected_global_rank,
    Named("selected_col_indices")   = col_idx_1based,
    Named("n_items")                = res.n_items,
    Named("inner_mean_auc")         = res.inner_mean_auc,
    Named("inner_mean_sensitivity") = res.inner_mean_sensitivity,
    Named("inner_mean_specificity") = res.inner_mean_specificity,
    Named("inner_mean_youden")      = res.inner_mean_youden,
    Named("auc")                    = res.auc,
    Named("cutoff")                 = res.cutoff,
    Named("sensitivity")            = res.sensitivity,
    Named("specificity")            = res.specificity,
    Named("youden")                 = res.youden,
    Named("accuracy")               = res.accuracy,
    Named("ppv")                    = res.ppv,
    Named("npv")                    = res.npv,
    Named("predictions")            = predictions
  );
}

static inline NativeOuterFoldResult r_list_to_native_outer_fold_result(const List& r_res) {
  NativeOuterFoldResult res;
  res.schema_version = as<int>(r_res["schema_version"]);
  if (res.schema_version != NATIVE_OUTER_FOLD_SCHEMA_VERSION) {
    stop("Unsupported NativeOuterFoldResult schema_version: %d", res.schema_version);
  }
  res.repeat_id              = as<int>(r_res["repeat_id"]);
  res.outer_fold_id          = as<int>(r_res["outer_fold_id"]);
  res.outer_fold             = as<std::string>(r_res["outer_fold"]);
  res.completed              = as<bool>(r_res["completed"]);
  res.status_code            = as<int>(r_res["status_code"]);
  res.status_message         = as<std::string>(r_res["status_message"]);
  res.backend_provenance     = as<std::string>(r_res["backend_provenance"]);
  res.has_seed               = as<bool>(r_res["has_seed"]);
  res.inner_seed_base        = as<int>(r_res["inner_seed_base"]);
  res.n_candidates_evaluated = as<int>(r_res["n_candidates_evaluated"]);
  res.selected_global_rank   = as<double>(r_res["selected_global_rank"]);

  IntegerVector col_idx_1based = as<IntegerVector>(r_res["selected_col_indices"]);
  res.selected_col_indices.resize(col_idx_1based.size());
  for (int j = 0; j < col_idx_1based.size(); ++j) {
    res.selected_col_indices[j] = col_idx_1based[j] - 1;
  }

  res.n_items                = as<int>(r_res["n_items"]);
  res.inner_mean_auc         = as<double>(r_res["inner_mean_auc"]);
  res.inner_mean_sensitivity = as<double>(r_res["inner_mean_sensitivity"]);
  res.inner_mean_specificity = as<double>(r_res["inner_mean_specificity"]);
  res.inner_mean_youden      = as<double>(r_res["inner_mean_youden"]);
  res.auc                    = as<double>(r_res["auc"]);
  res.cutoff                 = as<double>(r_res["cutoff"]);
  res.sensitivity            = as<double>(r_res["sensitivity"]);
  res.specificity            = as<double>(r_res["specificity"]);
  res.youden                 = as<double>(r_res["youden"]);
  res.accuracy               = as<double>(r_res["accuracy"]);
  res.ppv                    = as<double>(r_res["ppv"]);
  res.npv                    = as<double>(r_res["npv"]);

  DataFrame preds = as<DataFrame>(r_res["predictions"]);
  res.pred_row_index    = as<std::vector<int>>(preds["row_index"]);
  res.pred_true_outcome = as<std::vector<int>>(preds["true_outcome"]);
  res.pred_score        = as<std::vector<double>>(preds["predicted_score"]);
  res.pred_class        = as<std::vector<int>>(preds["predicted_class"]);

  if (!res.completed || res.status_code != 0) {
    stop("Cannot round-trip incomplete NativeOuterFoldResult.");
  }
  validate_result_payload(res, (int)res.pred_row_index.size(), 1, res.n_items);
  return res;
}

// [[Rcpp::export]]
List evaluate_outer_folds_native_cpp(
    NumericMatrix x,
    IntegerVector y,
    List fold_specs,
    int min_items,
    int max_items,
    std::string cutoff_method,
    std::string preselect_by,
    std::string selection_criterion,
    int preselect_top_n,
    bool prefer_fewer_items = true,
    int num_threads = 1,
    int schema_version = 1
) {
  if (IntegerVector::is_na(schema_version) || schema_version != NATIVE_OUTER_FOLD_SCHEMA_VERSION) {
    stop("Unsupported schema_version (%d); expected %d.", schema_version, NATIVE_OUTER_FOLD_SCHEMA_VERSION);
  }

  int n_full = x.nrow();
  int n_cols = x.ncol();
  if (n_full < 1 || n_cols < 1) {
    stop("x must have at least 1 row and 1 column.");
  }
  if (y.length() != n_full) {
    stop("Length of y (%d) must match nrow(x) (%d).", (int)y.length(), n_full);
  }

  const double* x_ptr = &x[0];
  for (int c = 0; c < n_cols; ++c) {
    for (int r = 0; r < n_full; ++r) {
      double val = x_ptr[r + c * n_full];
      if (!R_finite(val) || NumericVector::is_na(val)) {
        stop("x contains non-finite or NA value at row %d, col %d.", r + 1, c + 1);
      }
    }
  }

  for (int i = 0; i < n_full; ++i) {
    if (IntegerVector::is_na(y[i])) {
      stop("y contains NA values.");
    }
    if (y[i] != 0 && y[i] != 1) {
      stop("y must contain binary values (0 or 1).");
    }
  }

  if (IntegerVector::is_na(num_threads) || num_threads <= 0) {
    stop("num_threads must be a positive integer.");
  }
  if (IntegerVector::is_na(preselect_top_n) || preselect_top_n < 1) {
    stop("preselect_top_n must be a positive integer.");
  }
  if (IntegerVector::is_na(min_items) || min_items < 1) {
    stop("min_items must be an integer >= 1.");
  }
  if (IntegerVector::is_na(max_items) || max_items < min_items || max_items > n_cols) {
    stop("max_items must satisfy min_items <= max_items <= ncol(x).");
  }

  CutoffMethod cm;
  if (cutoff_method == "youden") cm = CUTOFF_YOUDEN;
  else if (cutoff_method == "closest_topleft") cm = CUTOFF_CLOSEST_TOPLEFT;
  else stop("Unsupported cutoff_method: '%s'", cutoff_method.c_str());

  auto parse_metric = [](const std::string& m, const char* arg_name) -> RankMetric {
    if (m == "auc") return RANK_AUC;
    if (m == "youden") return RANK_YOUDEN;
    if (m == "sensitivity") return RANK_SENSITIVITY;
    if (m == "specificity") return RANK_SPECIFICITY;
    stop("Unsupported %s metric: '%s'", arg_name, m.c_str());
    return RANK_AUC;
  };
  RankMetric pre_rm = parse_metric(preselect_by, "preselect_by");
  RankMetric sel_rm = parse_metric(selection_criterion, "selection_criterion");

  double total_combos = 0.0;
  for (int k = min_items; k <= max_items; ++k) {
    total_combos += binom(n_cols, k);
  }
  if (!std::isfinite(total_combos) || total_combos <= 0.0 || total_combos > NATIVE_OUTER_FOLD_MAX_CANDIDATES) {
    stop("Candidate space (%.0f) exceeds safe native outer-fold bounds (max %.0f).",
         total_combos, NATIVE_OUTER_FOLD_MAX_CANDIDATES);
  }
  int expected_n_cand = (total_combos < (double)preselect_top_n) ? (int)total_combos : preselect_top_n;

  int n_folds = fold_specs.size();
  if (n_folds < 1) {
    stop("fold_specs must be a non-empty list.");
  }

  std::vector<NativeOuterFoldInput> inputs(n_folds);
  for (int f = 0; f < n_folds; ++f) {
    if (!Rf_isNewList(fold_specs[f])) {
      stop("fold_specs[[%d]] must be a list.", f + 1);
    }
    List spec = fold_specs[f];
    NativeOuterFoldInput& in = inputs[f];
    in.schema_version = schema_version;
    in.repeat_id = as<int>(spec["repeat_id"]);
    in.outer_fold_id = as<int>(spec["outer_fold_id"]);
    if (in.repeat_id < 1 || in.outer_fold_id < 1) {
      stop("fold_specs[[%d]] repeat_id and outer_fold_id must be >= 1.", f + 1);
    }
    in.fold_name = as<std::string>(spec["fold_name"]);
    if (in.fold_name.empty()) {
      stop("fold_specs[[%d]] fold_name must be non-empty.", f + 1);
    }

    in.train_indices = validate_and_extract_0based_indices(spec["train_indices"], n_full, "train_indices");
    in.test_indices  = validate_and_extract_0based_indices(spec["test_indices"],  n_full, "test_indices");
    validate_partition_pair(in.train_indices, in.test_indices, n_full, f + 1);

    int n_train = (int)in.train_indices.size();
    List inner_list = as<List>(spec["inner_folds"]);
    if (inner_list.size() != expected_n_cand) {
      stop("fold_specs[[%d]] inner_folds length (%d) must equal expected candidate count (%d).",
           f + 1, (int)inner_list.size(), expected_n_cand);
    }
    in.inner_folds.resize(expected_n_cand);
    for (int c_i = 0; c_i < expected_n_cand; ++c_i) {
      List cand_folds = as<List>(inner_list[c_i]);
      in.inner_folds[c_i].resize(cand_folds.size());
      for (int k_i = 0; k_i < cand_folds.size(); ++k_i) {
        in.inner_folds[c_i][k_i] = validate_and_extract_0based_indices(
          cand_folds[k_i], n_train, "inner_folds"
        );
      }
      validate_inner_folds_partition(in.inner_folds[c_i], n_train, f + 1, c_i + 1);
    }

    in.min_items = min_items;
    in.max_items = max_items;
    in.preselect_top_n = preselect_top_n;
    in.cutoff_method = cm;
    in.preselect_by = pre_rm;
    in.selection_criterion = sel_rm;
    in.prefer_fewer_items = prefer_fewer_items;
    in.has_seed = as<bool>(spec["has_seed"]);
    in.inner_seed_base = as<int>(spec["inner_seed_base"]);
    in.num_threads = num_threads;
    in.backend_provenance = (num_threads == 1) ? "scope_b_native_serial" : "scope_b_native_tbb";
  }

  // Validate unique (repeat_id, outer_fold_id) pairs
  {
    std::vector<std::pair<int, int>> ids;
    ids.reserve(n_folds);
    for (const auto& in : inputs) {
      ids.emplace_back(in.repeat_id, in.outer_fold_id);
    }
    std::sort(ids.begin(), ids.end());
    for (int f = 1; f < n_folds; ++f) {
      if (ids[f] == ids[f - 1]) {
        stop("Duplicate (repeat_id, outer_fold_id) = (%d, %d) in fold_specs.",
             ids[f].first, ids[f].second);
      }
    }
  }

  std::vector<NativeOuterFoldResult> results(n_folds);
  const int* y_ptr = &y[0];

  OuterFoldCompleteWorker worker(x_ptr, y_ptr, n_full, n_cols, inputs, results);
  if (num_threads == 1) {
    worker(0, (std::size_t)n_folds);
  } else {
    RcppParallel::parallelFor(0, (std::size_t)n_folds, worker, 1, num_threads);
  }

  // Deterministic ordering by (repeat_id, outer_fold_id) regardless of task completion order
  std::stable_sort(results.begin(), results.end(),
    [](const NativeOuterFoldResult& a, const NativeOuterFoldResult& b) {
      if (a.repeat_id != b.repeat_id) return a.repeat_id < b.repeat_id;
      return a.outer_fold_id < b.outer_fold_id;
    }
  );

  // Deterministic failure selection by (repeat_id, outer_fold_id)
  for (const auto& res : results) {
    if (res.status_code != 0 || !res.completed) {
      stop("Native outer fold failed at %s (repeat_id=%d, outer_fold_id=%d, status=%d): %s",
           res.outer_fold.c_str(), res.repeat_id, res.outer_fold_id,
           res.status_code, res.status_message.c_str());
    }
  }

  List out(n_folds);
  for (int f = 0; f < n_folds; ++f) {
    out[f] = native_outer_fold_result_to_r_list(results[f]);
  }
  return out;
}

// Test-only in-memory plain-representation round-trip (no disk persistence / checkpointing)
// [[Rcpp::export]]
List native_outer_fold_result_roundtrip_cpp(List r_result) {
  NativeOuterFoldResult decoded = r_list_to_native_outer_fold_result(r_result);
  return native_outer_fold_result_to_r_list(decoded);
}
