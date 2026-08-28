[English README](../README.md) | [日本語 README](../README-ja.md)

# NCVROC 0.19.0 詳細技術リファレンス

> [!TIP]
> **入門ガイドや基本的な使い方の概要をお探しの場合は、[日本語 README](../README-ja.md) をご覧ください。** 本ドキュメントは、NCVROC の全公開関数、パラメータ、実行計画・進捗可観測性仕様、返り値の構造を記載した詳細技術リファレンスです。

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores（項目セット得点の組み合わせROC選択のためのネスト交差検証）

NCVROC は、項目の組み合わせ選択、ROCに基づく評価、通常・ネスト交差検証、およびモデル選択に伴う楽観度（Selection Optimism）の評価を通じて、短い項目ベースのスクリーニング尺度を開発するための R パッケージです。心理・臨床質問紙データにおいて、単純な非重み付け合計得点を用いて二値アウトカムを最もよく予測する項目の小サブセットを特定します。

---

## 目次

1. [主要な新機能 (v0.19.0 / v0.18.0)](#主要な新機能)
2. [基本的前提と数学的定義](#基本的前提と数学的定義)
3. [公開 API 体系](#公開-api-体系)
4. [実行計画・プレビュー・チューニング仕様](#実行計画プレビューチューニング仕様)
5. [並列計算と過剰並列化防止](#並列計算と過剰並列化防止)
6. [進捗通知と可観測性](#進捗通知と可観測性)
7. [各関数の詳細仕様](#各関数の詳細仕様)
   - [`plan_ncvroc_execution()`](#plan_ncvroc_execution)
   - [`ncvroc()`](#ncvroc)
   - [`cross_size_cv()`](#cross_size_cv)
   - [`cross_size_nested_cv()`](#cross_size_nested_cv)
   - [`compare_cv_selection()`](#compare_cv_selection)
   - [`candidate_stability_roc()`](#candidate_stability_roc)
   - [`cv_sum_roc()` / `loocv_sum_roc()`](#cv_sum_roc--loocv_sum_roc)
   - [`cv_select_sum_roc()` / `loocv_select_sum_roc()`](#cv_select_sum_roc--loocv_select_sum_roc)
   - [`cross_size_loocv()`](#cross_size_loocv)
   - [`exhaustive_sum_roc()` / `roc_bruteforce()`](#exhaustive_sum_roc--roc_bruteforce)
   - [`ncvroc_results()`](#ncvroc_results)
8. [参考文献](#参考文献)

---

## 主要な新機能

### NCVROC 0.19.0
- **公開実行プレビュー API (`plan_ncvroc_execution()`)**: 解析実行前に候補数と並列スケーリング曲線を測定・可視化（`plot(plan, type = "all")`）。
- **アフィン実行時間推定 ($T(n) = a + b \cdot n$)**: クラスタ起動コスト $a$ と候補数スループット $b$ を分離し、巨大候補数での過大推定を防止。
- **2段階ベンチマークゲート**: 180 秒（3 分）の推定シリアル実行時間ゲートと、5% 許容ベンチマークオーバーヘッドゲートを導入。
- **全候補リソーススイープ（フラット / CV）**: `exhaustive_sum_roc()` および `cross_size_cv()` において全整数並列レベルを測定し、最良から 5% 以内の最高リソース効率構成を選択。
- **ネスト交差検証の安全設計（制限事項）**: ネスト交差検証系では候補数有界プローブのみを実行。全並列スイープは安全のため実行せず、手動/デフォルト構成を維持（完全なネスト全リソーススイープは v0.20.0 へ延期）。
- **進捗表示の可観測性向上**: コンパイル済み C++ ループにおけるバッチ境界での厳密な完了候補数表示、実測ベースの単調更新 ETA、PSOCK 境界での開始・完了メッセージ通知、`progress = FALSE` 完全沈黙保証。

### NCVROC 0.18.0
- **自動実行計画基盤 (`tuning = c("off", "auto", "always")`)**: 決定論的パイロット測定に基づき、シリアル・C++ スレッド・PSOCK ワーカから最良近似構成を自動選定。
- **厳密な非統計的保証**: 実行計画機能は実行戦略のみを決定し、統計候補空間、分割、順位、カットオフ、制約、予測値、最終適合、および `.Random.seed` に一切影響を与えません。

---

## 基本的前提と数学的定義

1. **高得点 = 陽性判定:** 合計得点が高いほど陽性アウトカムの確率が高いと仮定します。逆転項目は事前に反転させてください。
2. **カットオフ判定ルール:** `predicted_positive = score >= cutoff`。
3. **タイ（同点）を含む場合の AUC:** $\text{AUC} = P(\text{pos} > \text{neg}) + 0.5 \times P(\text{pos} == \text{neg})$。
4. **欠損値の扱い:** 空文字列・空白のみの値は欠損として扱い、解析前に完全ケース抽出（complete cases）を行います。
5. **厳密な二値アウトカム:** アウトカム列には `positive_label`（デフォルト: 1）と `negative_label`（デフォルト: 0）のみが含まれる必要があります。

---

## 公開 API 体系

| 分析目標 | $K$ 分割交差検証 | 1例抜き交差検証（LOOCV） |
| :--- | :--- | :--- |
| **実行計画・スケーリング事前確認** | `plan_ncvroc_execution()` | — |
| **固定モデル評価**（選抜なし） | `cv_sum_roc()` | `loocv_sum_roc()` |
| **単一サイズモデル選抜**（項目数 $K$） | `cv_select_sum_roc()` | `loocv_select_sum_roc()` |
| **複数サイズ通常モデル選抜** | `cross_size_cv()` | `cross_size_loocv()` |
| **選抜手続きの汎化検証**（ネスト CV） | `cross_size_nested_cv()` | — *(非対応)* |
| **通常 CV vs. ネスト CV 比較**（選択楽観度） | `compare_cv_selection()` | *(通常 LOOCV vs. ネスト $K$-fold)* |
| **候補安定性・楽観度監査** | `candidate_stability_roc()` *(繰り返し $K$-fold & Bootstrap)* | — |
| **全自動一括解析** | `ncvroc()` *(ネスト CV + 全データ最終全探索)* | — |
| **全組み合わせ網羅的 ROC 探索** | `exhaustive_sum_roc()` / `roc_bruteforce()` | — |

---

## 実行計画・プレビュー・チューニング仕様

### 1. チューニングモード (`tuning`)
- `"off"`: プランナーを実行せず、ユーザ指定の手動設定（`parallel`, `n_workers`, `threads_per_worker`）をそのまま使用。
- `"auto"`: 推定シリアル実行時間が 180 秒以上の場合にのみ全並列構成スイープを実行。短時間処理ではオーバーヘッドを完全に回避。
- `"always"`: ワークロードの規模にかかわらず、常に候補並列構成を測定。

### 2. 決定論的選抜ルール
1. **最速の実測時間**: ベンチマークの中央値が最小となる構成 (`T_min`) を特定します。
2. **5% near-best 範囲**: `median_elapsed <= T_min * 1.05` を満たす候補プランを near-best として扱います。
3. **リソース効率**: 使用する CPU コア数／worker 数が最も少ないプラン (`min(resource_count)`) を優先します。
4. **バックエンド優先順位**:
   - フラット探索: `none` > `threads` > `chunks`
   - ネスト CV: `none` > `threads` > `outer` > `chunks` > `hybrid`
5. **タイブレーク**: `median_elapsed` の小ささ、次いで `plan_id` の辞書順。

---

## 各関数の詳細仕様

### `plan_ncvroc_execution()`

解析の実行前に、総組み合わせ数と各並列構成のスケーリング特性を事前測定・評価します。

```r
plan_ncvroc_execution(
  data,
  outcome,
  items = NULL,
  workflow = c("cross_size_cv", "exhaustive", "nested_sum_roc", "cross_size_nested_cv"),
  model_sizes = NULL,
  min_items = 1,
  max_items = 4,
  cv_method = c("kfold", "loocv"),
  folds = 5,
  repeats = 1,
  outer_folds = 5,
  inner_folds = 4,
  outer_repeats = 1,
  inner_repeats = 1,
  selection_metric = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  cutoff_method = c("youden", "closest_topleft"),
  sensitivity_min = NULL,
  specificity_min = NULL,
  engine = c("Rcpp", "R"),
  max_resources = NULL,
  positive_label = 1,
  negative_label = 0,
  ...
)
```

- **返り値**: S3 クラス `"ncvroc_execution_plan"`。
- **メソッド**: `print()`, `format()`, `plot(x, type = c("runtime", "speedup", "efficiency", "all"))`。

---

### `ncvroc()`

ネスト交差検証および全データ最終全探索を一括実行するメインエントリーポイント。

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
  n_workers         = NULL,
  progress          = interactive(),
  ...
)
```

---

### `cross_size_cv()`

複数項目数にわたる組み合わせ空間全体から、通常の交差検証（non-nested CV）により最良モデルを探索・選抜します。

```r
cross_size_cv(
  data,
  outcome,
  items,
  model_sizes      = NULL,
  min_items        = 1,
  max_items        = 4,
  item_count       = NULL,
  cv_method        = c("kfold", "loocv"),
  folds            = 5,
  repeats          = 1,
  stratified       = TRUE,
  selection_metric = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  cutoff_method    = c("youden", "closest_topleft"),
  sensitivity_min  = NULL,
  specificity_min  = NULL,
  top_n            = 20,
  prefer_fewer_items = TRUE,
  positive_label   = 1,
  negative_label   = 0,
  engine           = c("Rcpp", "R"),
  parallel         = "none",
  n_workers        = NULL,
  tuning           = c("off", "auto", "always"),
  ci               = FALSE,
  conf_level       = 0.95,
  seed             = NULL,
  progress         = interactive()
)
```

---

### `cross_size_nested_cv()`

複数項目数にわたるモデル探索・選抜手続き全体の汎化性能を独立した外側テストフォールドで評価するネスト交差検証を実行します。

```r
cross_size_nested_cv(
  data,
  outcome,
  items,
  model_sizes      = NULL,
  min_items        = 1,
  max_items        = 4,
  item_count       = NULL,
  outer_folds      = 5,
  inner_folds      = 4,
  outer_repeats    = 5,
  inner_repeats    = 1,
  selection_metric = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  cutoff_method    = c("youden", "closest_topleft"),
  sensitivity_min  = NULL,
  specificity_min  = NULL,
  prefer_fewer_items = TRUE,
  stratified       = TRUE,
  positive_label   = 1,
  negative_label   = 0,
  engine           = c("Rcpp", "R"),
  parallel         = "none",
  n_workers        = NULL,
  threads_per_worker = 1L,
  tuning           = "off",
  seed             = NULL,
  progress         = interactive(),
  ...
)
```

---

### `compare_cv_selection()`

通常の交差検証で選抜されたモデルの見かけの性能と、ネスト交差検証による選抜手続き全体の汎化性能を直接比較し、モデル選択に伴う楽観度（`selection_optimism = ordinary - nested`）を定量化します。

```r
compare_cv_selection(
  data,
  outcome,
  items,
  model_sizes      = NULL,
  min_items        = 1,
  max_items        = 4,
  item_count       = NULL,
  folds            = 5,
  repeats          = 1,
  outer_folds      = 5,
  inner_folds      = 4,
  outer_repeats    = 5,
  inner_repeats    = 1,
  selection_metric = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  cutoff_method    = c("youden", "closest_topleft"),
  sensitivity_min  = NULL,
  specificity_min  = NULL,
  top_n            = 20,
  prefer_fewer_items = TRUE,
  stratified       = TRUE,
  positive_label   = 1,
  negative_label   = 0,
  engine           = c("Rcpp", "R"),
  parallel         = "none",
  n_workers        = NULL,
  threads_per_worker = 1L,
  seed             = NULL,
  progress         = interactive()
)
```

---

### `candidate_stability_roc()`

データ摂動（繰り返し交差検証またはノンパラメトリックブートストラップ）を通じて、特定候補モデルの順位安定性、選抜頻度、臨床制約通過率、およびブートストラップ楽観度補正を包括的に監査します。

```r
candidate_stability_roc(
  data,
  outcome,
  candidate_sets   = NULL,
  items            = NULL,
  model_sizes      = 1:5,
  screen_top_n     = 200L,
  resampling       = c("repeated_cv", "bootstrap"),
  folds            = 5L,
  repeats          = 20L,
  bootstrap_reps   = 500L,
  bootstrap_test   = c("original", "oob"),
  cutoff_method    = c("youden", "closest_topleft"),
  sensitivity_min  = NULL,
  specificity_min  = NULL,
  rank_by          = c("youden", "auc", "sensitivity", "specificity", "accuracy"),
  prefer_fewer_items = TRUE,
  positive_label   = 1,
  negative_label   = 0,
  parallel         = c("none", "threads", "chunks"),
  n_workers        = NULL,
  seed             = NULL,
  engine           = c("Rcpp", "R"),
  progress         = interactive()
)
```

---

### `ncvroc_results()`

`ncvroc_analysis` または `roc_bruteforce_result` の候補モデル一覧を、臨床制約（感度・特異度など）に基づき動的に絞り込み・再順位付けします。

```r
ncvroc_results(
  x,
  sensitivity     = NULL,
  specificity     = NULL,
  auc             = NULL,
  youden          = NULL,
  accuracy        = NULL,
  ppv             = NULL,
  npv             = NULL,
  n_items         = NULL,
  cutoff          = NULL,
  rank_by         = c("youden", "auc", "sensitivity", "specificity", "accuracy", "ppv", "npv"),
  top_n           = 20,
  allow_full_load = FALSE
)
```

---

## 参考文献

- Fawcett, T. (2006). An introduction to ROC analysis. *Pattern Recognition Letters*, 27(8), 861–874. [doi:10.1016/j.patrec.2005.10.010](https://doi.org/10.1016/j.patrec.2005.10.010)
- Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91. [doi:10.1186/1471-2105-7-91](https://doi.org/10.1186/1471-2105-7-91)
- Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer*, 3(1), 32–35.
