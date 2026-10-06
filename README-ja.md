[English README](README.md) | [日本語詳細リファレンス](docs/reference-ja.md)

# NCVROC 0.23.0

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores（項目セット得点の組み合わせROC選択のためのネスト交差検証）

NCVROC は、項目の組み合わせ選択、Receiver Operating Characteristic (ROC) 曲線評価、通常およびネスト交差検証（Nested CV）、ならびにモデル選択に伴う楽観度（Selection Optimism）の評価を通じて、短い項目ベースのスクリーニング尺度を開発するための R パッケージです。心理・臨床質問紙データにおいて、単純な非重み付け合計得点を用いて二値アウトカムを最もよく予測する項目の小サブセットを厳密に特定します。

合計得点が高いほど陽性アウトカムの確率が高いと仮定します。必要に応じて事前に項目を逆転処理してください。

---

## NCVROC 0.23.0 の主要新機能

- **ネスト探索向け Native TBB オプション**: `nested_sum_roc()` で `parallel = "native_tbb"` を指定し、外側候補探索を native C++/TBB スレッドで実行できます。これはユーザーによる明示的な opt-in であり、自動実行プランナーはこの backend を選択しません。
- **安全要件**: Native TBB では `engine = "Rcpp"`、`tuning = "off"`、`threads_per_worker = 1` が必要です。要求スレッド数は `n_workers` で指定し、利用可能な CPU 予算、外側 fold 数、`_R_CHECK_LIMIT_CORES_` に応じて上限が適用されます。現在のプロトタイプでは候補空間は最大 2,000,000 組です。
- **結果と RNG の不変性**: 対応するスレッド数の間で候補順位、同点処理、統計結果、および呼び出し元の RNG 状態を維持します。Native TBB は候補数のパーセントや ETA を表示せず、処理が正常終了した場合にのみ完了を通知します。

自動実行プランナーは変更されておらず、`native_tbb` を選択しません。

---

## NCVROC 0.21.0 の主要新機能

- **臨床動作点カットオフ選択エンジン**:
  - `cross_size_cv()` および `cross_size_nested_cv()` に `"sensitivity_target"`（感度目標達成下での特異度最大化）と `"clinical_constraint"`（感度・特異度の同時制約下での Youden 指数最大化）の 2 つの新カットオフ選択規則を追加。
  - 契約 C1 を厳格に適用（`sensitivity_target` では `sensitivity_min` 必須・`specificity_min` 禁止、`clinical_constraint` では両方必須）。
  - 従来手法（`"youden"`, `"closest_topleft"`）の後方互換性を 100% 維持。
- **全データへの実運用カットオフ再推定**:
  - 最終選抜モデルに対して、同一のカットオフ規則・臨床制約を用いて全データ上で実運用カットオフを再推定。
  - 状態値（`"selected"`, `"no_feasible_cutoff_on_full_data"`, `"no_feasible_candidate"`）の分離。
- **ネスト交差検証の 16 列標準化テーブル**:
  - `outer_fold_results` を 16 列の標準スキーマに統一し、選抜不能フォールドには型付き NA を安全に設定。

---

## NCVROC 0.20.0 の主要新機能

- **実行プレビュー公開 API (`plan_ncvroc_execution()`)**:
  - 解析実行前に総組み合わせ数・計算負荷を算出し、利用可能な全並列リソース構成のスケーリングを事前計測して専用の S3 オブジェクト（`"ncvroc_execution_plan"`）を返します。
  - 専用 S3 メソッド（`print()`, `format()`）および base R による診断プロット `plot(plan, type = c("runtime", "speedup", "efficiency", "all"))` を提供。
- **セットアップ考慮アフィン実行時間推定 ($T(n) = a + b \cdot n$)**:
  - 固定のクラスタ起動・データ転送オーバーヘッド ($a$) と候補数依存のスループット ($b$) を分離してモデル化。PSOCK の起動時間が膨大な組み合わせ数によって過大外挿されることを防止します（適合不能または不安定な場合は保守的な線形推定へ安全にフォールバック）。
- **2段階ベンチマークトリガーゲート**:
  - **180秒プライマリゲート**: `tuning = "auto"` モード時、推定シリアル実行時間が 180 秒（3 分）以上の場合にのみ全並列構成ベンチマークを実行。短時間処理ではオーバーヘッドをゼロにして即座に実行します。
  - **5% 許容オーバーヘッドゲート**: ベンチマーク試行にかかる総時間を推定本番時間の 5% 以内に制限。予算不足時は安全に手動・デフォルト構成へフォールバックします。
- **全候補リソーススイープ（フラット探索・CV ワークフロー）**:
  - `exhaustive_sum_roc()` および `cross_size_cv()` において、CPU 上限までの全整数並列レベル（スレッド・ソケットチャンク）を実測し、最良から 5% 以内の最高リソース効率構成を選択します。
- **ネスト交差検証における安全設計と制限の明記**:
  - ネスト交差検証系（`nested_sum_roc()`, `cross_size_nested_cv()`）では、対応している範囲で安全な候補数有界プローブを実行します。
  - 現在のエバリュエータがランク有界候補サブセットを安全に評価できない場合は、全並列スイープを実行せず手動・デフォルト構成を維持し、メタデータに理由を記録します（完全なネスト全リソーススイープは v0.20.0 へ計画的に延期）。
- **長時間の処理可観測性と進捗表示 UX**:
  - **厳密な完了候補数表示**: コンパイル済み C++ パスにおいて、バッチ境界で厳密な完了候補数（例: `25,000 / 100,000 complete (25.0%)`）を正確に報告。
  - **実測ベース ETA**: 完了したバッチの実測値のみから近似残り時間を安定更新。
  - **PSOCK 境界の誠実な通知**: 内部状況が不透明な PSOCK ソケット処理では開始・完了メッセージのみを出力し（`progress_unit = "none"`, `progress_mode = "start_completion"`）、未検証のパーセントや架空のハートビートを表示しません。
  - **完全沈黙契約**: `progress = FALSE` は進捗表示およびコールバック通知を無効化し、`verbose = FALSE` は詳細メッセージを抑制します（完全なコンソール沈黙には `progress = FALSE` と `verbose = FALSE` の両方が必要です）。
- **統一実行メタデータ**:
  - 結果オブジェクトの `execution_plan` メタデータに `$progress_mode`, `$progress_unit`, `$benchmark_table`, `$saturation_summary` を格納。

---

## NCVROC 0.18.0 の主要新機能

- **自動実行計画基盤 (`tuning = c("off", "auto", "always")`)**:
  - 決定論的パイロットマイクロベンチマークに基づき、測定された最良近似実行構成（シリアル、C++ マルチスレッド、PSOCK ワーカ）を自動選択。
  - **`tuning = "off"`**: ユーザ指定の手動並列構成を維持し、プランナーオーバーヘッドをゼロに抑制。
  - **`tuning = "auto"`**: 推定シリアル実行時間が閾値を超えた場合のみ候補構成をベンチマーク。
  - **`tuning = "always"`**: 非自明な全ワークロードで候補構成を測定。
- **決定論的パイロットベンチマーク**:
  - 全サンプルサイズ $N$、クラス比率、フォールド・リピート構造を完全に保ったまま、等間隔の候補サブセットを用いてスループットを実測。
- **リソース効率重視の選抜ルール**:
  - 最速の実測構成を基準とし、5% 近傍枠（`median_elapsed <= fastest * 1.05`）内の候補から最小リソース割り当て（`resource_count`）およびバックエンド優先順位（`none` > `threads` > `outer` > `chunks` > `hybrid`）を適用。
- **統計的同一性と RNG 不変性の厳密な保証**:
  - 実行計画機能は実行戦略のみを変更し、候補空間、フォールド分割、カットオフ算出、モデル順位、臨床制約、OOF 予測値、最終モデル適合、および `.Random.seed` に一切影響を与えません。

---

## インストール

```r
# GitHub から NCVROC をインストール
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

---

## 統合交差検証・モデル選抜 API 体系

NCVROC は、実行プレビュー、固定モデル評価、単一サイズ選抜、複数サイズ選抜、ネスト交差検証、およびモデル選択楽観度評価のための包括的な関数群を提供します：

| 解析目的 | $K$ 分割交差検証（$K$-fold CV） | 1例抜き交差検証（LOOCV） |
| :--- | :--- | :--- |
| **実行計画・スケーリング事前確認** | `plan_ncvroc_execution()` | — |
| **固定モデル評価**（選抜なし） | `cv_sum_roc()` | `loocv_sum_roc()` |
| **単一サイズモデル選抜**（項目数 $K$） | `cv_select_sum_roc()` | `loocv_select_sum_roc()` |
| **複数サイズ通常モデル選抜** | `cross_size_cv()` | `cross_size_loocv()` |
| **選抜手続きの汎化検証**（ネスト CV） | `cross_size_nested_cv()` | — *(非対応)* |
| **通常 CV vs. ネスト CV 比較**（選択楽観度） | `compare_cv_selection()` | *(通常 LOOCV vs. ネスト $K$-fold)* |
| **候補安定性・楽観度監査** | `candidate_stability_roc()` *(繰り返し $K$-fold & Bootstrap)* | — |
| **全自動一括解析**（単一呼び出し） | `ncvroc()` *(ネスト CV + 全データ最終全探索)* | — |
| **全組み合わせ網羅的 ROC 探索** | `exhaustive_sum_roc()` / `roc_bruteforce()` | — |

---

## 実行計画・プレビュー・チューニング

大規模な組み合わせ探索では、候補数が数万〜数十万通りに達します。最適な並列構成はデータセット規模、フォールド数、利用可能コア数、OS オーバーヘッドによって異なります。

### 1. スケーリングの事前確認 (`plan_ncvroc_execution`)

長時間解析を実行する前に、`plan_ncvroc_execution()` を用いて候補数、各並列構成の実測時間、スケーリング曲線を確認できます：

```r
library(NCVROC)

# 1〜4項目モデルの実行スケーリングを事前確認
plan <- plan_ncvroc_execution(
  data        = analysis_dat,
  outcome     = y,
  items       = paste0("Q", 1:10),
  workflow    = "cross_size_cv",
  model_sizes = 1:4,
  folds       = 5
)

# 計画サマリーとベンチマーク表を表示
print(plan)

# 実行時間・高速化比・並列効率曲線をプロット
plot(plan, type = "all")
```

### 2. チューニングモードと選抜ルール

`tuning` パラメータを通じて自動実行計画を制御します：

```r
# 1. 手動モード（オーバーヘッドなし）
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "off")

# 2. 自動計画（180秒プライマリゲート + 5% 予算管理）
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "auto")

# 3. 強制ベンチマーク（常に全構成を測定）
fit <- cross_size_cv(data = d, outcome = y, items = Q1:Q10, model_sizes = 1:4, tuning = "always")
```

ベンチマーク実行時、NCVROC は以下の決定論的階層ルールにより最良近似構成を選択します：
1. **最速の実測時間**: ベンチマークの中央値が最小となる構成 (`T_min`) を特定します。
2. **5% near-best 範囲**: `median_elapsed <= T_min * 1.05` を満たす候補プランを near-best として扱います。
3. **リソース効率**: 使用する CPU コア数／worker 数が最も少ないプラン (`min(resource_count)`) を優先します。
4. **バックエンド優先順位**:
   - フラット探索: `none`（シリアル） > `threads`（C++ スレッド） > `chunks`（PSOCK ワーカ）。
   - ネスト CV: `none` > `threads` > `outer`（PSOCK 外側フォールド） > `chunks` > `hybrid`。
5. **タイブレーク**: `median_elapsed` の小ささ、次いで `plan_id` の辞書順。

---

## 並列計算

NCVROC は 4 種類の一般的な並列実行モードと、ネスト探索専用の Native TBB オプションを提供します。`parallel`（手動実行モード）と `tuning`（自動計画）は独立したパラメータです。`parallel = "native_tbb"` は `nested_sum_roc()` 専用の明示的 opt-in であり、自動プランナーでは選択されません：

| モード | `parallel` | 特徴 | 推奨用途 |
|---|---|---|---|
| **C++ マルチスレッド** | `"threads"` | `RcppParallel` を介してメイン R プロセス内で高速共有メモリ並列処理。ソケット起動オーバーヘッドゼロ。 | 高速インメモリ探索（`exhaustive_sum_roc()`, `cross_size_cv()`）。 |
| **外側フォールド並列** | `"outer"`（または `TRUE`） | PSOCK ワーカプロセスを用いて外側交差検証フォールドを並列化。 | 標準的なネスト交差検証（`nested_sum_roc()`, `cross_size_nested_cv()`, `ncvroc()`）。 |
| **ハイブリッド並列** | `"hybrid"` | PSOCK ワーカで外側フォールドを、C++ スレッドでフォールド内探索を多重並列化。 | 多数コア環境での大規模ネスト交差検証。 |
| **チャンク並列** | `"chunks"`（または `TRUE`） | PSOCK ワーカプロセスにチャンク単位で処理を分散。 | ディスク保存を併用する超大規模探索。 |
| **Native TBB 外側探索** | `"native_tbb"` | `nested_sum_roc()` の外側候補探索を native C++/TBB スレッドで実行。`engine = "Rcpp"`、`tuning = "off"`、候補数 2,000,000 以下が必要です。`n_workers` で要求スレッド数を指定し、安全な上限が適用されます。 | 対応するネスト探索での手動 opt-in。 |

```r
# nested_sum_roc() で Native TBB を明示的に opt-in（自動プランナーでは使用されません）
res <- nested_sum_roc(data = d, outcome = y, items = Q1:Q10,
                      min_items = 1, max_items = 3, outer_k = 5, inner_k = 4,
                      engine = "Rcpp", parallel = "native_tbb", n_workers = 4,
                      tuning = "off")
```

---

## 進捗表示と可観測性

`progress` パラメータにより実行中の進捗通知を制御します：

- **厳密な完了件数表示**: コンパイル済みループでは、バッチ境界ごとに厳密な完了候補数（例: `Evaluating 25,000 / 100,000 combinations (25.0%)`）を表示。
- **実測ベース ETA**: 完了バッチの実測速度のみから安定した推定残り時間を算出。
- **PSOCK 境界での誠実な通知**: ソケット通信中は開始・完了メッセージのみを出力し、不正確なパーセント表示を回避。
- **沈黙契約**: `progress = FALSE` は進捗表示およびコールバック通知を無効化し、`verbose = FALSE` は詳細メッセージを抑制します（完全なコンソール沈黙には `progress = FALSE` と `verbose = FALSE` の両方が必要です）。

---

## 簡潔な項目数指定構文 (`item_count`)

`item_count` 引数を用いて、`min_items` と `max_items` を簡潔に指定できます：

| 指定構文 | 意味 | 同等のパラメータ |
|---|---|---|
| `item_count = "<=4"` | 1〜4項目の全尺度 | `min_items = 1, max_items = 4` |
| `item_count = "==3"` | ちょうど3項目の尺度 | `min_items = 3, max_items = 3`（または `item_count = 3`） |
| `item_count = "2:4"` | 2〜4項目の全尺度 | `min_items = 2, max_items = 4` |

---

## 結果の保存とキャッシュ

大規模探索では、`ncvroc()` および `roc_bruteforce()` がメモリ管理と再実行高速化のための機能を提供します：

- **`results_storage = c("auto", "memory", "rds", "none")`**: `"auto"` では、小規模探索はメモリ内に保持し、100,000 通りを超える大規模探索はチャンク RDS ファイルとしてディスクにアトミック保存します。
- **`cache = c("off", "reuse", "refresh")`**: `"reuse"` モードでは、データ・項目・探索設定のハッシュ値が完全に一致する場合、キャッシュされた結果を即座に再利用します。

---

## 使用例

### 1. 実行プレビューとスケーリング確認 (`plan_ncvroc_execution`)

```r
library(NCVROC)

# サンプル質問紙データ生成
set.seed(42)
n <- 120
analysis_dat <- data.frame(
  matrix(rbinom(n * 10, 1, 0.4), nrow = n, ncol = 10),
  y = rbinom(n, 1, 0.5)
)
names(analysis_dat)[1:10] <- paste0("Q", 1:10)

# 実行計画とスケーリング指標を事前確認
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

### 2. 固定モデルの交差検証 (`cv_sum_roc`)

```r
# 固定尺度 Q1 + Q2 + Q3 の 5 分割 CV
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

### 3. 複数サイズ通常モデル選抜 (`cross_size_cv`)

```r
# 1〜4項目モデルを対象に 5 分割 CV で最良モデルを選抜
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

### 4. ネスト交差検証による汎化性能評価 (`cross_size_nested_cv`)

```r
# 1〜3項目モデルの選抜手続き全体の汎化性能を評価
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

### 5. 通常選抜 vs. ネスト検証の比較と楽観度評価 (`compare_cv_selection`)

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

### 6. 候補安定性・楽観度監査 (`candidate_stability_roc`)

```r
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

### 7. 全自動一括解析 (`ncvroc`)

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

### 8. 臨床制約による候補モデル絞り込み (`ncvroc_results`)

```r
# 感度 >= 0.70、特異度 >= 0.30 を満たす候補モデルを抽出
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

## 基本前提

1. **得点が高いほど陽性になりやすい:** 必要に応じて事前に項目を逆転処理してください。
2. **カットオフ判定ルール:** `predicted_positive = score >= cutoff`。
3. **タイ（同点）を含む場合の AUC 計算:** $\text{AUC} = P(\text{pos} > \text{neg}) + 0.5 \times P(\text{pos} == \text{neg})$。
4. **欠損値の扱い:** 空文字列および空白のみの値は欠損値として扱われます。アウトカムまたは選択項目に欠損値を含む行は解析前に除去されます。
5. **厳密な二値アウトカム:** アウトカム列には `positive_label` と `negative_label` の値のみが含まれている必要があります。

---

## 参考文献

- Fawcett, T. (2006). An introduction to ROC analysis. *Pattern Recognition Letters*, 27(8), 861–874. [doi:10.1016/j.patrec.2005.10.010](https://doi.org/10.1016/j.patrec.2005.10.010)
- Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91. [doi:10.1186/1471-2105-7-91](https://doi.org/10.1186/1471-2105-7-91)
- Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer*, 3(1), 32–35.
