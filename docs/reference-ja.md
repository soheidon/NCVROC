# NCVROC 0.9.0 リファレンス

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores（項目セット得点の組み合わせROC選択のためのネスト交差検証）

項目の組み合わせ選択、ROCに基づく評価、ネスト交差検証を通じて、短い項目ベースのスクリーニング尺度を開発します。心理・臨床質問紙データにおいて、単純な合計得点を用いて二値アウトカムを最もよく予測する項目の小サブセットを特定します。

合計得点が高いほど陽性アウトカムの確率が高いと仮定します。必要に応じて事前に項目を逆転処理してください。

---

## インストール

```r
# NCVROC を GitHub からインストール
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

## 基本的前提

1. **高得点 = 陽性の可能性が高い。** 必要に応じて事前に項目を逆転処理してください。
2. **カットオフルール:** `predicted_positive = score >= cutoff`。
3. **同点を考慮したAUC:** `AUC = P(pos > neg) + 0.5 * P(pos == neg)`。
4. **欠損値:** 空文字および空白のみの値は欠損として扱われます。アウトカム列または選択された項目列に欠損値を含む行は、分析前に除外されます。
5. **厳密な二値アウトカム。** アウトカム列には`positive_label`と`negative_label`の値のみが含まれている必要があります。

---

## 設定スタイル

`ncvroc()`には適切なデフォルト値が設定されています。ユーザーは短い呼び出しから始められます：

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  seed       = 20260705
)
```

`mode`は事前選択される候補セットのデフォルトサイズを制御します：

| mode           | preselect_top_n |
| -------------- | --------------: |
| `"quick"`      |             100 |
| `"balanced"`   |             500 |
| `"thorough"`   |            1000 |
| `"exhaustive"` |       全候補 |

その他の引数は、明示的に変更されない限りデフォルト値が維持されます。
例えば、次の例では計算エンジンのみを変更しています：

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  engine     = "R",
  seed       = 20260705
)
```

これは`mode = "balanced"`を使用しつつ`engine`のみを上書きしたのと同じです。
ユーザーは個別の設定を任意に上書きできます：

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  inner_repeats = 5,
  preselect_top_n = 1000,
  engine     = "Rcpp",
  seed       = 20260705
)
```

一般に、優先順位のルールは次のとおりです：

```text
デフォルト < モードに基づく推奨値 < 明示的に指定された引数
```

つまり、`mode = "balanced"`は`preselect_top_n = 500`を推奨しますが、明示的な`preselect_top_n`の値がその推奨を上書きします。

---

### 項目数指定

`item_count`引数は`min_items`/`max_items`の代わりに使用できる簡潔な記法です。`min_items`や`max_items`との併用はできません。

| `item_count` | 意味 |
|---|---|
| `"==4"` | ちょうど4項目 |
| `"<=4"` | 最大4項目（1〜4項目） |
| `"2:4"` | 2〜4項目 |

```r
# ちょうど4項目の尺度
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "==4",
  mode    = "balanced",
  seed    = 20260705
)

# 最大4項目の尺度（1〜4項目）
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "<=4",
  mode    = "balanced",
  seed    = 20260705
)

# 2〜4項目の尺度
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "2:4",
  mode    = "balanced",
  seed    = 20260705
)
```

`item_count`は`ncvroc()`、`roc_bruteforce()`（および`roc_bf()`）、`ncvroc_config()`で使用できます。

### 後方互換性

`min_items`と`max_items`は引き続きサポートされています。以下の表は、同等の旧記法と新記法を示しています：

| 旧（`min_items` / `max_items`） | 新（`item_count`） |
|---|---|
| `min_items = 4, max_items = 4` | `item_count = "==4"` |
| `min_items = 1, max_items = 4` | `item_count = "<=4"` |
| `min_items = 2, max_items = 4` | `item_count = "2:4"` |

低水準関数（`exhaustive_sum_roc()`、`nested_sum_roc()`、`fit_final_sum_scale()`、`count_item_combinations()`、`suggest_preselect_top_n()`）では、引き続き`min_items`と`max_items`を使用します。

---

### 結果の保存

`ncvroc()` と `roc_bruteforce()` は `results_storage` パラメータで完全な候補テーブルの保存方法を制御します。デフォルトの `"rds"` は、多数の項目に対する全探索で数十万行に及ぶ候補テーブルが生成され、メモリに保持すると数百MBを消費し続ける問題を回避するためのものです。RDSファイルに書き出すことでメモリを節約しつつ、`ncvroc_results()` で必要なときに全テーブルにアクセスできます。

| `results_storage` | 動作 |
|---|---|
| `"rds"`（デフォルト） | 完全な候補テーブルをRDSファイルに保存。デフォルトでは `getwd()` が返す現在のワーキングディレクトリに出力。RStudio ProjectやQuarto Projectでは通常プロジェクトルートになるが、必ずしもRmd/Qmdファイル自体が置かれているフォルダとは限らない。`results_dir = "path/"` で保存先を指定可能。`$final_exhaustive_ranked` は `NULL`。 |
| `"memory"` | 完全な候補テーブルをメモリに保持（v0.8.0以前の動作）。`$final_exhaustive_ranked` に data.frame が格納される。 |
| `"none"` | 完全な候補テーブルを破棄。`ncvroc_results()` はエラーになる。 |

`results_storage` が `"rds"` または `"memory"` の場合、全データの取得には `ncvroc_results()` を使用してください（RDSから透過的に読み込みます）：

```r
ncvroc_results(result, top_n = NULL)  # 全候補を取得
```

### 最終候補の出力

`ncvroc()`はデフォルトで最終全探索を実行し、ランク付けされた全データ候補テーブルをRDSファイルに保存します。

便宜上、以下がメモリに格納されます：

```r
result$final_candidates   # 上位N行（final_top_nで制御）
result$final_model        # 最良の単一モデル（先頭行）
result$final_n_combinations  # 評価された組み合わせの総数
result$final_results_storage # 保存モード（"rds"、"memory"、"none"）
result$final_exhaustive_file # RDSファイルのパス（"rds"モード時）
```

`selection_criterion`はネストCV中にどの候補が選択されるかを制御します。

`final_rank_by`は最終全データ候補テーブルのランク付け方法を制御します。

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  final_rank_by = "auc",
  final_top_n = 20,
  seed    = 20260705,
  save_results = TRUE
)

result$final_candidates
result$final_model
```

ランク付け基準を選択するには`final_rank_by`を使います：

```r
final_rank_by = "auc"          # デフォルト
final_rank_by = "youden"
final_rank_by = "sensitivity"
final_rank_by = "specificity"
final_rank_by = "accuracy"
```

モデルを選択する前に、`ncvroc_results()`を使って臨床的制約でランク付けテーブルを絞り込みます：

```r
ncvroc_results(
  result,
  sensitivity = ">= 0.90",
  specificity = ">= 0.85",
  rank_by = "youden",
  top_n = 20
)
```

条件には6つの演算子（`>=`, `>`, `<=`, `<`, `==`, `!=`）が使用でき、AND論理で組み合わされます。使用可能な列：`sensitivity`, `specificity`, `auc`, `youden`, `accuracy`, `ppv`, `npv`, `n_items`, `cutoff`。

---

## リファレンス

### `ncvroc()`

1回の呼び出しで完全なNCVROC分析を行う主要エントリポイント。ベースRスタイルの選択を用いてアウトカム列と項目列を解決し、データを準備し、ネストCVを実行し、オプションで最終全探索を実行し、オプションでCSV出力を保存します。

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
  results_storage   = c("rds", "memory", "none"),
  results_name      = NULL,
  results_dir       = NULL,
  save_results      = FALSE,
  output_dir        = ".",
  progress          = TRUE,
  verbose           = TRUE,
  return            = "full",
  item_count        = NULL
)
```

`outcome`はベアシンボル（`y`）または文字列（`"y"`）を受け付けます。
`items`はベア範囲（`Q1:Q5`）、`c()`によるベア名、文字ベクトル、既存変数、または数値位置を受け付けます。

`selection_criterion`はネストCV中にどの候補が選択されるかを制御します。
`final_rank_by`は最終全データ候補テーブルのランク付け方法を制御します。

**戻り値:** クラス`"ncvroc_analysis"`のS3オブジェクト。`print()`, `summary()`, `plot()`のS3メソッドが利用可能です。臨床的制約で最終候補テーブルを絞り込むには`ncvroc_results()`を使用してください。

---

### `ncvroc_results()`

`ncvroc_analysis` または `roc_bruteforce_result` オブジェクトから、臨床的または実用的な制約で候補モデルを絞り込み、ランク付けします。

```r
ncvroc_results(
  x,
  sensitivity  = NULL,
  specificity  = NULL,
  auc          = NULL,
  youden       = NULL,
  accuracy     = NULL,
  ppv          = NULL,
  npv          = NULL,
  n_items      = NULL,
  cutoff       = NULL,
  rank_by = c("youden", "auc", "sensitivity", "specificity", "accuracy", "ppv", "npv"),
  top_n  = 20
)
```

各条件は`">= 0.90"`や`"<= 3"`のような文字列です。6つの演算子（`>=`, `>`, `<=`, `<`, `==`, `!=`）がサポートされています。複数の条件はAND論理で組み合わされます。結果は`rank_by`でランク付けされ、安定したタイブレーカーが適用されます。すべての一致行を返すには`top_n = NULL`を、空のテーブルを返すには`0`を設定します。

**戻り値:** 絞り込まれランク付けされた候補モデルを含む data.frame。

`x` には次のいずれかを指定できます：

- `final_search = TRUE` で作成された `ncvroc_analysis` オブジェクト
- `roc_bruteforce()` または `roc_bf()` が返す `roc_bruteforce_result` オブジェクト

---

### `roc_bruteforce()`

NSEによる列解決を用いた、全データでの項目組み合わせROC分析。

```r
roc_bruteforce(
  data,
  outcome,
  items,
  min_items        = 1,
  max_items        = 4,
  cutoff_method    = c("youden", "closest_topleft"),
  positive_label   = 1,
  negative_label   = 0,
  engine           = c("Rcpp", "R"),
  rank_by          = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n            = 20,
  progress         = interactive(),
  save_results     = FALSE,
  output_dir       = ".",
  results_storage  = c("rds", "memory", "none"),
  results_name     = NULL,
  results_dir      = NULL,
  item_count       = NULL
)
```

**戻り値:** クラス `"roc_bruteforce_result"` のS3オブジェクト。`$candidates`（上位N件）、`$best_model`（先頭行）、`$results_storage`、`$results_file`、`$n_combinations` を含みます。デフォルトでは `$results` は `NULL` です（RDSに保存されます）。`print()` はパフォーマンスが楽観的である可能性の警告付きで整形されたサマリーを表示します。臨床的制約での絞り込みには `ncvroc_results()` を使用してください。

エイリアス `roc_bf()` は同じ引数を受け取り、同じ結果を返します。

---

### `ncvroc_config()`

すべての分析パラメータを単一の設定オブジェクトにバンドルします。分析スクリプトの冗長性を減らすために`run_ncvroc()`と共に使用します。

```r
ncvroc_config(
  outcome,
  items             = NULL,
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
  cutoff_method     = c("youden", "closest_topleft"),
  positive_label    = 1,
  negative_label    = 0,
  stratified        = TRUE,
  engine            = c("Rcpp", "R"),
  item_count        = NULL
)
```

`mode`はデフォルトの`preselect_top_n`を制御します：

| モード | 事前選択 | ユースケース |
|---|---|---|
| `"quick"` | 上位100 | 高速スクリーニング、探索 |
| `"balanced"` | 上位500（デフォルト） | 通常の分析 |
| `"thorough"` | 上位1000 | 網羅的探索 |
| `"exhaustive"` | 全候補 | 完全列挙（低速になる可能性あり） |

**戻り値:** クラス`"ncvroc_config"`のS3オブジェクト。`print()`は整形されたサマリーを表示し、`preselect_top_n >= 100,000`の場合に警告を出します。

---

### `run_ncvroc()`

`ncvroc_config`オブジェクトからすべてのパラメータを読み取る`nested_sum_roc()`の便利なラッパー。

```r
run_ncvroc(
  data,
  items,
  config,
  seed     = NULL,
  progress = TRUE,
  verbose  = TRUE,
  return   = c("full", "summary")
)
```

**戻り値:** `ncvroc_result`オブジェクト（`nested_sum_roc()`と同じ）。

---

### `nested_sum_roc()`

外側ループでパフォーマンス推定、内側ループでモデル選択を行うネスト交差検証。

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

**戻り値:** クラス`"ncvroc_result"`のS3オブジェクト。以下の要素を含みます：

| 要素 | 説明 |
|---|---|
| `summary` | data.frame: 外側foldごとに1行、AUC・感度・特異度などを含む |
| `outer_results` | list: 予測を含むfoldごとの完全な詳細 |
| `selected_models` | character: 各foldで選択された項目セット |
| `selected_model_frequency` | data.frame: 各項目セットの選択頻度 |
| `outer_predictions` | data.frame: スコア付きの全out-of-sample予測 |
| `settings` | list: すべての引数値 |

**S3メソッド:** `print()`, `summary()`, `plot(which = "selection"|"auc")`。

---

### `exhaustive_sum_roc()`

すべての項目の組み合わせを列挙し、単純合計得点を計算し、ROCで評価します。

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

**戻り値:** `rank`, `items`, `n_items`, `auc`, `cutoff`, `sensitivity`, `specificity`, `youden`, `accuracy`, `ppv`, `npv`, `n_positive`, `n_negative`の列を持つdata.frame。`rank_by`の降順でソートされます。

**パフォーマンスは見かけ上（インサンプル）のものであり、交差検証されていません。**

デフォルトは`engine = "R"`です。約7倍の高速化のために`engine = "Rcpp"`を使用してください。

---

### `fit_final_sum_scale()`

交差検証後に全データセットで最終尺度を適合させるための、`exhaustive_sum_roc()`の薄いラッパー。

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

**戻り値:** `attr(result, "performance_type") <- "apparent"`を持つdata.frame。これらはインサンプル推定値であり、交差検証されていません。検証済みのパフォーマンスには`nested_sum_roc()`を使用してください。

デフォルトは`engine = "R"`です。約7倍の高速化のために`engine = "Rcpp"`を使用してください。

---

### `make_stratified_folds()`

層化k分割交差検証のインデックスを作成します。

```r
make_stratified_folds(y, k = 5, repeats = 1, seed = NULL)
```

**戻り値:** 整数ベクトルの名前付きリスト。名前は`"Rep1_Fold1"`形式です。`k`が小さいクラスのサイズを超える場合、`k`は警告とともに縮小されます。

---

### `count_item_combinations()`

組み合わせを生成せずにk項目の組み合わせの総数をカウントします。

```r
count_item_combinations(
  items_or_n,
  min_items = 1,
  max_items = 4,
  detail    = FALSE
)
```

`items_or_n`は項目名の文字ベクトルまたは単一の整数nを受け付けます。  
`detail = TRUE`でkごとの内訳を持つdata.frameを返します。

---

### `suggest_preselect_top_n()`

総組み合わせ数と分析モードに基づいて実用的な`preselect_top_n`を提案します。

```r
suggest_preselect_top_n(
  items_or_n,
  min_items = 1,
  max_items = 4,
  mode      = c("balanced", "quick", "thorough", "exhaustive")
)
```

**戻り値:** 単一の数値。総組み合わせ数を上限とします。

---

## クイック例

```r
library(NCVROC)

set.seed(42)
d <- data.frame(
  y  = sample(0:1, 100, replace = TRUE),
  Q1 = sample(0:2, 100, replace = TRUE),
  Q2 = sample(0:2, 100, replace = TRUE),
  Q3 = sample(0:2, 100, replace = TRUE),
  Q4 = sample(0:2, 100, replace = TRUE),
  Q5 = sample(0:2, 100, replace = TRUE)
)

# ベースRスタイルの列選択による単一呼び出し分析
result <- ncvroc(d, y, Q1:Q5, item_count = "<=2", mode = "quick",
  outer_k = 3, inner_k = 2, outer_repeats = 1, engine = "R",
  seed = 42, final_search = FALSE)
print(result)
summary(result)
plot(result)
```

### 設定ワークフロー

```r
# 分析の意図を一度だけ定義
cfg <- ncvroc_config(
  outcome    = "y",
  items      = paste0("Q", 1:5),
  item_count = "<=2",
  mode       = "quick",
  engine     = "Rcpp"
)

print(cfg)

result <- run_ncvroc(d, paste0("Q", 1:5), cfg, seed = 42)
summary(result)
```

---

## 見かけ上のパフォーマンス vs ネストCVパフォーマンス

| 関数 | パフォーマンス | ユースケース |
|---|---|---|
| `ncvroc()` | ネスト交差検証済み | 単一呼び出しエントリポイント（推奨） |
| `roc_bruteforce()` | 見かけ上（インサンプル） | 全データ高速探索（NSE対応） |
| `exhaustive_sum_roc()` | 見かけ上（インサンプル） | 高速スクリーニング、探索 |
| `nested_sum_roc()` | ネスト交差検証済み | 検証済みパフォーマンス推定 |
| `run_ncvroc()` | ネスト交差検証済み | 便利なラッパー（設定駆動） |
| `fit_final_sum_scale()` | 見かけ上（インサンプル） | 全データでの最終尺度 |

---

## CVなしの全探索ROC検索

`roc_bruteforce()`（またはエイリアス `roc_bf()`）を使うと、ネスト交差検証なしで全データセット上で直接すべての項目組み合わせを評価できます。`ncvroc()` と同じNSEによる列解決を共有しています。

> パフォーマンスは項目とカットオフの選択に使ったのと同じデータで計算されます。これらの推定値は楽観的である可能性があります。ネスト交差検証済みのパフォーマンス推定には `ncvroc()` を使用してください。

```r
result <- roc_bruteforce(
  data       = d,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=3",
  rank_by    = "youden",
  engine     = "Rcpp",
  top_n      = 20
)

result
result$best_model
result$candidates

# 完全な候補テーブルを取得（デフォルトではRDSに保存）
ncvroc_results(result, top_n = NULL)
```

`ncvroc_results()` で `ncvroc()` の出力と同じように絞り込めます：

```r
ncvroc_results(result, sensitivity = ">= 0.90", specificity = ">= 0.85")
```

エイリアス `roc_bf()` は同等です：

```r
result <- roc_bf(d, y, Q1:Q5, item_count = "<=3", engine = "Rcpp")
```

## Rcppエンジン

`ncvroc()`, `roc_bruteforce()`, `exhaustive_sum_roc()`, `nested_sum_roc()`, `fit_final_sum_scale()` で `engine = "Rcpp"` を指定すると、ネイティブC++バックエンドを使用します。結果はRエンジンと数値的に同一で、中程度のワークロードで通常約7倍の高速化が得られます。

```r
exhaustive_sum_roc(d, "y", paste0("Q", 1:5), max_items = 2, engine = "Rcpp")
```

## ライセンス

MIT — [LICENSE](../LICENSE)を参照してください。
