[English README](README.md) | [日本語詳細リファレンス](docs/reference-ja.md)

# NCVROC 0.14.0

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores（項目セット得点の組み合わせROC選択のためのネスト交差検証）

項目の組み合わせ選択、ROCに基づく評価、ネスト交差検証を通じて、短い項目ベースのスクリーニング尺度を開発します。心理・臨床質問紙データにおいて、単純な合計得点を用いて二値アウトカムを最もよく予測する項目の小サブセットを特定します。

合計得点が高いほど陽性アウトカムの確率が高いと仮定します。必要に応じて事前に項目を逆転処理してください。

---

## NCVROC 0.14.0 の主要新機能

- **C++ 共有メモリマルチスレッド並列化 (`parallel = "threads"`)**: メイン R プロセス内で `RcppParallel` を介してネイティブ C++ スレッドにより組み合わせ探索空間を高速に並列評価。
- **ゼロソケットオーバーヘッド**: ソケットワーカプロセスの起動、IPC 通信、プロセス間データ複製を完全排除。
- **決定論的かつ厳密な解の一致**: すべての評価指標およびカットオフ選択手法において、シングルスレッドのシリアル実行と完全に同一の結果および順位付けを保証。
- **hybrid総並列度の明示的管理**: outer PSOCKワーカとC++スレッドを安全に併用し、総並列度を利用可能CPU数とCRAN制限内に自動調整。
- **高いクロスプラットフォーム移植性**: `RcppParallel` を採用し、Windows 環境でのクリーンコンパイルおよびテスト通過を検証済み。

---

## NCVROC 0.12.0 の主要新機能

- **チャンク単位の網羅探索並列化 (`parallel = "chunks"`)**: 数百万通りの組み合わせ探索空間（O(M choose K)）をチャンク単位で複数の CPU ソケットワーカー（`parallel::makePSOCKcluster`）に分散して並列評価。
- **並列化レベルの明確な分離**: 外側交差検証フォールド並列化（`"outer"`）とチャンク並列化（`"chunks"`）を明確に分離し、二重並列化（nested parallelism）による CPU・メモリの過負荷を防止。
- **C++ オンデマンド組み合わせ生成（Unranking）とストリーミング Top-N**: 100万件超の巨大な R list メモリ確保を完全に排除し、C++ 内部で数学的 unranking により候補を逐次評価。局所 Top-N 抽出により省メモリで高速な探索を実現。
- **PSOCK クラスタの永続再利用**: `nested_sum_roc(..., parallel = "chunks")` では、outer fold ループの開始時にクラスタを1回作成して全フォールドで再利用し、プロセス起動と DLL ロードのオーバーヘッドを排除。
- **アトミック RDS 保存とキャッシュ検証**: チャンクファイルは一時ファイル（`.tmp`）に書き込んだ後、同一ディレクトリ内でアトミックにリネーム。不完全なファイルがキャッシュとして認識されることを防止。
- **厳密な解の同一性（近似なし）**: 候補選抜や足切りを行わない完全な総当たり探索（exact exhaustive search）であり、1-based の `.global_combo_index` によりシリアル実行と完全に同一の順位・モデル選抜・統計数値を保証。
- **後方互換性の維持**: `parallel = TRUE` はネスト交差検証系では `"outer"`、全探索系では `"chunks"` として文脈に応じて解釈され、既存コードとの 100% 互換性を維持（デフォルトは `parallel = FALSE`）。

---

## インストール

```r
# NCVROC を GitHub からインストール
# install.packages("remotes")
remotes::install_github("soheidon/NCVROC")
```

## 基本前提

1. **得点が高いほど陽性になりやすい:** 必要に応じて事前に項目を逆転処理してください。
2. **カットオフ判定ルール:** `predicted_positive = score >= cutoff`。
3. **タイ（同点）を含む場合の AUC 計算:** `AUC = P(pos > neg) + 0.5 * P(pos == neg)`。
4. **欠損値の扱い:** 空文字列および空白のみの値は欠損値として扱われます。アウトカムまたは選択項目に欠損値を含む行は解析前に除去されます。
5. **厳密な二値アウトカム:** アウトカム列には `positive_label` と `negative_label` の値のみが含まれている必要があります。

---

## 設定スタイル

`ncvroc()` は使いやすいデフォルトを備えています。短いコードから開始できます：

```r
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  seed       = 20260705
)
```

`mode` は内部 CV で評価する候補セットの事前選抜数を制御します：

| mode | preselect_top_n |
|---|---|
| `"quick"` | 100 |
| `"balanced"` | 500 |
| `"thorough"` | 1000 |
| `"exhaustive"` | 全候補（全網羅） |

その他の引数は、明示的に指定しない限り独自のデフォルトを維持します。例えば、計算エンジンのみを変更する場合：

```r
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  engine     = "R",
  seed       = 20260705
)
```

これは `mode = "balanced"` を使いながら `engine` のみを上書きすることと同等です。個別の設定は柔軟に上書きできます：

```r
result <- ncvroc(
  data            = analysis_dat,
  outcome         = y,
  items           = Q1:Q5,
  item_count      = "<=4",
  mode            = "balanced",
  inner_repeats   = 5,
  preselect_top_n = 1000,
  engine          = "Rcpp",
  seed            = 20260705
)
```

一般的な優先順位ルールは以下の通りです：

```text
デフォルト値 < mode による推奨値 < 明示的に指定された引数
```

したがって、`mode = "balanced"` は `preselect_top_n = 500` を推奨しますが、`preselect_top_n` を明示的に指定した場合はその値が優先されます。

---

### 項目数指定構文（item_count）

`item_count` 引数は、`min_items` と `max_items` を簡潔に指定できる代替記法です。明示的な `min_items` / `max_items` と同時に指定することはできません。

| `item_count` | 意味 |
|---|---|
| `"==4"` | ちょうど4項目 |
| `"<=4"` | 最大4項目（1〜4項目） |
| `"2:4"` | 2〜4項目 |

```r
# ちょうど4項目の尺度
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "==4",
  mode       = "balanced",
  seed       = 20260705
)

# 最大4項目の尺度（1〜4項目）
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  seed       = 20260705
)

# 2〜4項目の尺度
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "2:4",
  mode       = "balanced",
  seed       = 20260705
)
```

`item_count` は `ncvroc()`、`roc_bruteforce()`（および `roc_bf()`）、`ncvroc_config()` で利用可能です。

### 後方互換性

`min_items` と `max_items` は引き続きサポートされています。同等の新旧記法は以下の通りです：

| 旧記法 (`min_items` / `max_items`) | 新記法 (`item_count`) |
|---|---|
| `min_items = 4, max_items = 4` | `item_count = "==4"` |
| `min_items = 1, max_items = 4` | `item_count = "<=4"` |
| `min_items = 2, max_items = 4` | `item_count = "2:4"` |

低水準関数（`exhaustive_sum_roc()`, `nested_sum_roc()`, `fit_final_sum_scale()`, `count_item_combinations()`, `suggest_preselect_top_n()`）では、引き続き `min_items` と `max_items` を使用します。

---

### 結果の保存（results_storage）

`ncvroc()` と `roc_bruteforce()` は、`results_storage` パラメータで完全な候補テーブルの保存先を制御します。v0.10.0 以降のデフォルトは `"auto"` で、探索サイズに応じて自動的に保存モードを選択します。

| `results_storage` | 動作 |
|---|---|
| `"auto"`（デフォルト） | 小規模な探索はメモリ内に保存。大規模な探索（100,000通り超）はディスク上にチャンク分割 RDS ファイルとして保存。 |
| `"memory"` | 完全な候補テーブルをメモリに保持（v0.9.0 以前の挙動）。 |
| `"rds"` | 完全な候補テーブルを単一の RDS ファイルに保存。保存先は実行時ログに表示されます。意図と異なる場合は `results_dir` で明示的にパスを指定してください。`$results` / `$final_exhaustive_ranked` は `NULL` になります。 |
| `"none"` | 完全な候補テーブルを破棄。`ncvroc_results()` はエラーになります。 |

`results_storage` が `"none"` 以外の場合、`ncvroc_results()` を使って候補テーブルを取得できます：

```r
ncvroc_results(result, top_n = 20)  # 上位20件の候補を取得
```

チャンク分割 RDS の場合、`top_n = NULL` で全件を取得するには `allow_full_load = TRUE` が必要です：

```r
ncvroc_results(result, top_n = NULL, allow_full_load = TRUE)
```

### キャッシュとアトミック保存

大規模な全探索は時間がかかる場合があります。`ncvroc()` と `roc_bruteforce()` は結果のキャッシュとディスクへのアトミックなチャンク保存をサポートしています：

```r
result <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  cache      = "reuse",     # "off"（デフォルト）、"reuse"、または "refresh"
  seed       = 20260705
)

# 2回目の同一呼び出しはキャッシュから即座にロードされます
result2 <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q5,
  item_count = "<=4",
  mode       = "balanced",
  cache      = "reuse",
  seed       = 20260705
)
```

| `cache` | 動作 |
|---|---|
| `"off"`（デフォルト） | キャッシュを使用しません。 |
| `"reuse"` | 利用可能な場合はキャッシュされた結果を使用（データ、項目、指標、探索パラメータの完全ハッシュで検証）。未キャッシュ時は計算してキャッシュに保存。 |
| `"refresh"` | 常に再計算し、キャッシュを上書きします。 |

`cache_dir` でキャッシュの保存先ディレクトリを制御します（デフォルト: `tempdir()`）。

**アトミック RDS 保存保証**: チャンク RDS ファイルは同一ディレクトリ内の一時ファイル（`.tmp`）に書き込まれた後、アトミックにリネーム（`.rds`）されます。中断による不完全な書き込みが有効なチャンクとして扱われることはなく、残存した一時ファイルは安全に無視されます。

### 並列計算（Parallel computing）

NCVROC は解析規模とワークフローに応じて 4 つの並列化モードを提供します：

| モード | `parallel` の指定 | 概要 | 推奨ユースケース |
|---|---|---|---|
| **C++ マルチスレッド** | `"threads"` | RcppParallel を介してメイン R プロセス内でネイティブ C++ スレッドにより組み合わせを並列評価（共有メモリ、ゼロソケットオーバーヘッド）。 | メモリ内で完結する高速全探索（`roc_bruteforce()`, `exhaustive_sum_roc()`, または thorough/exhaustive モードの `ncvroc()`）。 |
| **外部分割並列** | `"outer"`（またはネスト CV 系で `TRUE`） | PSOCK ソケットワーカプロセスを用いて外部 CV 分割を並列評価。 | 複数分割を持つ標準的なネスト CV（`ncvroc()`, `nested_sum_roc()`）。 |
| **ハイブリッド・ネストCV** | `"hybrid"` | outer foldをPSOCKワーカで並列化し、各ワーカ内の全探索をC++スレッドで並列化。 | fold間とfold内の両方の並列性を利用できるネストCV。 |
| **チャンク分割並列** | `"chunks"`（または全探索系で `TRUE`） | 永続 PSOCK ソケットワーカプロセスを用いて組み合わせチャンクを並列評価。 | ディスク保存、キャッシュ、中断再開を伴う大規模全探索。 |

```r
# 高速なメモリ内 C++ マルチスレッド全探索:
result <- roc_bruteforce(
  data       = d,
  outcome    = y,
  items      = Q1:Q10,
  max_items  = 4,
  parallel   = "threads",
  n_workers  = 4
)

# 外部 CV 分割の並列化:
result <- ncvroc(
  data       = d,
  outcome    = y,
  items      = Q1:Q10,
  max_items  = 3,
  parallel   = "outer",
  n_workers  = 4
)

# ハイブリッド（outerプロセス × C++スレッド、総並列度8）:
result <- ncvroc(
  data               = d,
  outcome            = y,
  items              = Q1:Q10,
  max_items          = 3,
  parallel           = "hybrid",
  n_workers          = 4,
  threads_per_worker = 2
)
```

**後方互換性**: `parallel = TRUE` を指定した場合、ネスト CV 系関数（`ncvroc()`, `nested_sum_roc()`）では `"outer"` に、全探索系関数（`roc_bruteforce()`, `exhaustive_sum_roc()`）では `"chunks"` に自動的にマップされます。

### チャンクサイズ（chunk_size）

`chunk_size` パラメータ（デフォルト `200000`）は、大規模な全探索で 1 チャンクあたりに評価する組み合わせ数を制御します。通常はこのデフォルト値を変更する必要はありません。

### 最終候補の出力

`ncvroc()` はデフォルトで最終全探索を実行し、ランク付けされた全データ候補テーブルを RDS ファイルに保存します。

利便性のため、以下はメモリ内に保持されます：

```r
result$final_candidates       # 上位 N 行（final_top_n で制御）
result$final_model            # 最良の単一モデル（先頭行）
result$final_n_combinations   # 評価された組み合わせの総数
result$final_results_storage  # 保存モード（"auto", "rds", "memory", または "none"）
result$final_exhaustive_file  # RDS ファイルパス（"rds" モード時）
```

`selection_criterion` はネスト CV 中にどの候補が選択されるかを制御します。
`final_rank_by` は最終全データ候補テーブルのランク付け方法を制御します。

```r
result <- ncvroc(
  data          = analysis_dat,
  outcome       = y,
  items         = Q1:Q5,
  item_count    = "<=4",
  mode          = "balanced",
  final_rank_by = "auc",
  final_top_n   = 20,
  seed          = 20260705,
  save_results  = TRUE
)

result$final_candidates
result$final_model
```

`final_rank_by` でランキング基準を選択できます：

```r
final_rank_by = "auc"          # デフォルト
final_rank_by = "youden"
final_rank_by = "sensitivity"
final_rank_by = "specificity"
final_rank_by = "accuracy"
```

モデルを選択する前に、臨床的制約に基づいて `ncvroc_results()` でランク付けテーブルをフィルタリングできます：

```r
ncvroc_results(
  result,
  sensitivity = ">= 0.90",
  specificity = ">= 0.85",
  rank_by     = "youden",
  top_n       = 20
)
```

条件指定では 6 種類の演算子（`>=`, `>`, `<=`, `<`, `==`, `!=`）を AND 条件で組み合わせることができます。利用可能な列: `sensitivity`, `specificity`, `auc`, `youden`, `accuracy`, `ppv`, `npv`, `n_items`, `cutoff`。

---

## 主要関数リファレンス

### `ncvroc()`

1 回の呼び出しで NCVROC の全解析を実行するメイン関数です。base-R スタイルの列選択でアウトカムと項目を解決し、データを準備し、ネスト CV を実行し、オプションで最終全探索と CSV 出力を行います。

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
  n_workers         = NULL
)
```

**返り値:** クラス `"ncvroc_analysis"` の S3 オブジェクト。`print()`, `summary()`, `plot()` メソッドが利用可能です。臨床的制約による候補の絞り込みには `ncvroc_results()` を使用します。

---

### `ncvroc_results()`

`ncvroc_analysis` または `roc_bruteforce_result` オブジェクトから、臨床的・実務的制約に基づいて候補モデルをフィルタリングおよび再ランク付けします。

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

**返り値:** フィルタリングおよびランク付けされた候補モデルを含む data.frame。

---

### `roc_bruteforce()`

NSE 列指定に対応した、全データでの網羅的項目組み合わせ ROC 解析関数です。

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
  results_storage  = c("auto", "memory", "rds", "none"),
  results_name     = NULL,
  results_dir      = NULL,
  item_count       = NULL,
  chunk_size       = 200000L,
  cache            = c("off", "reuse", "refresh"),
  cache_dir        = NULL,
  parallel         = FALSE,
  n_workers        = NULL
)
```

**返り値:** クラス `"roc_bruteforce_result"` の S3 オブジェクト。エイリアス `roc_bf()` も同等です。

---

### `ncvroc_config()`

すべての解析パラメータを 1 つの設定オブジェクトにまとめます。`run_ncvroc()` と組み合わせて使用します。

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
  item_count        = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL,
  parallel          = FALSE,
  n_workers         = NULL
)
```

**返り値:** クラス `"ncvroc_config"` の S3 オブジェクト。

---

### `run_ncvroc()`

`ncvroc_config` オブジェクトから全パラメータを読み取って `nested_sum_roc()` を実行するラッパー関数です。

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

**返り値:** `ncvroc_result` オブジェクト。

---

### `nested_sum_roc()`

外部ループで汎化性能を推定し、内部ループでモデル選択を行うネスト交差検証を実行します。

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
  file_prefix        = "NCVROC",
  parallel           = FALSE,
  n_workers          = NULL
)
```

**返り値:** クラス `"ncvroc_result"` の S3 オブジェクト。

---

### `exhaustive_sum_roc()`

全項目の組み合わせを列挙し、単純合計得点を計算して ROC 評価を行います。

```r
exhaustive_sum_roc(
  data,
  outcome,
  items,
  min_items          = 1,
  max_items          = 4,
  positive_label     = 1,
  negative_label     = 0,
  cutoff_method      = c("youden", "closest_topleft"),
  rank_by            = c("auc", "youden", "sensitivity", "specificity", "accuracy"),
  top_n              = NULL,
  prefer_fewer_items = TRUE,
  ci                 = FALSE,
  conf_level         = 0.95,
  engine             = c("R", "Rcpp"),
  parallel           = FALSE,
  n_workers          = NULL,
  progress           = TRUE
)
```

**返り値:** ランク付けされた候補モデルを含む data.frame。性能値は見かけの性能（in-sample）です。

---

### `fit_final_sum_scale()`

交差検証後に、全データセット上で最終尺度を当てはめるためのラッパー関数です。

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
  ci             = TRUE,
  conf_level     = 0.95,
  engine         = c("R", "Rcpp"),
  parallel       = FALSE,
  n_workers      = NULL,
  progress       = TRUE
)
```

---

### 補助関数

- **`make_stratified_folds(y, k = 5, repeats = 1, seed = NULL)`**: 層化 k 分割交差検証のインデックスリストを作成。
- **`count_item_combinations(items_or_n, min_items = 1, max_items = 4, detail = FALSE)`**: 実際に生成することなく組み合わせ総数を計算。
- **`suggest_preselect_top_n(items_or_n, min_items = 1, max_items = 4, mode = ...)`**: 探索空間の規模と mode に基づいて実践的な事前選抜数を提案。

---

## クイック使用例

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

# 単一の関数呼び出しによる解析
result <- ncvroc(d, y, Q1:Q5, item_count = "<=2", mode = "quick",
  outer_k = 3, inner_k = 2, outer_repeats = 1, engine = "R",
  seed = 42, final_search = FALSE)
print(result)
summary(result)
plot(result)
```

### 設定オブジェクトを用いたワークフロー

```r
# 解析設定を定義
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

## 見かけの性能（Apparent）とネスト交差検証性能の違い

| 関数 | 性能の種類 | 主な用途 |
|---|---|---|
| `ncvroc()` | ネスト交差検証（汎化性能） | 単一呼び出しによる推奨エントリーポイント |
| `roc_bruteforce()` | 見かけの性能（in-sample） | 全データでの高速な網羅的探索 |
| `exhaustive_sum_roc()` | 見かけの性能（in-sample） | 探索的スクリーニング |
| `nested_sum_roc()` | ネスト交差検証（汎化性能） | 厳密な汎化性能の推定 |
| `run_ncvroc()` | ネスト交差検証（汎化性能） | 設定オブジェクトを用いたバッチ解析 |
| `fit_final_sum_scale()` | 見かけの性能（in-sample） | 全データを用いた最終尺度の確定 |

---

## 信頼区間（Confidence intervals）

NCVROC は、全データセットで評価された最終尺度性能指標に対して信頼区間（CI）推定を提供します：

- **AUC CI:** DeLong et al. (1988) によるノンパラメトリック漸近正規近似法。得点の頻度分布から O(K) 時間で高速・直接計算。
- **感度・特異度・正解率・PPV・NPV CI:** Clopper and Pearson (1934) による二項正確信頼区間（Beta 分布分位数 `stats::qbeta` を利用）。
- **信頼水準:** `conf_level` 引数で指定（デフォルト `0.95` で 95% CI）。
- **デフォルトの動作:**
  - `fit_final_sum_scale()` および `ncvroc()` はデフォルトで信頼区間を計算します（`ci = TRUE`）。
  - `exhaustive_sum_roc()` は高速性を維持するためデフォルトで `ci = FALSE` となっており、`ci = TRUE` が指定された場合はランキング後の上位 N 候補に対してのみ CI を算出します。

```r
# 95% 信頼区間付きで最終尺度を当てはめる
final <- fit_final_sum_scale(
  data       = d,
  outcome    = "y",
  items      = c("Q1", "Q2", "Q3"),
  max_items  = 2,
  ci         = TRUE,
  conf_level = 0.95
)

# 点推定値と上下限の確認
final[, c("rank", "items", "auc", "auc_lower", "auc_upper",
          "sensitivity", "sensitivity_lower", "sensitivity_upper",
          "specificity", "specificity_lower", "specificity_upper")]
```

> [!IMPORTANT]
> **最終モデル性能の信頼区間は、固定された尺度を全データに適用した際の標本抽出の不確実性を定量化するものです。モデル選択やカットオフ選択に伴う不確実性は考慮されておらず、交差検証された信頼区間として解釈すべきではありません。**
>
> モデルの汎化性能と不確実性の評価には、外部交差検証ループ（`nested_sum_roc()` または `ncvroc()`）の結果を使用してください。

---

## 並列処理の詳細

NCVROC は以下の 4 つの並列実行モードをサポートしています：

- **C++ マルチスレッド (`parallel = "threads"`)**: メイン R プロセス内で `RcppParallel` (TBB) を用いて組み合わせをネイティブスレッドで並列評価。ソケット起動オーバーヘッドやプロセス間通信がなく、高速な探索が可能。
- **外部分割並列化 (`parallel = "outer"`)**: ソケットワーカプロセスを用いて外部 CV 分割を並列評価。各分割内の事前選抜はシーケンシャルに実行。
- **ハイブリッド・ネストCV (`parallel = "hybrid"`)**: outer foldをPSOCKワーカで並列化し、各ワーカ内の全探索を`threads_per_worker`本のC++スレッドで評価。`engine = "Rcpp"`のネストCV専用。
- **チャンク分割並列化 (`parallel = "chunks"`)**: 永続ソケットワーカを用いて大規模な組み合わせ空間をチャンク単位で並列評価。

### 並列モードの文脈依存自動解決

| 指定 | ネスト CV 系 (`ncvroc`, `nested_sum_roc`) | 全探索系 (`roc_bruteforce`, `exhaustive_sum_roc`) |
|---|---|---|
| `parallel = FALSE`（デフォルト） | シーケンシャル（単一プロセス）実行 | シーケンシャル（単一プロセス）実行 |
| `parallel = TRUE` | `"outer"`（外部分割並列化）に解決 | `"chunks"`（チャンク並列化）に解決 |
| `parallel = "none"` | シーケンシャル（単一プロセス）実行 | シーケンシャル（単一プロセス）実行 |
| `parallel = "threads"` | 外部分割は逐次実行、内部全探索を C++ マルチスレッドで並列化 | C++ マルチスレッドで全探索を並列化 |
| `parallel = "outer"` | 外部 CV 分割を並列化 | 非対応（エラー） |
| `parallel = "chunks"` | 外部分割は逐次実行、各分割内のチャンクを並列化（単一クラスタを再利用） | 組み合わせチャンクをソケットワーカで並列化 |
| `parallel = "hybrid"` | outer foldをPSOCK並列化し、各ワーカ内でC++スレッドを使用 | 非対応（エラー） |

### ワーカ数の指定 (`n_workers`)

- **`n_workers = NULL`（デフォルト）**: 利用可能な物理 CPU コア数を自動検出（`max(1L, parallel::detectCores(logical = FALSE) - 1L)`）。
- **`n_workers = 4`**: 最大 4 ワーカプロセスまたは 4 スレッドを使用。
- **自動上限制限**: 利用可能な CPU コア数、タスク数、CRAN 環境変数制限（`_R_CHECK_LIMIT_CORES_`）により安全に上限が適用されます。
- **hybridでの意味**: `parallel = "hybrid"` では、`n_workers` はouter PSOCK worker数の要求値です。`NULL` の場合はouter worker数を先に自動決定し、その後、残りのCPU budgetに応じて`threads_per_worker`をcapします。

### hybrid worker当たりのスレッド数 (`threads_per_worker`)

`threads_per_worker`は、hybrid modeで各outer PSOCK worker内に使用するC++ thread数の要求値です。実効値はCPU budget、決定済みのouter worker数、`_R_CHECK_LIMIT_CORES_`に応じて縮小されることがあります。outer worker数とworker当たりthread数の積は利用可能なbudget内に制限され、CRAN check時は最大2相当に抑えられます。

### 並列度の制御とoversubscription防止

明示的に総並列度を管理する`"hybrid"`以外では、外部分割並列化、チャンク並列化、C++マルチスレッドが多重にネストして実行されることはありません。hybridはouter PSOCKとC++スレッドだけを併用し、その積をCPU上限内に制限します。outer worker内でchunk PSOCKを起動することはありません。

### 高速チャンク/ストリーミング探索エンジンと厳密性

v0.12.0 および v0.13.0 の全探索エンジンは、C++ 内部で数学的 unranking により組み合わせをオンデマンド生成（`evaluate_combos_cpp_chunk()`）し、ストリーミング局所 Top-N プールを維持します。
- **厳密な全探索（近似なし）**: ヒューリスティクスや事前足切りを行わず、全組み合わせを完全に評価します。
- **決定論的タイブレーク**: 各候補はシリアル列挙時の 1-based な `.global_combo_index` を保持するため、シリアル実行と並列実行で完全に同一の順位・モデル選抜・統計数値を再現します。
- **ストリーミング Top-N 不変条件**: 各チャンクは最終的な全体 Top-N 以上の候補数を保持するため（`top_n_local >= global_top_n`）、チャンク統合時に最良候補が失われることはありません。

### 使用例

```r
# ハイブリッド・ネストCV
res_hybrid <- ncvroc(
  data               = analysis_dat,
  outcome            = y,
  items              = Q1:Q30,
  max_items          = 4,
  parallel           = "hybrid",
  n_workers          = 4,
  threads_per_worker = 2
)
```

hybridの性能はfold数、候補空間、データサイズ、ハードウェアに依存し、outer-onlyやthreads-onlyより常に高速とは限りません。

```r
# 例1: メモリ内 C++ マルチスレッド全探索（高速）
res_threads <- roc_bruteforce(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q30,
  item_count = "<=4",
  parallel   = "threads",
  n_workers  = 4
)

# 例2: 外部 CV 分割の並列化（標準的なネスト CV）
res_outer <- ncvroc(
  data          = analysis_dat,
  outcome       = y,
  items         = Q1:Q14,
  max_items     = 4,
  outer_k       = 5,
  inner_k       = 4,
  outer_repeats = 5,
  parallel      = "outer",
  n_workers     = 4,
  seed          = 42
)

# 例3: ディスク保存・キャッシュを伴う大規模チャンク並列全探索
res_chunks <- roc_bruteforce(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q40,
  item_count = "<=4",
  parallel   = "chunks",
  n_workers  = 4
)
```

---

## 交差検証を行わない総当たり ROC 探索

全データセット上で網羅的な組み合わせ ROC 解析を直接行う場合は、`roc_bruteforce()`（またはエイリアス `roc_bf()`）を使用します。`ncvroc()` と同じ NSE 列解決を共有しています。

> 算出される性能値は見かけの性能（in-sample）であり、楽観的な推定値となる可能性があります。過学習を抑えた汎化性能の推定には `ncvroc()` を使用してください。

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

# 完全な候補テーブルを取得する場合（デフォルトで RDS に保存）:
ncvroc_results(result, top_n = NULL)
```

---

## Rcpp エンジン

`ncvroc()`, `roc_bruteforce()`, `exhaustive_sum_roc()`, `nested_sum_roc()`, `fit_final_sum_scale()` で `engine = "Rcpp"` を指定すると、ネイティブ C++ バックエンドで高速計算が行われます。結果は R エンジンと完全に一致し、中規模のワークロードで約 7 倍の高速化が得られます。

```r
exhaustive_sum_roc(d, "y", paste0("Q", 1:5), max_items = 2, engine = "Rcpp")
```

---

## 詳細リファレンス

各関数の詳細な引数仕様、返り値の完全な構造、内部アルゴリズムの詳細については、[日本語詳細リファレンス（docs/reference-ja.md）](docs/reference-ja.md) を参照してください。

---

## ライセンス

MIT
