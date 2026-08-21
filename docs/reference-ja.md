# NCVROC 0.12.0 リファレンス

**N**ested **C**ross-**V**alidation for Combinatorial **ROC**-based Selection of Item-set Scores（項目セット得点の組み合わせROC選択のためのネスト交差検証）

項目の組み合わせ選択、ROCに基づく評価、ネスト交差検証を通じて、短い項目ベースのスクリーニング尺度を開発します。心理・臨床質問紙データにおいて、単純な合計得点を用いて二値アウトカムを最もよく予測する項目の小サブセットを特定します。

合計得点が高いほど陽性アウトカムの確率が高いと仮定します。必要に応じて事前に項目を逆転処理してください。

---

## NCVROC 0.12.0 の主要変更点

- **チャンク単位の網羅探索並列化 (`parallel = "chunks"`)**: 数百万通りの組み合わせ探索空間（$O(\binom{M}{K})$）をチャンク単位で複数の CPU ソケットワーカー（`parallel::makePSOCKcluster`）に分散して並列評価。
- **並列化レベルの明確な分離**: 外側交差検証フォールド並列化（`"outer"`）とチャンク並列化（`"chunks"`）を明確に分離し、二重並列化（nested parallelism）による CPU・メモリの過負荷を防止。
- **C++ オンデマンド組み合わせ生成（Unranking）とストリーミング Top-$N$**: 100万件超の巨大な R list メモリ確保を完全に排除し、C++ 内部で数学的 unranking により候補を逐次評価。局所 Top-$N$ 抽出により省メモリで高速な探索を実現。
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

`ncvroc()` と `roc_bruteforce()` は `results_storage` パラメータで完全な候補テーブルの保存方法を制御します。v0.10.0からデフォルトは `"auto"` で、探索サイズに応じて自動的に保存モードを選択します。

| `results_storage` | 動作 |
|---|---|
| `"auto"`（デフォルト） | 小規模な探索はメモリ内に保存。大規模な探索（100,000以上の組み合わせ）はチャンク分割RDSファイルとしてディスクに保存。 |
| `"memory"` | 完全な候補テーブルをメモリに保持（v0.8.0以前の動作）。 |
| `"rds"` | 完全な候補テーブルを単一RDSファイルに保存。デフォルトでは現在のワーキングディレクトリに出力。RStudio ProjectやQuarto Projectでは通常プロジェクトルートになる。保存先は常に実行時の出力に表示される。`getwd()` で現在の保存先を確認できる。意図と異なる場合は `results_dir = "path/"` で明示的に指定する。`$final_exhaustive_ranked` は `NULL`。 |
| `"none"` | 完全な候補テーブルを破棄。`ncvroc_results()` はエラーになる。 |

`results_storage` が `"none"` でない場合、`ncvroc_results()` で候補テーブルを取得できます：

```r
ncvroc_results(result, top_n = 20)  # 上位20候補を取得
```

チャンク分割RDS結果（`"auto"`で大規模探索または`"rds"`のチャンク分割時）で `top_n = NULL` を使用するには、`allow_full_load = TRUE` が必要です：

```r
ncvroc_results(result, top_n = NULL, allow_full_load = TRUE)
```

### キャッシュ（v0.10.0の新機能）

### キャッシュとアトミックストレージ

大規模な全探索は時間がかかる場合があります。`ncvroc()` と `roc_bruteforce()` は結果のキャッシュとディスクへのアトミックなチャンク保存をサポートしています：

```r
result <- ncvroc(
  data    = analysis_dat,
  outcome = y,
  items   = Q1:Q5,
  item_count = "<=4",
  mode    = "balanced",
  cache   = "reuse",     # "off"（デフォルト）、"reuse"、または "refresh"
  seed    = 20260705
)
```

| `cache` | 動作 |
|---|---|
| `"off"`（デフォルト） | キャッシュなし。 |
| `"reuse"` | 利用可能な場合はキャッシュされた結果を使用（データ、項目、探索指標、パラメータの完全ハッシュシグネチャで検証）。それ以外は計算してキャッシュ。 |
| `"refresh"` | 常に再計算し、キャッシュを上書き。 |

`cache_dir` でキャッシュされた結果の保存先を制御します（デフォルト：`tempdir()`）。

**アトミックなRDS保存保証**: チャンクRDSファイルは一時ファイル（`.tmp`）に書き込まれた後、同一ディレクトリ内でアトミックにリネーム（`.rds`）されます。処理中断時の不完全な書き込みが有効なキャッシュとして認識されることはなく、残存した `.tmp` ファイルはチャンク一覧から自動的に除外されます。

### チャンクサイズ

`chunk_size` パラメータ（デフォルト `200000`）は、大規模な全探索で1チャンクに評価される組み合わせ数を制御します。通常はデフォルト値を変更する必要はありません。

### 最終候補の出力

`ncvroc()`はデフォルトで最終全探索を実行し、ランク付けされた全データ候補テーブルをRDSファイルに保存します。

便宜上、以下がメモリに格納されます：

```r
result$final_candidates   # 上位N行（final_top_nで制御）
result$final_model        # 最良の単一モデル（先頭行）
result$final_n_combinations  # 評価された組み合わせの総数
result$final_results_storage # 保存モード（"auto"、"rds"、"memory"、"none"）
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
  results_storage   = c("auto", "memory", "rds", "none"),
  results_name      = NULL,
  results_dir       = NULL,
  save_results      = FALSE,
  output_dir        = ".",
  progress          = TRUE,
  verbose           = TRUE,
  return            = "full",
  ci                = TRUE,
  conf_level        = 0.95,
  parallel          = FALSE,
  n_workers         = NULL,
  item_count        = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL
)
```

`outcome`はベアシンボル（`y`）または文字列（`"y"`）を受け付けます。
`items`はベア範囲（`Q1:Q5`）、`c()`によるベア名、文字ベクトル、既存変数、または数値位置を受け付けます。

- `selection_criterion`: ネストCV中にどの候補が選択されるかを制御します。
- `final_rank_by`: 最終全データ候補テーブルのランク付け方法を制御します。
- `parallel`: 並列実行モード。`FALSE` または `"none"`（逐次実行、デフォルト）、`"outer"`（外側交差検証フォールドを並列化）、`"chunks"`（外側ループは逐次で、各フォールド内の事前選択チャンク探索を並列化）。`TRUE` を指定した場合は後方互換性のため `"outer"` として解釈されます。
- `n_workers`: 並列実行時のワーカープロセス数。`NULL`（デフォルト）の場合は利用可能な物理CPUコア数から自動決定されます。ワーカー数はフォールド数・チャンク数・物理コア数・CRANコア制限（`_R_CHECK_LIMIT_CORES_`）で自動的に上限キャップされます。
- `chunk_size`: 大規模な全探索で1チャンクに評価される組み合わせ数（デフォルト `200000L`）。
- `cache`: 結果キャッシュモード。`"off"`（デフォルト）、`"reuse"`（パラメータハッシュが一致するキャッシュを再利用）、`"refresh"`（常に再計算して上書き）。

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
  top_n  = 20,
  allow_full_load = FALSE
)
```

各条件は`">= 0.90"`や`"<= 3"`のような文字列です。6つの演算子（`>=`, `>`, `<=`, `<`, `==`, `!=`）がサポートされています。複数の条件はAND論理で組み合わされます。結果は`rank_by`でランク付けされ、安定したタイブレーカーが適用されます。すべての一致行を返すには`top_n = NULL`を、空のテーブルを返すには`0`を設定します。

チャンク分割RDS保存の場合、`top_n = NULL` には `allow_full_load = TRUE` が必要です。

**戻り値:** 絞り込まれランク付けされた候補モデルを含む data.frame。

`x` には次のいずれかを指定できます：

- `final_search = TRUE` で作成された `ncvroc_analysis` オブジェクト
- `roc_bruteforce()` または `roc_bf()` が返す `roc_bruteforce_result` オブジェクト

---

### `roc_bruteforce()`

NSEによる列解決を用いた、全データでの項目組み合わせROC全探索。

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
  parallel         = FALSE,
  n_workers        = NULL,
  item_count       = NULL,
  chunk_size       = 200000L,
  cache            = c("off", "reuse", "refresh"),
  cache_dir        = NULL
)
```

- `parallel`: 並列実行モード。`FALSE` または `"none"`（逐次実行、デフォルト）、`"chunks"`（候補組み合わせチャンクを PSOCK ワーカーで並列評価）。`TRUE` を指定した場合は `"chunks"` として解釈されます。
- `n_workers`: 並列実行ワーカー数（デフォルト `NULL` で自動検出）。
- `chunk_size`: 1チャンクあたりの組み合わせ評価数（デフォルト `200000L`）。
- `cache`: キャッシュモード（`"off"`, `"reuse"`, `"refresh"`）。

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
  parallel          = FALSE,
  n_workers         = NULL,
  item_count        = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL
)
```

`mode`はデフォルトの`preselect_top_n`を制御します：

| モード | 事前選択 | ユースケース |
|---|---|---|
| `"quick"` | 上位100 | 高速スクリーニング、探索 |
| `"balanced"` | 上位500（デフォルト） | 通常の分析 |
| `"thorough"` | 上位1000 | 網羅的探索 |
| `"exhaustive"` | 全候補 | 完全列挙（低速になる可能性あり） |

**戻り値:** クラス`"ncvroc_config"`のS3オブジェクト。`parallel` や `n_workers` を含むすべての設定が保持されます。`print()`は整形されたサマリー（並列設定を含む）を表示し、`preselect_top_n >= 100,000`の場合に警告を出します。

---

### `run_ncvroc()`

`ncvroc_config`オブジェクトからすべてのパラメータ（並列化設定 `parallel`, `n_workers` を含む）を読み取る`nested_sum_roc()`の便利なラッパー。

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
  parallel           = FALSE,
  n_workers          = NULL,
  progress           = TRUE,
  verbose            = TRUE,
  return             = c("full", "summary"),
  output_dir         = NULL,
  file_prefix        = "NCVROC"
)
```

- `parallel`: 並列実行モード。`FALSE` または `"none"`（逐次実行、デフォルト）、`"outer"`（外側交差検証フォールドを並列化）、`"chunks"`（外側ループは逐次で、各フォールド内の事前選択チャンク探索を並列化）。`TRUE` を指定した場合は後方互換性のため `"outer"` として解釈されます。`parallel = "chunks"` では、外側ループ開始時に PSOCK クラスタが1回作成され、全フォールド間で永続的に再利用されます。
- `n_workers`: 並列実行ワーカー数。`NULL` の場合は利用可能コア数から自動決定されます。

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

すべての項目の組み合わせを列挙・評価し、単純合計得点を計算してROCで評価します。

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
  progress          = TRUE,
  parallel          = FALSE,
  n_workers         = NULL,
  chunk_size        = 200000L,
  cache             = c("off", "reuse", "refresh"),
  cache_dir         = NULL
)
```

- `parallel`: 並列実行モード。`FALSE` または `"none"`（逐次実行、デフォルト）、`"chunks"`（候補組み合わせチャンクを PSOCK ワーカーで並列評価）。`TRUE` を指定した場合は `"chunks"` として解釈されます。
- `n_workers`: 並列実行ワーカー数（デフォルト `NULL` で自動検出）。
- `chunk_size`: 1チャンクあたりの組み合わせ評価数（デフォルト `200000L`）。
- `cache`: キャッシュモード（`"off"`, `"reuse"`, `"refresh"`）。

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

## 信頼区間（Confidence intervals）

`NCVROC` は全データで評価された最終尺度の性能指標に対して信頼区間（CI）推定を提供します：

- **AUC CI:** DeLongら（1988）によるノンパラメトリック漸近正規近似法。スコア度数分布から $O(K)$ で高速に計算されます。
- **Sensitivity, Specificity, Accuracy, PPV, NPV CI:** Clopper and Pearson（1934）による二項分布の正確信頼区間（Beta分布分位数 `stats::qbeta` を使用）。
- **信頼水準:** `conf_level` で指定（デフォルト `0.95` で95% CI）。
- **デフォルトの動作:**
  - `fit_final_sum_scale()` および `ncvroc()` は、最終モデル/候補に対してデフォルトでCIを付与します（`ci = TRUE`）。
  - `exhaustive_sum_roc()` は探索速度維持のためデフォルトで `ci = FALSE` となっており、`ci = TRUE` の場合もランキング・top-$N$抽出後にのみCIを計算します。

### 使用例

```r
# 95%信頼区間付きで最終尺度を適合
final <- fit_final_sum_scale(
  data       = d,
  outcome    = "y",
  items      = c("Q1", "Q2", "Q3"),
  max_items  = 2,
  ci         = TRUE,
  conf_level = 0.95
)

# 出力には点推定値とともに下限（_lower）と上限（_upper）が含まれます
final[, c("rank", "items", "auc", "auc_lower", "auc_upper",
          "sensitivity", "sensitivity_lower", "sensitivity_upper",
          "specificity", "specificity_lower", "specificity_upper")]
```

小標本や完全分類（例: 10例中10例正解の10/10）の場合でも、Clopper–Pearson法により標本不確実性が適切に定量化されます：
```text
Sensitivity: 1.000 [0.692, 1.000]
```

### 信頼区間の解釈に関する重要な注意

> **重要:** 最終モデルの性能に対する信頼区間は、全データで評価された固定モデルの**標本不確実性（sampling uncertainty）**を定量化するものです。モデル選択やカットオフ選択によって生じる不確実性は考慮されておらず、**交差検証された信頼区間（cross-validated confidence intervals）として解釈すべきではありません**。
>
> 汎化性能の検証には `nested_sum_roc()` または `ncvroc()` の外部交差検証ループ結果（`nested_cv_summary`）を使用してください。

---

## 並列実行（Outer Fold & Chunk Parallelization）

`NCVROC`（>= 0.12.0）は、ソケットクラスタ（`parallel::makePSOCKcluster`）を用いた**外側交差検証フォールド（outer fold）並列化**および**大規模全探索チャンク（chunk）並列化**をサポートしています。Windows、macOS、Linux でシームレスに動作します：

- **外側フォールド並列化 (`parallel = "outer"`)**: 外側交差検証フォールドをワーカー間で並列評価します。フォールド内の事前選択は逐次実行されます。
- **チャンク単位の並列化 (`parallel = "chunks"`)**: 数百万通りにおよぶ大規模な組み合わせ探索空間（$O(\binom{M}{K})$ 通り）をチャンク単位でワーカー間に分散して並列評価します。

### 並列化モードと文脈による解釈

| 設定値 | ネスト系（`ncvroc`, `nested_sum_roc`） | 全探索系（`roc_bruteforce`, `exhaustive_sum_roc`） |
|---|---|---|
| `parallel = FALSE`（デフォルト） | 単一プロセス逐次実行 | 単一プロセス逐次実行 |
| `parallel = TRUE` | `"outer"` として解釈（外側フォールド並列化） | `"chunks"` として解釈（チャンク並列化） |
| `parallel = "none"` | 単一プロセス逐次実行 | 単一プロセス逐次実行 |
| `parallel = "outer"` | 外側交差検証フォールドを並列化 | 未サポート（エラー） |
| `parallel = "chunks"` | 外側フォールドは逐次、各フォールド内の事前選択チャンク探索を並列化（クラスタ再利用） | 候補組み合わせチャンクを並列化 |

> [!NOTE]
> `parallel = "auto"` は将来のリリース向けに予約されています。v0.12.0 で `"auto"` を指定すると、`"none"`、`"outer"`、または `"chunks"` のいずれかを明示するよう促すエラーメッセージが表示されます。

### ワーカー数指定 (`n_workers`)

- **`n_workers = NULL`（デフォルト）**: 利用可能な物理 CPU コア数（`max(1L, parallel::detectCores(logical = FALSE) - 1L)`）から自動決定されます。
- **`n_workers = 4`**: 最大4つのワーカープロセスを使用します。
- **安全な上限キャップ**: 実際のワーカー数は、タスク数（フォールド数またはチャンク数）、利用可能な物理コア数、および CRAN 環境変数（`_R_CHECK_LIMIT_CORES_`）によって安全に自動上限調整されます。

### 相互排他ルールの強制（二重並列化の禁止）

NCVROC では、外側フォールド並列とチャンク並列を同時にネスト実行すること（例: 4 outer $\times$ 4 chunks = 16 プロセス）は**禁止**されています。CPU oversubscription やソケット枯渇、メモリ複製のオーバーヘッドを防ぎ、1回の実行につき1つの並列化レベルのみをクリーンに適用します。

### ネストCVにおけるクラスタの永続再利用

`nested_sum_roc(..., parallel = "chunks")` の実行時、PSOCK クラスタは外側フォールドループの開始前に**1回だけ生成**され、パッケージ環境や C++ DLL を1回だけエクスポートします。各フォールドではフォールド固有の学習データのみを更新してクラスタを全フォールドで再利用し、処理終了時に安全にシャットダウンされます。

### チャンク／ストリーミング探索エンジンと厳密な解の同一性

v0.12.0 の全探索エンジンは、C++ 内部で数学的 unranking（`evaluate_combos_cpp_chunk()`）を用いてオンデマンドで候補を生成し、局所 Top-$N$ をストリーミング集約します：
- **完全な網羅探索（Exact Exhaustive Search）**: 候補の足切りや近似を行わず、指定されたすべての組み合わせを完全に評価します。
- **決定論的タイブレーク**: 各候補はシリアル全列挙時の 1-based な `.global_combo_index` を保持しており、チャンク境界をまたぐ同点（tie）が存在する場合でも、シリアル実行と完全に同一の順位・モデル選抜・統計量（AUC、カットオフ、感度、特異度、Youden指数等）が得られます。
- **ストリーミング Top-$N$ 不変条件**: $\text{top\_n\_local} \ge \text{global\_top\_n}$ を保証することで、マスター統合時に真の Top-$N$ 候補が脱落しない設計となっています。

### 性能特性とベンチマーク

- **エンジン単体の大幅な高速化**: 1,031,346 通りの組み合わせ探索ベンチマーク（$M=71, K \le 4, N=200$）において、C++ unranking とストリーミング Top-$N$ メモリ最適化により、単一ワーカー（1 worker）の実行時間が従来の全列挙シリアル経路（約 21.3 秒）から **約 4.9 秒**（約 4.4 倍）へ短縮されました。
- **マルチワーカースケーリングの依存性**: PSOCK マルチワーカーの性能向上はワークロードに依存します（workload-dependent）。C++ 内部での評価が極めて高速なため、小さな探索空間では Windows 上のソケット IPC 初期化オーバーヘッドが相対的に大きくなります。候補数（$M \ge 40, K \ge 5$）やサンプルサイズ $N$ がさらに大きくなるにつれて、並列分散の恩恵が支配的になります。

### 使用例

#### 使用例 1: 外側フォールド並列化（中規模項目数 $M \le 25$ に推奨）

```r
res_outer <- ncvroc(
  data          = analysis_dat,
  outcome       = y,
  items         = Q1:Q14,
  max_items     = 4,
  outer_k       = 5,
  inner_k       = 4,
  outer_repeats = 5,
  parallel      = "outer",   # または parallel = TRUE
  n_workers     = 4,
  seed          = 42
)
```

#### 使用例 2: チャンク単位の全探索並列化（大規模項目数 $M \ge 40$ に推奨）

```r
res_chunks <- roc_bruteforce(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q40,
  item_count = "<=4",
  parallel   = "chunks",  # または parallel = TRUE
  n_workers  = 4
)
```

#### 使用例 3: ネストCV内の事前選択チャンク並列化

```r
res_nested_chunks <- ncvroc(
  data       = analysis_dat,
  outcome    = y,
  items      = Q1:Q40,
  item_count = "<=4",
  mode       = "thorough",
  parallel   = "chunks",  # 外側フォールドは逐次、各フォールド内の事前選択を並列化
  n_workers  = 4,
  seed       = 42
)
```

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

MIT
