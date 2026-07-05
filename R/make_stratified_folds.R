# make_stratified_folds.R — Stratified k-fold cross-validation indices

#' Create stratified folds for cross-validation
#'
#' Creates stratified k-fold cross-validation indices, maintaining class
#' proportions across folds. Supports repeated fold creation with
#' independent shuffling per repeat.
#'
#' @param y A binary vector (exactly two unique values allowed).
#' @param k Integer, number of folds. Must be ≥ 2.
#' @param repeats Integer, number of independent repeats of the k-fold split
#'   (default 1).
#' @param seed Integer, seed for reproducibility. If provided, the seed for
#'   repeat `r` is set to `seed + r - 1`.
#'
#' @return A named list of length `k * repeats`. Each element is an integer
#'   vector of test indices for that fold. Names follow the pattern
#'   `"Rep1_Fold1"`, `"Rep1_Fold2"`, ..., `"Rep2_Fold1"`, etc.
#'
#' @details
#' Within each repeat, every index appears in exactly one test fold.
#' The `k` folds within a repeat partition all indices 1:n.
#'
#' If `k` exceeds the size of the smaller class, `k` is reduced with a warning.
#' If after reduction `k < 2`, an error is raised.
#'
#' @examples
#' y <- c(rep(1, 30), rep(0, 70))
#' folds <- make_stratified_folds(y, k = 5, seed = 42)
#' table(y[folds[[1]]])  # class balance in first fold
#'
#' @export
make_stratified_folds <- function(y, k = 5, repeats = 1, seed = NULL) {
  # ---- Validate arguments ----
  if (!is.numeric(k) || length(k) != 1 || k < 2 || k != as.integer(k)) {
    stop("`k` must be an integer >= 2.", call. = FALSE)
  }
  k <- as.integer(k)

  if (!is.numeric(repeats) || length(repeats) != 1 || repeats < 1 || repeats != as.integer(repeats)) {
    stop("`repeats` must be a positive integer.", call. = FALSE)
  }
  repeats <- as.integer(repeats)

  # ---- Validate y ----
  if (length(y) == 0) {
    stop("`y` must not be empty.", call. = FALSE)
  }

  if (anyNA(y)) {
    stop("`y` contains NA values. Missing values are not supported in v0.1.", call. = FALSE)
  }

  y_vals <- unique(y)
  if (length(y_vals) < 2) {
    stop("`y` has only one unique value. A binary outcome is required.", call. = FALSE)
  }
  if (length(y_vals) > 2) {
    stop("`y` must be binary (exactly two unique values). Found: ",
         paste(y_vals, collapse = ", "), ".", call. = FALSE)
  }

  n <- length(y)
  # Treat the first unique value as "class 0", second as "class 1" for splitting
  class0_val <- y_vals[1]
  class1_val <- y_vals[2]

  idx0 <- which(y == class0_val)
  idx1 <- which(y == class1_val)
  n0 <- length(idx0)
  n1 <- length(idx1)
  n_min <- min(n0, n1)

  # ---- Handle k > min class size ----
  if (k > n_min) {
    warning(
      "`k` (", k, ") exceeds the size of the smaller class (", n_min,
      "). Reducing `k` to ", n_min, ".",
      call. = FALSE
    )
    k <- n_min
  }

  if (k < 2) {
    stop(
      "After reducing `k`, it is ", k, " (< 2). ",
      "Cannot create stratified folds. Consider using more balanced data.",
      call. = FALSE
    )
  }

  # ---- Helper: split n indices into k roughly-equal chunks ----
  split_into_k <- function(indices_shuffled, n_total, k_folds) {
    # Assign fold membership: first (n_total %% k) folds get ceiling(n/k),
    # rest get floor(n/k)
    sizes <- rep(floor(n_total / k_folds), k_folds)
    extra <- n_total %% k_folds
    if (extra > 0) {
      sizes[1:extra] <- sizes[1:extra] + 1L
    }
    # Split shuffled indices according to sizes
    end <- cumsum(sizes)
    start <- c(1L, end[-k_folds] + 1L)
    lapply(seq_len(k_folds), function(f) indices_shuffled[start[f]:end[f]])
  }

  # ---- Create folds for each repeat ----
  all_folds <- vector("list", k * repeats)
  idx <- 1

  for (r in seq_len(repeats)) {
    if (!is.null(seed)) {
      set.seed(seed + r - 1L)
    }

    # Shuffle indices within each class
    idx0_shuffled <- sample(idx0)
    idx1_shuffled <- sample(idx1)

    # Split each class evenly across k folds
    folds0 <- split_into_k(idx0_shuffled, n0, k)
    folds1 <- split_into_k(idx1_shuffled, n1, k)

    # Combine for each fold
    for (f in seq_len(k)) {
      name <- paste0("Rep", r, "_Fold", f)
      all_folds[[idx]] <- c(folds0[[f]], folds1[[f]])
      names(all_folds)[idx] <- name
      idx <- idx + 1
    }
  }

  all_folds
}
