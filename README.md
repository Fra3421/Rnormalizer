# Normalizer

`Normalizer` is an [**R6**](https://r6.r-lib.org/) class for stateful column-wise data normalization and inverse scaling on numeric vectors and matrices, designed for R forecasting and modeling pipelines.

It stores fitted scaling parameters internally during `$fit()` / `$fit_transform()` so they can be consistently reapplied to unseen data (`$transform()`) and mapped back to the original scale (`$inverse()`).

## Dependencies

* [`R6`](https://r6.r-lib.org/)
* [`matrixStats`](https://cran.r-project.org/package=matrixStats) (`colMeans2`, `colSds`, `colMedians`, `colMads`, `colMins`, `colMaxs`)

## Normalization Methods

### 1. Z-Score and Robust Scaling (`method = "z_score"`)
* **Standard (`robust = FALSE`, default):** centers each column by subtracting the column mean and divides by the standard deviation.
* **Robust (`robust = TRUE`):** uses the column median and **MAD** (*Median Absolute Deviation*). If MAD is zero or non-finite in a column, it falls back to the standard deviation (emitting a warning); if the dispersion remains zero or non-computable, the scale is set to `1` with a warning to prevent division by zero.

### 2. Min-Max Scaling (`method = "min_max"`)
* Rescales each column using the observed minimum (`lo`) and maximum (`hi`) from the fit phase.
* If a column is constant (zero range), the range is set to `1` with a warning, effectively shifting the values without rescaling.

## Public API

| Method | Description | Return Value |
| :--- | :--- | :--- |
| `$new(method = c("z_score", "min_max"), robust = FALSE)` | Initializes a `Normalizer` instance. | `Normalizer` object (invisible) |
| `$fit(x)` | Computes scaling parameters from `x` and stores them internally. | `self` (invisible, supports chaining) |
| `$transform(x)` | Applies the fitted parameters to new data `x`. | Normalized data (same shape as `x`) |
| `$inverse(z)` | Maps normalized data `z` back to the original scale. | Data on the original scale |
| `$fit_transform(x)` | Fits on `x` and immediately returns the normalized output. | Normalized data |
| `$is_fitted()` | Checks whether `$fit()` or `$fit_transform()` has been called. | `logical(1)` |
| `$get_state()` | Returns the fitted parameters (`mu`/`s` or `lo`/`hi`), or `NULL`. | `list` or `NULL` |

## To-Do

* **Python implementation:** Port or mirror the `Normalizer` interface in Python (e.g., for cross-language pipelines or ML workflows).
* **Addsitional normalization types:** Add further scaling and transformation methods (such as quantile transformer, power/Box-Cox/Yeo-Johnson transforms, or custom range scaling).

## Usage Examples

### Z-Score on a Matrix and Inverse Transform

```r
library(R6)
library(matrixStats)

set.seed(42)
train_mat <- cbind(
  a = rnorm(100, mean = 50, sd = 10),
  b = runif(100, min = 0, max = 200)
)
test_mat <- cbind(
  a = c(45, 55, NA),
  b = c(20, 180, 100)
)

norm <- Normalizer$new(method = "z_score", robust = FALSE)
train_scaled <- norm$fit_transform(train_mat)
test_scaled  <- norm$transform(test_mat)

# Map back to the original scale
test_reconstructed <- norm$inverse(test_scaled)

# Inspect fitted parameters
norm$get_state()
