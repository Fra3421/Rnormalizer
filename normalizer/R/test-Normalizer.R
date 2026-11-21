# How to run (from the folder containing Normalizer.R and this file):
#   install.packages(c("R6", "matrixStats", "testthat"))   # if missing
#   testthat::test_file("test-Normalizer.R")
# Or from a terminal:
#   Rscript -e 'testthat::test_file("test-Normalizer.R")'

library(testthat)

# Load the class if it is not already available (e.g. when not inside the package)
if (!exists("Normalizer", inherits = TRUE)) {
  candidates <- c("Normalizer.R", "R/Normalizer.R", "../R/Normalizer.R", "../../R/Normalizer.R")
  f <- candidates[file.exists(candidates)][1]
  if (is.na(f)) stop("Normalizer.R not found: run the tests from the folder containing the file.")
  source(f)
}

x_vec <- c(5, 3, 8, 3, 10, 1, NA, 7)

# ---------------------------------------------------------------------------
# Round-trip
# ---------------------------------------------------------------------------

test_that("round-trip on a vector (with NA) for both methods", {
  for (m in c("z_score", "min_max")) {
    nz  <- Normalizer$new(method = m)
    z   <- nz$fit_transform(x_vec)
    expect_null(dim(z))
    expect_equal(nz$inverse(z), x_vec, info = m)
  }
})

test_that("round-trip with robust z-score", {
  nz <- Normalizer$new("z_score", robust = TRUE)
  z  <- nz$fit_transform(x_vec)
  expect_equal(nz$inverse(z), x_vec)
})

test_that("round-trip on a matrix with different columns and NA", {
  set.seed(1)
  m <- cbind(a = rnorm(30, 10, 3), b = runif(30), c = rexp(30))
  m[c(2, 9), "b"] <- NA
  for (method in c("z_score", "min_max")) {
    nz <- Normalizer$new(method)
    z  <- nz$fit_transform(m)
    expect_true(is.matrix(z))
    expect_equal(dimnames(z), dimnames(m))
    expect_equal(nz$inverse(z), m, info = method)
  }
})

test_that("numeric data.frame is accepted and the result is a matrix", {
  df <- data.frame(a = 1:4, b = c(2, 4, 6, 9))
  z  <- Normalizer$new()$fit_transform(df)
  expect_true(is.matrix(z))
  expect_equal(dim(z), c(4L, 2L))
})

# ---------------------------------------------------------------------------
# Expected values
# ---------------------------------------------------------------------------

test_that("z-score: correct values and transform on new data", {
  nz <- Normalizer$new()
  z  <- nz$fit_transform(c(1, 2, 3))
  expect_equal(z, c(-1, 0, 1))
  expect_equal(nz$transform(c(2, 4)), c(0, 2))
})

test_that("integer input produces double output", {
  z <- Normalizer$new()$fit_transform(1:5)
  expect_type(z, "double")
  expect_equal(z, as.vector(scale(1:5)))
})

test_that("min-max: correct values, also outside the fitted range", {
  nz <- Normalizer$new("min_max")
  nz$fit(c(0, 10))
  expect_equal(nz$transform(c(5, 20)), c(0.5, 2))
})

test_that("robust z-score uses median and MAD", {
  x  <- c(1, 2, 3, 4, 100)
  nz <- Normalizer$new("z_score", robust = TRUE)
  z  <- nz$fit_transform(x)
  expect_equal(z[3], 0)
  expect_equal(as.numeric(nz$get_state()$s), mad(x))
})

test_that("Inf and NaN are treated as NA", {
  z <- Normalizer$new("min_max")$fit_transform(c(1, 2, Inf, NaN, 3))
  expect_equal(z, c(0, 0.5, NA, NA, 1))
})

test_that("fit_transform == fit + transform", {
  x <- cbind(a = c(1, 5, 9, NA), b = c(2, 4, 8, 16))
  for (method in c("z_score", "min_max")) {
    z1 <- Normalizer$new(method)$fit_transform(x)
    z2 <- Normalizer$new(method)$fit(x)$transform(x)
    expect_equal(z1, z2, info = method)
  }
})

# ---------------------------------------------------------------------------
# Output shape
# ---------------------------------------------------------------------------

test_that("vector -> vector, one-column matrix -> matrix", {
  nz <- Normalizer$new()
  z  <- nz$fit_transform(c(1, 2, 3, 4))
  expect_null(dim(z))
  expect_null(dim(nz$inverse(z)))

  m1 <- matrix(c(1, 2, 3, 4), ncol = 1)
  z1 <- nz$fit_transform(m1)
  expect_true(is.matrix(z1))
  expect_true(is.matrix(nz$transform(m1)))
  expect_true(is.matrix(nz$inverse(z1)))
})

test_that("a failed fit does not alter the previous state", {
  nz <- Normalizer$new()$fit(c(1, 2, 3))
  expect_error(nz$fit(matrix(NA_real_, 2, 2)), "No valid")
  expect_true(nz$is_fitted())
  expect_null(dim(nz$transform(c(1, 2))))   # it is still a fit on a vector
})

# ---------------------------------------------------------------------------
# Degenerate cases and warnings reporting the affected columns
# ---------------------------------------------------------------------------

test_that("constant column, z-score: scale 1, warning with column name", {
  m  <- cbind(a = c(1, 2, 3, 4), k = rep(5, 4))
  nz <- Normalizer$new()
  expect_warning(z <- nz$fit_transform(m), "column\\(s\\) 'k'")
  expect_equal(unname(z[, "k"]), rep(0, 4))
  expect_equal(unname(nz$get_state()$s[2]), 1)
  expect_equal(nz$inverse(z), m)
})

test_that("constant column, min-max: warning with column name, invertible", {
  m  <- cbind(a = c(1, 2, 3, 4), k = rep(5, 4))
  nz <- Normalizer$new("min_max")
  expect_warning(z <- nz$fit_transform(m), "column\\(s\\) 'k'")
  expect_equal(unname(z[, "k"]), rep(0, 4))
  expect_equal(nz$inverse(z), m)
})

test_that("without column names the warning uses the index", {
  m <- unname(cbind(a = c(1, 2, 3, 4), k = rep(5, 4)))
  expect_warning(Normalizer$new()$fit_transform(m), "column\\(s\\) 2")
})

test_that("multiple problematic columns are all listed", {
  m <- cbind(a = c(1, 2, 3, 4), k1 = rep(5, 4), k2 = rep(7, 4))
  expect_warning(Normalizer$new("min_max")$fit_transform(m), "'k1', 'k2'")
})

test_that("column with a single valid value: scale 1 and warning", {
  m  <- cbind(a = c(1, 2, 3, 4), s = c(7, NA, NA, NA))
  nz <- Normalizer$new()
  expect_warning(z <- nz$fit_transform(m), "column\\(s\\) 's'")
  expect_equal(unname(z[, "s"]), c(0, NA, NA, NA))
})

test_that("all-NA column: no warning, NA output, round-trip ok", {
  m <- cbind(a = c(1, 2, 3), b = NA_real_)
  for (method in c("z_score", "min_max")) {
    nz <- Normalizer$new(method)
    expect_warning(z <- nz$fit_transform(m), NA)
    expect_true(all(is.na(z[, "b"])))
    expect_false(anyNA(z[, "a"]))
    expect_equal(nz$inverse(z), m, info = method)
  }
})

test_that("robust: MAD = 0 falls back to the sd and reports the column", {
  x  <- cbind(v = c(1, 1, 1, 1, 1, 1, 10))
  nz <- Normalizer$new("z_score", robust = TRUE)
  expect_warning(nz$fit_transform(x), "MAD is zero or not finite in column\\(s\\) 'v'")
  expect_gt(nz$get_state()$s, 0)
})

test_that("robust: constant column -> only the dispersion warning", {
  x  <- cbind(v = rep(3, 5))
  nz <- Normalizer$new("z_score", robust = TRUE)
  expect_warning(nz$fit_transform(x), "Dispersion is zero")
  expect_equal(unname(nz$get_state()$s), 1)
})

# ---------------------------------------------------------------------------
# Input validation / misuse
# ---------------------------------------------------------------------------

test_that("invalid inputs", {
  nz <- Normalizer$new()
  expect_error(nz$fit(NULL), "NULL")
  expect_error(nz$fit(c("a", "b")), "numeric")
  expect_error(nz$fit(c(TRUE, FALSE)), "numeric")
  expect_error(nz$fit(c(NA_real_, Inf)), "No valid")
  expect_error(nz$fit(matrix(numeric(0), nrow = 0, ncol = 2)), "at least one row")
})

test_that("invalid constructor arguments", {
  expect_error(Normalizer$new("foo"))
  expect_error(Normalizer$new("z_score", robust = "yes"), "robust")
  expect_error(Normalizer$new("z_score", robust = NA), "robust")
})

test_that("transform/inverse before fit raise an error", {
  nz <- Normalizer$new()
  expect_false(nz$is_fitted())
  expect_null(nz$get_state())
  expect_error(nz$transform(1:3), "fit")
  expect_error(nz$inverse(1:3), "fit")
})

test_that("different number of columns than at fit time -> error", {
  nz <- Normalizer$new()$fit(cbind(a = 1:4, b = c(2, 4, 6, 9)))
  expect_error(nz$transform(cbind(1:4, 1:4, 1:4)), "columns")
  expect_error(nz$inverse(cbind(1:4, 1:4, 1:4)), "columns")
})

test_that("documented behaviour: columns are matched by position", {
  m  <- cbind(a = c(1, 2, 3, 4), b = c(10, 20, 30, 40))
  nz <- Normalizer$new("min_max")$fit(m)
  swapped <- m[, c("b", "a")]
  # no error when the columns are reordered...
  expect_error(z <- nz$transform(swapped), NA)
  # ...but the parameters are those of the position, not of the name
  expect_equal(unname(z[, 1]), (c(10, 20, 30, 40) - 1) / (4 - 1))
})

test_that("fit() returns the object (chaining)", {
  expect_s3_class(Normalizer$new()$fit(1:3), "Normalizer")
})
