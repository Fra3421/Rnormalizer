#' @title Stateful Column-Wise Normalizer for Forecasting and Modeling Pipelines
#' @name Normalizer
#'
#' @description
#' An [R6::R6Class] component providing stateful data normalization and
#' inverse transformation (`$fit()`, `$transform()`, `$inverse()`, and
#' `$fit_transform()`) for numeric vectors and matrices (applied column-wise
#' on matrices), supporting z-score / robust scaling and min-max scaling.
#'
#' @details
#' `Normalizer` stores fitted scaling parameters internally (`private$.state`,
#' inspectable via `$get_state()`) after `$fit()` or `$fit_transform()`:
#' \itemize{
#'   \item **Z-score / Robust scaling (`method = "z_score"`):** standardizes
#'     each column using column mean and standard deviation (`robust = FALSE`)
#'     or column median and MAD (`robust = TRUE`), falling back to standard
#'     deviation when MAD is non-finite/zero and setting zero/non-finite
#'     dispersion to `1` with a warning.
#'   \item **Min-max scaling (`method = "min_max"`):** rescales each column
#'     using observed column minimum and maximum (`lo` and `hi`), setting range
#'     to `1` with a warning when a column has zero range.
#' }
#'
#' **Valid and missing values.** `NA`, `NaN` and `+/-Inf` are treated as
#' missing: they are ignored when fitting and returned as `NA` in the output.
#' A column with no finite values yields `NA` for all its entries; an input
#' with no finite value at all raises an error. Warnings identify the
#' affected columns by name (or by index if the matrix has no column names).
#'
#' **Columns are matched by position, not by name.** The fitted parameters
#' are stored per column index. `$transform()` and `$inverse()` only check that
#' the number of columns equals the one seen at fit time; column names are not
#' compared. The input must therefore keep the same column order used in
#' `$fit()`, otherwise the result is silently wrong.
#'
#' **Shape.** A plain vector is treated as a single column and returned as a
#' vector; a matrix (including a one-column matrix) is returned as a matrix.
#'
#' @return An `R6` object of class `"Normalizer"`.
#'
#' @importFrom R6 R6Class
#' @importFrom matrixStats colMeans2 colSds colMedians colMads colMins colMaxs
#' @export
Normalizer <- R6::R6Class(
  "Normalizer",

  private = list(
    .method = "z_score",
    .robust = FALSE,
    .state = NULL,
    .was_vector = FALSE,

    # Validate a numeric matrix and return it as double with every non-finite value converted to NA.
    check_matrix = function(x) {
      if (is.null(x)) stop("Cannot normalize/de-normalize a NULL matrix.", call. = FALSE)

      x <- if (!is.matrix(x)) as.matrix(x) else x
      if (!is.numeric(x)) stop("'x' must be a numeric matrix. Cannot normalize/de-normalize.", call. = FALSE)
      if (ncol(x) == 0L || nrow(x) == 0L) stop("'x' must have at least one row and one column. Cannot normalize/de-normalize.", call. = FALSE)

      storage.mode(x) <- "double"
      x[!is.finite(x)] <- NA_real_
      if (all(is.na(x))) stop("No valid (finite) values in matrix 'x'. Cannot normalize/de-normalize.", call. = FALSE)
      return(x)
    },

    # Validate the attributes list for apply_* and from_* functions
    check_attr = function(x, attr, fields) {
      if (!is.list(attr) || !all(fields %in% names(attr))) stop(sprintf("'attr' must be a list containing: %s.", paste(fields, collapse = ", ")), call. = FALSE)
      if (length(attr[[fields[1]]]) != ncol(x)) stop(sprintf("'x' has %d columns but the fitted attributes have %d elements.", ncol(x), length(attr[[fields[1]]])), call. = FALSE)
      invisible(TRUE)
    },

    # Readable labels for columns idx: 'name' when available, otherwise the index.
    col_labels = function(x, idx) {
      lab <- as.character(idx)
      cn <- colnames(x)
      if (!is.null(cn)) {
        nm <- cn[idx]
        ok <- !is.na(nm) & nzchar(nm)
        lab[ok] <- sprintf("'%s'", nm[ok])
      }
      paste(lab, collapse = ", ")
    },

    # Compute z-score or robust (median/MAD) standardization and return the scaling parameters
    to_z_score = function(x) {
      all_na <- colSums(!is.na(x)) == 0L

      if (private$.robust) {
        mu <- matrixStats::colMedians(x, na.rm = TRUE)
        s  <- matrixStats::colMads(x, center = mu, na.rm = TRUE)

        # Fallback to stand. dev. where MAD is unusable and the sd is valid
        bad <- which(!all_na & (!is.finite(s) | s <= 0))
        if (length(bad) > 0L) {
          s_sd <- matrixStats::colSds(x[, bad, drop = FALSE], na.rm = TRUE)
          fb   <- is.finite(s_sd) & s_sd > 0
          if (any(fb)) {
            warning(sprintf("MAD is zero or not finite in column(s) %s; falling back to the standard deviation.",
                            private$col_labels(x, bad[fb])), call. = FALSE)
            s[bad[fb]] <- s_sd[fb]
          }
        }
      } else {
        mu <- matrixStats::colMeans2(x, na.rm = TRUE)
        s  <- matrixStats::colSds(x, na.rm = TRUE)
      }

      bad <- which(!all_na & (!is.finite(s) | s <= 0))
      if (length(bad) > 0L) {
        warning(sprintf("Dispersion is zero or not computable (constant or single-value series) in column(s) %s. Scale set to 1 to avoid zero division.",
                        private$col_labels(x, bad)), call. = FALSE)
        s[bad] <- 1
      }

      # All-NA columns stay NA
      mu[all_na] <- NA_real_
      s[all_na]  <- NA_real_

      z <- sweep(sweep(x, 2, mu, "-"), 2, s, "/")
      list(x_norm = z, attr = list(mu = mu, s = s))
    },

    # Apply pre-computed z-score parameters to a numeric matrix.
    apply_z_score = function(x, attr) {
      private$check_attr(x, attr, c("mu", "s"))
      z <- sweep(sweep(x, 2, attr$mu, "-"), 2, attr$s, "/")
      return(z)
    },

    # Invert z-score standardization.
    from_z_score = function(z, attr) {
      private$check_attr(z, attr, c("mu", "s"))
      x <- sweep(sweep(z, 2, attr$s, "*"), 2, attr$mu, "+")
      return(x)
    },

    # Min-max normalization using the observed minimum and maximum.
    to_min_max = function(x) {
      mn <- matrixStats::colMins(x, na.rm = TRUE)
      mx <- matrixStats::colMaxs(x, na.rm = TRUE)

      all_na <- !is.finite(mn) | !is.finite(mx)
      mn[all_na] <- NA_real_
      mx[all_na] <- NA_real_

      const <- which(!all_na & (mx - mn <= 0))
      if (length(const) > 0L) {
        warning(sprintf("Constant series in column(s) %s: min-max range set to 1 (values are only shifted).",
                        private$col_labels(x, const)), call. = FALSE)
        mx[const] <- mn[const] + 1
      }

      z <- sweep(sweep(x, 2, mn, "-"), 2, mx - mn, "/")
      list(x_norm = z, attr = list(lo = mn, hi = mx))
    },

    # Apply pre-computed min-max bounds.
    apply_min_max = function(x, attr) {
      private$check_attr(x, attr, c("lo", "hi"))
      z <- sweep(sweep(x, 2, attr$lo, "-"), 2, attr$hi - attr$lo, "/")
      return(z)
    },

    # Invert min-max normalization.
    from_min_max = function(z, attr) {
      private$check_attr(z, attr, c("lo", "hi"))
      x <- sweep(sweep(z, 2, attr$hi - attr$lo, "*"), 2, attr$lo, "+")
      return(x)
    },

    fit_matrix = function(x) {
      switch(private$.method,
             z_score  = private$to_z_score(x),
             min_max  = private$to_min_max(x))
    },

    transform_matrix = function(x, attr) {
      switch(private$.method,
             z_score  = private$apply_z_score(x, attr),
             min_max  = private$apply_min_max(x, attr))
    },

    inverse_matrix = function(x, attr) {
      switch(private$.method,
             z_score  = private$from_z_score(x, attr),
             min_max  = private$from_min_max(x, attr))
    }
  ),

  public = list(
    #' @description Initialize a `Normalizer`.
    #' @param method Method used by the API: `"z_score"` (default) or `"min_max"`.
    #' @param robust Logical; for `"z_score"` use median/MAD instead of mean/standard deviation (default `FALSE`).
    #' @return Invisibly returns `self`.
    initialize = function(method = c("z_score", "min_max"), robust = FALSE) {
      if (!is.logical(robust) || length(robust) != 1L || is.na(robust)) stop("'robust' must be TRUE or FALSE.", call. = FALSE)

      private$.method <- match.arg(method)
      private$.robust <- robust

      invisible(self)
    },

    #' @description Fit the normalizer on a numeric vector or matrix
    #'   (column-wise for matrices) and store the parameters internally.
    #' @param x A numeric vector or numeric matrix.
    #' @return Invisibly returns `self` (allows chaining).
    fit = function(x) {
      was_vector <- is.null(dim(x))
      x <- private$check_matrix(x)

      res <- private$fit_matrix(x)
      private$.state      <- res$attr
      private$.was_vector <- was_vector   # set only after a successful fit

      invisible(self)
    },

    #' @description Transform new data with the fitted parameters.
    #' @param x A numeric vector (if fitted on a vector) or a numeric matrix
    #'   with the same number and order of columns used at fit time (columns
    #'   are matched by position; names are not checked).
    #' @return Normalized data, same shape as `x`.
    transform = function(x) {
      if (!self$is_fitted()) stop("Must call 'fit' method before 'transform'", call. = FALSE)
      x <- private$check_matrix(x)

      z <- private$transform_matrix(x, private$.state)
      z <- if (private$.was_vector) as.vector(z) else z
      return(z)
    },

    #' @description Map normalized data back to the original scale.
    #' @param z Normalized data with the same shape and column order as the
    #'   output of `$transform()`.
    #' @return Data on the original scale.
    inverse = function(z) {
      if (!self$is_fitted()) stop("Must call 'fit' method before 'inverse'", call. = FALSE)
      z <- private$check_matrix(z)

      x <- private$inverse_matrix(z, private$.state)
      x <- if (private$.was_vector) as.vector(x) else x

      return(x)
    },

    #' @description Fit on `x` and return its normalized version.
    #' @param x A numeric vector or matrix.
    #' @return Normalized `x`.
    fit_transform = function(x) {
      was_vector <- is.null(dim(x))
      x <- private$check_matrix(x)

      res <- private$fit_matrix(x)
      private$.state      <- res$attr
      private$.was_vector <- was_vector   # set only after a successful fit

      z <- if (was_vector) as.vector(res$x_norm) else res$x_norm
      return(z)
    },

    #' @description Whether `$fit()` has been called.
    #' @return Logical scalar.
    is_fitted = function() !is.null(private$.state),

    #' @description Return the fitted parameters.
    #' @return The attributes or `NULL` if not fitted.
    get_state = function() {
      if (is.null(private$.state)) NULL else private$.state
    }
  )
)
