#' Params for the accuracy indices functions
#'
#' @name accuracy_params
#'
#' @param actual A numeric vector with the actual values
#' @param predicted A numeric vector with the predicted values. Each element in
#'   this vector must be a prediction for the corresponding element in
#'   \code{actual}.
#'
#' @keywords internal
NULL

#' Error
#'
#' Computes the element-wise error between the input vectors.
#'
#' @inheritParams accuracy_params
#'
#' @return Returns a double vector with the element-wise error values.
#'
#' @export
#'
#' @seealso \code{\link[=error_pct]{error_pct()}},
#'   \code{\link[=error_abs]{error_abs()}},
#'   \code{\link[=error_abs_pct]{error_abs_pct()}},
#'   \code{\link[=error_sqr]{error_sqr()}}.
#'
#' @examples
#' actual <- runif(10)
#' predicted <- runif(10)
#'
#' error(actual, predicted)
error <- function(actual, predicted) {
  if (!is.numeric(actual)) {
    abort_argument_type(
      arg = "actual",
      must = "be numeric",
      not = actual
    )
  }
  if (!is.numeric(predicted)) {
    abort_argument_type(
      arg = "predicted",
      must = "be numeric",
      not = predicted
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  actual - predicted
}

#' Percent error
#'
#' Computes the element-wise percent error between the input vectors.
#'
#' @inheritParams accuracy_params
#'
#' @return Returns a double vector with the element-wise percent error values.
#'
#' @return A vector of the class \code{lvmisc_percent} with the element-wise
#'   percent error values.
#'
#' @export
#'
#' @seealso \code{\link[=error]{error()}},
#'   \code{\link[=error_abs]{error_abs()}},
#'   \code{\link[=error_abs_pct]{error_abs_pct()}},
#'   \code{\link[=error_sqr]{error_sqr()}}.
#'
#' @examples
#' actual <- runif(10)
#' predicted <- runif(10)
#'
#' error_pct(actual, predicted)
error_pct <- function(actual, predicted) {
  if (!is.numeric(actual)) {
    abort_argument_type(
      arg = "actual",
      must = "be numeric",
      not = actual
    )
  }
  if (!is.numeric(predicted)) {
    abort_argument_type(
      arg = "predicted",
      must = "be numeric",
      not = predicted
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  as_percent(error(actual, predicted) / actual)
}

#' Absolute error
#'
#' Computes the element-wise absolute errors between the input vectors.
#'
#' @inheritParams accuracy_params
#'
#' @return Returns a double vector with the element-wise absolute error values.
#'
#' @export
#'
#' @seealso \code{\link[=error]{error()}},
#'   \code{\link[=error_pct]{error_pct()}},
#'   \code{\link[=error_abs_pct]{error_abs_pct()}},
#'   \code{\link[=error_sqr]{error_sqr()}}.
#'
#' @examples
#' actual <- runif(10)
#' predicted <- runif(10)
#'
#' error_abs(actual, predicted)
error_abs <- function(actual, predicted) {
  if (!is.numeric(actual)) {
    abort_argument_type(
      arg = "actual",
      must = "be numeric",
      not = actual
    )
  }
  if (!is.numeric(predicted)) {
    abort_argument_type(
      arg = "predicted",
      must = "be numeric",
      not = predicted
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  abs(error(actual, predicted))
}

#' Absolute percent error
#'
#' Computes the element-wise absolute percent errors between the input vectors.
#'
#' @inheritParams accuracy_params
#'
#' @return Returns a double vector with the element-wise absolute percent
#'   error values.
#'
#' @return A vector of the class \code{lvmisc_percent} with the element-wise
#'   absolute percent error values.
#'
#' @export
#'
#' @seealso \code{\link[=error]{error()}},
#'   \code{\link[=error_pct]{error_pct()}},
#'   \code{\link[=error_abs]{error_abs()}},
#'   \code{\link[=error_sqr]{error_sqr()}}.
#'
#' @examples
#' actual <- runif(10)
#' predicted <- runif(10)
#'
#' error_abs_pct(actual, predicted)
error_abs_pct <- function(actual, predicted) {
  if (!is.numeric(actual)) {
    abort_argument_type(
      arg = "actual",
      must = "be numeric",
      not = actual
    )
  }
  if (!is.numeric(predicted)) {
    abort_argument_type(
      arg = "predicted",
      must = "be numeric",
      not = predicted
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  as_percent(error_abs(actual, predicted) / abs(actual))
}

#' Squared error
#'
#' Computes the element-wise squared errors between the input vectors.
#'
#' @inheritParams accuracy_params
#'
#' @return Returns a double vector with the element-wise squared error values.
#'
#' @export
#'
#' @seealso
#' @seealso \code{\link[=error]{error()}},
#'   \code{\link[=error_pct]{error_pct()}},
#'   \code{\link[=error_abs]{error_abs()}},
#'   \code{\link[=error_abs_pct]{error_abs_pct()}}.
#'
#' @examples
#' actual <- runif(10)
#' predicted <- runif(10)
#'
#' error_sqr(actual, predicted)
error_sqr <- function(actual, predicted) {
  if (!is.numeric(actual)) {
    abort_argument_type(
      arg = "actual",
      must = "be numeric",
      not = actual
    )
  }
  if (!is.numeric(predicted)) {
    abort_argument_type(
      arg = "predicted",
      must = "be numeric",
      not = predicted
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  error(actual, predicted) ^ 2
}
