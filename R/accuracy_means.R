#' Params for the accuracy indices summary functions
#' 
#' @name accuracy_means_params
#'
#' @param actual A numeric vector with the actual values
#' 
#' @param predicted A numeric vector with the predicted values. Each element in
#'   this vector must be a prediction for the corresponding element in 
#'   \code{actual}.
#'
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#'
#' @keywords internal
NULL

#' Mean error
#'
#' Computes the average error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @export
#'
#' @seealso \code{\link[=mean_error_pct]{mean_error_pct()}}, 
#'   \code{\link[=mean_error_abs]{mean_error_abs()}},
#'   \code{\link[=mean_error_abs_pct]{mean_error_abs_pct()}},
#'   \code{\link[=mean_error_sqr]{mean_error_sqr()}},
#'   \code{\link[=mean_error_sqr_root]{mean_error_sqr_root()}}
mean_error <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error(actual, predicted), na.rm = na.rm)
}

#' Mean percent error
#'
#' Computes the average percent error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @return A vector of the class `lvmisc_percent`.
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}}, 
#'   \code{\link[=mean_error_abs]{mean_error_abs()}},
#'   \code{\link[=mean_error_abs_pct]{mean_error_abs_pct()}},
#'   \code{\link[=mean_error_sqr]{mean_error_sqr()}},
#'   \code{\link[=mean_error_sqr_root]{mean_error_sqr_root()}}
mean_error_pct <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_pct(actual, predicted), na.rm = na.rm)
}

#' Mean absolute error
#'
#' Computes the average absolute error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}}, 
#'   \code{\link[=mean_error_pct]{mean_error_pct()}},
#'   \code{\link[=mean_error_abs_pct]{mean_error_abs_pct()}},
#'   \code{\link[=mean_error_sqr]{mean_error_sqr()}},
#'   \code{\link[=mean_error_sqr_root]{mean_error_sqr_root()}}
mean_error_abs <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_abs(actual, predicted), na.rm = na.rm)
}

#' Mean absolute percent error
#'
#' Computes the average absolute percent error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @return A vector of the class `lvmisc_percent`.
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}}, 
#'   \code{\link[=mean_error_abs]{mean_error_abs()}},
#'   \code{\link[=mean_error_pct]{mean_error_pct()}},
#'   \code{\link[=mean_error_sqr]{mean_error_sqr()}},
#'   \code{\link[=mean_error_sqr_root]{mean_error_sqr_root()}}
mean_error_abs_pct <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_abs_pct(actual, predicted), na.rm = na.rm)
}

#' Mean square error
#'
#' Computes the average square error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}}, 
#'   \code{\link[=mean_error_abs]{mean_error_abs()}},
#'   \code{\link[=mean_error_pct]{mean_error_pct()}},
#'   \code{\link[=mean_error_abs_pct]{mean_error_abs_pct()}},
#'   \code{\link[=mean_error_sqr_root]{mean_error_sqr_root()}}
mean_error_sqr <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_sqr(actual, predicted), na.rm = na.rm)
}

#' Root mean square error
#'
#' Computes the root mean square error between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}}, 
#'   \code{\link[=mean_error_abs]{mean_error_abs()}},
#'   \code{\link[=mean_error_pct]{mean_error_pct()}},
#'   \code{\link[=mean_error_abs_pct]{mean_error_abs_pct()}},
#'   \code{\link[=mean_error_sqr]{mean_error_sqr()}}
mean_error_sqr_root <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  sqrt(mean_error_sqr(actual, predicted, na.rm = na.rm))
}

#' Bias
#'
#' Computes the bias (mean error) between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}},
#'   \code{\link[=loa]{loa()}}
bias <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean_error(actual, predicted, na.rm = na.rm)
}

#' Limits of agreement
#'
#' Computes the Bland-Altman limits of agreement between the input vectors.
#'
#' @inheritParams accuracy_means_params
#'
#' @return A vector of the class `lvmisc_percent`.
#'
#' @export
#'
#' @seealso \code{\link[=mean_error]{mean_error()}},
#'   \code{\link[=bias]{bias()}}
loa <- function(actual, predicted, na.rm = TRUE) {
  if(!is.numeric(actual)) {
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
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
  if (length(actual) != length(predicted)) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  bias <- bias(actual, predicted, na.rm = na.rm)
  SD <- stats::sd(error(actual, predicted), na.rm = na.rm)
  lower <- bias - 1.96 * SD
  upper  <- bias + 1.96 * SD
  loa <- list(lower = lower, upper = upper)
  loa
}
