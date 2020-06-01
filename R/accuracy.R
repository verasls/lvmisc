error <- function(actual, predicted) {
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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  actual - predicted
}

error_pct <- function(actual, predicted) {
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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  as_percent(error(actual, predicted) / actual)
}

error_abs <- function(actual, predicted) {
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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  abs(error(actual, predicted))
}

error_abs_pct <- function(actual, predicted) {
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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  as_percent(error_abs(actual, predicted) / abs(actual))
}

error_sqr <- function(actual, predicted) {
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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  error(actual, predicted) ^ 2
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error(actual, predicted), na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_pct(actual, predicted), na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_abs(actual, predicted), na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_abs_pct(actual, predicted), na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean(error_sqr(actual, predicted), na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  sqrt(mean_error_sqr(actual, predicted, na.rm = na.rm))
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  mean_error(actual, predicted, na.rm = na.rm)
}

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
  if (!(length(actual) == length(predicted))) {
    abort_argument_diff_length(
      arg1 = "actual",
      arg2 = "predicted"
    )
  }
  bias <- bias(actual, predicted, na.rm = na.rm)
  SD <- sd(error(actual, predicted), na.rm = na.rm)
  lower <- bias - 1.96 * SD
  upper  <- bias + 1.96 * SD
  loa <- list(lower = lower, upper = upper)
  loa
}
