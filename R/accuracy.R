error <- function(actual, predicted) {
  actual - predicted
}

error_pct <- function(actual, predicted) {
  error(actual, predicted) / actual
}

error_abs <- function(actual, predicted) {
  abs(error(actual, predicted))
}

error_abs_pct <- function(actual, predicted) {
  error_abs(actual, predicted) / abs(actual)
}

error_sqr <- function(actual, predicted) {
  error(actual, predicted) ^ 2
}

mean_error <- function(actual, predicted) {
  mean(error(actual, predicted))
}

mean_error_pct <- function(actual, predicted, na.rm = TRUE) {
  mean(error_per(actual, predicted), na.rm = na.rm)
}

mean_error_abs <- function(actual, predicted, na.rm = TRUE) {
  mean(error_abs(actual, predicted), na.rm = na.rm)
}

mean_error_abs_pct <- function(actual, predicted, na.rm = TRUE) {
  mean(error_abs_per(actual, predicted), na.rm = na.rm)
}

mean_error_sqr <- function(actual, predicted, na.rm = TRUE) {
  mean(error_sqr(actual, predicted), na.rm = na.rm)
}

mean_error_sqr_root <- function(actual, predicted, na.rm = TRUE) {
  sqrt(mean_error_sqr(actual, predicted, na.rm = na.rm))
}

bias <- function(actual, predicted, na.rm = TRUE) {
  mean_error(actual, predicted, na.rm = na.rm)
}

loa <- function(actual, predicted, na.rm = TRUE) {
  bias <- bias(actual, predicted, na.rm = na.rm)
  SD <- sd(error(actual, predicted), na.rm = na.rm)
  lower <- bias - 1.96 * SD
  upper  <- bias + 1.96 * SD
  loa <- list(lower = lower, upper = upper)
  loa
}
