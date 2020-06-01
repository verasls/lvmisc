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

mean_error_pct <- function(actual, predicted) {
  mean(error_per(actual, predicted))
}

mean_error_abs <- function(actual, predicted) {
  mean(error_abs(actual, predicted))
}

mean_error_abs_pct <- function(actual, predicted) {
  mean(error_abs_per(actual, predicted))
}

mean_error_sqr <- function(actual, predicted) {
  mean(error_sqr(actual, predicted))
}

mean_error_sqr_root <- function(actual, predicted) {
  sqrt(mean_error_sqr(actual, predicted))
}

bias <- function(actual, predicted) {
  mean_error(actual, predicted)
}

loa <- function(actual, predicted) {
  bias <- bias(actual, predicted)
  SD <- sd(error(actual, predicted))
  lower <- bias - 1.96 * SD
  upper  <- bias + 1.96 * SD
  loa <- list(lower = lower, upper = upper)
  loa
}
