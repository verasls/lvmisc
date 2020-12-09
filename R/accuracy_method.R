#' Model accuracy
#' @export
accuracy <- function(x, na.rm = FALSE) {
  UseMethod("accuracy")
}

#' @rdname accuracy
#' @export
accuracy.default <- function(x, na.rm = FALSE) {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("accuracy", class(x), msg)
}

#' @rdname accuracy
#' @export
accuracy.loocv <- function(x, na.rm = FALSE) {
  MAE <- mean_error_abs(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)
  MAPE <- mean_error_abs_pct(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)
  RMSE <- mean_error_sqr_root(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)

  data.frame(MAE, MAPE, RMSE)
}

#' @rdname accuracy
#' @export
accuracy.lm <- function(x, na.rm = FALSE) {
  formula <- stats::formula(x)
  outcome <- as.character(rlang::f_lhs(formula))
  actual <- x$model[[outcome]]
  predicted <- stats::predict(x)

  R2 <- summary(x)$adj.r.squared
  MAE <- mean_error_abs(actual, predicted, na.rm = na.rm)
  MAPE <- mean_error_abs_pct(actual, predicted, na.rm = na.rm)
  RMSE <- mean_error_sqr_root(actual, predicted, na.rm = na.rm)

  data.frame(R2, MAE, MAPE, RMSE)
}
