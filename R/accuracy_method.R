#' Model accuracy
#'
#' Computes some common model accuracy indices, such as the R squared, mean
#'   absolute error, mean absolute percent error and root mean square error.
#'
#' @param x An object of class \code{lvmisc_cv} or an object containing a model.
#' @param na.rm A logical value indicating whether or not to strip \code{NA}
#'   values to compute the indices. Defaults to \code{FALSE}.
#'
#' @return An object of class \code{lvmisc_accuracy}. See "Details" for more
#'   information.
#'
#' @details The method for the \code{lm} class (or for the \code{lvmisc_cv}
#'   class of a \code{lm}) returns a data frame with the columns \code{AIC}
#'   (Akaike information criterion), \code{BIC} (Bayesian information
#'   criterion), \code{R2} (R squared), \code{R2_adj} (adjusted R squared),
#'   \code{MAE} (mean absolute error), \code{MAPE} (mean absolute percent
#'   error) and \code{RMSE} (root mean square error).
#'
#'   The method for the \code{lmerMod} (or for the \code{lvmisc_cv} class of a
#'   \code{lmerMod}) returns a data frame with the columns \code{R2_marg} and
#'   \code{R2_cond} instead of the columns \code{R2} and \code{R2_adj}.
#'   All the other columns are the same as the method for \code{lm}.
#'   \code{R2_marg} is the marginal R squared, which considers only the variance
#'   by the fixed effects of a mixed model, and \code{R2_cond} is the
#'   conditional R squared, which considers both fixed and random effects
#'   variance.
#'
#' @export
#'
#' @examples
#' mtcars <- tibble::as_tibble(mtcars, rownames = "car")
#' m <- stats::lm(disp ~ mpg, mtcars)
#' cv <- loo_cv(m, mtcars, car, keep = "used")
#'
#' accuracy(m)
#' accuracy(cv)
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
accuracy.lvmisc_cv <- function(x, na.rm = FALSE) {
  model <- attributes(x)$lvmisc_cv_model
  model_class <- paste("lvmisc_cv_model", class(model), sep = "/")

  check_args_accuracy(x, na.rm)

  AIC <- stats::AIC(model)
  BIC <- stats::BIC(model)
  R2 <- get_r2(model)
  MAE <- mean_error_abs(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)
  MAPE <- mean_error_abs_pct(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)
  RMSE <- mean_error_sqr_root(x[[".actual"]], x[[".predicted"]], na.rm = na.rm)

  accuracy_data <- round(data.frame(AIC, BIC, R2, MAE, MAPE, RMSE), 2)
  new_lvmisc_accuracy(accuracy_data, model_class)
}

#' @rdname accuracy
#' @export
accuracy.lm <- function(x, na.rm = FALSE) {
  model_class <- class(x)
  check_args_accuracy(x, na.rm)

  formula <- stats::formula(x)
  outcome <- as.character(rlang::f_lhs(formula))
  actual <- x$model[[outcome]]
  predicted <- stats::predict(x)

  AIC <- stats::AIC(x)
  BIC <- stats::BIC(x)
  R2 <- summary(x)$r.squared
  R2_adj <- summary(x)$adj.r.squared
  MAE <- mean_error_abs(actual, predicted, na.rm = na.rm)
  MAPE <- mean_error_abs_pct(actual, predicted, na.rm = na.rm)
  RMSE <- mean_error_sqr_root(actual, predicted, na.rm = na.rm)

  accuracy_data <- round(data.frame(AIC, BIC, R2, R2_adj, MAE, MAPE, RMSE), 2)
  new_lvmisc_accuracy(accuracy_data, model_class)
}

#' @rdname accuracy
#' @export
accuracy.lmerMod <- function(x, na.rm = FALSE) {
  model_class <- class(x)
  attr(model_class, "package") <- NULL
  check_args_accuracy(x, na.rm)

  formula <- stats::formula(x)
  outcome <- as.character(rlang::f_lhs(formula))
  actual <- stats::model.frame(x)[[outcome]]
  predicted <- stats::predict(x)

  AIC <- stats::AIC(x)
  BIC <- stats::BIC(x)
  R2_marg <- r2(x)[["R2_marg"]]
  R2_cond <- r2(x)[["R2_cond"]]
  MAE <- mean_error_abs(actual, predicted, na.rm = na.rm)
  MAPE <- mean_error_abs_pct(actual, predicted, na.rm = na.rm)
  RMSE <- mean_error_sqr_root(actual, predicted, na.rm = na.rm)

  accuracy_data <- round(
    data.frame(AIC, BIC, R2_marg, R2_cond, MAE, MAPE, RMSE), 2
  )
  new_lvmisc_accuracy(accuracy_data, model_class)
}

check_args_accuracy <- function(x, na.rm) {
  if ("lvmisc_cv" %!in% class(x) & length(class(x)) > 1) {
    classes <- class(x)[class(x) %!in% c("lm", "lmerMod")]
    msg <- glue::glue(
      "If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    )
    abort_no_method_for_class("accuracy", classes, msg)
  }
  if (!is.logical(na.rm)) {
    abort_argument_type(
      arg = "na.rm",
      must = "be logical",
      not = na.rm
    )
  }
}

get_r2 <- function(model) {
  if (class(model) == "lm") {
    R2 <- summary(model)$r.squared
    R2_adj <- summary(model)$adj.r.squared
    data.frame(R2, R2_adj)
  } else if (class(model) == "lmerMod") {
    R2_marg <- r2(model)[["R2_marg"]]
    R2_cond <- r2(model)[["R2_cond"]]
    data.frame(R2_marg, R2_cond)
  }
}

#' Constructor for lvmisc_accuracy object
#'
#' @param accuracy_data A data frame with accuracy indices.
#' @param model_class The class of the model.
#' @keywords internal
new_lvmisc_accuracy <- function(accuracy_data, model_class) {
  stopifnot(is.data.frame(accuracy_data))
  structure(
    accuracy_data,
    model_class = model_class,
    rownames = NULL,
    class = c("lvmisc_accuracy", "data.frame")
  )
}
