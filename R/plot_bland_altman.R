#' Create a Bland-Altman plot
#'
#' Create a Bland-Altman plot as described by
#' \href{=https://bit.ly/3mbf8bL}{Bland & Altman (1986)}.
#'
#' @param x An object of class \code{lvmisc_cv} or an object containing a model.
#' @param ... Additional arguments to be passed to \code{ggplot2::aes()}.
#'
#' @export
#'
#' @examples
#' mtcars <- tibble::as_tibble(mtcars, rownames = "car")
#' m <- stats::lm(disp ~ mpg, mtcars)
#' cv <- loo_cv(m, mtcars, car)
#' plot_bland_altman(cv, colour = as.factor(am))
plot_bland_altman <- function(x, ...) {
  data <- model_data(x)
  plot_data <- data$model_data
  bias <- data$bias
  lower_loa <- data$loa$lower
  upper_loa <- data$loa$upper

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(ggplot2::aes(x = mean, y = diff, ...)) +
    ggplot2::geom_hline(yintercept = bias) +
    ggplot2::geom_hline(yintercept = lower_loa, linetype = "longdash") +
    ggplot2::geom_hline(yintercept = upper_loa, linetype = "longdash")
}

model_data <- function(x) {
 UseMethod("model_data")
}

model_data.default <- function(x) {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("model_data", class(x), msg)
}

model_data.lvmisc_cv <- function(x) {
  check_args_model_data(x)

  mean <- (x[[".actual"]] + x[[".predicted"]]) / 2
  diff <- x[[".actual"]] - x[[".predicted"]]

  bias <- bias(x[[".actual"]], x[[".predicted"]], na.rm = TRUE)
  loa <- loa(x[[".actual"]], x[[".predicted"]], na.rm = TRUE)

  list(
    model_data = cbind(x, mean, diff),
    bias = bias, loa = loa
  )
}

model_data.lm <- function(x) {
  check_args_model_data(x)

  formula <- stats::formula(x)
  outcome <- as.character(rlang::f_lhs(formula))

  actual <- x$model[[outcome]]
  predicted <- stats::predict(x)
  mean <- (actual + predicted) / 2
  diff <- actual - predicted

  bias <- bias(actual, predicted, na.rm = TRUE)
  loa <- loa(actual, predicted, na.rm = TRUE)

  list(
    model_data = tibble::tibble(mean, diff),
    bias = bias, loa = loa
  )
}

model_data.lmerMod <- function(x) {
  check_args_model_data(x)

  formula <- stats::formula(x)
  outcome <- as.character(rlang::f_lhs(formula))

  actual <- stats::model.frame(x)[[outcome]]
  predicted <- stats::predict(x)
  mean <- (actual + predicted) / 2
  diff <- actual - predicted

  bias <- bias(actual, predicted, na.rm = TRUE)
  loa <- loa(actual, predicted, na.rm = TRUE)

  list(
    model_data = tibble::tibble(mean, diff),
    bias = bias, loa = loa
  )
}

check_args_model_data <- function(x) {
  if ("lvmisc_cv" %!in% class(x) & length(class(x)) > 1) {
    classes <- class(x)[class(x) %!in% c("lm", "lmerMod")]
    msg <- glue::glue(
      "If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    )
    abort_no_method_for_class("model_data", classes, msg)
  }
}
