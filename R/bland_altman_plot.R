bland_altman_plot <- function(x, ...) {
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

model_data.lvmisc_cv <- function(x) {
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
