#' Plot model diagnostics
#'
#' Plotting functions for some common model diagnostics.
#'
#' @name plot_model
#'
#' @param model An object containing a model.
#'
#' @details \code{plot_model_multicollinearity()} plots a bar chart of the
#'   variance inflation factor (VIF) for each of the model terms.
#'   \code{plot_model_qq()} plots a QQ plot of the model standardized
#'   residuals. \code{plot_model_residual_fitted()} plots the model residuals
#'   versus the fitted values. \code{plot_model_scale_location()} plots the
#'   square root of absolute value of the model residuals versus the fitted
#'   values.
#'
#' @importFrom rlang .data
#'
#' @examples
#' m <- lm(disp ~ mpg + hp + cyl + mpg:cyl, mtcars)
#' plot_model_multicollinearity(m)
#' plot_model_qq(m)
#' plot_model_residual_fitted(m)
#' plot_model_scale_location(m)
NULL

#' @rdname plot_model
#' @export
plot_model_multicollinearity <- function(model) {
  plot_data <- vif(model)
  plot_data$Classification <- factor(
    plot_data$Classification,
    levels = c("Low", "Moderate", "High")
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Term, y = .data$VIF, fill = .data$Classification)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = c("#27ae60", "#f39c12", "#e74c3c")) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = "Multicollinearity",
      x = "Model terms",
      y = "Variance Inflation Factor (VIF)"
    )
}

#' @rdname plot_model
#' @export
plot_model_qq <- function(model) {
  plot_data <- get_standardized_residuals(model)

  ggplot2::ggplot(plot_data, ggplot2::aes(sample = .data$std_res)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(colour = "#2980b9", size = 1) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = "Normality of residuals",
      x = "Teoretical quantiles",
      y = "Standardized residuals"
    )
}

#' @rdname plot_model
#' @export
plot_model_residual_fitted <- function(model) {
  plot_data <- data.frame(
    residual = stats::residuals(model),
    fitted = stats::fitted(model)
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$fitted, y = .data$residual)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      method = "loess", formula = "y ~ x", se = FALSE,
      colour = "#2980b9", size = 1
    ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = "Residual vs. fitted",
      x = "Fitted values",
      y = "Residuals"
    )
}

#' @rdname plot_model
#' @export
plot_model_scale_location <- function(model) {
  residual <- unname(purrr::as_vector(get_standardized_residuals(model)))
  plot_data <- data.frame(
    sqrt_residual = sqrt(abs(residual)),
    fitted = stats::fitted(model)
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$fitted, y = .data$sqrt_residual)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      method = "loess", formula = "y ~ x", se = FALSE,
      colour = "#2980b9", size = 1
    ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = "Scale-location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    )
}

get_standardized_residuals <- function(model) {
  std_res <- stats::residuals(model) /
    (summary(model)[["sigma"]] * sqrt(1 - stats::hatvalues(model)))
  data.frame(std_res)
}
