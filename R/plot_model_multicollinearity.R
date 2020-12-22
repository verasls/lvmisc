#' Plot multicollinearity
#'
#' Plots a bar chart of the variance inflation factor (VIF) for each of the
#'   model terms.
#'
#' @param model An object containing a model.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' m <- lm(disp ~ mpg + hp + cyl + mpg:cyl, mtcars)
#' plot_model_multicollinearity(m)
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
