#' Center variable
#'
#' Center a variable by subtracting the mean from each element. Centering can
#'   be performed by the grand mean when \code{by = NULL} (the default), or by
#'   group means when \code{by} is a factor variable.
#'
#' @param variable A numeric vector.
#' @param scale A logical vector. If \code{scale = TRUE}, the centered values
#'   of \code{variable} are divided by their standard deviation.
#' @param by A vector with the \code{factor} class.
#'
#' @return A numeric vector.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:20,
#'   group = as.factor(sample(c("A", "B"), 20, replace = TRUE)),
#'   body_mass = rnorm(20, mean = 65, sd = 12)
#' )
#'
#' df$body_mass_centered <- center_variable(df$body_mass, by = df$group)
#' df
center_variable <- function(variable, scale = FALSE, by = NULL) {
  if (!is.numeric(variable)) {
    abort_argument_type("variable", must = "be numeric", not = variable)
  }
  if (!is.logical(scale)) {
    abort_argument_type("scale", must = "be logical", not = scale)
  }
  if (!is.factor(by) & !is.null(by)) {
    abort_argument_type("by", must = "be factor", not = by)
  }

  if (is.null(by)) {
    as.numeric(scale(variable, scale = scale))
  } else {
    as.numeric(variable - tapply(variable, by, mean, na.rm = TRUE)[by])
  }
}

#' Check whether value is outlier
#'
#' \code{is_outlier} returns a logical vector indicating whether a value is an
#' outlier based on the rule of 1.5 times the interquartile range above the
#' third quartile or below the first quartile.
#'
#' @param x A numerical vector
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds. Defaults to \code{FALSE}.
#'
#' @return A logical vector.
#'
#' @export
#'
#' @seealso \code{\link[stats:IQR]{stats::IQR()}},
#'   \code{\link[stats:quantile]{stats::quantile()}}
#'
#' @examples
#' x <- c(1:8, NA, 15)
#' is_outlier(x, na.rm = TRUE)
is_outlier <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort_argument_type("x", must = "be numeric", not = x)
  }
  x < stats::quantile(x, 0.25, na.rm = na.rm) -
    1.5 * stats::IQR(x, na.rm = na.rm) |
  x > stats::quantile(x, 0.75, na.rm = na.rm) +
    1.5 * stats::IQR(x, na.rm = na.rm)
}
