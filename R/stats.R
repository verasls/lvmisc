#' Check whether value is outlier
#' 
#' \code{is_outlier} returns a logical vector indicating whether a value is an
#' outlier based on the rule of 1.5 times the interquartile range above the
#' third quartile or below the first quartile.
#' 
#' @param x A numerical vector
#' 
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
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
