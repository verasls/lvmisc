#' Divide variable based on quantiles
#'
#' Creates a factor based on equally spaced quantiles of a variable.
#'
#' @param data A numeric vector.
#' 
#' @param n An interger specifying the number of levels in the factor to be
#'   created.
#'
#' @param na.rm A logical vector indicating whether the \code{NA} values should
#'   be removed before the quantiles are computed.
#'
#' @export
#'
#' @seealso \code{\link[stats:quantile]{stats::quantile()}}.
#'
#' @examples
#' x <- c(sample(1:20, 9), NA)
#' divide_by_quantile(x, 3)
divide_by_quantile <- function(data, n, na.rm = TRUE) {
  if (!is.numeric(data)) {
    abort_argument_type(arg = "data", must = "be numeric", not = data)
  }
  if (n %% 1 != 0) {
    abort_argument_type(arg = "n", must = "be interger", not = n)
  }
  if (!is.logical(na.rm)) {
    abort_argument_type(arg = "na.rm", must = "be logical", not = na.rm)
  }
  q <- seq(0, 1, length.out = n + 1)
  q <- stats::quantile(data, q, na.rm = na.rm)
  cut(data, q, labels = seq(1, n, 1), include.lowest = TRUE)
}
