#' Center variable
#'
#' Center a variable by subtracting the mean from each element. Centering can
#'   be performed by the grand mean when \code{by = NULL} (the default), or by
#'   group means when \code{by} is a factor variable.
#'
#' @param variable A numeric vector.
#'
#' @param by A vector with the \code{factor} class.
#'
#' @param scale A logical vector. If \code{scale = TRUE}, the centered values
#'   of \code{variable} are divided by their standard deviation.
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
#' df$body_mass_centered <- center_variable(df$body_mass, df$group)
center_variable <- function(variable, by = NULL, scale = FALSE) {
  if (!is.numeric(variable)) {
    abort_argument_type("variable", must = "be numeric", not = variable)
  }
  if (!is.factor(by) & !is.null(by)) {
    abort_argument_type("by", must = "be factor", not = by)
  }
  if (!is.logical(scale)) {
    abort_argument_type("scale", must = "be logical", not = scale)
  } 
  
  if (is.null(by)) {
    as.numeric(scale(variable, scale = scale))
  } else {
    as.numeric(variable - tapply(variable, by, mean, na.rm = TRUE)[by])
  }
}
