#' Computes the percent change
#'
#' \code{percent_change} returns the element-wise percent change between two
#'   numeric vectors.
#'
#' @param baseline,followup A numeric vector with data to compute the percent
#'   change.
#'
#' @return An object of class `lvmisc_percent`
#'
#' @export
#'
#' @seealso \code{\link[=percent]{percent()}},
#' \code{{\link[=error_pct]{error_pct()}}}
#'
#' @examples
#' baseline <- sample(20:40, 10)
#' followup <- baseline * runif(10, min = 0.5, max = 1.5)
#'
#' percent_change(baseline, followup)
percent_change <- function(baseline, followup) {
  if (!is.numeric(baseline)) {
    abort_argument_type("baseline", must = "be numeric", not = baseline)
  }
  if (!is.numeric(followup)) {
    abort_argument_type("followup", must = "be numeric", not = followup)
  }
  if (length(baseline) != length(followup)) {
    abort_argument_diff_length("baseline", "followup")
  }
  percent((followup - baseline) / baseline)
}
