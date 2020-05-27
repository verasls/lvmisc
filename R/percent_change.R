#' Computes the percent change
#'
#' \code{percent_change} returns the row-wise percent change between two
#'   columns in a data frame.
#'
#' @param data The data frame or tibble with the data to be computed.
#'
#' @param baseline,followup The bare (unquoted) names of the columns to be
#'   used to compute the percent change.
#'
#' @return An object of class `lvmisc_percent`
#' 
#' @export
#'
#' @seealso \code{\link[=percent]{percent()}}
#'
#' @examples
#' df <- data.frame(a = sample(20:40, 10))
#' df$b <- df$a * runif(10, min = 0.5, max = 1.5) 
#' 
#' percent_change(df, a, b)
percent_change <- function(data, baseline, followup) {
  baseline <- rlang::enquo(baseline)
  followup <- rlang::enquo(followup)

  if (!is.numeric(rlang::eval_tidy(baseline, data))) {
    abort_argument_type(
      "baseline",
      must = "be numeric",
      not = rlang::eval_tidy(baseline, data)
    )
  }
  if (!is.numeric(rlang::eval_tidy(followup, data))) {
    abort_argument_type(
      "followup",
      must = "be numeric",
      not = rlang::eval_tidy(followup, data)
    )
  }

  data <- dplyr::mutate(
    data,
    percent_change = ((!! followup - !! baseline) / !! baseline),
    percent_change = percent(percent_change)
  )

  data
}
