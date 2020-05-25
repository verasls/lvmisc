#' Computes the percent change
#'
#' \code{percent_change} returns the row-wise percent change between two
#'   columns in a data frame.
#'
#' @param data The data frame or tibble with the data to be computed.
#'
#' @param baseline,followup The bare (unquoted) name of the columns to be
#'   used to compute the percent change.
#'
#' @param round The number of decimal places to round. If NULL (default),
#'   rounding will not be applied.
#'
#' @export
#'
#' @examples
#' # Generate data
#' df <- data.frame(a = sample(20:40, 10))
#' df$b <- df$a * runif(10, min = 0.5, max = 1.5) 
#' 
#' percent_change(df, a, b, round = 2)
percent_change <- function(data, baseline, followup, round = NULL) {
  baseline <- rlang::enquo(baseline)
  followup <- rlang::enquo(followup)
  
  if (!is.numeric(rlang::eval_tidy(baseline, data))) {
    not <- typeof(rlang::eval_tidy(baseline, data))
    msg <- glue::glue("`baseline` must be numeric; not {not}.")
    stop(msg, call. = FALSE)
  }
  if (!is.numeric(rlang::eval_tidy(followup, data))) {
    not <- typeof(rlang::eval_tidy(followup, data))
    msg <- glue::glue("`followup` must be numeric; not {not}.")
    stop(msg, call. = FALSE)
  }

  data <- dplyr::mutate(
    data,
    percent_change = ((!! followup - !! baseline) / !! baseline) * 100
  )
  if (!is.null(round)) {
    if (!is.numeric(round)) {
     not <- typeof(round)
     msg <- glue::glue("`round` must be numeric; not {not}.")
     stop(msg, call. = FALSE)
    }
    data <- dplyr::mutate(
      data,
      percent_change = round(percent_change, round)
    )
  }
  data
}
