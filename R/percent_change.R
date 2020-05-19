percent_change <- function(data, baseline, followup) {
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
  #' @export
  #'
  #' @examples
  #' # Generate data
  #' df <- data.frame(a = sample(20:40, 10))
  #' df$b <- df$a * runif(10, min = 0.5, max = 1.5) 
  #' 
  #' percent_change(df, a, b)
  baseline <- rlang::enquo(baseline)
  followup <- rlang::enquo(followup)
  data <- dplyr::mutate(
    data,
    percent_change = ((!! followup - !! baseline) / !! baseline) * 100
  )
  data
}
