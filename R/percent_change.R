percent_change <- function(baseline, followup) {
  #' Compute percent change between two vectors.
  #'
  #' @param baseline A numeric vector with baseline values.
  #' @param followup A numeric vector with follow-up values.
  #' 
  #' @return A numeric vector with the percent change.
  #' @export 
  UseMethod("percent_change")
}

percent_change.default <- function(baseline, followup) {
  percent_change <- ((followup - baseline) / baseline) * 100
  percent_change
}

percent_change.data.frame <- function(data, baseline, followup) {
  #' @describeIn percent_change S3 method for data.frame
  #'
  #' @param data A data frame
  #' @param baseline Name of the column with the baseline values
  #' @param followup Name of the column with the follow-up values
  #'
  #' @return The data.frame with a new column with percent change values
  #' @export
  baseline <- rlang::enquo(baseline)
  followup <- rlang::enquo(followup)
  data <- dplyr::mutate(
    data,
    percent_change = ((!! followup - !! baseline) / !! baseline) * 100
  )
  data
}
