#' Repeat baseline levels
#'
#' Returns a vector with the length equal to the number of rows in the
#'   \code{data} with the baseline value of the \code{var} repeated for every
#'   \code{time} value of each \code{id}.
#'
#' @param data A data frame.
#'
#' @param var The bare (unquoted) name of the column with the values to be
#'   repeated.
#'
#' @param id The bare (unquoted) name of the column that identifies each
#'   subject.
#'
#' @param time The bare (unquoted) name of the column with the time values.
#'
#' @param baseline_level The value of \code{time} corresponding the baseline.
#'
#' @param repeat_NA A logical vector indicating whether or not \code{NA} values
#'   in the \code{var} will correspond to \code{NA} values in return vector.
#'   Defaults to \code{TRUE}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'  id = rep(1:5, each = 4),
#'  time = rep(1:4, 5),
#'  score = rnorm(20, mean = 10, sd = 2)
#' )
#'
#' df$baseline_score <- repeat_baseline_values(df, score, id, time, 1)
repeat_baseline_values <- function(data, 
                                   var, 
                                   id, 
                                   time, 
                                   baseline_level, 
                                   repeat_NA = TRUE) {
  var_col_name <- rlang::as_string(rlang::ensym(var))
  id_col_name <- rlang::as_string(rlang::ensym(id))
  time_col_name <- rlang::as_string(rlang::ensym(time))
  data_name <- rlang::as_string(rlang::ensym(data))

  if (var_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = var_col_name)
  }
  if (id_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = id_col_name)
  }
  if (time_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = time_col_name)
  }

  lookup <- dplyr::filter(data, {{ time }} == baseline_level)
  lookup <- dplyr::select(lookup, {{ id }}, baseline = {{ var }})
  
  df <- dplyr::left_join(data, lookup, by = rlang::as_string(rlang::ensym(id)))
  if (repeat_NA == TRUE) {
    purrr::map2_dbl(
      df[[var_col_name]],
      df[["baseline"]],
      ~ ifelse(is.na(.x), NA, .y)
    )
  } else {
    df[["baseline"]]
  }
}
