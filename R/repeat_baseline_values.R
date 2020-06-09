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
#' @export
repeat_baseline_values <- function(data, var, id, time, baseline_level) {
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
  df[["baseline"]]
}
