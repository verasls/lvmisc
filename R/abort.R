#' Abort based on the argument type
#'
#' @param arg A character string with the argument name.
#'
#' @param must A character string specifying which type the arg must have.
#' 
#' @param not The argument name (unquoted). The function evaluates the type of
#'   the argument and displays it in the error message.
#' 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' x <- letters
#' if (!is.numeric(x)) {
#'   abort_argument_type("x", must = "be numeric", not = x)
#' }
#' }
abort_argument_type <- function(arg, must, not) {
  not <- typeof(not)
  msg <- glue::glue("`{arg}` must {must}; not {not}.")

  rlang::abort(
    "error_argument_type",
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}

#' Abort based on the argument length
#'
#' @param arg A character string with the argument name.
#'
#' @param must A character string specifying which length the arg must have.
#' 
#' @param not The argument name (unquoted). The function evaluates the length of
#'   the argument and displays it in the error message.
#' 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' if (lenght(x) > 1) {
#'   abort_argument_length("x", must = "have length 1", not = x)
#' }
#' }
abort_argument_length <- function(arg, must, not) {
  not <- length(not)
  msg <- glue::glue("`{arg}` must {must}; not {not}.")

  rlang::abort(
    "error_argument_length",
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}

#' Abort based on arguments having different lengths
#'
#' @param arg1,arg2 A character vector with the argument name.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' x <- 1:5
#' y <- 1:10
#' if (lenght(x) != length(y)) {
#'   abort_argument_diff_length("x", "y")
#' }
#' }
abort_argument_diff_length <- function(arg1, arg2) {
  msg <- glue::glue("`{arg1}` and `{arg2}` must have the same length.")

  rlang::abort(
    "error_argument_diff_length",
    message = msg,
    arg = list(arg1, arg2),
    must = "have the same length"
  )
}

#' Abort based on column not being found in a data frame
#'
#' @param data A data frame.
#'
#' @param col_name A character vector with the column name.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = 1:10)
#' if ("y" %!in% names(data)) {
#'   abort_column_not_found(data, "y")
#' }
#' }
abort_column_not_found <- function(data, col_name) {
  msg <- glue::glue("Column `{col_name}` not found in `{data}`.")

  rlang::abort("error_column_not_found", message = msg)
}
