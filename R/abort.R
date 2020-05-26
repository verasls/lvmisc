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
