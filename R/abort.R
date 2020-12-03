#' Abort based on issues with function argument
#'
#' Returns a custom error condition created with
#'   \code{\link[rlang:abort]{rlang::abort()}} with a - hopefully - more useful
#'   error message and metadata.
#'
#' @name abort_argument
#'
#' @param arg A character string with the argument name.
#' @param must A character string specifying a condition the argument must
#'   fulfill.
#' @param not Either a character string specifying a condition the argument
#'   must not fulfill or the bare (unquoted) argument name. In the last case,
#'   the function evaluates the argument type (\code{abort_argument_type()}) or
#'   length (\code{abort_argument_length()}) and displays the result in the
#'   error message.
#'
#' @export
#'
#' @seealso \code{\link[=abort_column_not_found]{abort_column_not_found()}},
#'   \code{\link[=abort_class_not_implemented]{abort_class_not_implemented()}}
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

#' @rdname abort_argument
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' if (length(x) > 1) {
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

#' @rdname abort_argument
#'
#' @param arg1,arg2 A character string with the argument name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:5
#' y <- 1:10
#' if (length(x) != length(y)) {
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

#' @rdname abort_argument
#'
#' @param arg A character string with the argument name.
#' @param valid_values A character vectpr with the vaid values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' keep <- "no"
#' valid_values <- c("all", "used", "none")
#' if (keep %!in% valid_values) {
#'   abort_argument_value("keep", valid_values)
#' }
#' }
abort_argument_value <- function(arg, valid_values) {
  valid_values <- glue::glue_collapse(
    glue::double_quote(valid_values), sep = ", ", last = " or "
  )
  msg <- glue::glue("`{arg}` must be one of {valid_values}")

  rlang::abort(
    "error_argument_value",
    message = msg,
    arg = arg
  )
}

#' Abort based on column not being found in a data frame
#'
#' Returns a custom error condition created with
#'   \code{\link[rlang:abort]{rlang::abort()}} with a - hopefully - more useful
#'   error message and metadata.
#'
#' @param data A data frame.
#' @param col_name A character vector with the column name.
#'
#' @export
#'
#' @seealso \code{\link[=abort_argument_type]{abort_argument_type()}},
#'   \code{\link[=abort_argument_length]{abort_argument_length()}},
#'   \code{\link[=abort_argument_diff_length]{abort_argument_diff_length()}},
#'   \code{\link[=abort_class_not_implemented]{abort_class_not_implemented()}}
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

#' Abort method if class is not implemented
#'
#' Returns a custom error condition created with
#'   \code{\link[rlang:abort]{rlang::abort()}} with a - hopefully - more useful
#'   error message and metadata.
#'
#' @param fun A character vector with the function name.
#' @param class A character vector with the class name.
#'
#' @export
#'
#' @seealso \code{\link[=abort_argument_type]{abort_argument_type()}},
#'   \code{\link[=abort_argument_length]{abort_argument_length()}},
#'   \code{\link[=abort_argument_diff_length]{abort_argument_diff_length()}},
#'   \code{\link[=abort_column_not_found]{abort_column_not_found()}}
abort_class_not_implemented <- function(fun, class) {
  if (length(class) > 1) {
    class <- glue::glue_collapse(
     glue::glue("{class}"), sep = ", ", last = " and "
    )
    noun <- "classes"
  } else {
    noun <- "class"
  }
  msg <- glue::glue(
    "The method `{fun}`, is not yet implemented for an object \\
    of {noun} {class}.
    If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )

  rlang::abort("error_class_not_implemented", message = msg)
}
