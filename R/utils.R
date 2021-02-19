#' Capture a backtrace
#'
#' Captures the sequence of calls that lead to the current function. It is just
#'   a wrapper to \code{\link[rlang:trace_back]{rlang::trace_back()}}.
#'
#' @param ... Passed to \code{\link[rlang:trace_back]{rlang::trace_back()}}.
#'
#' @return An object of class \code{rlang_trace}.
#'
#' @export
tb <- function(...) rlang::trace_back(...)

#' Last error
#'
#' \code{lt()} prints the last error and the full backtrace and \code{le()}
#'   returns the last error with a simplified backtrace. These functions are
#'   just wrappers to \code{\link[rlang:last_trace]{rlang::last_trace()}} and
#'   \code{\link[rlang:last_error]{rlang::last_error()}} respectively.
#'
#' @return An object of class \code{rlang_trace}.
#'
#' @export
lt <- function() rlang::last_trace()

#' @rdname lt
#' @return An object of class \code{rlang_error}.
#' @export
le <- function() rlang::last_error()

#' Print all rows of a data frame or tibble
#'
#' Shortcut to print all rows of a data frame or tibble. Useful to inspect the
#'   whole tibble, as it prints by default only the first 20 rows.
#'
#' @param data A data frame or tibble.
#'
#' @return Prints \code{data} and returns it invisibly.
#'
#' @export
#'
#' @seealso \code{\link[base:print]{print()}} and
#'   \href{https://tibble.tidyverse.org/reference/formatting.html}{printing tibbles}.
#'
#' @examples
#' df <- dplyr::starwars
#' pa(df)
pa <- function(data) {
  print(data, n = Inf)
}

#' Number of elements in a vector.
#'
#' \code{lunique} returns the number of non-\code{NA} unique elements and
#' \code{lna}
#'   returns the number of \code{NA}s.
#'
#' @param x A vector.
#'
#' @return A non-negative integer.
#'
#' @export
#'
#' @seealso \code{\link[base:length]{length()}},
#'   \code{\link[base:unique]{unique()}},
#'   \code{\link[base:is.na]{is.na()}}
#'
#' @examples
#' x <- sample(c(1:3, NA), 10, replace = TRUE)
#' lunique(x)
lunique <- function(x) {
  length(unique(x[!is.na(x)]))
}

#' @rdname lunique
#' @export
#' @examples
#' lna(x)
lna <- function(x) {
  sum(is.na(x))
}
