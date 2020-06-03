#' Capture a backtrace
#'
#' Captures the sequence of calls that lead to the current function. It is just
#'   a wrapper to \code{\link[rlang:trace_back]{rlang::trace_back()}}.
#'
#' @param ... Passed to \code{\link[rlang:trace_back]{rlang::trace_back()}}.
#'
#' @export
#'
#' @seealso \code{\link[rlang:trace_back]{rlang::trace_back()}}.
#'
#' @examples
#' f <- function() g()
#' g <- function() h()
#' h <- function() tb()
tb <- function(...) rlang::trace_back(...)

#' Print all rows of a data frame or tibble
#'
#' Shortcut to print all rows of a data frame or tibble. Useful to inspect the
#'   whole tibble, as it prints by default only the first 20 rows.
#'
#' @param data A data frame or tibble.
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

#' Number elements in a vector.
#'
#' \code{lu} returns the number of non-\code{NA} unique elements and \code{lna}
#'   returns the number of \code{NA}s.
#'
#' @param x A vector.
#'
#' @export
#'
#' @seealso \code{\link[base:length]{length()}},
#'   \code{\link[base:unique]{unique()}},
#'   \code{\link[base:is.na]{is.na()}}
#'
#' @examples
#' x <- sample(c(1:3, NA), 10, replace = TRUE)
#' lu(x)
lu <- function(x) {
  length(unique(x[!is.na(x)]))
}

#' @rdname lu
#' @export
#' @examples
#' lna(x)
lna <- function(x) {
  sum(is.na(x))
}
