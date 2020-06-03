#' Value matching
#'
#' \code{%!in%} returns a logical vector indicating which values are not in 
#'   `table`.
#'
#' @name notin
#' @rdname notin
#'
#' @param x Vector with the values to be matched.
#'
#' @param table Vector with the values to be matched against.
#'
#' @export
#'
#' @seealso \code{\link[base:match]{match()}}.
#'
#' @examples
#' x <- 8:12
#' x %!in% 1:10
'%!in%' <- function(x, table) !(x %in% table)
