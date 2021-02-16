#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name lvmisc-vctrs
NULL

new_percent <- function(x = double()) {
  vec_assert(x, double())
  new_vctr(x, class = "lvmisc_percent")
}

#' `percent` vector
#'
#' Creates a double vector that represents percentages. When printed, it is
#'   multiplied by 100 and suffixed with `%`.
#'
#' @param x
#'  * For `percent()`: A numeric vector
#'  * For `is_percent()`: An object to test.
#'  * For `as_percent()`: An object to cast.
#'
#' @return An S3 vector of class `lvmisc_percent`.
#'
#' @export
#'
#' @examples
#' percent(c(0.25, 0.5, 0.75))
percent <- function(x = double()) {
  x <- vec_cast(x, double())
  new_percent(x)
}

#' @export
#' @rdname percent
is_percent <- function(x) {
  inherits(x, "lvmisc_percent")
}

#' @export
#' @rdname percent
as_percent <- function(x) {
  vec_cast(x, new_percent())
}

#' @export
format.lvmisc_percent <- function(x, ...) {
  out <- vec_data(x) * 100
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

#' @export
round.lvmisc_percent <- function(x, digits = 0) {
  x <- round(unclass(x), digits + 2)
  percent(x)
}

#' @export
vec_ptype_abbr.lvmisc_percent <- function(x, ...) {
  "prcnt"
}

#' @export
vec_ptype2.lvmisc_percent.lvmisc_percent <- function(x, y, ...) new_percent()

#' @export
vec_ptype2.lvmisc_percent.double <- function(x, y, ...) double()

#' @export
vec_ptype2.double.lvmisc_percent <- function(x, y, ...) double()

#' @export
vec_cast.lvmisc_percent.lvmisc_percent <- function(x, to, ...) x

#' @export
vec_cast.lvmisc_percent.double <- function(x, to, ...) percent(x)

#' @method vec_cast.double lvmisc_percent
#' @export
vec_cast.double.lvmisc_percent <- function(x, to, ...) vec_data(x)


# For compatibility with the S4 system
#' @importFrom methods setOldClass
setOldClass(c("lvmisc_percent", "vctrs_vctr"))
