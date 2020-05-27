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

percent <- function(x = double()) {
  x <- vec_cast(x, "lvmisc_percent")
  new_percent(x)
}

is_percent <- function(x) {
  inherits(x, "lvmisc_percent")
}

format.lvmisc_percent <- function(x, ...) {
  out <- formatC(signif(vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

vec_ptype_abbr.lvmisc_percent <- function(x, ...) {
  "prcnt"
}

vec_ptype2.lvmisc_percent.lvmisc_percent <- function(x, y, ...) new_percent()

vec_ptype2.lvmisc_percent.double <- function(x, y, ...) double()
vec_ptype2.double.lvmisc_percent <- function(x, y, ...) double()

vec_cast.lvmisc_percent.lvmisc_percent <- function(x, to, ...) x

vec_cast.lvmisc_percent.double <- function(x, to, ...) percent(x)
vec_cast.double.lvmisc_percent <- function(x, to, ...) vec_data(x)

as_percent <- function(x) {
  vec_cast(x, new_percent())
}
