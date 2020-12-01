test_accuracy_means_error_handling <- function(.f) {
  test_that("error handling works", {
    a <- 1:10
   b <- 1
   c <- 1:10
   d <- letters[1:10]

   expect_error(
     .f(d, a),
     "`actual` must be numeric; not character.",
     class = "error_argument_type"
   )
   expect_error(
     .f(a, d),
     "`predicted` must be numeric; not character.",
     class = "error_argument_type"
   )
   expect_error(
     .f(a, b),
     "`actual` and `predicted` must have the same length.",
     class = "error_argument_diff_length"
   )
   expect_error(
     .f(a, c, na.rm = "true"),
     "`na.rm` must be logical; not character.",
     class = "error_argument_type"
   )
  })
}

test_accuracy_error_handling <- function(.f) {
  test_that("error handling works", {
    a <- 1:10
   b <- 1
   c <- letters[1:10]

   expect_error(
     .f(c, a),
     "`actual` must be numeric; not character.",
     class = "error_argument_type"
   )
   expect_error(
     .f(a, c),
     "`predicted` must be numeric; not character.",
     class = "error_argument_type"
   )
   expect_error(
     .f(a, b),
     "`actual` and `predicted` must have the same length.",
     class = "error_argument_diff_length"
   )
  })
}
