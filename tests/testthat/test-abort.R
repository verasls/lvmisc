context("abort")

test_that("abort_argument_type works", {
  x <- letters
  err <- rlang::catch_cnd(
    abort_argument_type("x", must = "be numeric", not = x)
  )
  expect_s3_class(err, "error_argument_type")
  expect_equal(err$message, "`x` must be numeric; not character.")
  expect_equal(err$arg, "x")
  expect_equal(err$must, "be numeric")
  expect_equal(err$not, "character")
})
