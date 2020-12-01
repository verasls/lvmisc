test_that("abort_argument_type() works", {
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

test_that("abort_argument_length() works", {
  x <- 1:10
  err <- rlang::catch_cnd(
    abort_argument_length("x", must = "have length 1", not = x)
  )

  expect_s3_class(err, "error_argument_length")
  expect_equal(err$message, "`x` must have length 1; not 10.")
  expect_equal(err$arg, "x")
  expect_equal(err$must, "have length 1")
  expect_equal(err$not, 10)
})

test_that("abort_argument_diff_length() works", {
  x <- 1:5
  y <- 1:10
  err <- rlang::catch_cnd(abort_argument_diff_length("x", "y"))

  expect_s3_class(err, "error_argument_diff_length")
  expect_equal(err$message, "`x` and `y` must have the same length.")
  expect_equal(err$arg, list("x", "y"))
  expect_equal(err$must, "have the same length")
})
