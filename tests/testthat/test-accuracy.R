context("accuracy")

test_that("error works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- error(a, b)

  expect_equal(out, c(- 3, - 2, - 1, 0, 1, 2, 3))
})

test_that("error_pct works", {
  a <- rep(10, 7)
  b <- c(0, 5, 7.5, 10, 12.5, 15, 20)

  out <- error_pct(a, b)
  expected <- as_percent(c(1, 0.5, 0.25, 0, - 0.25, - 0.5, - 1))

  expect_equal(out, expected)
  expect_s3_class(out, c("lvmisc_percent", "vctrs_vctr"))
})

test_that("error_abs works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- error_abs(a, b)

  expect_equal(out, c(3, 2, 1, 0, 1, 2, 3))
})

test_that("error_abs_pct works", {
  a <- rep(10, 7)
  b <- c(0, 5, 7.5, 10, 12.5, 15, 20)

  out <- error_abs_pct(a, b)
  expected <- as_percent(c(1, 0.5, 0.25, 0, 0.25, 0.5, 1))

  expect_equal(out, expected)
  expect_s3_class(out, c("lvmisc_percent", "vctrs_vctr"))
})

test_that("error_sqr works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- error_sqr(a, b)

  expect_equal(out, c(9, 4, 1, 0, 1, 4, 9))
})

test_that("handling errors works", {
  a <- 1:10
  b <- 1
  c <- letters[1:10]

  expect_error(
    error(c, a),
    "`actual` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    error(a, c),
    "`predicted` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    error(a, b),
    "`actual` and `predicted` must have the same length.",
    class = "error_argument_diff_length"
  )
})
