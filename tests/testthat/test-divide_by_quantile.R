test_that("error handling works", {
  expect_error(
    divide_by_quantile(letters, 3),
    "`data` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    divide_by_quantile(1:10, 4.5),
    "`n` must be interger; not double.",
    class = "error_argument_type"
  )
  expect_error(
    divide_by_quantile(1:10, 4, na.rm = "true"),
    "`na.rm` must be logical; not character.",
    class = "error_argument_type"
  )
})

test_that("divide_by_quantile() works", {
  x <- c(1:9, NA)
  out <- divide_by_quantile(x, 3)

  expect_is(out, "factor")
  expect_equal(
    out,
    factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, NA))
  )
})
