context("stats")

test_that("is_outlier works", {
  x <- c(1, 20:25, NA, 100)
  out <- is_outlier(x, na.rm = TRUE)
  
  expect_equal(out, c(TRUE, rep(FALSE, 6), NA, TRUE))
  
  expect_error(is_outlier(x))
})

test_that("is_outlier error handling works", {
  chr <- letters[1:10]
  lgc <- rep(c(TRUE, FALSE), 5)
  
  expect_error(
    is_outlier(chr),
    "`x` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    is_outlier(lgc),
    "`x` must be numeric; not logical.",
    class = "error_argument_type"
  )
})
