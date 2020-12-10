test_that("error handling works", {
  chr <- letters[1:10]
  lgc <- rep(c(TRUE, FALSE), 5)
  nmr <- 1:10
  nmr2 <- 1

  expect_error(
    bmi(chr, nmr),
    "`mass` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(lgc, nmr),
    "`mass` must be numeric; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(nmr, chr),
    "`height` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(nmr, lgc),
    "`height` must be numeric; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(nmr, nmr2),
    "`mass` and `height` must have the same length.",
    class = "error_argument_diff_length"
  )
  expect_error(
    bmi_cat(chr),
    "`bmi` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi_cat(lgc),
    "`bmi` must be numeric; not logical",
    class = "error_argument_type"
  )
})

test_that("bmi() works", {
  h <- seq(1.55, 1.8, 0.05)
  m <- c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)

  out <- bmi(m, h)

  expect_equal(out, c(18, 22, 27, 32, 37, 42))
})

test_that("bmi() deals with height in centimeters", {
  h <- seq(155, 180, 5)
  m <- c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)

  expect_warning(out <- bmi(m, h))
  expect_equal(out, c(18, 22, 27, 32, 37, 42))
})

test_that("bmi_cat() works", {
  h <- seq(1.55, 1.8, 0.05)
  m <- c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)
  bmi <- bmi(m, h)

  out <- bmi_cat(bmi)

  expect_s3_class(out, "factor")
  expect_equal(
    out,
    forcats::as_factor(c(
      "Underweight", "Normal weight", "Overweight",
      "Obesity class I", "Obesity class II", "Obesity class III"
    ))
  )
})
