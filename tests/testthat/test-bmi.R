context("bmi")

test_that("bmi works", {
  df <- data.frame(
    h = seq(1.55, 1.8, 0.05),
    m = c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)
  )

  out <- bmi(df, m, h)
  out <- out[["bmi"]]

  expect_equal(out, c(18, 22, 27, 32, 37, 42))
})

test_that("bmi deals with height in centimeters", {
  df <- data.frame(
    h = seq(155, 180, 5),
    m = c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)
  )

  expect_warning(out <- bmi(df, m, h))
  
  out <- out[["bmi"]]

  expect_equal(out, c(18, 22, 27, 32, 37, 42))
})

test_that("bmi_cat works", {
  df <- data.frame(
    h = seq(1.55, 1.8, 0.05),
    m = c(43.245, 56.32, 73.5075, 92.48, 113.3125, 136.08)
  )
  df <- bmi(df, m, h)
  df <- bmi_cat(df, bmi)

  out <- df[["bmi_cat"]]

  expect_is(out, "factor")
  expect_equal(
    out,
    forcats::as_factor(c(
      "Underweight", "Normal weight", "Overweight",
      "Obesity class I", "Obesity class II", "Obesity class III"
    ))
  )
})

test_that("error handling works", {
  df <- data.frame(
    chr = letters[1:10],
    lgc = rep(c(TRUE, FALSE), 5),
    nmr = 1:10
  )

  expect_error(
    bmi(df, chr, nmr),
    "`mass` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(df, lgc, nmr),
    "`mass` must be numeric; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(df, nmr, chr),
    "`height` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi(df, nmr, lgc),
    "`height` must be numeric; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    bmi_cat(df, chr),
    "`bmi` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    bmi_cat(df, lgc),
    "`bmi` must be numeric; not logical",
    class = "error_argument_type"
  )
})
