test_that("error handling works", {
  df <- data.frame(
    subj = 1:30,
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m1 <- stats::lm(y ~ x, df)
  m2 <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    accuracy(m2),
    glue::glue(
      "The method `accuracy` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    accuracy(m1, na.rm = "yes"),
    "`na.rm` must be logical; not character.",
    class = "error_argument_type"
  )
})
