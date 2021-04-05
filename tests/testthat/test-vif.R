test_that("error handling works", {
  df <- data.frame(
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m1 <- stats::glm(y ~ x, df, family = poisson())
  m2 <- stats::lm(y ~ x, df)

  expect_error(
    vif(m1),
    glue::glue(
      "The method `vif` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    vif("m"),
    glue::glue(
      "The method `vif` is not yet implemented for an object \\
      of class `character`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    vif(m2),
    "Not enough terms in `model` to check for multicollinearity."
  )
})

test_that("model without intercept gets a warning", {
  m1 <- lm(disp ~ mpg + hp - 1, mtcars)
  m2 <- lme4::lmer(disp ~ mpg + hp - 1 + (1 | cyl), mtcars)
  expect_warning(
    vif(m1),
    "`model` has no intercept; VIFs may not be sensible."
  )
  expect_warning(
    vif(m2),
    "`model` has no intercept; VIFs may not be sensible."
  )
})

test_that("vif() produces the right output", {
  m <- lm(disp ~ mpg + cyl + hp + I(hp^2), mtcars)
  out <- vif(m)

  expect_equal(names(out), c("Term", "VIF", "Classification"))
  expect_equal(
    as.character(out[["Term"]]),
    c("mpg", "cyl", "hp", "I(hp^2)")
  )
  expect_equal(
    as.character(out[["Classification"]]),
    c("Low", "Moderate", "High", "High")
  )
})
