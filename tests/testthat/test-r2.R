test_that("error handling works", {
  df <- data.frame(
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    r2(m),
    glue::glue(
      "The method `r2` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    r2("m"),
    glue::glue(
      "The method `r2` is not yet implemented for an object \\
      of class `character`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
})

test_that("r2 lm and lmerMod methods works", {
  m <- stats::lm(disp ~ mpg, mtcars)
  out <- r2(m)

  expect_equal(names(out), c("R2", "R2_adj"))

  m <- lme4::lmer(
    Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
  )
  out <- r2(m)

  expect_equal(names(out), c("R2_marg", "R2_cond"))
})
