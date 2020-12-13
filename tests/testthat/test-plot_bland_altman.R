test_that("error handling works", {
  df <- data.frame(
    subj = 1:30,
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    plot_bland_altman(m),
    glue::glue(
      "The method `model_data` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    plot_bland_altman("m"),
    glue::glue(
      "The method `model_data` is not yet implemented for an object \\
      of class `character`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
})

test_that("output of plot_bland_altman() is stable", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m1 <- stats::lm(disp ~ mpg, mtcars)
  m2 <- lme4::lmer(disp ~ mpg + (1 | gear), mtcars)
  cv <- loo_cv(m1, mtcars, car)

  vdiffr::expect_doppelganger(
    "Bland Altman plot - cv",
    plot_bland_altman(cv, colour = as.factor(am))
  )
  vdiffr::expect_doppelganger(
    "Bland Altman plot - lm",
    plot_bland_altman(m1)
  )
  vdiffr::expect_doppelganger(
    "Bland Altman plot - lmerMod",
    plot_bland_altman(m2)
  )
})
