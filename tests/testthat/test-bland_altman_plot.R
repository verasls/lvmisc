test_that("output of bland_altman_plot() is stable", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m1 <- stats::lm(disp ~ mpg, mtcars)
  m2 <- lme4::lmer(disp ~ mpg + (1 | gear), mtcars)
  cv <- loocv(m1, mtcars, car)

  vdiffr::expect_doppelganger(
    "Bland Altman plot - cv",
    bland_altman_plot(cv, colour = as.factor(am))
  )
  vdiffr::expect_doppelganger(
    "Bland Altman plot - lm",
    bland_altman_plot(m1)
  )
  vdiffr::expect_doppelganger(
    "Bland Altman plot - lmerMod",
    bland_altman_plot(m2)
  )
})
