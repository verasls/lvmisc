test_that("output of bland_altman_plot() is stable", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- stats::lm(disp ~ mpg, mtcars)
  cv <- loocv(m, mtcars, car)

  vdiffr::expect_doppelganger(
    "Bland Altman plot",
    bland_altman_plot(cv, colour = as.factor(am))
  )
})
