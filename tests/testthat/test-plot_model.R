test_that("output of plot_model() is stable", {
  m1 <- lm(disp ~ mpg, mtcars)
  m2 <- lm(disp ~ mpg + hp + cyl + mpg:cyl, mtcars)

  skip_on_ci()
  skip_on_cran()

  vdiffr::expect_doppelganger("Plot model - m1", plot_model(m1))
  vdiffr::expect_doppelganger("Plot model - m2", plot_model(m2))
})
