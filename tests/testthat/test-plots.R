test_that("error handling works", {
  expect_error(
    plot_scatter("mtcars", disp, mpg),
    "`data` must be data.frame; not character.",
    class = "error_argument_type"
  )
  expect_error(
    plot_scatter(mtcars, Disp, mpg),
    "Column `Disp` not found in `mtcars`.",
    class = "error_column_not_found"
  )
  expect_error(
    plot_scatter(mtcars, disp, MPG),
    "Column `MPG` not found in `mtcars`.",
    class = "error_column_not_found"
  )
  expect_error(
    plot_hist(mtcars, mpg, bin_width = "none"),
    "`bin_width` must be one of \"Sturges\", \"scott\" or \"FD\".",
    class = "error_argument_value"
  )
})

test_that("output of plot_scatter() is stable", {
  vdiffr::expect_doppelganger(
    "Scatterplot",
    plot_scatter(mtcars, disp, mpg, color = factor(cyl))
  )
})

test_that("output of plot_line() is stable", {
  vdiffr::expect_doppelganger(
    "Line plot",
    plot_line(Orange, age, circumference, colour = Tree)
  )
})

test_that("output of plot_hist() is stable", {
  vdiffr::expect_doppelganger(
    "Histogram - NULL",
    plot_hist(iris, Petal.Width)
  )
  vdiffr::expect_doppelganger(
    "Histogram - Sturges",
    plot_hist(iris, Petal.Width, bin_width = "Sturges")
  )
  vdiffr::expect_doppelganger(
    "Histogram - scott",
    plot_hist(iris, Petal.Width, bin_width = "scott")
  )
  vdiffr::expect_doppelganger(
    "Histogram - FD",
    plot_hist(iris, Petal.Width, bin_width = "FD")
  )
})

test_that("output of plot_qq() is stable", {
  vdiffr::expect_doppelganger(
    "QQ plot",
    plot_qq(mtcars, mpg)
  )
})
