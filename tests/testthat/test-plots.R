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
