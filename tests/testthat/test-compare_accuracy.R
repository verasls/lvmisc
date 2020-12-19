test_that("error handling works", {
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)

  expect_error(
    compare_accuracy(m1, m2, quiet = "true"),
    "`quiet` must be logical; not character.",
    class = "error_argument_type"
  )
  expect_error(
    compare_accuracy(m1, m2, rank_by = "R2_marg"),
    glue::glue(
      "`rank_by` must be one of \"Model\", \"Class\", \"R2\", \"R2_adj\", \\
      \"AIC\", \"BIC\", \"MAE\", \"MAPE\" or \"RMSE\""
    ),
    class = "error_argument_value"
  )
})
