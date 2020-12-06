test_that("error handling worls", {
  df <- data.frame(
    subj = 1:30,
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m1 <- stats::lm(y ~ x, df)
  m2 <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    loocv(m2, df, subj),
    glue::glue(
      "The method `loocv` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    loocv(m1, "df", subj),
    "`data` must be data.frame; not character.",
    class = "error_argument_type"
  )
  expect_error(
    loocv(m1, df, ind),
    "Column `ind` not found in `df`.",
    class = "error_column_not_found"
  )
  expect_error(
    loocv(m1, df, subj, keep = "no"),
    "`keep` must be one of \"all\", \"used\" or \"none\".",
    class = "error_argument_value"
  )
})

test_that("returned object is of class loocv", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- stats::lm(disp ~ mpg, mtcars)
  cv <- loocv(m, mtcars, car)
  
  expect_s3_class(cv, "loocv")
})
