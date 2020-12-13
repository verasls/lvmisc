test_that("error handling works", {
  df <- data.frame(
    subj = 1:30,
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m1 <- stats::lm(y ~ x, df)
  m2 <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    accuracy(m2),
    glue::glue(
      "The method `accuracy` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    accuracy(m1, na.rm = "yes"),
    "`na.rm` must be logical; not character.",
    class = "error_argument_type"
  )
  expect_error(
    accuracy("m1"),
    glue::glue(
      "The method `accuracy` is not yet implemented for an object \\
      of class `character`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
})

test_that("accuracy() returns an object of class lvmisc_accuracy", {
  m <- stats::lm(disp ~ mpg, mtcars)
  out <- accuracy(m)

  expect_s3_class(out, "lvmisc_accuracy")
})

test_that("lvmisc_accuracy class has the correct attributes", {
  m <- stats::lm(disp ~ mpg, mtcars)
  out <- accuracy(m)

  expect_true("model_name" %in% names(attributes(out)))
  expect_true("model_class" %in% names(attributes(out)))
})

test_that("accuracy() returns a data frame with the right columns", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m1 <- stats::lm(disp ~ mpg, mtcars)
  m2 <- lme4::lmer(disp ~ mpg + (1 | gear), mtcars)
  cv1 <- loo_cv(m1, mtcars, car)
  cv2 <- loo_cv(m2, mtcars, car)

  expect_equal(
    names(accuracy(m1)),
    c("AIC", "BIC", "R2", "R2_adj", "MAE", "MAPE", "RMSE")
  )
  expect_equal(
    names(accuracy(m2)),
    c("AIC", "BIC", "R2_marg", "R2_cond", "MAE", "MAPE", "RMSE")
  )
  expect_equal(
    names(accuracy(cv1)),
    c("AIC", "BIC", "R2", "R2_adj", "MAE", "MAPE", "RMSE")
  )
  expect_equal(
    names(accuracy(cv2)),
    c("AIC", "BIC", "R2_marg", "R2_cond", "MAE", "MAPE", "RMSE")
  )
})
