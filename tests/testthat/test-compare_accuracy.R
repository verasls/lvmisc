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

test_that("compare_accuracy() works for models of class lm", {
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  out <- compare_accuracy(m1, m2)

  col_names <- c(
    "Model", "Class", "R2", "R2_adj", "AIC", "BIC", "MAE", "MAPE", "RMSE"
  )
  expect_equal(names(out), col_names)
  expect_equal(out[["Class"]], c("lm", "lm"))
})

test_that("compare_accuracy() works for models of class lmerMod", {
  m1 <- lme4::lmer(
    Sepal.Length ~ Sepal.Width + (1 | Species), data = iris, REML = FALSE
  )
  m2 <- lme4::lmer(
    Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
  )
  out <- compare_accuracy(m1, m2, quiet = TRUE)

  col_names <- c(
    "Model", "Class", "R2_marg", "R2_cond", "AIC", "BIC", "MAE", "MAPE", "RMSE"
  )
  expect_equal(names(out), col_names)
  expect_equal(out[["Class"]], c("lmerMod", "lmerMod"))
})

test_that("compare_accuracy() works for models of class lvmisc_cv", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "cars")
  m1 <- lm(disp ~ mpg, mtcars)
  m2 <- lm(disp ~ mpg + hp, mtcars)
  cv1 <- loo_cv(m1, mtcars, cars)
  cv2 <- loo_cv(m2, mtcars, cars)
  out <- compare_accuracy(cv1, cv2, quiet = TRUE)

  col_names <- c(
    "Model", "Class", "R2", "R2_adj", "AIC", "BIC", "MAE", "MAPE", "RMSE"
  )
  expect_equal(names(out), col_names)
  expect_equal(out[["Class"]], c("lvmisc_cv_model/lm", "lvmisc_cv_model/lm"))
})

test_that("compare_accuracy() works for models of different classes", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "cars")
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lme4::lmer(
    Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
  )
  m3 <- lm(disp ~ mpg * hp, mtcars)
  cv3 <- loo_cv(m3, mtcars, cars)
  out <- compare_accuracy(m1, m2, cv3, quiet = TRUE)

  col_names <- c(
    "Model", "Class", "R2", "R2_adj", "R2_marg", "R2_cond",
    "AIC", "BIC", "MAE", "MAPE", "RMSE"
  )
  expect_equal(names(out), col_names)
  expect_equal(
    out[["Class"]],
    c("lm", "lmerMod", "lvmisc_cv_model/lm")
  )
})

test_that("compare_accuracy() throws the correct warnigns", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "cars")
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lme4::lmer(
    Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
  )
  m3 <- lme4::lmer(
    Sepal.Length ~ Sepal.Width * Petal.Length + (1 | Species), data = iris
  )
  m4 <- lm(disp ~ mpg * hp, mtcars)
  cv4 <- loo_cv(m4, mtcars, cars)

  expect_warning(
    compare_accuracy(m1, m4),
    "Not all models have the same response variable."
  )
  expect_warning(
    compare_accuracy(m4, cv4),
    "Not all models are of the same class."
  )
  expect_warning(
    compare_accuracy(m2, m3),
    "Some models were refit using maximum likelihood."
  )
})

test_that("rank_by argument works", {
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  out <- compare_accuracy(m1, m2, rank_by = "AIC")

  expect_true(out$AIC[1] < out$AIC[2])
})
