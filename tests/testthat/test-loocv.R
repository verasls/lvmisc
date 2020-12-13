test_that("error handling works", {
  df <- data.frame(
    subj = 1:30,
    y = sample.int(30, replace = TRUE),
    x = sample(c("A", "B"), 30, replace = TRUE, prob = c(0.6, 0.4))
  )
  m1 <- stats::lm(y ~ x, df)
  m2 <- stats::glm(y ~ x, df, family = poisson())

  expect_error(
    loo_cv(m2, df, subj),
    glue::glue(
      "The method `loo_cv` is not yet implemented for an object \\
      of class `glm`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
  expect_error(
    loo_cv(m1, "df", subj),
    "`data` must be data.frame; not character.",
    class = "error_argument_type"
  )
  expect_error(
    loo_cv(m1, df, ind),
    "Column `ind` not found in `df`.",
    class = "error_column_not_found"
  )
  expect_error(
    loo_cv(m1, df, subj, keep = "no"),
    "`keep` must be one of \"all\", \"used\" or \"none\".",
    class = "error_argument_value"
  )
  expect_error(
    loo_cv("m1", df, subj),
    glue::glue(
      "The method `loo_cv` is not yet implemented for an object \\
      of class `character`.
      If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    ),
    class = "error_no_method_for_class"
  )
})

test_that("loo_cv() returns an object of class lvmisc_cv", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- stats::lm(disp ~ mpg, mtcars)
  cv <- loo_cv(m, mtcars, car)

  expect_s3_class(cv, "lvmisc_cv")
})

test_that("lvmisc_cv class has a lvmisc_cv_model attribute", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- stats::lm(disp ~ mpg, mtcars)
  cv <- loo_cv(m, mtcars, car)

  expect_true("lvmisc_cv_model" %in% names(attributes(cv)))
})

test_that("`keep` argument works", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- stats::lm(disp ~ mpg, mtcars)
  cv1 <- loo_cv(m, mtcars, car, keep = "all")
  cv2 <- loo_cv(m, mtcars, car, keep = "used")
  cv3 <- loo_cv(m, mtcars, car, keep = "none")

  expect_equal(names(cv1), c(names(mtcars), ".actual", ".predicted"))
  expect_equal(names(cv2), c("car", ".actual", ".predicted"))
  expect_equal(names(cv3), c(".actual", ".predicted"))
})

test_that("loo_cv method for lmerMod class works", {
  mtcars <- tibble::as_tibble(mtcars, rownames = "car")
  m <- lme4::lmer(disp ~ mpg + (1 | gear), mtcars)
  cv <- loo_cv(m, mtcars, car, keep = "none")

  expect_equal(names(cv), c(".actual", ".predicted"))
})
