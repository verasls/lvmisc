fun_list <- list(
  mean_error, mean_error_pct, mean_error_abs,
  mean_error_abs_pct, mean_error_sqr,
  mean_error_sqr_root, bias, loa
)
purrr::walk(fun_list, test_accuracy_means_error_handling)

test_that("mean_error() works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- mean_error(a, b)

  expect_equal(out, 0)
})

test_that("mean_error_pct() works", {
  a <- rep(10, 7)
  b <- c(0, 5, 7.5, 10, 12.5, 15, 20)
  out <- mean_error_pct(a, b)

  expect_equal(out, percent(0))
  expect_s3_class(out, c("lvmisc_percent", "vctrs_vctr"))
})

test_that("mean_error_abs() works", {
  a <- rep(10, 4)
  b <- seq(10, 16, 2)
  out <- mean_error_abs(a, b)

  expect_equal(out, 3)
})

test_that("mean_error_abs_pct() works", {
  a <- rep(10, 7)
  b <- c(0, 5, 7.5, 10, 12.5, 15, 20)
  out <- mean_error_abs_pct(a, b)

  expect_equal(out, percent(0.5))
  expect_s3_class(out, c("lvmisc_percent", "vctrs_vctr"))
})

test_that("mean_error_sqr() works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- mean_error_sqr(a, b)

  expect_equal(out, 4)
})

test_that("mean_error_sqr_root() works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- mean_error_sqr_root(a, b)

  expect_equal(out, 2)
})

test_that("bias() works", {
  a <- rep(10, 7)
  b <- seq(13, 7, - 1)
  out <- bias(a, b)

  expect_equal(out, 0)
})

test_that("loa() works", {
  a <- rep(10, 3)
  b <- 9:11
  out <- loa(a, b)

  expect_equal(length(out), 2)
  expect_equal(is.list(out), TRUE)
  expect_equal(out, list(lower = - 1.96, upper = 1.96))
})
