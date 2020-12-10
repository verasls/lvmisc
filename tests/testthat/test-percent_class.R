test_that("lvmisc_percent prints correctly", {
  set.seed(20200527)
  x <- c(runif(9), NA)

  # expect_snapshot_output(cat(percent(x)))

  x <- percent(0.5)

  expect_equal(vec_ptype_abbr(x), "prcnt")
})

test_that("vector casting works", {
  x <- percent(0.25)
  y <- percent(0.50)
  z <- 0.75

  expect_s3_class(vec_c(x, y), c("lvmisc_percent", "vctrs_vctr"), exact = TRUE)
  expect_type(vec_c(x, z), "double")
  expect_type(vec_c(z, x), "double")
  expect_s3_class(
    as_percent(z), c("lvmisc_percent", "vctrs_vctr"), exact = TRUE
  )
  expect_equal(is_percent(x), TRUE)
  expect_equal(is_percent(z), FALSE)
})
