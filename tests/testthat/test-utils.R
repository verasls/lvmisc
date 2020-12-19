test_that("tb() works", {
  1 + 1
  out <- tb()

  expect_s3_class(out, "rlang_trace")
})

test_that("pa() works", {
  verify_output(test_path("test-utils-pa.txt"), {
    df <- dplyr::starwars
    pa(df)
  })
})

test_that("lunique() and lna() works", {
  x <- c(1, 1, 2, 3, 3, 3, 4, 4, NA, NA)

  expect_equal(lunique(x), 4)
  expect_equal(lna(x), 2)
})
