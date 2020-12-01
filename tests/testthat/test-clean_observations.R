test_that("error handling works", {
  data <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    clean_observations(data, id, x, 1),
    "Column `id` not found in `data`.",
    class = "error_column_not_found"
  )
  expect_error(
    clean_observations(data, x, var, 1),
    "Column `var` not found in `data`.",
    class = "error_column_not_found"
  )
  expect_error(
    clean_observations(data, x, y, 1.5),
    "`max_na` must be interger; not double.",
    class = "error_argument_type"
  )
})

test_that("clean_observations() works", {
  set.seed(20200606)
  data <- data.frame(
    id = rep(1:5, each = 4),
    var = sample(c(1:5, rep(NA, 3)), 20, replace = TRUE)
  )

  out <- clean_observations(data, id, var, 1)
  out <- out[["var"]]

  expect_equal(
    out,
    c(NA, 1, 1, 4, NA, NA, NA, NA, 3, 5, 5, NA, 3, 1, NA, 1, NA, NA, NA, NA)
  )
})
