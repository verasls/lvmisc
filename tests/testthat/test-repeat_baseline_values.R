context("repeat_baseline_values")

test_that("repeat_baseline_values works", {
  df <- data.frame(
    id = rep(1:5, each = 4),
    time = rep(1:4, 5),
    score = rep(10:13, 5)
  )
  out <- repeat_baseline_values(df, score, id, time, 1)

  expect_equal(out, rep(10, 20))
})

test_that("error handling works", {
  df <- data.frame(x = 1, y = 2)

  expect_error(
    repeat_baseline_values(df, score, x, y, 1),
    "Column `score` not found in `df`.",
    class = "error_column_not_found"
  )
  expect_error(
    repeat_baseline_values(df, x, id, y, 1),
    "Column `id` not found in `df`.",
    class = "error_column_not_found"
  )
  expect_error(
    repeat_baseline_values(df, x, y, time, 1),
    "Column `time` not found in `df`.",
    class = "error_column_not_found"
  )
})
