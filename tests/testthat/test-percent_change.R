context("percent_change")

test_that("percent_change works", {
  set.seed(20200525)

  df <- data.frame(
   change = c(
      1, 1.005, 1.05, 1.5, 2,
      0.995, 0.95, 0.5, 0, 1
    ),
   bl = sample(1:10)
  )
  df$bl[10] <- NA
  df$fu <- df$bl * df$change

  out <- percent_change(df, bl, fu)
  out <- out[["percent_change"]]

  expect_equal(out, c(0, 0.5, 5, 50, 100, - 0.5, - 5, - 50, - 100, NA))
})

test_that("rounding works", {
  df <- data.frame(
   change = c(1.0114, 1.0116),
   bl = 10
  )
  df$fu <- df$bl * df$change

  out <- percent_change(df, bl, fu, 1)
  out <- out[["percent_change"]]

  expect_equal(out, c(1.1, 1.2))

  df$change <- c(1.0001, 1.0099)
  df$fu <- df$bl * df$change

  out <- percent_change(df, bl, fu, 3)
  out <- out[["percent_change"]]

  expect_equal(out, c(0.01, 0.99))
})

test_that("error handling works", {
  df <- data.frame(
    chr = letters[1:10],
    lgc = rep(c(TRUE, FALSE), 5),
    nmr = 1:10
  )

  expect_error(
    percent_change(df, chr, nmr),
    "`baseline` must be numeric; not character."
  )
  expect_error(
    percent_change(df, lgc, nmr),
    "`baseline` must be numeric; not logical."
  )
  expect_error(
    percent_change(df, nmr, chr),
    "`followup` must be numeric; not character."
  )
  expect_error(
    percent_change(df, nmr, lgc),
    "`followup` must be numeric; not logical."
  )
})
