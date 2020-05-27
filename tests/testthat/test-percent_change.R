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

  expect_s3_class(out, c("lvmisc_percent", "vctrs_vctr"), exact = TRUE)

  expected <- vec_c(0, 0.005, 0.05, 0.5, 1, - 0.005, - 0.05, - 0.5, - 1, NA)
  expected <- as_percent(expected)
  
  expect_equal(out, expected)
})

test_that("error handling works", {
  df <- data.frame(
    chr = letters[1:10],
    lgc = rep(c(TRUE, FALSE), 5),
    nmr = 1:10
  )

  expect_error(
    percent_change(df, chr, nmr),
    "`baseline` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    percent_change(df, lgc, nmr),
    "`baseline` must be numeric; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    percent_change(df, nmr, chr),
    "`followup` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    percent_change(df, nmr, lgc),
    "`followup` must be numeric; not logical.",
    class = "error_argument_type"
  )
})
