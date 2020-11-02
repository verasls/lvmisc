context("stats")

test_that("center_variable by grand mean works", {
  var <- 1:5
  out <- center_variable(var)
  
  expect_equal(out, c(- 2, - 1, 0, 1, 2))
})

test_that("center_variable by group means works", {
  df <- data.frame(
    var = c(1:5, 11:15),
    group = as.factor(rep(c("A", "B"), each = 5))
  )
  out <- center_variable(df$var, df$group)
  
  expect_equal(out, rep(c(- 2, - 1, 0, 1, 2), 2))
})

test_that("scaling works", {
  var <- 1:5
  out <- center_variable(var, scale = TRUE)
  out <- sd(out)
  
  expect_equal(out, 1)
})

test_that("center_variable error handling works", {
  chr <- letters
  nmr <- 1:10
  
  expect_error(
    center_variable(chr),
    "`variable` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    center_variable(nmr, chr),
    "`by` must be factor; not character.",
    class = "error_argument_type"
  )
  expect_error(
    center_variable(nmr, scale = chr),
    "`scale` must be logical; not character.",
    class = "error_argument_type"
  )
})

test_that("is_outlier works", {
  x <- c(1, 20:25, NA, 100)
  out <- is_outlier(x, na.rm = TRUE)
  
  expect_equal(out, c(TRUE, rep(FALSE, 6), NA, TRUE))
  
  expect_error(is_outlier(x))
})

test_that("is_outlier error handling works", {
  chr <- letters[1:10]
  lgc <- rep(c(TRUE, FALSE), 5)
  
  expect_error(
    is_outlier(chr),
    "`x` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    is_outlier(lgc),
    "`x` must be numeric; not logical.",
    class = "error_argument_type"
  )
})
