test_that("abort_argument_type() works", {
  x <- letters
  err <- expect_error(
    abort_argument_type("x", must = "be numeric", not = x)
  )

  expect_s3_class(err, "error_argument_type")
  expect_equal(unclass(err$message), "`x` must be numeric; not character.")
  expect_equal(err$arg, "x")
  expect_equal(err$must, "be numeric")
  expect_equal(err$not, "character")
})

test_that("abort_argument_class() works", {
  x <- letters
  err <- expect_error(
    abort_argument_class("x", must = "be of class data.frame", not = x)
  )

  expect_s3_class(err, "error_argument_class")
  expect_equal(
    unclass(err$message), "`x` must be of class data.frame; not character."
  )
  expect_equal(err$arg, "x")
  expect_equal(err$must, "be of class data.frame")
  expect_equal(unclass(err$not), "character")
})

test_that("abort_argument_length() works", {
  x <- 1:10
  err <- expect_error(
    abort_argument_length("x", must = "have length 1", not = x)
  )

  expect_s3_class(err, "error_argument_length")
  expect_equal(unclass(err$message), "`x` must have length 1; not 10.")
  expect_equal(err$arg, "x")
  expect_equal(err$must, "have length 1")
  expect_equal(err$not, 10)
})

test_that("abort_argument_diff_length() works", {
  x <- 1:5
  y <- 1:10
  err <- expect_error(abort_argument_diff_length("x", "y"))

  expect_s3_class(err, "error_argument_diff_length")
  expect_equal(unclass(err$message), "`x` and `y` must have the same length.")
  expect_equal(err$arg, list("x", "y"))
  expect_equal(err$must, "have the same length")
})

test_that("abort_argument_value() works", {
  valid_values <- c("one", "two", "three")
  x <- "zero"
  err <- expect_error(abort_argument_value("x", valid_values))

  expect_s3_class(err, "error_argument_value")
  expect_equal(
    unclass(err$message), "`x` must be one of \"one\", \"two\" or \"three\"."
  )
  expect_equal(err$arg, "x")
})

test_that("abort_column_not_found() works", {
  data <- data.frame(a = 1, b = 2)
  err <- expect_error(abort_column_not_found("data", "c"))

  expect_s3_class(err, "error_column_not_found")
  expect_equal(unclass(err$message), "Column `c` not found in `data`.")
})

test_that("abort_no_method_for_class() works", {
  err <- expect_error(abort_no_method_for_class("my_fun", "my_class"))

  expect_s3_class(err, "error_no_method_for_class")
  expect_equal(
    err$message,
    glue::glue(
      "The method `my_fun` is not yet implemented \\
      for an object of class `my_class`."
    )
  )
})

test_that("abort_no_method_for_class() works with ... argument", {
  err <- expect_error(
    abort_no_method_for_class("my_fun", "my_class", "Extra message.")
  )

  expect_s3_class(err, "error_no_method_for_class")
  expect_equal(
    err$message,
    glue::glue(
      "The method `my_fun` is not yet implemented \\
      for an object of class `my_class`.
      Extra message."
    )
  )
})

test_that(
  "abort_no_method_for_class() works in objects with multiple classes", {
    err <- expect_error(
      abort_no_method_for_class("my_fun", c("my_class_1", "my_class_2"))
    )

    expect_s3_class(err, "error_no_method_for_class")
    expect_equal(
      err$message,
      glue::glue(
       "The method `my_fun` is not yet implemented \\
       for an object of classes `my_class_1` and `my_class_2`."
      )
    )
  }
)

test_that("abort_no_method_for_class() error handling for ... works", {
  err <- expect_error(abort_no_method_for_class("my_fun", "my_class", 1))

  expect_s3_class(err, "error_argument_type")
  expect_equal(
    unclass(err$message),
    "`...` must be character; not double."
  )
})

test_that("abort_package_not_installed() works", {
  err1 <- expect_error(abort_package_not_installed("a"))
  err2 <- expect_error(abort_package_not_installed(c("a", "b")))

  expect_s3_class(err1, "error_package_not_installed")
  expect_s3_class(err2, "error_package_not_installed")
  expect_equal(
    unclass(err1$message),
    "Package \"a\" is needed for this function to work. Please install it."
  )
  expect_equal(
    err2$message,
    glue::glue(
      "Packages \"a\" and \"b\" are needed for this function to work. \\
      Please install them."
    )
  )
})

test_that(
  "abort_package_not_installed() does nothing if all required packages are
  already installed", {
    out <- abort_package_not_installed("testthat")
    expect_true(out)
  }
)
