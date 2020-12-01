test_that("%!in% works", {
  x <- 8:12
  out <- x %!in% 1:10

  expect_equal(out, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})
