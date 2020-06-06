context("clean_observations")

test_that("clean_observations works", {
  set.seed(20200606)
  data <- data.frame(
    id = rep(1:5, each = 4),
    var = sample(c(1:5, rep(NA, 3)), 20, replace = TRUE)
  )
  
  out <- clean_observations(data, id, var, 1)
  out <- out[["var"]]

  expect_equal(
    out,
    c(NA, 1, 1, 4, NA, NA, NA , NA, 3, 5, 5, NA, 3, 1, NA, 1, NA, NA, NA, NA)
  )
})
