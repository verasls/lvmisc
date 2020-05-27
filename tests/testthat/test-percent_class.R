context("lvmisc_percent class")

test_that("lvmisc_percent prints correctly", {
  verify_output(test_path("test-print-lvmisc_percent.txt"), {
    set.seed(20200527)
    x <- c(runif(9), NA) 
    percent(x)
  })
})
