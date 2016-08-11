library(toxboot)
context("Utility functions are performing correctly")

test_that("NULLs are replaced by NAs", {
  x <- list(1,2,'a', NA, 6, NULL, '10')
  y <- nullToNA(x)
  expect_is(y, "list")
  expect_equal(length(x), length(y))
  expect_equal(sum(sapply(y, is.null)),0L)
  expect_true(is.na(y[6]))
})
