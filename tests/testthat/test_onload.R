library(toxboot)
context("Confirm that configuration file was loaded correctly")

test_that("mongo connect values are set", {
  toxget <- toxbootConfList(show.pass = TRUE)
  expect_equal(toxget$TOXBOOT_HOST, NA)
  expect_equal(toxget$TOXBOOT_COLLECTION, NA)
  expect_equal(toxget$TOXBOOT_USER, NA)
  expect_equal(toxget$TOXBOOT_PASS, NA)
  expect_equal(toxget$TOXBOOT_DB, NA)
  expect_equal(toxget$TOXBOOT_PORT, NA)
})
