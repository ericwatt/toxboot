library(toxboot)
context("Confirm that configuration file was loaded correctly")

test_that("mongo connect values are set", {
  toxget <- toxbootConfList(show.pass = TRUE)
  expect_is(toxget$TOXBOOT_HOST, "character")
  expect_is(toxget$TOXBOOT_DBNS, "character")
  expect_is(toxget$TOXBOOT_USER, "character")
  expect_is(toxget$TOXBOOT_PASS, "character")
  expect_is(toxget$TOXBOOT_DB, "character")
})
