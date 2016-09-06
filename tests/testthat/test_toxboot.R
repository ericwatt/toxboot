library(toxboot)
library(data.table)
context("Bootstrap Calculation")

test_that("toxboot returns correct object", {
  dat <- toxbootmc(dat = erl3data,
                   m4ids = unique(erl3data[, m4id])[1:5],
                   boot_method = "smooth",
                   cores = 1,
                   destination = "memory",
                   replicates = 10)

  expect_equal(dim(dat)[1], 50)
  expect_equal(dim(dat)[2], 52)
  expect_equal(unique(dat[, replicates]), 10)
  expect_equal(length(unique(dat[, m4id])), 5)
  expect_equal(unique(dat[, list(number = .N), by = m4id][, number]), 10)
  expect_error(toxbootmc(dat = erl3data,
                         m4ids = unique(erl3data[, m4id])[1:5],
                         boot_method = "smooth",
                         cores = 1,
                         destination = "destination_unknown",
                         replicates = 10))

  dat_hit <- toxbootHitParamCI(dat, erl5data)

  expect_true("coff"      %in% names(dat_hit))
  expect_true("maic"      %in% names(dat_hit))
  expect_true("modl"      %in% names(dat_hit))
  expect_true("boot_hitc" %in% names(dat_hit))
  expect_true("modl_ga"   %in% names(dat_hit))
  expect_true("modl_tp"   %in% names(dat_hit))
  expect_equal(dim(dat_hit)[1], 50)
  expect_equal(dim(dat_hit)[2], 61)
  expect_type(dat_hit[, coff],      "double")
  expect_type(dat_hit[, maic],      "double")
  expect_type(dat_hit[, modl],      "character")
  expect_type(dat_hit[, boot_hitc], "integer")
  expect_type(dat_hit[, modl_ga],   "double")
  expect_type(dat_hit[, modl_tp],   "double")
})
