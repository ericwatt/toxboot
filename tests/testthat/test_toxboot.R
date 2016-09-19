library(toxboot)
library(data.table)
library(mongolite)
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

context("MongoDB")

test_that("toxboot writes to and reads from MongoDB", {
  toxbootConf(mongo_host = "ds033046.mlab.com",
              collection = "toxboot_test",
              user = "toxboot_test",
              pass = "bootstrap",
              db = "toxboot",
              port = 33046)

  con <- toxbootConnectMongo()
  if(con$count() > 0)
    con$drop()
  rm(con)

  m4ids <- c(tail(erl5data[hitc == 1L, m4id], 10),
             tail(erl5data[hitc == 0L, m4id], 10))

  set.seed(1234)
  list <- lapply(m4ids,
                 toxboot,
                 dat = erl3data,
                 boot_method = "smooth",
                 destination = "memory",
                 replicates = 2)
  dat <- rbindlist(list)

  set.seed(1234)
  lapply(m4ids,
         toxboot,
         dat = erl3data,
         boot_method = "smooth",
         destination = "mongo",
         replicates = 2)

  dat_fetch <- toxbootGetMongoFields(fields = names(dat),
                                     m4id = unique(dat[, m4id]))

  setcolorder(dat_fetch, names(dat))

  expect_equal(dim(dat)[1], 40)
  expect_equal(dim(dat)[2], 52)
  expect_equal(dim(dat_fetch)[1], 40)
  expect_equal(dim(dat_fetch)[2], 52)
  expect_equal(names(dat), names(dat_fetch))

  dat_memory <- dat[, !c("started", "modified"), with=FALSE]
  dat_mongo <- dat_fetch[, !c("started", "modified"), with=FALSE]

  expect_equal(dat_memory, dat_mongo, tolerance = .0000001, scale = 1)
})
