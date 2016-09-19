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

  nmed_gtbls <- c(2, 2, 2, 2, 4, 4, 3, 3, 4, 3, 4, 4, 6, 6, 3, 4, 1, 2, 5, 5,
                  0, 0, 0, 0, 2, 2, 0, 0, 1, 1, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0)
  gnls_ers <- c(-1.807594985, -1.459023911, -1.573828628, -1.777077464, 2.40987324,
                2.406260863, 2.191250177, 1.781406928, 1.167992319, 1.863780877,
                2.160360009, 2.142468526, 2.057942036, 2.232296488, 2.242811639,
                2.194470631, 1.549692612, 1.99877665, 2.287199183, 1.784798892,
                NA, NA, NA, NA, 1.72576192, 1.705334943, NA, NA, 1.62146045,
                2.007935596, 1.795829507, NA, NA, NA, NA, 1.731623176, NA, NA,
                NA, NA)

  gnls_er_sds <- c(0.282330867, 0.259495502, 0.404799878, NaN, 0.243378855, 0.247652604,
                   NaN, NaN, NaN, 0.281104975, 0.221801601, 0.224600087, 0.279923346,
                   0.265922126, NaN, NA, NaN, 0.215854774, NaN, 0.232327683, NA,
                   NA, NA, NA, 0.268741279, 0.263008787, NA, NA, 0.224170319, NaN,
                   0.216426479, NA, NA, NA, NA, 0.237256641, NA, NA, NA, NA)

  expect_equal(nmed_gtbls, dat_mongo[, nmed_gtbl])
  expect_equal(nmed_gtbls, dat_memory[, nmed_gtbl])
  expect_equal(gnls_ers, dat_mongo[, gnls_er])
  expect_equal(gnls_ers, dat_memory[, gnls_er])


  expect_equal(dat_memory, dat_mongo, tolerance = .0000001, scale = 1)
})
