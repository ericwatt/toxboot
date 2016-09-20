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
  expect_error(toxbootmc(dat = erl3data,
                         m4ids = unique(erl3data[, m4id])[1:5],
                         boot_method = "chunky",
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

context("MySQL")

test_that("MySQL table is created correctly", {
  expect_error(toxbootMysqlCreateTable(),
               "table_name not provided, please specify a table to create")

  toxbootMysqlCreateTable(table_name = "testthat")
  con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
  expect_true(DBI::dbExistsTable(con, "testthat"))

  #remove test table
  catch <- DBI::dbRemoveTable(con, "testthat")
  catch <- DBI::dbDisconnect(con)
})

test_that("MySQL destination works", {

  mysql_db <- ""

  try({
    con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
    mysql_info <- DBI::dbGetInfo(con)
    DBI::dbDisconnect(con)
    mysql_host <- mysql_info$host
    mysql_db   <- mysql_info$dbname
    mysql_user <- mysql_info$user
  }, silent = TRUE)

  if (mysql_db == "") {
    skip("MySQL not fully configured for testing.")
  }

  m4ids <- c(tail(erl5data[hitc == 1L, m4id], 10),
             tail(erl5data[hitc == 0L, m4id], 10))

  toxbootMysqlCreateTable(table_name = "testthat")

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
         destination = "mysql",
         replicates = 2,
         table_name = "testthat")

  dat_fetch <- toxbootGetMySQLFields(table = "testthat")

  dat_mysql <- dat_fetch[, !c("started", "modified"), with=FALSE]
  dat_memory <- dat[, !c("started", "modified"), with=FALSE]

  setcolorder(dat_mysql, names(dat_memory))

  expect_equal(dat_memory, dat_mysql, tolerance = .0000001, scale = 1)

  #cleanup database

  con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
  catch <- DBI::dbRemoveTable(con, "testthat")
  catch <- DBI::dbDisconnect(con)
})

context("Destination File")

test_that("Destination file works", {
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
         destination = "file",
         replicates = 2)

  df_list <- vector(mode = "list", length = length(m4ids))
  i <- 1
  for (chem in m4ids){
    file.name <- paste("toxboot/", chem, ".csv", sep = "")
    df_list[[i]] <- read.csv(file.name, stringsAsFactors = F)
    i <- i + 1
  }

  dat_file <- rbindlist(df_list)[, !c("started", "modified"), with=FALSE]
  dat_memory <- dat[, !c("started", "modified"), with=FALSE]

  expect_equal(dat_memory, dat_file, tolerance = .0000001, scale = 1)

  #cleanup directory

  unlink("./toxboot", recursive = TRUE)
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


context("toxbootConf")

test_that("configuration files are set and ready correctly", {
  start_opts <- toxbootConfList(show.pass = TRUE)

  toxbootConfLoad()
  start_file <- toxbootConfList(show.pass = TRUE)

  toxbootConf(mongo_host = "ds033046.mlab.com",
              collection = "toxboot_test",
              user = "toxboot_test",
              pass = "bootstrap",
              db = "toxboot",
              port = "33046")

  toxget <- toxbootConfList(show.pass = TRUE)
  expect_equal(toxget$TOXBOOT_HOST, "ds033046.mlab.com")
  expect_equal(toxget$TOXBOOT_COLLECTION, "toxboot_test")
  expect_equal(toxget$TOXBOOT_USER, "toxboot_test")
  expect_equal(toxget$TOXBOOT_PASS, "bootstrap")
  expect_equal(toxget$TOXBOOT_DB, "toxboot")
  expect_equal(toxget$TOXBOOT_PORT, "33046")

  # Check that file can be reset
  toxbootConfReset()
  toxbootConfLoad()
  toxget <- toxbootConfList(show.pass = TRUE)
  expect_equal(toxget$TOXBOOT_HOST, NA)
  expect_equal(toxget$TOXBOOT_COLLECTION, NA)
  expect_equal(toxget$TOXBOOT_USER, NA)
  expect_equal(toxget$TOXBOOT_PASS, NA)
  expect_equal(toxget$TOXBOOT_DB, NA)
  expect_equal(toxget$TOXBOOT_PORT, NA)

  #password isn't returned by default

  expect_null(toxbootConfList()$TOXBOOT_PASS)

  #return file to previous state

  toxbootConf(mongo_host = start_file$TOXBOOT_HOST,
              collection = start_file$TOXBOOT_COLLECTION,
              user = start_file$TOXBOOT_USER,
              pass = start_file$TOXBOOT_PASS,
              db = start_file$TOXBOOT_DB,
              port = start_file$TOXBOOT_PORT)
  toxbootConfSave()
  toxbootConfLoad()
  expect_equal(start_file, toxbootConfList(show.pass = TRUE))

  #return settings to previous state

  toxbootConf(mongo_host = start_opts$TOXBOOT_HOST,
              collection = start_opts$TOXBOOT_COLLECTION,
              user = start_opts$TOXBOOT_USER,
              pass = start_opts$TOXBOOT_PASS,
              db = start_opts$TOXBOOT_DB,
              port = start_opts$TOXBOOT_PORT)

  expect_equal(start_opts, toxbootConfList(show.pass=TRUE))

})
