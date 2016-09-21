library(toxboot)
library(data.table)
context("Bootstrap Sampling Methods")


datchemval <- erl3data[m4id == 1294849, list(logc, resp, bmad)]
setkey(datchemval, logc) #sorts into correct order
replicates <- 10000

test_that("smooth sampling works", {
  boot_replicates <- toxbootReplicates(datchemval = datchemval,
                                         boot_method = "smooth",
                                         replicates = replicates)

  expect_type(boot_replicates, "list")
  expect_equal(length(boot_replicates), 2)

  datchemsample <- boot_replicates$datchemsample
  tempmat <- boot_replicates$tempmat

  expect_true(is.data.table(datchemsample))
  expect_true(is.matrix(tempmat))
  expect_equal(datchemsample, data.table(tempmat))

  expect_equal(dim(datchemsample),
               c(length(datchemval[, logc]), replicates))

  #smooth specific checks
  dat_ave <- datchemval[, list(ave = mean(resp)), by = list(logc)]
  expect_equal(rowMeans(datchemsample),
               rep(dat_ave[, ave], each = 3),
               tolerance = .1,
               scale = 1)
  rm(dat_ave)


  rm(boot_replicates)
  rm(datchemsample)
  rm(tempmat)
})

test_that("case sampling works", {
  boot_replicates <- toxbootReplicates(datchemval = datchemval,
                                       boot_method = "case",
                                       replicates = replicates)

  expect_type(boot_replicates, "list")
  expect_equal(length(boot_replicates), 2)

  datchemsample <- boot_replicates$datchemsample
  tempmat <- boot_replicates$tempmat

  expect_true(is.data.table(datchemsample))
  expect_true(is.matrix(tempmat))
  expect_equal(datchemsample, data.table(tempmat))

  expect_equal(dim(datchemsample),
               c(length(datchemval[, logc]), replicates))

  #case specific checks
  #should only sample from the observed response set
  expect_equal(sort(unique(c(tempmat))),
               sort(datchemval[, resp]))


  rm(boot_replicates)
  rm(datchemsample)
  rm(tempmat)
})

test_that("residuals_hill sampling works", {
  boot_replicates <- toxbootReplicates(datchemval = datchemval,
                                       boot_method = "residuals_hill",
                                       replicates = replicates)

  expect_type(boot_replicates, "list")
  expect_equal(length(boot_replicates), 2)

  datchemsample <- boot_replicates$datchemsample
  tempmat <- boot_replicates$tempmat

  expect_true(is.data.table(datchemsample))
  expect_true(is.matrix(tempmat))
  expect_equal(datchemsample, data.table(tempmat))

  expect_equal(dim(datchemsample),
               c(length(datchemval[, logc]), replicates))

  #residuals_hill specific checks
  #there should only be a maximum number of observed values
  #equal to number of observations multiplied by
  #the number of unique concentrations
  expect_equal(length(unique(c(tempmat))),
               length(datchemval[, resp]) * length(unique(datchemval[, logc])))


  rm(boot_replicates)
  rm(datchemsample)
  rm(tempmat)
})

test_that("residuals_gnls sampling works", {
  # change m4id to a gnls hit
  datchemval <- erl3data[m4id == 8509426, list(logc, resp, bmad)]
  setkey(datchemval, logc) #sorts into correct order

  boot_replicates <- toxbootReplicates(datchemval = datchemval,
                                       boot_method = "residuals_gnls",
                                       replicates = 10000)

  expect_type(boot_replicates, "list")
  expect_equal(length(boot_replicates), 2)

  datchemsample <- boot_replicates$datchemsample
  tempmat <- boot_replicates$tempmat

  expect_true(is.data.table(datchemsample))
  expect_true(is.matrix(tempmat))
  expect_equal(datchemsample, data.table(tempmat))

  expect_equal(dim(datchemsample),
               c(length(datchemval[, logc]), replicates))

  #residuals_gnls specific checks
  #there should only be a maximum number of observed values
  #equal to number of observations multiplied by
  #the number of unique concentrations
  expect_equal(length(unique(c(tempmat))),
               length(datchemval[, resp]) * length(unique(datchemval[, logc])))


  rm(boot_replicates)
  rm(datchemsample)
  rm(tempmat)
})
