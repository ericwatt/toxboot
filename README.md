<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/ericwatt/toxboot.svg?branch=master)](https://travis-ci.org/ericwatt/toxboot) [![Coverage Status](https://img.shields.io/codecov/c/github/ericwatt/toxboot/master.svg)](https://codecov.io/github/ericwatt/toxboot?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/toxboot)](https://cran.r-project.org/package=toxboot) [![DOI](https://zenodo.org/badge/18526/ericwatt/toxboot.svg)](https://zenodo.org/badge/latestdoi/18526/ericwatt/toxboot)

toxboot
=======

toxboot is a package used to quantify uncertainty in the USEPA's [ToxCast](https://www.epa.gov/chemical-research/toxicity-forecaster-toxcasttm-data) data using bootstrap methods. There are three main steps:

-   Query and format ToxCast data
-   Perform bootstrap calculation
    -   Generate bootstrap resamples
    -   Perform non linear regression on each resample
    -   Write results to memory, file, or database
-   Analyze results
    -   Query data from database
    -   Calculate model selection and hit call
    -   Analyze results, generate plots

Two included datasets, erl3data and erl5data, let you perform all steps of the analysis and learn how to use the package. For complete functionality, install the ToxCast [MySQL Database](ftp://newftp.epa.gov/comptox/High_Throughput_Screening_Data/MySQL_Data) to gain access to all available concentration response data.

Configuration
=============

There are multiple options for how to store the results. When running `toxboot()` or `toxbootmc()` functions you pass a parameter destination that can have one of 4 values:

-   memory
-   file
-   mongo
-   mysql

Options for mongo or mysql require some configuration.

MongoDB
-------

For bootstrapping a large number of curves, or performing a large number of resamples per curve, the suggested destination is `mongo`. This package uses `mongolite` as the package to communicate with a Mongo database. Authentication and connection parameters for the MongoDB are managed with `toxbootConf` and related functions. Users of the `tcpl` package will be familiar with these settings.

After installing `toxboot` you can setup these parameters:

``` r
library(toxboot)
toxbootConf(mongo_host = "123.45.67.89",
            collection = "prod_external_invitrodb_v2"
            user = "username",
            pass = "password",
            db = "bootstrap",
            port = "27017")
```

You can view the current parametes with `toxbootConfList`, save them to the configuration file with `toxbootConfSave`, and load from the configuration file with `toxbootConfLoad`. The configuration file can be reset to the installation defaults of `NA` using `toxbootConfReset`.

``` r
toxbootConfList(show.pass = TRUE)
toxbootConfSave()
toxbootConf(mongo_host = NA, 
            collection = NA, 
            user = NA, 
            pass = NA, 
            db = NA,
            port = NA)
toxbootConfList(show.pass = TRUE)
toxbootConfLoad()
toxbootConfList(show.pass = TRUE)
```

The design and implementation of `toxbootConf` mirrors `tcplConf` so that users familiar with the `tcpl` package will find it easy to use. Note however that `toxbootConf` is used only to manage MongoDB parameters. Unlike `tcpl`, `toxboot` stores MySQL parameters differently as explained in the next section.

MySQL
-----

Authentication and connection parameters are handled using a MySQL configuration file as recommended by the `RMySQL` package. This file can be used to maintain all of your MySQL parameters, which can then be accessed by name.

    [toxboot]
    user = username
    password = password
    host = website.com
    database = dev_toxboot

This package will use the group named `toxboot` for connection parameters.

The function `toxbootMysqlCreateTable` will create and format the correct table. Use caution as this will drop the table if it already exists.

Example Workflow
================

Using the included data, and keeping everything in memory, explore the uncertainty in potency for all curves that flagged as active:

``` r
m4ids <- erl5data[hitc == 1L, m4id]
dat <- toxbootmc(dat = erl3data, 
                 boot_method = "smooth",
                 m4ids = m4ids,
                 cores = 1, 
                 destination = "memory", 
                 replicates = 100) %>%
  toxbootHitParamCI(erl5data)
```

A real world example pulling from the ToxCast database using MongoDB to store the results would run as the following.

``` r
assay_names <- c("NVS_NR_bER",
                 "OT_ER_ERaERa_1440",
                 "ATG_ERa_TRANS_up",
                 "TOX21_ERa_LUC_BG1_Agonist",
                 "ACEA_T47D_80hr_Positive")

aeid_table_full <- tcplLoadAeid()
aeid_table <- aeid_table_full[aenm %in% assay_names]
aeids <- aeid_table[, aeid]

dat_to_boot <- toxbootQueryToxCast(aeids = aeids)
toxbootmc(dat = dat_to_boot, 
          boot_method = "smooth",
          cores = 40, 
          destination = "mongo", 
          replicates = 1000) 

fields <- c("m4id", "max_med", "hill_ga", "hill_gw", "hill_tp", "hill_aic", 
            "gnls_ga", "gnls_gw", "gnls_tp",  "gnls_la", "gnls_lw", "gnls_aic", 
            "cnst_aic")
dat_L5 <- tcplLoadData(5L, fld = "aeid", val = aeids, type = "mc")
dat <- toxbootGetMongoFields(aeid = aeids, fields = fields) %>%
  toxbootHitParamCI(dat_L5)
```
