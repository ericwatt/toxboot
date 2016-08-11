# toxboot 0.0.2.9002

Bugfixes

## Documentation 

### New functions

* `toxbootGetMySQLFields`

### Modified functions

* `toxbootmc`

### Deprecated functions

## Bugfixes

* Filtering in toxbootmc was not working. The list of old m4ids, old_m4ids, was returned as a data.table, not a vector. `m4ids[!(m4ids %in% old_m4ids)]` was failing silently, and no filtering was happening. Hundreds of thousands of new curves were run a second time. This has now been modified so that filtering is working correctly.

# toxboot 0.0.2.9001

Focus is on functions to simplify the preparation of input data and streamline the calculation and writing of bootstrap results. The vignette was heavily updated to reflect these new changes.

## Documentation 

* The previous workflow required the user to generate a script to query tcpl or otherwise obtain conc response data formatted like a level 3 dataset from toxcast but also including columns for bmad and m4id. The new function `toxbootQueryToxCast` handles this using by taking aeids as input.
* The data from `toxbootQueryToxCast` then needs to be filtered to select a subset of m4ids, either based on user input or by filtering out any m4id that is already found in the database. The new function `toxbootmc` has this filtering as a flag, and then this filtered list of m4ids is used as a vector that will be forked within `mclapply`. In this way, each core will be given a unique m4id to bootstrap.
* Separated function to write to MongoDB using new function `toxbootWriteMongo`. Will allow expansion to also allow writing to file, keeping in memory, or writing to a SQL database.
* Added parameter `destination` to `toxboot()` that allows the user to select `mongo`, `mysql`, `file`, or `memory` as destinations. See README and vignettes for detailed examples.

### New functions

* `toxbootmc`
* `toxbootQueryToxCast`
* `toxbootWriteMongo`

### Modified functions

* `toxboot`

### Deprecated functions

## Bugfixes

# toxboot 0.0.2.9000

This is a large update with several backward incompatible changes.

## Documentation 

* Changes to use toxbootConf and related functions. This matches the recent changes in `tcpl` so that the user experience is similar with both packages. This is also to get around having the `.onLoad()` function writing to toxboot.config, which was giving a NOTE in R CMD Check. Writing to toxboot.config is now handled by `toxbootConfSave` and `toxbootConfReset` rather than `.onload()`. These new functions are listed below, and documentation is now found in config_funcs.R.
* Moved package startup message to `.onAttach()` following advice from [R Packages](http://r-pkgs.had.co.nz/namespace.html).
* Created functions `toxbootConnectMongo`, `toxbootQueryBuild`, `toxbootProjectionBuild`. This code was repeated in different functions. Making stand alone functions follows DRY principles.

* Added a `NEWS.md` file to track changes to the package.
* Added a `README.Rmd` to give a more detailed description of package usage. `README.Rmd` is used to generate `README.md`, therefore `README.md` should not be modified by hand.

### New functions

* `toxbootConf`
* `toxbootConfList`
* `toxbootConfSave`
* `toxbootConfLoad`
* `toxbootConfReset`
* `.onAttach`
* `toxbootHitParamCI`
* `toxbootConnectMongo`
* `toxbootQueryBuild`
* `toxbootProjectionBuild`
* `toxbootMongoCount`

### Modified functions

* `.onLoad`
* `toxbootGetMongoFields` 
    * now using new function `toxbootConnectMongo` to connect, reusing connection code.
    * now using new function `toxbootQueryBuild` to build the query
    * now using new function `toxbootProjectionBuild` to build the projection
* `toxboot` 
    * using importFrom for each rmongodb function, removes need for rmongo:: with each. Using new function `toxbootConnectMongo` to connect, reusing connection code.

### Deprecated functions

* `toxbootSetOpts` -> `toxbootConf`
* `toxbootGetOpts` -> `toxbootConfList`

## Bugfixes





