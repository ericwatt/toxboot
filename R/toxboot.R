if(getRversion() >= "2.15.1")  utils::globalVariables(c("logc", "resp",
                                                        "bmad", "m3id"))

#' Main function to sample, fit, and write results to mongoDB
#'
#' \code{toxboot} is the main function that performs the bootstrap sampling,
#'   fitting, and writing to the database
#'
#' @param dat A data.table typically generated with
#'   \code{\link{toxbootQueryToxCast}}. Required columns are:
#'     \itemize{
#'       \item logc: numeric, contains concentrations
#'       \item resp: numeric, normalized response values
#'       \item m3id: numeric, value unique to each row corresponding to an
#'                   individual concentration and response
#'       \item m4id: numeric, value unique to an aeid/spid pair. Multiple
#'                   m3ids per m4id
#'       \item aeid: numeric, assay id
#'       \item spid: character, sample ID
#'       \item bmad: numeric, baseline mad. Unique to an aeid.
#'     }
#' @param m4id numeric length 1, m4id to bootstrap. Choice of m4id will
#'   determine which rows are selected, and therefore the values of logc, resp,
#'   m3id, aeid, spid, and bmad.
#' @param boot_method parameter passed to \code{\link{toxbootReplicates}} to
#'   determine sampling method
#' @param replicates number of bootstrap samples. Default 100
#' @param concvals logical, default is FALSE. If TRUE, dose response samples
#'   written to the database as well.
#' @param destination string length 1, options are "mongo", "mysql", "file",
#'   "memory"
#' @param table_name string length 1, the name of the MySQL table to write
#'   results to. Default is "toxboot". The table can be erased and configured
#'   used \code{\link{toxbootMysqlCreateTable}}
#'
#' @details \code{toxboot} is the workhorse function of this package. This
#'   function will typically be wrapped in a mclapply to perform in parallel
#'   using \code{\link{toxbootmc}}. The dose response data is passed to
#'   \code{\link{toxbootReplicates}}. The returned matrix is passed to
#'   \code{\link[tcpl]{tcplFit}}.
#'
#'   There are multiple options for saving the results, based on the value of
#'   \code{destination}.
#'
#'   \describe{
#'
#'   \item{mongo}{If \code{destination} is set to "mongo" a connection to the
#'   mongo database will be created and the results will be written using the
#'   \code{mongolite} package. The connection will be established using the
#'   parameters retrieved using \code{\link{toxbootConfList}} by the function
#'   \code{\link{toxbootConnectMongo}}. See the documentation on these functions
#'   as well as \code{\link{toxbootConf}} for how to properly setup the MongoDB
#'   environment. For large scale screening of uncertainty parameters it is
#'   recommended that MongoDB be used for performance and scaling.}
#'
#'   \item{mysql}{Authentication and connection parameters are handled using a
#'   MySQL configuration file as recommended by the `RMySQL` package. This file
#'   can be used to maintain all of your MySQL parameters, which can then be
#'   accessed by name.
#'
#'   If the table \code{toxboot} has not already been setup or has been setup
#'   incorrectly, execute function \code{\link{toxbootMysqlCreateTable}} which
#'   will drop the table if it exists and create a new table with the correct
#'   columns.}
#'
#'   \item{file}{A directory \code{toxboot/} will be created. A csv file for
#'   each m4id will be created with name set to the value of m4id. The format
#'   of the file will be tabular with one row for each bootstrap replicate.
#'   Subsequent runs will be appended onto the file. This way further bootstrap
#'   results can be created without loss of previous computational work. Note
#'   that the file size can get quite large if many curves are run. With 1000
#'   replicates the file size will typically range from 300 to 600 KB per m4id.}
#'
#'   \item{memory}{Results will be returned as a single data.table. This is a
#'   reasonable option for checking a few curves or even an entire assay with
#'   1000 replicates if a suitable amount of memory is available. Care must be
#'   taken as the resulting data.table can become multiple GB in memory.}
#'   }
#'
#' @seealso \code{\link{toxbootReplicates}}
#'
#' @import data.table
#' @importFrom tcpl tcplFit
#' @importFrom utils packageVersion write.table
#'
#' @export
toxboot <- function(dat,
                    m4id,
                    boot_method,
                    replicates = 100,
                    concvals = F,
                    destination = "memory",
                    table_name = "toxboot"
                    ){

  if (destination == "mongo"){
    if (!requireNamespace("mongolite", quietly = TRUE)) {
      stop("mongolite needed to use destination 'mongo'. Please install it.",
           call. = FALSE)
    }
  } else if (destination == "mysql"){
    if (!(requireNamespace("RMySQL", quietly = TRUE) &
          requireNamespace("DBI", quietly = TRUE))) {
      stop("RMySQL and DBI needed to use destination 'mysql'.
             Please install them.",
           call. = FALSE)
    }
  } else if (!(destination %in% c("file", "memory"))){
    stop("value of destination not recognized")
  }

  starttime <- Sys.time()

  this_m4id <- m4id


  datchemval <- dat[m4id == this_m4id, list(logc, resp, bmad)]
  setkey(datchemval, logc) #sorts into correct order
  logc_vect <- datchemval[, logc]

  boot_replicates <- toxbootReplicates(datchemval = datchemval,
                                       boot_method = boot_method,
                                       replicates = replicates)


  datchemsample <- boot_replicates$datchemsample
  tempmat <- boot_replicates$tempmat

  # Fit each bootstrap sample using tcplFit
  temp2 <- sapply(datchemsample,
                  tcplFit,
                  logc = logc_vect,
                  bmad = datchemval$bmad[1])

  datchemresult <- as.data.table(t(temp2))[,1:44]
  setnames(datchemresult, fitpars)
  datchemresult <- sapply(datchemresult, as.numeric)
  datchemresult <- as.data.table(datchemresult)

  datsample <- data.table(t(tempmat))
  setnames(datsample, make.unique(as.character(logc_vect), sep = "__"))

  if (destination == "mongo"){
    toxbootWriteMongo(dat = dat, #insert into database
                      this_m4id = this_m4id,
                      boot_method = boot_method,
                      replicates = replicates,
                      logc_vect = logc_vect,
                      starttime = starttime,
                      concvals = concvals,
                      datchemresult = datchemresult,
                      datsample = datsample)
  } else if (destination == "memory"){
    datchemresult[, `:=` (m4id = this_m4id,
                          boot_method = boot_method,
                          replicates = replicates,
                          started = starttime,
                          modified = Sys.time(),
                          concvals = concvals,
                          toxboot_version = as.character(packageVersion("toxboot")),
                          bmad = datchemval$bmad[1])]
    return(datchemresult[])

  } else if (destination == "file"){
    datchemresult[, `:=` (m4id = this_m4id,
                          boot_method = boot_method,
                          replicates = replicates,
                          started = starttime,
                          modified = Sys.time(),
                          concvals = concvals,
                          toxboot_version = as.character(packageVersion("toxboot")),
                          bmad = datchemval$bmad[1])]
    dir.create(path="toxboot/", showWarnings = FALSE, recursive = TRUE)
    file.name <- paste("toxboot/", this_m4id, ".csv", sep = "")
    write.table(datchemresult,
                file = file.name,
                append = (file.exists(file.name)),
                sep = ",",
                quote = FALSE,
                row.names = FALSE,
                col.names = !(file.exists(file.name)))
  } else if (destination == "mysql"){
    datchemresult[, `:=` (m4id = this_m4id,
                          boot_method = boot_method,
                          replicates = replicates,
                          started = starttime,
                          modified = Sys.time(),
                          concvals = concvals,
                          toxboot_version = as.character(packageVersion("toxboot")),
                          bmad = datchemval$bmad[1])]
    con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
    DBI::dbWriteTable(con,
                      name = table_name,
                      value = datchemresult,
                      row.names = FALSE,
                      append = TRUE)
    DBI::dbDisconnect(con)
  } else cat("Uh, how did this happen?")

}
