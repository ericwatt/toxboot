#' Filter and multicore wrapper for \code{\link{toxboot}}
#'
#' \code{toxbootmc} calculates bootstrapped curves for multiple m4ids using
#' mclapply to perform the calculation on many cores.
#'
#' @param dat data.table returned from \code{\link{toxbootQueryToxCast}}
#' @param m4ids  list of m4ids to perform bootstrap calculation. By default
#'   NULL, which will run on all m4ids in the in dat.
#' @param filter logical, default TRUE. If TRUE dat will be filtered to remove
#'   m4ids that are already in the mongoDB. Ignored if \code{destination} is not
#'   "mongo".
#' @param boot_method parameter passed to \code{\link{toxboot}}. Used also if
#'   \code{filter} is TRUE.
#' @param cores integer length 1, number of cores to supply as mc.cores to
#'   \code{\link{mclapply}}. By default set to 1L which will work on all systems
#'   including Windows. Increase to the number of cores on your system (see
#'   \code{\link{detectCores}}) to perform calculations in parallel.
#' @param destination string length 1, options are "mongo", "mysql", "file", "memory"
#' @param ... parameters passed to \code{\link{toxboot}}
#'
#' @return dat, a data.table corresponding to the level 3 data for the aeids
#'   with columns added for \code{m4id} and \code{bmad}.
#'
#' @details The function \code{\link{toxboot}} takes performs bootstrap analysis
#'   on a single m4id included in the data.table supplied. This function acts as
#'   a wrapping and cleaning function around \code{\link{toxboot}} to both
#'   simplify setup with common defaults and to provide multicore support
#'   through the use of \code{\link{mclapply}}.
#'
#' @import data.table
#' @importFrom tcpl tcplLoadData
#' @importFrom parallel detectCores mclapply
#'
#' @export
toxbootmc <- function(dat,
                      m4ids = NULL,
                      filter = TRUE,
                      boot_method = "smooth",
                      cores = 1L,
                      destination = "memory",
                      ...){

  # Check that correct packages are loaded for a given destination value

  if (destination == "mongo"){
    if (!requireNamespace("mongolite", quietly = TRUE)) {
      stop("mongolite needed to use destination 'mongo'. Please install it.",
           call. = FALSE)
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("jsonlite needed to use destination 'mongo'. Please install it.",
           call. = FALSE)
    }
  } else if (destination == "mysql"){
    if (!requireNamespace("RMySQL", quietly = TRUE)) {
      if (!requireNamespace("DBI", quietly = TRUE)) {
        stop("RMySQL and DBI needed to use destination 'mysql'.
             Please install them.",
             call. = FALSE)
      }
    }
  } else if (!(destination %in% c("file", "memory"))){
    stop("value of destination not recognized")
  }

  if (is.null(m4ids)) m4ids <- unique(dat[, m4id])
  if (filter & destination == "mongo"){ #remove m4ids already run
    try({
      old_m4ids <- toxbootGetMongoFields(boot_method = boot_method,
                                         m4id = m4ids,
                                         fields = "m4id")
      m4ids <- m4ids[!(m4ids %in% old_m4ids[, m4id])]
    }, silent = TRUE)
  }

  start_time <- Sys.time()

  boot_table <- mclapply(m4ids,
                         toxboot,
                         dat = dat,
                         boot_method = boot_method,
                         destination = destination,
                         ...,
                         mc.cores = cores,
                         mc.preschedule = FALSE)


  Sys.time() - start_time
  if (destination == "memory") {
    dat <- rbindlist(boot_table)
    return(dat[])
  }
  #return(boot_table)
}
