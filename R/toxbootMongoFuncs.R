#' Setup connection to toxboot MongoDB
#'
#' \code{toxbootConnectMongo} imports toxboot options and creates the connection
#' to the mongoDB.
#'
#' @return mongo, a mongo connection object.
toxbootConnectMongo <- function(){

  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite needed to connect to MongoDB. Please install it.",
         call. = FALSE)
  }

  #Set log level to 3 to reduce messages

  mongologval <- mongolite::mongo_options()$log_level

  mongolite::mongo_options(log_level = 3)

  #Connect to database, confirm connection
  toxget <- toxbootConfList(show.pass = TRUE)
  mongo_host <- toxget$TOXBOOT_HOST
  collection <- toxget$TOXBOOT_COLLECTION
  user <- toxget$TOXBOOT_USER
  pass <- toxget$TOXBOOT_PASS
  toxdb <- toxget$TOXBOOT_DB
  port <- toxget$TOXBOOT_PORT

  mongo <- tryCatch(
    {
      mongolite::mongo(collection,
                       url = paste0("mongodb://",
                                    user,
                                    ":",
                                    pass,
                                    "@",
                                    mongo_host,
                                    ":",
                                    port,
                                    "/",
                                    toxdb))
    },
    error = function(cond) {
      message("There was trouble connecting to MongoDB")
      message("Here's the original error message:")
      message(cond)
      message("\n")
      # Choose a return value in case of error
      return(NA)
    },
    warning = function(cond) {
      message("There was trouble connecting to MongoDB")
      message("Here's the original warning message:")
      message(cond)
      message("\n")
      # Choose a return value in case of warning
      return(NA)
    },
    finally = NA
  )

  #Set mongo log value back to what it was

  mongolite::mongo_options(log_level = mongo_log_convert(mongologval))

  return(mongo)
}

#' Build query for toxboot MongoDB
#'
#' \code{toxbootQueryBuild} imports toxboot query from list of input parameters.
#'
#' @param ...  parameters to query on. Format is query_field = query_values
#'
#' @return query, a json object used for a query
toxbootQueryBuild <- function(...){

  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite needed to connect to MongoDB. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite needed to construct MongoDB query. Please install it.",
         call. = FALSE)
  }

  #Build the query to select documents
  morequery <- list(...)

  if (length(morequery) > 0){
    for (i in 1:length(morequery)){
      if (length(morequery[[i]]) > 1){
        morequery[[i]] <- list("$in" = morequery[[i]])
      }
    }
    query <- jsonlite::toJSON(morequery, auto_unbox = TRUE, POSIXt = "mongo")
  } else {
    query <- NULL
  }

  return(query)
}

#' Write bootstrap results to mongoDB
#'
#' \code{toxbootWriteMongo} writes results to a mongo database
#'
#' @param dat A data.table. Required columns are: logc: numeric, contains
#'   concentrations resp: numeric, normalized response values paired with
#'   concentrations m3id: numeric, value unique to each row corresponding to an
#'   individual concentration and response m4id: numeric, value unique to an
#'   aeid/spid pair. Multiple m3ids per m4id aeid: numeric, assay id spid:
#'   character, sample ID bmad: numeric, baseline mad. Unique to an aeid.
#' @param this_m4id numeric length 1, m4id to bootstrap. Choice of m4id will
#'   determine which rows are selected, and therefore the values of logc, resp,
#'   m3id, aeid, spid, and bmad.
#' @param boot_method parameter passed to \code{toxbootReplicates} to determine
#'   sampling method
#' @param replicates number of bootstrap samples. Default 100
#' @param logc_vect vect
#' @param starttime time
#' @param concvals logical, default is FALSE. If TRUE, dose response samples
#'   written to the database as well.
#' @param datchemresult data.table
#' @param datsample data.table used if concval = T
#'
#' @details The fitted results are assembled
#'   into a json object using \code{jsonlite} and written to the mongoDB.
#'
#' @seealso \code{\link{toxboot}}
#'
#' @import data.table
#' @importFrom utils packageVersion
#'
#' @export
toxbootWriteMongo <- function(dat,
                    this_m4id,
                    boot_method,
                    replicates,
                    logc_vect,
                    starttime,
                    concvals,
                    datchemresult,
                    datsample){

  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite needed to connect to MongoDB. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite needed to format data for MongoDB. Please install it.",
         call. = FALSE)
  }

  #combine data into json format

  list_data <- as.list(datchemresult)

  #add identification fields

  list_data$spid <- unique(dat[m4id == this_m4id, spid])
  list_data$aeid <- unique(dat[m4id == this_m4id, aeid])
  list_data$bmad <- unique(dat[m4id == this_m4id, bmad])
  list_data$boot_method <- boot_method
  list_data$m4id <- this_m4id
  list_data$replicates <- replicates
  list_data$started <- starttime
  list_data$modified <- Sys.time()
  list_data$toxboot_version <- as.character(packageVersion("toxboot"))
  list_data$logc_vect <- logc_vect
  list_data$m3id_vect <- dat[m4id == this_m4id, m3id]
  list_data$resp_vect <- dat[m4id == this_m4id, resp]
  list_data$conc_vect <- dat[m4id == this_m4id, logc]
  list_data$concvals <- concvals

  if(concvals == T){
    list_conc <- as.list(datsample)
    list_data <- c(list_data, list_conc)
  }

  #insert into database

  mongo <- toxbootConnectMongo()
  mongo$insert(jsonlite::toJSON(list_data, digits = 9))
  rm(mongo)

}

#' Query mongoDB and get requested fields
#'
#' \code{toxbootGetMongoFields} will query the mongo database for matching
#' bootstrap type(s), assay aeid(s), and sample id(s). It will then return a
#' data.table with the requested fields.
#'
#' Longer description here
#'
#' @param fields String, a vector of fields to return from the database. Passed
#'   to \code{toxbootProjectionBuild}
#' @param ...  parameters to query on, passed to \code{toxbootQueryBuild}.
#'   Format is query_field = query_values
#'
#' @return data table of requested fields
#'
#' @examples
#' \dontrun{
#' toxbootGetMongoFields(boot_type = "smooth", aeid = 1508L,
#' fields = c("hill_ga", "gnls_ga"))
#' }
#'
#' @import data.table
#'
#' @export
toxbootGetMongoFields <- function(fields, ...){

  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite needed to connect to MongoDB. Please install it.",
         call. = FALSE)
  }

  mongo <- toxbootConnectMongo()
  query <- toxbootQueryBuild(...)
  projection <- toxbootProjectionBuild(fields = fields)

  num_results <- toxbootMongoCount(mongo = mongo, query = query)
  if(num_results < 1){
    stop("No results returned for query provided specified\n
         Query:\n", query)
  }

  result_list <- mongo$iterate(query = query, fields = projection)$batch(num_results)

  rm(mongo)

  list_df <- vector(mode = "list", length = num_results)

  for (i in 1:length(result_list)){
    result_clean <- lapply(result_list[[i]], unlist)
    list_df[[i]] <- as.data.frame(result_clean, stringsAsFactors = FALSE)
  }

  dat <- rbindlist(list_df, use.names = TRUE, fill = TRUE)

  numeric_cols <- c("resp_max", "resp_min", "max_mean", "max_mean_conc", "max_med",
                    "max_med_conc", "logc_max", "logc_min", "cnst", "hill", "hcov",
                    "gnls", "gcov", "cnst_er", "cnst_aic", "cnst_rmse", "hill_tp",
                    "hill_tp_sd", "hill_ga", "hill_ga_sd", "hill_gw", "hill_gw_sd",
                    "hill_er", "hill_er_sd", "hill_aic", "hill_rmse", "gnls_tp",
                    "gnls_tp_sd", "gnls_ga", "gnls_ga_sd", "gnls_gw", "gnls_gw_sd",
                    "gnls_la", "gnls_la_sd", "gnls_lw", "gnls_lw_sd", "gnls_er",
                    "gnls_er_sd", "gnls_aic", "gnls_rmse", "nconc", "npts", "nrep",
                    "nmed_gtbl", "m4id", "replicates", "bmad")
  char_cols <- c("boot_method", "toxboot_version", "spid")
  logic_cols <- c("concvals")
  time_cols <- c("started", "modified")

  for (j in names(dat)) {
    if (j %in% char_cols) {
      set(dat, j = j, value = as.character(dat[[j]]))
    } else if (j %in% time_cols) {
      set(dat, j = j, value = as.POSIXct(dat[[j]]))
    } else if (j %in% logic_cols) {
      set(dat, j = j, value = as.logical(dat[[j]]))
    } else {
      set(dat, j = j, value = suppressWarnings(as.numeric(dat[[j]])))
    }
  }

  return(dat)
}

#' Build projection for toxboot MongoDB
#'
#' \code{toxbootProjectionBuild} builds a list that determined which fields
#' are returned.
#'
#' @param fields  string vector, fields to return.
#' @param id_val integer 0L or 1L. By default 0L, which will supress Object_ID
#'   from the projection. This unique ID is often not needed. Can be set to 1L
#'   to include in projection.
#'
#' @return projection, a named list where names are the fields to return in the
#'   projection and the values are 0L (don't return) or 1L (return).
toxbootProjectionBuild <- function(fields, id_val = 0L){

  if(!(id_val %in% c(0L, 1L))){
    warning("id_val should be 0L or 1L.
            Changing value to 1L")
    id_val <- 1L
  }
  #Build a list of fields to return, always add aeid, spid, boot_type
  id_val <- list("_id" = id_val)
  select_val <- as.list(rep(1L, length(fields)))
  names(select_val) <- fields
  projection <- jsonlite::toJSON(c(id_val, select_val),
                                 auto_unbox = TRUE,
                                 POSIXt = "mongo")

}
