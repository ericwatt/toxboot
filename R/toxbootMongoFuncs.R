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


  if(is.na(mongo)){
    stop("Connection to MongoDB not established.
         Check your toxbootConf settings.
         See ?toxbootConf for more information.")
  }

  return(mongo)
}

#' Build query for toxboot MongoDB
#'
#' \code{toxbootQueryBuild} imports toxboot query from list of input parameters.
#'
#' @param ...  parameters to query on. Format is query_field = query_values
#'
#' @return query, a mongo.bson object used for a query
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

  for (i in 1:length(morequery)){
    if (length(morequery[[i]]) > 1){
      morequery[[i]] <- list("$in" = morequery[[i]])
    }
  }

  query <- jsonlite::toJSON(morequery, auto_unbox=TRUE, POSIXt = "mongo")

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
#'   into a bson object using \code{rmongodb} and written to the mongoDB.
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

  #insert into database
  mongo <- toxbootConnectMongo()
  if(rmongodb::mongo.is.connected(mongo)){
    #create buffer to put into mongo
    m3id_vect <- dat[m4id == this_m4id, m3id]
    resp_vect <- dat[m4id == this_m4id, resp]
    conc_vect <- dat[m4id == this_m4id, logc]
    spid <- unique(dat[m4id == this_m4id, spid])
    aeid <- unique(dat[m4id == this_m4id, aeid])
    bmad <- unique(dat[m4id == this_m4id, bmad])
    buf <- rmongodb::mongo.bson.buffer.create()
    rmongodb::mongo.bson.buffer.append.int(buf, "aeid", aeid)
    rmongodb::mongo.bson.buffer.append.int(buf, "bmad", bmad)
    rmongodb::mongo.bson.buffer.append(buf, "boot_type", boot_method)
    rmongodb::mongo.bson.buffer.append(buf, "spid", spid)
    rmongodb::mongo.bson.buffer.append(buf, "m4id", this_m4id)
    rmongodb::mongo.bson.buffer.append.int(buf, "replicates", replicates)
    rmongodb::mongo.bson.buffer.append.time(buf, "started", starttime)
    rmongodb::mongo.bson.buffer.append.time(buf, "modified", Sys.time())
    rmongodb::mongo.bson.buffer.append(buf, "toxboot_version", as.character(packageVersion("toxboot")))
    rmongodb::mongo.bson.buffer.append(buf, "logc_vect", logc_vect)
    rmongodb::mongo.bson.buffer.append(buf, "m3id_vect", m3id_vect)
    rmongodb::mongo.bson.buffer.append(buf, "resp_vect", resp_vect)
    rmongodb::mongo.bson.buffer.append(buf, "conc_vect", conc_vect)
    rmongodb::mongo.bson.buffer.append(buf, "concvals", concvals)
    for (i in 1:length(fitpars)){
      rmongodb::mongo.bson.buffer.append(buf, fitpars[i], datchemresult[, get(fitpars[i])])
    }
    if(concvals == T){
      concnames <- names(datsample)
      for (i in 1:length(concnames)){
        rmongodb::mongo.bson.buffer.append(buf, concnames[i], datsample[, get(concnames[i])])
      }
    }
    b <- rmongodb::mongo.bson.from.buffer(buf)
    rmongodb::mongo.insert(mongo, toxbootConfList()$TOXBOOT_DBNS, b)
  }
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
#' @export
toxbootGetMongoFields <- function(fields, ...){

  if (!requireNamespace("rmongodb", quietly = TRUE)) {
    stop("rmongodb needed to connecto to MongoDB. Please install it.",
         call. = FALSE)
  }

  mongo <- toxbootConnectMongo()
  query <- toxbootQueryBuild(...)
  projection <- toxbootProjectionBuild(fields = fields)

  num_results <- toxbootMongoCount(mongo = mongo, query = query)
  if(num_results < 1){
    stop("No results returned for assay, chem, boot_method specified")
  }

  #Get the results, convert them to a data.table
  result_list <- rmongodb::mongo.find.all(mongo = mongo,
                                          ns = toxbootConfList()$TOXBOOT_DBNS,
                                          query = query,
                                          fields = projection)
  result_df <- list()

  rmongodb::mongo.destroy(mongo)

  for (i in 1:length(result_list)){
    numnull <- sum(unlist(lapply(result_list[[i]], anynulls)))
    result_clean <- NULL
    result_cleaner <- NULL
    if(numnull){
      result_clean <- lapply(result_list[[i]], nullToNA)
      result_cleaner <- lapply(result_clean, listTonumeric)
    }else{
      result_cleaner <- result_list[[i]]
    }
    result_df[[i]] <- as.data.frame(result_cleaner)
  }


  dat_result <- rbindlist(result_df, use.names=TRUE, fill=TRUE)

  return(dat_result)
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
  projection <- c(id_val, select_val)

}
