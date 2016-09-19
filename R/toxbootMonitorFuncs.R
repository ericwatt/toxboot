#' Get number of documents that match query.
#'
#' \code{toxbootMongoCount} returns the number of objects that match the query.
#'
#' @param mongo, a mongo connection object returned from
#'   \code{toxbootConnectMongo()}
#' @param query, a mongo.bson object returned from \code{toxbootQueryBuild}. By
#'   default query will match all documents in the collection.
#'
#' @return num_results, an integer equal to the number of documents that match the query.
#'
#' @export
toxbootMongoCount <- function(mongo, query = NULL){

  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite needed to connect to MongoDB. Please install it.",
         call. = FALSE)
  }

  if (is.null(query)) query <- '{}'

  num_results <- mongo$count(query)

  return(num_results)

}
