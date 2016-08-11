#' Replace NULL values with NA.
#'
#' \code{nullToNA} is used to convert any NULL values to NA in a list.
#'
#' This function is used when reading vectors from mongoDB. If the original
#' vector had a value of NA, rmongodb converts to NULL before writing to the
#' database. When rmongodb performs a find, these NULL values are left as NULL,
#' and when unlisted will shorten the vector. Before unlisting the vector, this
#' function is run to convert the NULL values to NA so that the vector remains
#' the correct length and that NAs are in the correct positions.
#'
#' @param x A list.
#' @return x with all NULL at the top level of the list replaced by NA.
#' @examples
#' \dontrun{nullToNA(list(1,5,'a', NULL, NA, 4))}
nullToNA <- function(x){
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' Convert x to numeric if x is a list.
#'
#' \code{listTonumeric} is used to convert x to a numeric type if x is a list.
#'
#' This function is used as part of reading data from mongoDB using rmongodb.
#' Typically values will be stored as numeric vectors. However, if an NA is in
#' the original vector, rmongodb will convert to NULL prior to writing to the
#' database. This gets read from the database as a list of length Y rather than
#' a numeric of length Y with positions that were NA now with value NULL. The
#' function \code{nullToNA} converts the NULL values to NA, and this function
#' converts the list back to a numeric type.
#'
#' @param x A list of length Y without NULL values.
#' @return x a numeric vector of length Y.
#' @examples
#' \dontrun{class(listTonumeric(list(1,2,3,4,NA,6,7)))}
listTonumeric <- function(x){
  if(is.list(x)){
    x <- as.numeric(x)
  }
  return(x)
}

anynulls <- function(x){
  numnull <- sum(sapply(x, is.null))
  return(numnull)
}
