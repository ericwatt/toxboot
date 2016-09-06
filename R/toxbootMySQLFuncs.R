#' Function to query toxboot results from MySQL
#'
#' \code{toxbootGetMySQLFields} queries the toxboot MySQL table and returns a
#' data.table with the requested results
#'
#' @param fields  a vector specifying which columns to return. Default to '*'
#'   which will return all columns
#' @param table  the name of the table to query. By default 'toxboot'.
#' @param ... parameters to query on. Format is query_field = query_values.
#'
#' @details Use the fields parameters to specify which columns to return. The
#'   parameter 'table' defaults to 'toxboot' which is the default table for
#'   writing and reading results. All other parameters will be passed as values
#'   to select on.
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' toxbootmc(dat = erl3data,
#'           m4ids = unique(erl3data[, m4id])[1:50],
#'           boot_method = "smooth",
#'           destination = "mysql",
#'           replicates = 10,
#'           cores = 8)
#' toxbootGetMySQLFields()
#' toxbootGetMySQLFields(m4id = 1288500)
#' toxbootGetMySQLFields(m4id = 1288500, fields = c("boot_method", "hill_ga"))
#' toxbootGetMySQLFields(m4id = 1288500, replicates = 10, fields = c("boot_method", "hill_ga"))
#' }
#'
#' @export
toxbootGetMySQLFields <- function(fields = '*', table = "toxboot", ...){

  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("RMySQL and DBI needed to use this function.
             Please install them.",
           call. = FALSE)
    }
  }

  con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
  columns <- paste(fields, collapse = ", ")
  query_list <- list(...)
  query <- paste('SELECT ', columns, ' FROM ', table, sep = "")
  if(length(query_list) > 0){
    query_sel_list <- vector("list", length = length(query_list))
    for(i in 1:length(query_list)){
      field_name <- names(query_list)[i]
      field_values <- query_list[[i]]
      if(length(field_values) > 1){
        query_sel_list[[i]] <- paste(field_name, " IN ('", paste(field_values, collapse = "' , '"), "')", sep = "")
      }else{
        query_sel_list[[i]] <- paste(field_name, " = '", field_values, "'", sep = "")
      }
    }
    query_selection <- paste(query_sel_list, collapse = " AND ")
    query <- paste(c(query, query_selection), collapse = " WHERE ")
  }
  res <- DBI::dbSendQuery(con, query)
  dat_toxboot <- data.table(DBI::dbFetch(res, n = -1))
  DBI::dbClearResult(res)
  DBI::dbDisconnect(con)

  return(dat_toxboot[])
}
