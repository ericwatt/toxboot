#' Function to create properly formatted toxboot table in MySQL
#'
#' \code{toxbootMysqlCreateTable} creates table with correct column types which
#' will be written to by \code{toxboot} and \code{toxbootmc}.
#'
#'
#' @details Caution. This function will delete the current toxboot table if it
#'   exists. Still need to add some safeguards.
#'
#' @param table_name string length 1, the name of the MySQL table format and
#'   create
#'
#'
#' @export
toxbootMysqlCreateTable <- function(table_name = NULL){

  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("RMySQL and DBI needed to use this function.
           Please install them.",
           call. = FALSE)
    }
  }

  if (is.null(table_name)){
    stop("table_name not provided, please specify a table to create",
         call. = FALSE)
  }

  con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")

  if (DBI::dbExistsTable(con, table_name)) DBI::dbRemoveTable(con, table_name)

  crt <- DBI::dbSendStatement(con,
                              paste("CREATE TABLE", table_name, "(
                              m4id            bigint(20),
                              boot_method     VARCHAR(50),
                              replicates      INT,
                              started         text,
                              modified        text,
                              concvals        tinyint(1),
                              toxboot_version VARCHAR(50),
                              bmad            double,
                              resp_max        double,
                              resp_min        double,
                              max_mean        double,
                              max_mean_conc   double,
                              max_med         double,
                              max_med_conc    double,
                              logc_max        double,
                              logc_min        double,
                              cnst            tinyint(1),
                              hill            tinyint(1),
                              hcov            tinyint(1),
                              gnls            tinyint(1),
                              gcov            tinyint(1),
                              cnst_er         double,
                              cnst_aic        double,
                              cnst_rmse       double,
                              hill_tp         double,
                              hill_tp_sd      double,
                              hill_ga         double,
                              hill_ga_sd      double,
                              hill_gw         double,
                              hill_gw_sd      double,
                              hill_er         double,
                              hill_er_sd      double,
                              hill_aic        double,
                              hill_rmse       double,
                              gnls_tp         double,
                              gnls_tp_sd      double,
                              gnls_ga         double,
                              gnls_ga_sd      double,
                              gnls_gw         double,
                              gnls_gw_sd      double,
                              gnls_la         double,
                              gnls_la_sd      double,
                              gnls_lw         double,
                              gnls_lw_sd      double,
                              gnls_er         double,
                              gnls_er_sd      double,
                              gnls_aic        double,
                              gnls_rmse       double,
                              nconc           int,
                              npts            int,
                              nrep            double,
                              nmed_gtbl       int);"))

  DBI::dbClearResult(crt)

  if (!DBI::dbExistsTable(con, table_name)){
    stop("Table", table_name,  "was not created correctly.\n
         check your MySQL settings and permissions.",
         call. = FALSE)
  }

  catch <- DBI::dbDisconnect(con)
}
