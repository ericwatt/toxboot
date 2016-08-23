if(getRversion() >= "2.15.1")  utils::globalVariables(c("mongo_host", "DBNS", "user", "pass", "db"))

.onAttach <- function(libname, pkgname) {

  toxget <- toxbootConfList()

  mysql_host <- "Not Configured"
  mysql_db   <- "Not Configured"
  mysql_user <- "Not Configured"

  if (requireNamespace("RMySQL", quietly = TRUE)) {
    if (requireNamespace("DBI", quietly = TRUE)) {
      try({
        con <- DBI::dbConnect(drv = RMySQL::MySQL(), group = "toxboot")
        mysql_info <- DBI::dbGetInfo(con)
        DBI::dbDisconnect(con)
        mysql_host <- mysql_info$host
        mysql_db   <- mysql_info$dbname
        mysql_user <- mysql_info$user
      }, silent = TRUE)
    }
  }

  packageStartupMessage("toxboot ", as.character(utils::packageVersion("toxboot")),
                        "\nMongoDB settings (?toxbootConf):\n  ",
                        "TOXBOOT_HOST: ", toxget$TOXBOOT_HOST,
                        "\n  TOXBOOT_DBNS: ", toxget$TOXBOOT_DBNS,
                        "\n  TOXBOOT_USER: ", toxget$TOXBOOT_USER,
                        "\n  TOXBOOT_DB: ", toxget$TOXBOOT_DB,
                        "\nMySQL settings:\n",
                        "  Host: ", mysql_host,
                        "\n  Database: ", mysql_db,
                        "\n  User: ", mysql_user)

}
