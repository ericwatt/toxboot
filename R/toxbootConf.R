#' @rdname toxboot_config_funcs
#' @export
toxbootConf <- function (mongo_host = NULL,
                         collection = NULL,
                         user = NULL,
                         pass = NULL,
                         db = NULL,
                         port = NULL) {

  if (!is.null(mongo_host)) options("TOXBOOT_HOST" = mongo_host)
  if (!is.null(collection)) options("TOXBOOT_COLLECTION" = collection)
  if (!is.null(user)) options("TOXBOOT_USER" = user)
  if (!is.null(pass)) options("TOXBOOT_PASS" = pass)
  if (!is.null(db)) options("TOXBOOT_DB" = db)
  if (!is.null(port)) options("TOXBOOT_PORT" = port)

}

#' @rdname toxboot_config_funcs
#' @export
toxbootConfSave <- function () {

  if(any(sapply(toxbootConfList(), is.null))) {
    stop("One of the toxboot settings is NULL. Saving the configuration file ",
         "with a NULL setting\nwill keep the package from loading in future ",
         "sessions.")
  }

  mongo_host <- options()$TOXBOOT_HOST
  mongo_host <- if(is.na(mongo_host)) NA else shQuote(mongo_host)
  collection <- options()$TOXBOOT_COLLECTION
  collection <- if(is.na(collection)) NA else shQuote(collection)
  user <- options()$TOXBOOT_USER
  user <- if(is.na(user))             NA else shQuote(user)
  pass   <- options()$TOXBOOT_PASS
  pass   <- if(is.na(pass))           NA else shQuote(pass)
  db   <- options()$TOXBOOT_DB
  db   <- if(is.na(db))               NA else shQuote(db)
  port   <- options()$TOXBOOT_PORT
  port   <- if(is.na(port))           NA else shQuote(port)

  pkgd <- system.file(package = "toxboot")
  conf_file <- file.path(pkgd, "toxboot.config")

  cat("## Toxboot MongoDB authentication settings.\n",
      "## See ?toxbootConf for more info about these settings.\n",
      "\n",
      "mongo_host =", mongo_host, "# the database server IP address", "\n",
      "collection =", collection, "# the database server collection", "\n",
      "user =", user, "# username for database db", "\n",
      "pass =", pass, "# password for username on db", "\n",
      "db   =", db, "# the database where the user is authenticated", "\n",
      "port =", port, "# the port to connect on. Default for mongoDB is 27017", "\n",
      sep = " ",
      file = conf_file,
      append = FALSE)

}

#' @rdname toxboot_config_funcs
#' @export
toxbootConfReset <- function () {

  pkgd <- system.file(package = "toxboot")
  conf_file <- file.path(pkgd, "toxboot.config")

  cat("## Toxboot MongoDB authentication settings.",
      "## See ?toxbootConf for more info about these settings.",
      "",
      "mongo_host = NA # the database server IP address",
      "collection = NA # the database server collection",
      "user = NA# username for database db",
      "pass = NA # password for username on db",
      "db = NA # the database where the user is authenticated",
      "port = NA # the port to connect on. Default for mongoDB is 27017",
      sep = "\n",
      file = conf_file,
      append = FALSE)

}

#' @rdname toxboot_config_funcs
#' @export
toxbootConfLoad <- function () {

  pkgd <- system.file(package = "toxboot")
  conf_file <- file.path(pkgd, "toxboot.config")

  source(conf_file, local = TRUE)

  toxbootConf(mongo_host, collection, user, pass, db, port)
}

#' @rdname toxboot_config_funcs
#' @export
toxbootConfList <- function(show.pass = FALSE) {

  opts <- list("TOXBOOT_HOST",
               "TOXBOOT_COLLECTION",
               "TOXBOOT_USER",
               "TOXBOOT_DB",
               "TOXBOOT_PORT")
  if (show.pass) opts <- c(opts, "TOXBOOT_PASS")
  opt_list <- do.call(options, opts)
  return (opt_list)

}
