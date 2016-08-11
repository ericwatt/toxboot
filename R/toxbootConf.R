#' @rdname toxboot_config_funcs
#' @export
toxbootConf <- function (mongo_host = NULL, DBNS = NULL, user = NULL, pass = NULL, db = NULL) {

  if (!is.null(mongo_host)) options("TOXBOOT_HOST" = mongo_host)
  if (!is.null(DBNS)) options("TOXBOOT_DBNS" = DBNS)
  if (!is.null(user)) options("TOXBOOT_USER" = user)
  if (!is.null(pass)) options("TOXBOOT_PASS" = pass)
  if (!is.null(db)) options("TOXBOOT_DB" = db)

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
  mongo_host <- if(is.na(mongo_host)) shQuote("NA_character_") else shQuote(mongo_host)
  DBNS <- options()$TOXBOOT_DBNS
  DBNS <- if(is.na(DBNS)) shQuote("NA_character_") else shQuote(DBNS)
  user <- options()$TOXBOOT_USER
  user <- if(is.na(user)) shQuote("NA_character_") else shQuote(user)
  pass   <- options()$TOXBOOT_PASS
  pass   <- if(is.na(pass))   shQuote("NA_character_")   else shQuote(pass)
  db   <- options()$TOXBOOT_DB
  db   <- if(is.na(db))   shQuote("NA_character_")   else shQuote(db)

  pkgd <- system.file(package = "toxboot")
  conf_file <- file.path(pkgd, "toxboot.config")

  cat("## Toxboot MongoDB authentication settings.\n",
      "## See ?toxbootConf for more info about these settings.\n",
      "\n",
      "mongo_host =", mongo_host, "# the database server IP address", "\n",
      "DBNS =", DBNS, "# the database server collection", "\n",
      "user =", user, "# username for database db", "\n",
      "pass =", pass, "# password for username on db", "\n",
      "db   =", db, "# the database where the user is authenticated", "\n",
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
      "mongo_host = NA_character_ # the database server IP address",
      "DBNS = NA_character_ # the database server collection",
      "user = NA_character_ # username for database db",
      "pass = NA_character_ # password for username on db",
      "db = NA_character_ # the database where the user is authenticated",
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

  toxbootConf(mongo_host, DBNS, user, pass, db)
}

#' @rdname toxboot_config_funcs
#' @export
toxbootConfList <- function(show.pass = FALSE) {

  opts <- list("TOXBOOT_HOST", "TOXBOOT_DBNS", "TOXBOOT_USER", "TOXBOOT_DB")
  if (show.pass) opts <- c(opts, "TOXBOOT_PASS")
  opt_list <- do.call(options, opts)
  return (opt_list)

}
