#' @title DEPRECATED
#'
#' @description
#' \code{toxbootSetOtps} is deprecated. See \code{toxbootConf}.
#'
#' @param mongo_host Character of length 1, database IP address
#' @param DBNS Character of length 1, the collection on the database to read
#'   from and write to
#' @param user Character of length 1, username to authenticate
#' @param pass Character of length 1, password that corresponds to username
#' @param db Character of length 1, database where the username and pass are
#'   authenicated. Can be an IP address or a url.
#'
#' @export
toxbootSetOpts <- function (mongo_host = NULL, DBNS = NULL, user = NULL, pass = NULL, db = NULL) {

  .Deprecated("toxbootConf")
  if (!is.null(mongo_host)) options("TOXBOOT_HOST" = mongo_host)
  if (!is.null(DBNS)) options("TOXBOOT_DBNS" = DBNS)
  if (!is.null(user)) options("TOXBOOT_USER" = user)
  if (!is.null(pass)) options("TOXBOOT_PASS" = pass)
  if (!is.null(db)) options("TOXBOOT_DB" = db)

}
