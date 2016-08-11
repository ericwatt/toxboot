#' @title DEPRECATED
#'
#' @description
#' \code{toxbootGetOtps} is deprecated. See \code{toxbootConfList}.
#'
#' @param show.pass Logical, should the password be returned
#'
#' @export
toxbootGetOpts <- function(show.pass = FALSE) {

  .Deprecated("toxbootConfList")

  opts <- list("TOXBOOT_HOST", "TOXBOOT_DBNS", "TOXBOOT_USER", "TOXBOOT_DB")
  if (show.pass) opts <- c(opts, "TOXBOOT_PASS")
  opt_list <- do.call(options, opts)
  return (opt_list)

}
