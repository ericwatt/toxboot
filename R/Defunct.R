#' @title DEFUNCT
#'
#' @description
#' These functions were renamed and now have diverged. They are no longer
#' compatible and will not run.
#'
#' @rdname toxboot-defunct
#'
#' @aliases toxbootGetOpts toxbootSetOpts
#'
#' @details The following functions are defunct and will be removed; use
#' the replacements indicated below:
#'   \itemize{
#'     \item{toxbootGetOpts: \code{\link{toxbootConfList}}}
#'     \item{toxbootSetOpts: \code{\link{toxbootConf}}}
#'   }
#'
#' @export toxbootGetOpts toxbootSetOpts
toxbootGetOpts <- function() {
  .Defunct("toxbootConfList")
}
toxbootSetOpts <- function () {
  .Defunct("toxbootConf")
}
