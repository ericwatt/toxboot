if(getRversion() >= "2.15.1")  utils::globalVariables(c("cndx", "wllt", "aeid",
                                                        "spid", "resp", "m3id",
                                                        "m4id", "logc"))

#' Query ToxCast and format data to pass to \code{toxboot}
#'
#' \code{toxbootQueryToxCast} passes .
#'
#' @param aeids  integer vector contains which aeids to query. Values will be
#'   passed to tcplLoadData.
#'
#' @return dat, a data.table corresponding to the level 3 data for the aeids
#'   with columns added for \code{m4id} and \code{bmad}.
#'
#' @details Because \code{bmad} is calculated for all chemicals in the assay,
#'   the entire aeid must be queried. After calculation of \code{bmad} chemicals
#'   can be further subset prior to running toxboot.
#'
#'   The assumption is that \code{\link{tcplConf}} has been setup to pull data
#'   from the correct database.
#'
#' @import data.table
#' @importFrom tcpl tcplLoadData
#'
#' @export
toxbootQueryToxCast <- function(aeids){

  dat_L3_all <- tcplLoadData(lvl = 3L, fld = "aeid", val = aeids)
  dat_L4_agg <- tcplLoadData(lvl = "agg", fld = "aeid", val = aeids)

  dat <- merge(dat_L3_all[, list(m3id, spid, aeid, logc, resp, cndx, wllt)],
               dat_L4_agg[, list(m4id, m3id)],
               by = "m3id",
               all=TRUE)
  dat[,
      bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE),
      by = aeid]

  dat <- dat[m4id %in% dat_L4_agg[!(is.na(m4id)), m4id]]

  #remove unwanted columns before returning
  dat[, cndx := NULL]
  dat[, wllt := NULL]
  return(dat)
}
