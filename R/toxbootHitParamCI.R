if(getRversion() >= "2.15.1")  utils::globalVariables(c("m4id",
                                                        "coff",
                                                        "maic",
                                                        "cnst_aic",
                                                        "hill_aic",
                                                        "gnls_aic",
                                                        "modl",
                                                        "boot_hitc",
                                                        "hill_tp",
                                                        "max_med",
                                                        "gnls_tp",
                                                        "modl_ga",
                                                        "modl_tp",
                                                        "hill_ga",
                                                        "gnls_ga",
                                                        "modl_gw",
                                                        "hill_gw",
                                                        "gnls_gw"))


#' Performs Hit Call and Model Winner Selection on Bootstrap Results
#'
#' \code{toxbootHitParamCI} takes input dose response data and returns a matrix
#' of sampled dose response values.
#'
#' @param dat_db data.table, a data.table resulting from a toxboot query
#'   minimally with columns m4id, max_med, hill_ga, hill_gw, hill_tp, hill_aic,
#'   gnls_ga, gnls_gw, gnls_tp, gnls_la, gnls_lw, gnls_aic, cnst_aic
#' @param dat_pipe data.table. Minimally this data.table must have a column for
#'   m4id and coff (cutoff value, used in determining the hitcall). Often use
#'   the resulting data.table from \code{tcplLoadData(lvl = 5L)}.
#'
#' @details Based upon mc5 processing in \code{tcpl}.
#'
#'   This function will first choose a winning model for each bootstrap
#'   replicate, based on that row's values for cnst_aic, hill_aic, gnls_aic.
#'
#'   Second, if the winning model is hill or gnls, the max_med is greater than
#'   the cutoff, and the winning model modl_tp parameter is greater than the
#'   cutoff, the hitcall for that row is 1 (boot_hitc == 1).
#'
#'   Finally, if boot_hitc == 1, values for modl_ga, modl_gw, and modl_tp are
#'   set to the corresponding values in the winning model. Else, these
#'   parameters are set to NA.
#'
#' @return dat_boot, a data.table equivalent to dat_db with added columns coff
#'   (from dat_pipe), maic (winning model aic), modl (winning model), boot_hitc,
#'   modl_ga, modl_gw, and modl_tp
#'
#' @import data.table
#'
#' @export
toxbootHitParamCI <- function(dat_db, dat_pipe){
  dat_boot <- merge(dat_db,
                    dat_pipe[, list(m4id,
                                    coff)],
                    by = "m4id")

  dat_boot[, maic := pmin(cnst_aic, hill_aic, gnls_aic, na.rm = TRUE)]
  dat_boot[, modl := NA_character_]
  dat_boot[gnls_aic == maic, modl := "gnls"]
  dat_boot[hill_aic == maic, modl := "hill"]
  dat_boot[cnst_aic == maic, modl := "cnst"]


  # Make the hitcall ----------
  dat_boot[ , boot_hitc := 0L]

  dat_boot[modl == "hill" &
             hill_tp >= coff &
             max_med >= coff,
           boot_hitc := 1L]

  dat_boot[modl == "gnls" &
             gnls_tp >= coff &
             max_med >= coff,
           boot_hitc := 1L]


  dat_boot[, modl_ga := NA_real_]
  dat_boot[, modl_tp := NA_real_]
  dat_boot[, modl_gw := NA_real_]
  dat_boot[boot_hitc==1L & modl == "hill", modl_ga := hill_ga]
  dat_boot[boot_hitc==1L & modl == "gnls", modl_ga := gnls_ga]
  dat_boot[boot_hitc==1L & modl == "hill", modl_tp := hill_tp]
  dat_boot[boot_hitc==1L & modl == "gnls", modl_tp := gnls_tp]
  dat_boot[boot_hitc==1L & modl == "hill", modl_gw := hill_gw]
  dat_boot[boot_hitc==1L & modl == "gnls", modl_gw := gnls_gw]

  return(dat_boot[])

}
