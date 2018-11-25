#' Hill Curve.
#'
#' \code{hill_curve} is used calculate response values given hill parameters.
#'
#' Given input values for the hill parameters and a concentration, this function
#' returns the response value at the given concentration. Used to plot the hill
#' equation resulting from a given fit.
#'
#' @param hill_tp Numeric, hill top parameter.
#' @param hill_ga Numeric, hill AC50 parameter.
#' @param hill_gw Numeric, hill coefficient parameter.
#' @param lconc   Numeric, concentration (in log uM) to calculate the response.
#' @return y Numeric, the calculated response value at lconc.
#' @examples
#' hill_curve(hill_tp = 100, hill_ga = 0.5, hill_gw = 1, lconc = 0)
#' @export
hill_curve <- function(hill_tp, hill_ga, hill_gw, lconc){
  hill_tp/(1+10^((hill_ga - lconc)*hill_gw))
}

#' Gain Loss Curve.
#'
#' \code{gnls_curve} is used calculate response values given gnls parameters.
#'
#' Given input values for the gnls parameters and a concentration, this function
#' returns the response value at the given concentration. Used to plot the gnls
#' equation resulting from a given fit.
#'
#' @param top     Numeric, gnls top parameter.
#' @param ga      Numeric, gnls gain AC50 parameter.
#' @param gw      Numeric, gnls gain coefficient parameter.
#' @param la      Numeric, gnls loss AC50 parameter.
#' @param lw      Numeric, gnls loss coefficient parameter.
#' @param lconc   Numeric, concentration (in log uM) to calculate the response.
#' @return y Numeric, the calculated response value at lconc.
#' @examples
#' gnls_curve(top = 100, ga = 0.5, gw = 1, la = 1.5, lw = 8, lconc = 0)
#' @export
gnls_curve <- function(top, ga, gw, la, lw, lconc){
  gain <- 1/(1+10^((ga - lconc)*gw))
  loss <- 1/(1+10^((lconc - la)*lw))
  return(top*gain*loss)
}
