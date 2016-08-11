#' Concentration response data from 200 ER assay curves
#'
#' A dataset containing the output from \code{\link{toxbootQueryToxCast}}. A
#' representative assay from each vendor was chosen to pass to
#' toxbootQueryToxCast, and a random sampling of 200 m4ids were then selected
#' from this output.
#'
#' The assays chosen were:
#' \itemize{
#'   \item NVS_NR_bER
#'   \item OT_ER_ERaERa_1440
#'   \item ATG_ERa_TRANS_up
#'   \item TOX21_ERa_LUC_BG1_Agonist
#'   \item ACEA_T47D_80hr_Positive
#' }
#'
#' @format A data.table with 5580 rows and 16 variables:
#' \itemize{
#'   \item m3id : unique identifier for each concentration response value
#'   \item spid : sample id
#'   \item aeid : assay endpoint id
#'   \item logc : log10 of the uM concentration
#'   \item resp : response value
#'   \item m4id : unique identifier for the concentration response curve
#'   \item bmad : baseline median absolute deviation. A measure of baseline
#'     noise that is used in the fitting routine by \code{\link{tcplFit}} and
#'     is used as by \code{\link{toxboot}} as a measurement of noise when
#'     \code{boot_method = smooth}.
#' }
#' @source \url{https://www.epa.gov/chemical-research/toxicity-forecaster-toxcasttm-data/}
"erl3data"

#' Fit parameters, model selection, and hit calls from 200 ER assay curves
#'
#' A dataset containing the ToxCast level 5 data corresponding to the m4ids in
#' \code{\link{erl3data}}. This data is provided so that the cutoff values for
#' these assays can be used in \code{\link{toxbootHitParamCI}} to determine
#' winning model and hit call selection for bootstrap results. This dataset is
#' also useful to compare min/med/max bootstrap results to the point estimate in
#' the ToxCast pipeline value.
#'
#' @format A data.table with 200 rows and 67 variables
#' @source
#' \url{https://www.epa.gov/chemical-research/toxicity-forecaster-toxcasttm-data/}
#'
"erl5data"

#' Bootstrap results for m4id = 9057756 using 1000 replicates
#'
#' A dataset containing the bootstrapped results of m4id = 9057756 found in the
#' erl3data dataset. Used as an example for plotting in the vignette, which
#' shows the code used to generate the data.
#'
#' @format A data.table with 1000 rows and 59 columns
#'
"dat1000"

#' Bootstrap results for m4id = 9057756 using 10000 replicates
#'
#' A dataset containing the bootstrapped results of m4id = 9057756 found in the
#' erl3data dataset. Used as an example for plotting in the vignette, which
#' shows the code used to generate the data.
#'
#' @format A data.table with 10000 rows and 59 columns
#'
"dat10000"
