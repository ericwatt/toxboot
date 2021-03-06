% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{erl3data}
\alias{erl3data}
\title{Concentration response data from 200 ER assay curves}
\format{A data.table with 5580 rows and 16 variables:
\itemize{
  \item m3id : unique identifier for each concentration response value
  \item spid : sample id
  \item aeid : assay endpoint id
  \item logc : log10 of the uM concentration
  \item resp : response value
  \item m4id : unique identifier for the concentration response curve
  \item bmad : baseline median absolute deviation. A measure of baseline
    noise that is used in the fitting routine by \code{\link{tcplFit}} and
    is used as by \code{\link{toxboot}} as a measurement of noise when
    \code{boot_method = smooth}.
}}
\source{
\url{https://www.epa.gov/chemical-research/toxicity-forecaster-toxcasttm-data/}
}
\usage{
erl3data
}
\description{
A dataset containing the output from \code{\link{toxbootQueryToxCast}}. A
representative assay from each vendor was chosen to pass to
toxbootQueryToxCast, and a random sampling of 200 m4ids were then selected
from this output.
}
\details{
The assays chosen were:
\itemize{
  \item NVS_NR_bER
  \item OT_ER_ERaERa_1440
  \item ATG_ERa_TRANS_up
  \item TOX21_ERa_LUC_BG1_Agonist
  \item ACEA_T47D_80hr_Positive
}
}
\keyword{datasets}
