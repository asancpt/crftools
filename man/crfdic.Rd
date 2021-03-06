% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crfdic.R
\name{crfdic}
\alias{crfdic}
\title{Compare variable names between CRF and data dictionary}
\usage{
crfdic(CRFcsv, Dictionaryxlsx, FocusCol = NULL)
}
\arguments{
\item{CRFcsv}{A filename of CRF csv file, exported from PDFCRF}

\item{Dictionaryxlsx}{A filename of data dictionary xlsx file mandatorily containing tabs of List, SUFFIX, EXCEPT}

\item{FocusCol}{Column name of data dictionary which focuses analysis}
}
\value{
List of output data of comparison of variables between a CRF-derived csv file and a data dictionary
}
\description{
\code{crfdic} uses CRF-derived csv file and a data dictionary excel to compare
variable names of each file
}
\examples{
\dontrun{
crfdic(CRFcsv = "foo.csv", Dictionaryxlsx = "foo.xlsx", Focus = NULL)
}
}
