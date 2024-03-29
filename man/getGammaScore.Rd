% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGammaScore.R
\name{getGammaScore}
\alias{getGammaScore}
\title{This function calculates gamma from screen raw read counts}
\usage{
getGammaScore(
  df,
  initcount,
  finalcount,
  spacertype = "spacertype",
  negctrllabel = "negctrl",
  doublings = "totaldoublings"
)
}
\arguments{
\item{df}{is a data frame, must contain column names matching initcount, finalcount, and spacertype}

\item{initcount}{is a string for name of column in df containing raw read counts of initial condition (e.g. T0 in growth screen)}

\item{finalcount}{is a string for name of a column in df containing raw read counts of final condition (e.g. Tfinal in growth screen)}

\item{spacertype}{string corresponding to column name in df that categorizes which rows are negative controls}

\item{negctrllabel}{string corresponding to label given to negative controls in the column specified by spacertype}

\item{doublings}{is a string for name of numeric column in df containing total number of cell population doublings between start and end of screen}
}
\value{
a data frame with gammascore added as column
}
\description{
This function calculates gamma from screen raw read counts
}
