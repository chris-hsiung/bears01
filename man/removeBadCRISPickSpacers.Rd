% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeBadCRISPickSpacers.R
\name{removeBadCRISPickSpacers}
\alias{removeBadCRISPickSpacers}
\title{Function reads CRISPick results as data frame and remove spacers that 1. contain BsmbI sites, 2. contain TTTT, 3. start with TTT}
\usage{
removeBadCRISPickSpacers(CRISPickfile)
}
\arguments{
\item{CRISPickfile}{A data frame containing CRISPick results. Minimally must contain 'sgRNA.Sequence' column, which must consist of DNA bases only}
}
\value{
A data frame after eliminating rows in the input CRISPickfile where 'sgRNA.Sequence' column contains BsmbI sites or TTTT or starts with TTT
}
\description{
Function reads CRISPick results as data frame and remove spacers that 1. contain BsmbI sites, 2. contain TTTT, 3. start with TTT
}
\author{
Chris Hsiung
}
