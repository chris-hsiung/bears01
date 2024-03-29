% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOligosCas12aTriplegRNALbDR_BsmbI.R
\name{getOligosCas12aTriplegRNALbDR_BsmbI}
\alias{getOligosCas12aTriplegRNALbDR_BsmbI}
\title{for individually cloning Cas12a triple spacer array using Lb DR into vector with BsmBI site (e.g. pCH011)
Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.}
\usage{
getOligosCas12aTriplegRNALbDR_BsmbI(
  pos1name,
  pos1spacer,
  pos2name,
  pos2spacer,
  pos3name,
  pos3spacer,
  LbDRWT = "AATTTCTACTAAGTGTAGAT",
  outputdir = getwd()
)
}
\value{
data frame of oligos
}
\description{
for individually cloning Cas12a triple spacer array using Lb DR into vector with BsmBI site (e.g. pCH011)
Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.
}
