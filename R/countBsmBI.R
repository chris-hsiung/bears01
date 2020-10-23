#' sum total occurrence of sites for a dsDNA sequence (input one strand)
#'@export
#'@return numerical count of the number of sites found

countBsmBI <- function( x ){
      x <- Biostrings::DNAString(x)

      rc_x <- Biostrings::reverseComplement(x)

      REpattern <- 'CGTCTC'

      count <- Biostrings::countPattern( Biostrings::DNAString( REpattern), x , fixed = FALSE ) + Biostrings::countPattern( Biostrings::DNAString(REpattern), rc_x , fixed = FALSE ) # this is a non-palindromic pattern, no division by 2

      return(count)
}
