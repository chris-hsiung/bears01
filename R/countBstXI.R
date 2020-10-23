#' sum total occurrence of sites for a dsDNA sequence (input one strand)
#'@export
#'@return numerical count of the number of sites found
countBstXI <- function( x ){
      x <- Biostrings::DNAString(x)

      rc_x <- Biostrings::reverseComplement(x)

      REpattern <- 'CCANNNNNNTGG'

      count <- ( Biostrings::countPattern( Biostrings::DNAString( REpattern), x , fixed = FALSE ) + Biostrings::countPattern( Biostrings::DNAString(REpattern), rc_x , fixed = FALSE ) )/2 #divide by 2 after matching pattern on both strands because this is a palindromic pattern, each pattern will always match once on each strand

      return( count )
}
