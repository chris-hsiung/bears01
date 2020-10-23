#' sum total occurrence of restriction sites for a dsDNA sequence (input one strand)
#'@export
countBstXI <- function( x ){
      x <- Biostrings::DNAString(x)

      rc_x <- Biostrings::reverseComplement(x)

      REpattern <- 'CCANNNNNNTGG'

      count <- ( Biostrings::countPattern( Biostrings::DNAString( REpattern), x , fixed = FALSE ) + Biostrings::countPattern( Biostrings::DNAString(REpattern), rc_x , fixed = FALSE ) )/2 #divide by 2 after matching pattern on both strands because this is a palindromic pattern, each pattern will always match once on each strand

      return( count )
}

countBlpI <- function( x ){
      x <- Biostrings::DNAString(x)

      rc_x <- Biostrings::reverseComplement(x)

      REpattern <- 'GCTNAGC'

      count <- ( Biostrings::countPattern( Biostrings::DNAString( REpattern), x , fixed = FALSE ) + Biostrings::countPattern( Biostrings::DNAString(REpattern), rc_x , fixed = FALSE ) )/2 #divide by 2 after matching pattern on both strands because this is a palindromic pattern, each pattern will always match once on each strand

      return(count)
}

countBsmBI <- function( x ){
      x <- Biostrings::DNAString(x)

      rc_x <- Biostrings::reverseComplement(x)

      REpattern <- 'CGTCTC'

      count <- Biostrings::countPattern( Biostrings::DNAString( REpattern), x , fixed = FALSE ) + Biostrings::countPattern( Biostrings::DNAString(REpattern), rc_x , fixed = FALSE ) # this is a non-palindromic pattern, no division by 2

      return(count)
}
