#' Function to assemble oligo for pooled synthesis for Cas12a dual gRNA expression.
#' 2024-12-28 updated to allow specifying the middle DR as an input argument, which defaults to 19nt WT AsCas12a DR. The oligo will consist of: Adaptor5p-BsmbI5p-pos1spacer-DR-pos2spacer-BsmbI3p-Adaptor3p. Also include input argument to set whether TTTT is allowed.
#' Assumes the oligo will be PCR'd using user-provided adaptor sequences, then digested with BsmbI and ligated into backbone.  The BsmbI5p and BsmbI3p sites are hard-coded to be compatible with vector backbone containing 5' DR and 3' DR sequences, which are not encoded in the oligo itself (e.g. compatible with vectors designs of pRG212, pCH39, pCH49, etc.).
#'@param pos1spacer,pos2spacer string containing only spacer DNA sequences of length 19-23nt. Must not contain BsmbI sites. Must not contain TTTT. Must not start with TTT, because the last base of each DR is a T and that would form an undesirable TTTT at the junction.
#'@param pos1name,pos2name String containing name for each spacer. Defaults to "None".
#'@param sep String that will be used to separate names. Defaults to '_'.
#'@param Adaptor5p,Adaptor3p string containing PCR adaptor sequences to be used for subpool amplification (recommend taking from https://weissman.wi.mit.edu/crispr/)
#' The final oligo is checked for undesirable BsmbI sites (in excess of the 2 that should be there); if fails, throws error.
#'@author Chris Hsiung
#'@return A list containing oligoname and oligosequence.
#'@export
#'@examples getLibraryOligoCas12aDoublegRNA( pos1name = 'CD81-1', pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC', pos2name = 'B2M-1', pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG', Adaptor5p = 'ATTTTGCCCCTGGTTCTT', Adaptor3p = 'CCAGTTCATTTCTTAGGG')

getLibraryOligoCas12aDoublegRNA <- function( pos1name = 'None',
                                             pos1spacer,
                                             pos2name = 'None',
                                             pos2spacer,
                                             DR = 'AATTTCTACTGTCGTAGAT',
                                             Adaptor5p,
                                             Adaptor3p,
                                             sep = '_',
                                             polyT = 'allowed'
){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer) ) >= 19) == 2 &
                  sum( nchar( c(pos1spacer, pos2spacer) ) <= 23) == 2,
            msg = 'input spacer contains incorrect length' )

      assertthat::assert_that(
            is.character( c( pos1name, pos2name) ), msg = 'names must be character' )

      if ( !( nchar(Adaptor5p) == 18 ) | !(nchar(Adaptor3p) == 18 ) ){
            warning( 'adaptors are not 18nt long, this is unexpected if using typical PCR adaptors from Weissman lab' )
      }

      if ( !( nchar(DR) >= 19 & nchar(DR) <= 20 ) ){
            warning( 'DR is not 19-20nt, this is unexpected' )

            assertthat::assert_that( polyT %in% c('allowed', 'excluded'), msg = 'invalid polyT input argument')
      }

BsmbI5p <- 'cgtctcAAGAT'
      BsmbI3p <- 'AATTcgagacg'

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, DR, pos2spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligosequence <- as.character( Biostrings::DNAString( oligo ) )
      oligoname <- paste0( pos1name, sep, pos2name)

      # check for BsmbI sites in oligo
      assertthat::assert_that( bears01::countBsmBI(oligosequence) == 2, msg = paste0( oligoname, ' contains undesirable BsmbI sites') )

      ###### check for polyT
      transcript <- stringr::str_c( pos1spacer, DR, pos2spacer)
      allspacers <- c(pos1spacer, pos2spacer )
      if( polyT == 'excluded' ){
            # check "transcript" (oligo minus adaptors) for TTTT
            assertthat::assert_that( !(grepl('TTTT', transcript)), msg = paste0( oligoname, ' transcribed region contains TTTT' ) )

            # check spacers for starting with TTT, this checks for the case where pos1spacer starts with TTT, which would otherwise be missed
            assertthat::assert_that( !(any( grepl( '^TTT', allspacers) )), msg = paste0( oligoname, ' spacer starts with TTT' ) )

      } else if( polyT == 'allowed'){
            # just produce warning

            if( any( grepl('TTTT', transcript) ) | any( grepl( '^TTT', allspacers) ) ) {
                  warning( paste0('guide design includes TTTT') )
            }
      }

      return( list( 'oligosequence' = oligosequence, 'oligoname' = oligoname ) )
}


