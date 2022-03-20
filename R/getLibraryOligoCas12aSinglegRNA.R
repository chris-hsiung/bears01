#' Function to assemble oligo for pooled synthesis for Cas12a 3-plex gRNA expression. The oligo will consist of: Adaptor5p-BsmbI5p-pos1spacer-DR1-pos2spacer-DR3-pos3spacer-BsmbI3p-Adaptor3p. DR sequences are taken from Deweirdt et al., Nat. Biotech 2020.
#' Assumes the oligo will be PCR'd using user-provided adaptor sequences, then digested with BsmbI and ligated into backbone.  The BsmbI5p and BsmbI3p sites are hard-coded to be compatible with vector backbone containing 5' DR and 3' DR sequences, which are not encoded in the oligo itself (e.g. compatible with vectors designs of pRG212, pCH39, pCH49, etc.).
#'@param pos1spacer string containing only spacer DNA sequences of length 19-23nt. Must not contain BsmbI site.
#'@param pos1name string containing name for each spacer.
#'@param Adaptor5p,Adaptor3p string containing PCR adaptor sequences to be used for subpool amplification (recommend taking from https://weissman.wi.mit.edu/crispr/). Must not contain BsmbI site.
#'@param polyTcheck if set to TRUE, will check spacer for presence of 'TTTT' or starting with 'TTT' and throw error in either case.
#' The final oligo is checked for undesirable BsmbI sites (in excess of the 2 that should be there); if fails, throws error.
#'@author Chris Hsiung
#'@return A list containing oligoname and oligosequence.
#'@export

getLibraryOligoCas12aSinglegRNA <- function( pos1name,
                                             pos1spacer,
                                             Adaptor5p,
                                             Adaptor3p,
                                             polyTcheck
){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer) ) >= 19) == 1 &
                  sum( nchar( c(pos1spacer) ) <= 23) == 1,
            msg = 'input spacer contains incorrect length' )

      assertthat::assert_that(
            is.character( c( pos1name) ), msg = 'names must be character' )

      if ( !( nchar(Adaptor5p) == 18 ) | !(nchar(Adaptor3p) == 18 ) ){
            warning( 'adaptors are not 18nt long, this is unexpected if using typical PCR adaptors from Weissman lab' )
      }

      assertthat::assert_that(
            polyTcheck %in% c( 'TRUE', 'FALSE'), msg = 'polyTcheck must be either TRUE or FALSE' )

      BsmbI5p <- 'cgtctcAAGAT'
      BsmbI3p <- 'AATTcgagacg'

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligosequence <- as.character( Biostrings::DNAString( oligo ) )
      oligoname <- paste0( pos1name )

      # check for BsmbI sites in oligo
      assertthat::assert_that( bears01::countBsmBI(oligosequence) == 2, msg = paste0( oligoname, ' contains undesirable BsmbI sites') )

      if ( polyTcheck == TRUE ){
            # check "transcript" (oligo minus adaptors) for TTTT
            transcript <- stringr::str_c( pos1spacer )

            assertthat::assert_that( !(grepl('TTTT', transcript)), msg = paste0( oligoname, ' transcribed region contains TTTT' ) )

            # check spacer for starting with TTT
            allspacers <- c( pos1spacer )
            assertthat::assert_that( !(any( grepl( '^TTT', allspacers) )), msg = paste0( oligoname, ' spacer starts with TTT' ) )
      }


      return( list( 'oligosequence' = oligosequence, 'oligoname' = oligoname ) )
}


