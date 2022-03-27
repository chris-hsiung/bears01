#' Function to assemble oligo for pooled synthesis for Cas12a 6-plex gRNA expression. The oligo will consist of: Adaptor5p-BsmbI5p-pos1spacer-DR1-pos2spacer-DR16-pos3spacer-DR18-pos4spacer-DR10-pos5spacer-DR3-pos6spacer-BsmbI3p-Adaptor3p.
#' DR sequences are taken from Deweirdt et al., Nat. Biotech 2020.
#' Assumes the oligo will be PCR'd using user-provided adaptor sequences, then digested with BsmbI and ligated into backbone.  The BsmbI5p and BsmbI3p sites are hard-coded to be compatible with vector backbone containing 5' DR and 3' DR sequences, which are not encoded in the oligo itself (e.g. compatible with vectors designs of pRG212, pCH39, pCH49, etc.).
#'@param pos1spacer,pos2spacer,pos3spacer,pos4spacer,pos5spacer,pos6spacer string containing only spacer DNA sequences of length 19-23nt. Must not contain BsmbI sites. Must not contain TTTT. Must not start with TTT, because the last base of each DR is a T and that would form an undesirable TTTT at the junction.
#'@param pos1name,pos2name,pos3name,pos4name,pos5name,pos6name string containing name for each spacer.
#'@param Adaptor5p,Adaptor3p string containing PCR adaptor sequences to be used for subpool amplification (recommend taking from https://weissman.wi.mit.edu/crispr/)
#' The final oligo is checked for undesirable BsmbI sites (in excess of the 2 that should be there); if fails, throws error.
#'@author Chris Hsiung
#'@return A list containing oligoname and oligosequence. oligoname is in the format 'pos1name_pos2name_pos3name_pos4name_pos5name_pos6name'.
#'@export

getLibraryOligoCas12a6plexgRNA <- function( pos1name,
                                            pos1spacer,
                                            pos2name,
                                            pos2spacer,
                                            pos3name,
                                            pos3spacer,
                                            pos4name,
                                            pos4spacer,
                                            pos5name,
                                            pos5spacer,
                                            pos6name,
                                            pos6spacer,
                                            Adaptor5p,
                                            Adaptor3p,
                                            outputdir = getwd()
){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer) ) >= 19) == 6 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer ) ) <= 23) == 6,
            msg = 'input spacer contains incorrect length' )

      assertthat::assert_that(
            is.character( c( pos1name, pos2name, pos3name, pos4name, pos5name, pos6name ) ), msg = 'names must be character' )

      if ( !( nchar(Adaptor5p) == 18 ) | !(nchar(Adaptor3p) == 18 ) ){
            warning( 'adaptors are not 18nt long, this is unexpected if using typical PCR adaptors from Weissman lab' )
      }

      DR1 = 'AATTTCTACTGTCGTAGAT'
      DR16 = 'AATTCCTACTATTGTAGGT'
      DR18 = 'AATTCCTACTCTAGTAGGT'
      DR10 = 'AATTCCTACTCTCGTAGGT'
      DR3 = 'AATTTCTACTCTAGTAGAT'
      BsmbI5p = 'cgtctcAAGAT'
      BsmbI3p = 'AATTcgagacg'

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, DR1, pos2spacer, DR16, pos3spacer, DR18, pos4spacer, DR10, pos5spacer, DR3, pos6spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligosequence <- as.character( Biostrings::DNAString( oligo ) )
      oligoname <- paste0( pos1name, '_', pos2name, '_', pos3name, '_', pos4name, '_', pos5name, '_', pos6name)

      # check for BsmbI sites in oligo
      assertthat::assert_that( bears01::countBsmBI(oligosequence) == 2, msg = paste0( oligoname, ' contains undesirable BsmbI sites') )

      # check "transcript" (oligo minus adaptors) for TTTT
      transcript <- stringr::str_c( pos1spacer, DR1, pos2spacer, DR16, pos3spacer, DR18, pos4spacer, DR10, pos5spacer, DR3, pos6spacer )

      assertthat::assert_that( !(grepl('TTTT', transcript)), msg = paste0( oligoname, ' transcribed region contains TTTT' ) )

      # check spacers for starting with TTT, this checks for the case where pos1spacer starts with TTT, which would otherwise be missed
      allspacers <- c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer )
      assertthat::assert_that( !(any( grepl( '^TTT', allspacers) )), msg = paste0( oligoname, ' spacer starts with TTT' ) )

      return( list( 'oligosequence' = oligosequence, 'oligoname' = oligoname ) )
}


