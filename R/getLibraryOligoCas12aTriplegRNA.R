#' Function to assemble 6-plex oligo for pooled synthesis for Cas12a 6-plex gRNA expression. The oligo will consist of:
#' Assumes the oligo will be PCR'd using user-provided adaptor sequences, then digested with BsmbI and ligated into backbone (e.g. compatible with vectors pRG212, pCH39, pCH49, etc.) The backbone is assumed to contain DRWT 5' to the pos1spacer and DR8 3' to the pos3spacer. The final transcript is: DRWT-pos1spacer-DR1-pos2spacer-DR3-pos3spacer-DR8. DR sequences are taken from Deweirdt et al., Nat. Biotech 2020.
#' The input spacers and adaptors should not contain BsmbI sites.
#' The input spacers should not contain >= 4 consecutive T's, and should not start with TTT, because the last base of each DR is a T and this will form an undesirable TTTT at the junction.
#' Checks final oligo for undesirable BsmbI sites (in excess of the 2 that should be there); if fails, throws error.
#' Checks for undesirable >= 4 consecutive T's in transcript; if fails, throws error.
#' Checks final oligo contains valid DNA bases only by calling Biostrings::DNAString(); if fails, throws error.
#' by Chris Hsiung 2022-03-03
#'@return A list containing oligoname and oligosequence. oligoname is in the format 'pos1name_pos2name_pos3name'.
#'@export

getLibraryOligoCas12aTriplegRNA <- function( pos1name,
                                                 pos1spacer,
                                                 pos2name,
                                                 pos2spacer,
                                                 pos3name,
                                                 pos3spacer,
                                                 Adaptor5p,
                                                 Adaptor3p,
                                                 outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer ) ) <= 23) == 3,
            msg = 'input spacer contains incorrect length' )

      # these are in the backbone, not in the oligo, but will be included in transcript check for polyT
      DRWT <- 'aatttctactcttgtAGAT'
      DR8 <- 'AATTTCTCCTCTAGGAGAT'

      # these are in the oligo
      DR1 <- 'AATTTCTACTGTCGTAGAT'
      DR3 <- 'AATTTCTACTCTAGTAGAT'
      BsmbI5p <- 'cgtctcAAGAT'
      BsmbI3p <- 'AATTcgagacg'

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, DR1, pos2spacer, DR3, pos3spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligosequence <- as.character( Biostrings::DNAString( oligo ) )
      oligoname <- paste0( pos1name, '_', pos2name, '_', pos3name)

      # check for BsmbI sites in oligo
      assertthat::assert_that( bears01::countBsmBI(oligosequence) == 2, msg = paste0( oligoname, ' contains undesirable BsmbI sites') )

      # check for polyT in transcript
      transcript <- stringr::str_c( DRWT, pos1spacer, DR1, pos2spacer, DR3, pos3spacer, DR8 )

      assertthat::assert_that( !(grepl('TTTT', transcript)), msg = paste0( oligoname, ' transcript contains >= 4 consecutive Ts' ) )

      return( list( 'oligosequence' = oligosequence, 'oligoname' = oligoname ) )
}


