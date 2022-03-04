#' Function to assemble 6-plex oligo for pooled synthesis for Cas12a 6-plex gRNA expression
#' The oligo will be PCR'd using adaptor sequences, then digested with BsmbI and ligated into backbone (e.g. compatible with pCH49)
#' by Chris Hsiung 2022-03-03
#'@return A list containing oligoname and oligosequence
#'@export

getLibraryOligoCas12aTriplegRNA <- function( pos1name,
                                                 pos1spacer,
                                                 pos2name,
                                                 pos2spacer,
                                                 pos3name,
                                                 pos3spacer,
                                                 Adaptor5p,
                                                 Adaptor3p,
                                                 DR1 = 'AATTTCTACTGTCGTAGAT',
                                                 DR3 = 'AATTTCTACTCTAGTAGAT',
                                                 BsmbI5p = 'cgtctcAAGAT',
                                                 BsmbI3p = 'AATTcgagacg',
                                                 outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer ) ) <= 23) == 3,
            msg = 'input spacer contains incorrect length' )

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, DR1, pos2spacer, DR3, pos3spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligosequence <- as.character( Biostrings::DNAString( oligo ) )
      oligoname <- paste0( pos1name, '_', pos2name, '_', pos3name)
      return( list( 'oligosequence'=oligosequence, 'oligoname'=oligoname ) )
}


