#' Function to assemble 6-plex oligo for pooled synthesis for Cas12a 6-plex gRNA expression
#' The oligo will be PCR'd using adaptor sequences, then digested with BsmbI and ligated into backbone.
#' by Chris Hsiung 2022-02-28
#'@return
#'@export

assembleCas12a6plexgRNALibraryOligo <- function( pos1name,
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
                                                 DR1 = 'AATTTCTACTGTCGTAGAT',
                                                 DR16 = 'AATTCCTACTATTGTAGGT',
                                                 DR18 = 'AATTCCTACTCTAGTAGGT',
                                                 DR10 = 'AATTCCTACTCTCGTAGGT',
                                                 DR3 = 'AATTTCTACTCTAGTAGAT',
                                                 BsmbI5p = 'cgtctcAAGAT',
                                                 BsmbI3p = 'AATTcgagacg',
                                                 outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer) ) >= 19) == 6 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer ) ) <= 23) == 6,
            msg = 'input spacer contains incorrect length' )

      oligo <- stringr::str_c( Adaptor5p, BsmbI5p, pos1spacer, DR1, pos2spacer, DR16, pos3spacer, DR18, pos4spacer, DR10, pos5spacer, DR3, pos6spacer, BsmbI3p, Adaptor3p )

      # assertion to check all inputs are DNA
      oligo <- as.character( Biostrings::DNAString( oligo ) )

      return( oligo )
}
