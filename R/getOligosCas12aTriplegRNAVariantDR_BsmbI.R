#' for individually cloning Cas12a triple spacer array into vector withi BsmBI site (e.g. pRG212). incorporating Variant DR sequences from DeWeirdt et al.
#'@export
#'@return data frame of oligos

getOligosCas12aTriplegRNAVariantDR_BsmbI <- function( pos1name,
                                             pos1spacer,
                                             pos2name,
                                             pos2spacer,
                                             pos3name,
                                             pos3spacer,
                                             DR1 = 'TAATTTCTACTGTCGTAGAT',
                                             DR3 = 'AAATTTCTACTCTAGTAGAT',
                                             DRWT = 'AATTTCTACTCTTGTAGAT',
                                             outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer) ) <= 23) == 3,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, DR1, pos2spacer, DR3, pos3spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, DR1, pos2spacer, DR3, pos3spacer) ) )

      senseoligo1 <- substr( insert_sense, 1, 60 )
      senseoligo2 <- substr( insert_sense, 61, nchar(insert_sense) )

      antisenseoligo2 <- substr( insert_antisense, 1, 60)
      antisenseoligo1 <- substr( insert_antisense, 61, nchar(insert_antisense) )

      concatname <- paste0( pos1name, '_', pos2name, '_', pos3name )

      oligodf <- data.frame(
            name = c(
                  paste0( concatname, '_sense1' ),
                  paste0( concatname, '_sense2' ),
                  paste0( concatname, '_antisense1'),
                  paste0( concatname, '_antisense2')),

            sequence = c(
                  senseoligo1,
                  senseoligo2,
                  antisenseoligo1,
                  antisenseoligo2)
      )

      fileprefix  <- paste0( pos1name, '_', pos2name, '_', pos3name )

      return( oligodf )
}
