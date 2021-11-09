#' for individually cloning Cas12a triple spacer array using Lb DR into vector with BsmBI site (e.g. pCH011)
#' Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.
#'@export
#'@return data frame of oligos

getOligosCas12aTriplegRNALbDR_BsmbI <- function( pos1name,
                                                        pos1spacer,
                                                        pos2name,
                                                        pos2spacer,
                                                        pos3name,
                                                        pos3spacer,
                                                        LbDRWT = 'AATTTCTACTAAGTGTAGAT',
                                                        outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, LbDRWT, pos2spacer, LbDRWT, pos3spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, LbDRWT, pos2spacer, LbDRWT, pos3spacer) ) )

      senseoligo1 <- substr( insert_sense, 1, 60 )
      senseoligo2 <- substr( insert_sense, 61, nchar(insert_sense) )

      antisenseoligo2 <- substr( insert_antisense, 1, 60)
      antisenseoligo1 <- substr( insert_antisense, 61, nchar(insert_antisense) )

      concatname <- paste0( pos1name, '_', pos2name, '_', pos3name )

      oligodf <- data.frame(
            name = c(
                  paste0( 'Lb-sense1_', concatname ),
                  paste0( 'Lb-sense2_', concatname ),
                  paste0( 'Lb-antisense1_', concatname),
                  paste0( 'Lb-antisense2_', concatname)
            ),

            sequence = c(
                  senseoligo1,
                  senseoligo2,
                  antisenseoligo1,
                  antisenseoligo2)
      )


      fileprefix  <- paste0( pos1name, '_', pos2name, '_', pos3name )

      return( oligodf )
}
