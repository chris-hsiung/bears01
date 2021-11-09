#' for individually cloning Cas12a four-plex spacer array into vector with BsmBI site (e.g. pRG212). Modified from protocol by Qingzhou Chen and Junwei Shi. Variant DR sequences from Deweirdt et al. are used to minimize recombination.
#' Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.
#'@export
#'@return data frame of oligos

getOligosCas12a4plexgRNAVariantDR_BsmbI <- function( pos1name,
                                       pos1spacer,
                                       pos2name,
                                       pos2spacer,
                                       pos3name,
                                       pos3spacer,
                                       pos4name,
                                       pos4spacer,
                                       DRWT = 'aatttctactcttgtagat',
                                       DR1 = 'TAATTTCTACTGTCGTAGAT',
                                       DR2 = 'TAATTTCTACTATCGTAGAT',
                                       DR3 = 'AAATTTCTACTCTAGTAGAT',
                                       outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer) ) >= 19) == 4 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer) ) <= 23) == 4,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, DR1, pos2spacer, DR2, pos3spacer, DR3, pos4spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, DR1, pos2spacer, DR2, pos3spacer, DR3, pos4spacer ) ) )

      senseoligo1 <- substr( insert_sense, 1, 52 )
      senseoligo2 <- substr( insert_sense, 53, 105 )
      senseoligo3 <- substr( insert_sense, 106, nchar(insert_sense))

      assertthat::assert_that( nchar(senseoligo3) <= 60, msg = 'oligo length incorrect' )
      ## note convention here -- senseoligo1 anneals with antisenseoligo1, senseoligo2 anneals iwth antisenseoligo2, etc
## come back and check this ##

      antisenseoligo1 <- substr( insert_antisense, 106, nchar(insert_antisense) ) ###
      antisenseoligo2 <- substr( insert_antisense, 53, 105)
      antisenseoligo3 <- substr( insert_antisense, 1, 52 )


      assertthat::assert_that( nchar(antisenseoligo1) <= 60, msg = 'oligo length incorrect' )

      concatname <- paste0( pos1name, '_', pos2name, '_', pos3name, '_', pos4name )

      oligodf <- data.frame(
      name = c(
            paste0( 'sense1_', concatname ),
            paste0( 'sense2_', concatname ),
            paste0( 'sense3_', concatname ),

            paste0( 'antisense1_', concatname),
            paste0( 'antisense2_', concatname),
            paste0( 'antisense3_', concatname)
            ),

      sequence = c(
            senseoligo1,
            senseoligo2,
            senseoligo3,
            antisenseoligo1,
            antisenseoligo2,
            antisenseoligo3 )
      )

      # ultramerdf <- data.frame(
      # name = c(
      #       paste0( concatname, '_sense' ),
      #       paste0( concatname, '_antisense' )
      # ),
      # sequence = c(
      #       insert_sense,
      #       insert_antisense
      # ) )


      return( oligodf )
}
