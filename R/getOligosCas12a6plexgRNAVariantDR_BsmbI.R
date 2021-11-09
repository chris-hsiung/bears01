#' for individually cloning Cas12a 6-plex spacer array into vector with BsmBI site (e.g. pRG212). Modified from protocol by Qingzhou Chen and Junwei Shi. Variant DR sequences from Deweirdt et al. are used to minimize recombination.
#' Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.
#' Returns a list with ultramers OR 60-mer style oligos
#'@export
#'@return data frame of oligos

getOligosCas12a6plexgRNAVariantDR_BsmbI <- function( pos1name,
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
                                                     DRWT = 'aatttctactcttgtagat',
                                                     DR1 = 'TAATTTCTACTGTCGTAGAT',
                                                     DR2 = 'TAATTTCTACTATCGTAGAT',
                                                     DR3 = 'AAATTTCTACTCTAGTAGAT',
                                                     DR5 = 'TaatttctactAtAgtagat',
                                                     DR6 = 'AaatttctCctctCgGagat',

                                                     outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer) ) >= 19) == 6 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer ) ) <= 23) == 6,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, DR1, pos2spacer, DR2, pos3spacer, DR3, pos4spacer, DR5, pos5spacer, DR6, pos6spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, DR1, pos2spacer, DR2, pos3spacer, DR3, pos4spacer, DR5, pos5spacer, DR6, pos6spacer ) ) )

      # senseoligo1 <- substr( insert_sense, 1, 60 )
      # senseoligo2 <- substr( insert_sense, 61, 102 )
      # senseoligo3 <- substr( insert_sense, 103, 145 )
      # senseoligo4 <- substr( insert_sense, 146, 192 )
      # senseoligo5 <- substr( insert_sense, 193, nchar(insert_sense) )
      #
      # assertthat::assert_that( nchar(senseoligo3) <= 60, msg = 'oligo length incorrect' )
      # ## note convention here -- senseoligo1 anneals with antisenseoligo1, senseoligo2 anneals iwth antisenseoligo2, etc
      #
      # antisenseoligo1 <- substr( insert_antisense, 193, nchar(insert_antisense) ) ###
      # antisenseoligo2 <- substr( insert_antisense, 146, 192)
      # antisenseoligo3 <- substr( insert_antisense, 103, 145 )
      # antisenseoligo4 <- substr( insert_antisense, 61, 102 )
      # antisenseoligo5 <-substr( insert_antisense, 1, 60 )
      #
      # assertthat::assert_that( nchar(antisenseoligo1) <= 60, msg = 'oligo length incorrect' )

      # Design oligos, the will be combination of ultramers and standard <= 60bp oligos
      sense1 <- substr( insert_sense, 1, 200 )
      sense2 <-substr( insert_sense, 201, nchar(insert_sense))

      antisense1 <- substr( insert_antisense, 201, nchar(insert_antisense) )
      antisense2 <- substr( insert_antisense, 1, 200 )


      concatname <- paste0( pos1name, '_', pos2name, '_', pos3name, '_', pos4name, '_', pos5name, '_', pos6name )

      oligodf <- data.frame(
            name = c(
                  paste0( 'sense1_', concatname ),
                  paste0( 'sense2_', concatname ),
                  paste0( 'antisense1_', concatname),
                  paste0( 'antisense2_', concatname)
            ),

            sequence = c(
                  sense1,
                  sense2,
                  antisense1,
                  antisense2)
      )

      return( oligodf )
}
