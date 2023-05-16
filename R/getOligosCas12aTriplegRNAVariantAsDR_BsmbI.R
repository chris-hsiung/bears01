#' for individually cloning Cas12a triple spacer array into vector with BsmBI site containing a 3' DR in the style of pRG212 (see Gier et al. 2020, uses As DR WT on 5' end and As DR WT on 3' end). For middle 2 spacers, uses variant As DR1 and DR3 sequences from DeWeirdt et al., excluding the first nt in Fig. 4c, i.e. each DR is 19nt, same as in pRG212).  Also compatible with pCH28 (uses As DR WT on 5' end and As DR8 variant on 3' end).
#' Nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.
#' updated 9/9/21, updated 2023-05-15
#'@export
#'@return data frame of oligos

getOligosCas12aTriplegRNAVariantAsDR_BsmbI <- function( pos1name,
                                             pos1spacer,
                                             pos2name,
                                             pos2spacer,
                                             pos3name,
                                             pos3spacer,
                                             AsDR1 = 'AATTTCTACTGTCGTAGAT',
                                             AsDR3 = 'AATTTCTACTCTAGTAGAT',
                                             outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, AsDR1, pos2spacer, AsDR3, pos3spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, AsDR1, pos2spacer, AsDR3, pos3spacer) ) )

      senseoligo1 <- substr( insert_sense, 1, 60 )
      senseoligo2 <- substr( insert_sense, 61, nchar(insert_sense) )

      antisenseoligo2 <- substr( insert_antisense, 1, 60)
      antisenseoligo1 <- substr( insert_antisense, 61, nchar(insert_antisense) )

      concatname <- paste0( pos1name, '_', pos2name, '_', pos3name )

      oligodf <- data.frame(
         name = c(
            paste0( 'As-sense1_', concatname ),
            paste0( 'As-antisense1_', concatname),
            paste0( 'As-sense2_', concatname ),
            paste0( 'As-antisense2_', concatname)
         ),

         sequence = c(
            senseoligo1,
            antisenseoligo1,
            senseoligo2,
            antisenseoligo2)
      )


      fileprefix  <- paste0( pos1name, '_', pos2name, '_', pos3name )

      return( oligodf )
}
