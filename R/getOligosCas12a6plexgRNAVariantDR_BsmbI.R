#' for individually cloning Cas12a 6-plex spacer array into vector with BsmBI site (e.g. pRG212). Modified from protocol by Qingzhou Chen and Junwei Shi. Variant DR sequences from Deweirdt et al. are used to minimize recombination. Also as much as possible avoid TTTCT motif (weak Pol III terminator)
#' Returns a list with two data frames, one containing oligos/ultramers, the other containing gene block. User can choose to use either one for synthesis, but gene block is more cost effective.
#' For gene block approach, by default insert is appended with homology regions (Gibson5p and Gibson3p as input arguments) compatible with Gibson assembly into pCH49.
#' For oligos/ultramer approach: nomenclature for naming sense and antisense strands is as follows: sense1 anneals with antisense1, sense2 anneals with antisense2, sense 3 anneals with antisense3, etc. These prefixes are printed at the front of the string to facilitate setting up annealing reactions because the IDT tubes truncates the end of the long string.

#' by Chris Hsiung, updated 2021-12-23
#'@export
#'@return list of data frames containing 1. oligos (this is too expensive but is kept as an available alternative for now) and 2. gene block

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
                                                     DR1 = 'AATTTCTACTGTCGTAGAT',
                                                     DR16 = 'AATTCCTACTATTGTAGGT',
                                                     DR18 = 'AATTCCTACTCTAGTAGGT',
                                                     DR10 = 'AATTCCTACTCTCGTAGGT',
                                                     DR3 = 'AATTTCTACTCTAGTAGAT',
                                                     Gibson5p = 'atcttgtggaaaggacgaaacaccgaatttctactcttgt',
                                                     Gibson3p = 'AATTTCTCCTCTAGGAGATtttttttaagcttggcgtaactagatcttga',

                                                     outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer) ) >= 19) == 6 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer, pos4spacer, pos5spacer, pos6spacer ) ) <= 23) == 6,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, DR1, pos2spacer, DR16, pos3spacer, DR18, pos4spacer, DR10, pos5spacer, DR3, pos6spacer )

      # assertion to check all inputs are DNA
      insert_sense <- as.character( Biostrings::DNAString( insert_sense ) )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, DR1, pos2spacer, DR16, pos3spacer, DR18, pos4spacer, DR10, pos5spacer, DR3, pos6spacer ) ) )

      # Design oligos, they will be combination of ultramers and standard <= 60bp oligos --> check that the ends make sense
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

      geneblock <- stringr::str_c( Gibson5p, insert_sense, Gibson3p )

      geneblockdf <- data.frame(
            name = paste0( 'gb_', concatname),
            sequence = geneblock
            )

      return( list( oligodf = oligodf, geneblock = geneblockdf ) )
}
