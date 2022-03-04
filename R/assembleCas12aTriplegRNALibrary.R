#' Function to generate final oligos for ordering pooled synthesis of Cas12a 6-plex gRNA library for cloning into BsmbI site (e.g. pCH49). Assignment of PCR adaptors to each oligo must be in input file.
#' Performs QC check for absence of BsmbI sites in spacers; if fails, produces error.
#' Performs QC check for sequences being DNA sequences; if fails, produces error.
#' Performs final QC check for absence of any unexpected BsmbI sites in final oligos; if fails, produces warning.
#' Input data frame must contain these columns: 'name1','spacer1','name2','spacer2','name3','spacer3','adaptor5p','adaptor3p'
#' Oligoname is automatically generated as name1_name2_name3
#' by Chris Hsiung 2022-03-03
#'@return A data frame including oligosequence and oligoname
#'@export

assembleCas12aTriplegRNALibrary <- function( spacerdf ){
      assertthat::assert_that(is.data.frame(spacerdf), msg = 'input must be data frame')

      assertthat::assert_that( sum( c('name1','spacer1','name2','spacer2','name3','spacer3','adaptor5p','adaptor3p') %in% names(spacerdf) ) == 8,
                               msg = 'incorrect column names' )

      allspacers <- c( spacerdf$spacer1, spacerdf$spacer2, spacerdf$spacer3 )

      BsmbIabsent <- !( any( sapply(allspacers, bears01::countBsmBI) > 0 ) )

      assertthat::assert_that( BsmbIabsent, msg = 'spacers contain BsmbI site' ) # spacers must not contain BsmbI site

      # assertions checking that spacers and adaptor columns must contain valid DNA sequences are done in getLibraryOligoCas12aTriplegRNA()
      df <- dplyr::rowwise( spacerdf ) %>%
            dplyr::mutate( oligosequence = bears01::getLibraryOligoCas12aTriplegRNA( pos1name = name1,
                                                     pos1spacer = spacer1,
                                                     pos2name = name2,
                                                     pos2spacer = spacer2,
                                                     pos3name = name3,
                                                     pos3spacer = spacer3,
                                                     Adaptor5p = adaptor5p,
                                                     Adaptor3p = adaptor3p)$oligosequence,
                  oligoname = getLibraryOligoCas12aTriplegRNA( pos1name = name1,
                                                                  pos1spacer = spacer1,
                                                                  pos2name = name2,
                                                                  pos2spacer = spacer2,
                                                                  pos3name = name3,
                                                                  pos3spacer = spacer3,
                                                                  Adaptor5p = adaptor5p,
                                                                  Adaptor3p = adaptor3p)$oligoname,
                  oligoBsmbIcount = bears01::countBsmBI(oligosequence) # final check of oligo sequences for BsmbI sites
            )

      if ( any( !( df$oligoBsmbIcount == 2 ) ) ){
        warning( 'incorrect number of BsmbI sites detected in oligos')
      }
      return(df)
}
