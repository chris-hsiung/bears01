#' for cloning single spacer into vector with BsmBI site
#' compatible with both As DR (e.g. pRG212) and Lb DR crRNA expression vector designs (e.g. pCH011)
#'@export
getOligosCas12aSinglegRNA_BsmbI <- function( name, spacer, outputdir = getwd() ){
      assertthat::assert_that(
            nchar(spacer) >= 19,
            msg = 'incorrect spacer length'
      )

      senseoligo <- stringr::str_c( 'AGAT', spacer)
      antisenseoligo <- stringr::str_c( 'AATT', bears01::getReverseComplement( spacer) )

      oligodf <- data.frame(
            name = c( paste0( name, '_sense'),
                      paste0( name, '_antisense') ),
            sequence = c( senseoligo, antisenseoligo)
      )

      return( oligodf )
}
