#' for designing oligos to be cloned into BstxI/BlpI site (e.g. pLG1 vectors) for Cas9 gRNA
#' returns data frame of oligos
#'@export

getCas9gRNAoligos_BstxIBlpI <- function( name, spacer, spacerlength = 20 ){

      assertthat::assert_that(
            nchar( spacer ) == spacerlength, msg = 'input contains incorrect length')

      senseoligo <- stringr::str_c("ttg", spacer ,"gtttaagagc")

      antisenseoligo <- as.character( bears01::getReverseComplement( stringr::str_c( "cttgttg", spacer,"gtttaagagctaa") ) )

      oligodf <- data.frame(
            name = c( paste0( name, '_sense'),
                      paste0( name, '_antisense') ),
            sequence = c( senseoligo, antisenseoligo )
      )

      return( oligodf )
}
