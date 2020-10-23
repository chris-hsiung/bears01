#' not thoroughly vetted yet
getOligosCas12aDualgRNA_BsmbI <- function( pos1name, pos1spacer, pos2name, pos2spacer, outputdir = getwd() ){


      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer ) ) >= 19) == 2 &
                  sum( nchar( c(pos1spacer, pos2spacer ) ) <= 23) == 2,
            msg = 'input spacer contains incorrect length' )

      pos1senseoligo <- stringr::str_c( 'AGAT', pos1spacer, 'AATTTCT' )

      pos1antisenseoligo <- stringr::str_c( Biostrings::reverse('TTAAAGATGAG'), bears01::getReverseComplement( pos1spacer) )

      pos2senseoligo <- stringr::str_c( 'ACTCTTGTAGAT', pos2spacer )

      pos2antisenseoligo <- stringr::str_c( Biostrings::reverse('TTAA'), bears01::getReverseComplement( pos2spacer ), Biostrings::reverse('AACATCTA') )

      oligodf <- data.frame( name = c(
            paste0(pos1name, '_sense'),
            paste0(pos1name, '_antisense' ),
            paste0(pos2name, '_sense'),
            paste0(pos2name, '_antisense') ),

            sequence = c(
                  pos1senseoligo,
                  pos1antisenseoligo,
                  pos2senseoligo,
                  pos2antisenseoligo )
      )

      # insertdf <- data.frame( name = c(
      #       paste0( pos1name, '_', pos2name, '_sense' ),
      #       paste0( pos1name, '_', pos2name, '_antisense' )
      # ),
      #       sequence = c(
      #             stringr::str_c( pos1senseoligo, pos2senseoligo ),
      #             stringr::str_c( pos2antisenseoligo, pos1antisenseoligo))
      # )

      fileprefix  <- paste0( pos1name, '_', pos2name)

      return( oligodf )
}

