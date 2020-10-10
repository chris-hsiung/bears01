# getOligosCas12aDualgRNAFor1stPosition <- function( name, spacer ){
#
#       senseoligo <- stringr::str_c( 'AGAT', spacer, 'AATTTCT' )
#
#       antisenseoligo <- stringr::str_c( Biostrings::reverse('TTAAAGATGAG'), bears01::getReverseComplement(spacer) )
#
#       return( data.frame( name = name, sense = senseoligo, antisense = antisenseoligo ) )
# }
#
#
# getOligosCas12aDualgRNAFor2ndPosition <- function( name, spacer ){
#
#       senseoligo <- stringr::str_c( 'ACTCTTGTAGAT', spacer )
#
#       antisenseoligo <- stringr::str_c( Biostrings::reverse('TTAA'), bears01::getReverseComplement( spacer ), Biostrings::reverse('AACATCTA') )
#
#       return( data.frame( name = name, sense = senseoligo, antisense = antisenseoligo ) )
# }

getOligosCas12aSinglegRNA_BsmbI <- function( name, spacer, outputdir = getwd() ){
      assertthat::assert_that(
            nchar(spacer) >= 19 & nchar(spacer) <= 23,
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




getOligosCas12aTriplegRNA_BsmbI <- function( pos1name,
                                       pos1spacer,
                                       pos2name,
                                       pos2spacer,
                                       pos3name,
                                       pos3spacer,
                                       DR = 'aatttctactcttgtagat',
                                       outputdir = getwd() ){

      assertthat::assert_that(
            sum (nchar( c(pos1spacer, pos2spacer, pos3spacer) ) >= 19) == 3 &
                  sum( nchar( c(pos1spacer, pos2spacer, pos3spacer) ) <= 23) == 3,
            msg = 'input spacer contains incorrect length' )

      insert_sense <- stringr::str_c( 'AGAT', pos1spacer, DR, pos2spacer, DR, pos3spacer )

      insert_antisense <- stringr::str_c( 'AATT', bears01::getReverseComplement( stringr::str_c( pos1spacer, DR, pos2spacer, DR, pos3spacer) ) )

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
