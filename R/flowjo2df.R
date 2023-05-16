#' Function to read csv file exported from flowjo. The original .fcs files from which the .csv file was generated must contain keywords. Parses sampleID from $FIL keyword and platename from $PLATENAME keyword and date from $DATE keyword.
#' Returns data frame
#' Intended for use within the function processFlowjoExportDir() for processing an entire directory of .csv files.
#' Updated 2022-08-02 to remove additional header lines inserted by Arc Attune that do not start with '$'
#' Updated 2023-04-04 added additional regex matches for identifying datacsv in case some variables are not exported from flowjo
#' @export
flowjo2df <- function( file ){

      #print(file)
      lines <- readLines(file)
      lines2 <- lines[2:length(lines)] ## remove the first line which does not follow patterns

      metainfo <- lines2[ grepl("^\\$|^\\#", lines2) ]

      headercsv <- lines2[ grepl('FSC-A', lines2) & !grepl("^\\$|^\\#|FJ_FCS_VERSION|^HOSTID,XE3-IoT-", lines2) ]

      datacsv <- lines2[ !grepl("^\\$|^\\#|FJ_FCS_VERSION|^HOSTID,XE3-IoT-", lines2) & !grepl('FSC-A|SSC-A|BL1-A|VL1-A|RL1-A|YL1-A', lines2) ]

      if ( length(datacsv) == 0 ){
            message( paste0( file, ' contains no data, skipping.'))
            return( NULL )
      } else{
            header <- str_remove_all( unlist( strsplit( headercsv, "," ) ), '\"')

            df <- as.data.frame( matrix( unlist( strsplit( datacsv, ",")), nrow = length(datacsv), byrow=T))

            names(df) <- header

            platename <- str_remove( metainfo[ grepl( '^\\$PLATENAME', metainfo ) ], '\\$PLATENAME,' )

            # assertthat::assert_that( is.character(platename),
            #                          msg = 'platename must be character')

            filename <- str_remove( metainfo[ grepl( '^\\$FIL', metainfo ) ], '\\$FIL,' )

            filename <- str_remove( filename, '.fcs$' )

            date <- str_remove( metainfo[ grepl('^\\$DATE', metainfo)], '\\$DATE,' )

            # assertthat::assert_that( is.character(filename),
            #                          msg = 'filename must be character')

            df <- dplyr::mutate( df,
                                 Sample = filename,
                                 platename = platename,
                                 date = date)

            return(df)

      }


}
