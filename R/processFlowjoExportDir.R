#' Function to batch process .csv files (containing the substring specified by 'csvstring') exported from flowjo within the directory specified by 'dir'. Directory must contain 'platesetupfile' (a tab-delimited .txt file) that contains column with name specified by 'IDvar' (defaults to 'Sample') that should contain 96well ID's that are matched to the $FIL keyword in the .fcs file exported from the Attune.
#' Outputs a tab-delimited .txt file with file name specified by 'fileout'.
#' @export
#' 2023-06-09 added fcsheader option to denote whether exported csv file contains FCS headers. if FALSE then will parse Sample based removing csvstring from file name.
processFlowjoExportDir <- function( dir, csvstring = '_P1.csv', fileout = 'P1_combodf.txt', platesetupfile = 'platesetup.txt', IDvar = 'Sample', fcsheader = TRUE ){

      assertthat::assert_that( file.exists( file.path(dir, platesetupfile)), msg = paste0( platesetupfile, ' does not exist') )

      annodf <- fread( file.path( dir, platesetupfile) )

      assertthat::assert_that( IDvar %in% names(annodf), msg = paste0( IDvar, 'is not a column name in ', platesetupfile ) )

      csvfiles <- list.files( dir )[ grepl( csvstring, list.files( dir )  ) ]

      assertthat::assert_that( length(csvfiles) > 0, msg = 'no matching csv files in directory' )

      filelist <- vector( mode = 'list', length(csvfiles) )

      for ( f in 1:length(csvfiles) ){
            if( fcsheader == TRUE ){

                  df <- bears01::flowjo2df( file.path( dir, csvfiles[f]) )
            } else{
                  df <- fread( file.path( dir, csvfiles[f]) ) %>%
                        dplyr::mutate(
                              filename = csvfiles[f],
                              Sample = stringr::str_remove(string = csvfiles[f], pattern = csvstring)
                        )
            }


            filelist[[f]] <- df
      }

      dfout <- bind_rows(filelist) %>% #note this binds rows even if there are different columns
            dplyr::mutate(
                  csvstring = csvstring
            )

      # check that all annotations have a matching csv file
      if ( !(sum( annodf$Sample %in% dfout$Sample ) == length(annodf$Sample) ) ) {
            message( paste0('these sample annotations do not match any $FIL fields: ', toString(annodf$Sample[!(annodf$Sample %in% dfout$Sample)] ) ) )
      }


      dfout <- dplyr::inner_join( annodf, dfout, by = IDvar)


      write.table( dfout, file.path( dir, fileout ), sep = '\t', col.names = TRUE, row.names = FALSE, quote = FALSE )

      return(dfout)
}
