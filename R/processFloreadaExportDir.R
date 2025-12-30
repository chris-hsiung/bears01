#' Process a Floreada Flow Cytometry export directory and merge with plate annotations
#'
#' This function reads a directory of Floreada-exported CSV files derived from FCS
#' files (e.g. \code{A1.fcs.csv}), extracts well identifiers from filenames, combines
#' all event-level data into a single data frame, and joins it with a plate setup
#' annotation file. The merged data frame is written to disk and returned.
#'
#' @param dir Character scalar. Path to the directory containing Floreada CSV files
#'   and the plate setup file.
#' @param csvstring Character scalar. Regular expression used to identify CSV files
#'   within \code{dir}. Defaults to \code{"\\.fcs\\.csv$"}.
#' @param wellIDstring Character scalar. Regular expression used to extract the well
#'   identifier from each CSV filename. Defaults to
#'   \code{"([A-Z][0-9]|[A-Z][0-9][0-9])\\.fcs\\.csv$"}.
#' @param fileout Character scalar. Name of the combined output file written to
#'   \code{dir}. Defaults to \code{"combodf.txt"}.
#' @param platesetupfile Character scalar. Name of the plate annotation file located
#'   in \code{dir}. Must be readable by \code{data.table::fread}. Defaults to
#'   \code{"platesetup.txt"}.
#' @param IDvar Character scalar. Name of the column representing well identifiers.
#'   This column must exist uniquely and without duplicates in the plate setup file.
#'   Defaults to \code{"wellID"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates the existence of the plate setup file.
#'   \item Identifies CSV files matching \code{csvstring}.
#'   \item Reads the plate setup file and validates \code{IDvar}.
#'   \item Extracts well identifiers from CSV filenames using \code{wellIDstring}.
#'   \item Reads each CSV file and appends filename and well ID metadata.
#'   \item Row-binds all CSV files into a single data frame.
#'   \item Checks for plate annotation wells that do not appear in the combined data.
#'   \item Inner-joins event-level data with plate annotations.
#'   \item Writes the merged data to disk as a tab-delimited text file.
#' }
#'
#' If any wells in the plate setup file do not match data in the combined CSVs,
#' a message is emitted listing the unmatched identifiers.
#'
#' @return A data.frame containing combined Floreada event-level data joined with
#'   plate annotations.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table fread
#' @importFrom dplyr mutate bind_rows inner_join
#' @importFrom stringr str_extract str_replace
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' combodf <- processFloreadaExportDir(
#'   dir = "floreadaexport/events-P1",
#'   platesetupfile = "platesetup.txt",
#'   fileout = "combined_events.txt"
#' )
#' }
#'
#' @export


processFloreadaExportDir <- function( dir,
                                      csvstring = '\\.fcs\\.csv$',
                                      wellIDstring = '([A-Z][0-9]|[A-Z][0-9][0-9])\\.fcs\\.csv$',
                                      fileout = 'combodf.txt',
                                      platesetupfile = 'platesetup.txt',
                                      IDvar = 'wellID'){

      assertthat::assert_that( file.exists( file.path( dir, platesetupfile)), msg = paste0( platesetupfile, ' does not exist') )

      filenames <- list.files(dir)
      csvfilenames <- filenames[ grepl( pattern = csvstring, x = filenames ) ]

      assertthat::assert_that( length(csvfilenames) > 0, msg = 'no matching csv files in directory' )

      annodf <- data.table::fread( file.path(dir, platesetupfile))

      assertthat::assert_that( sum( names(annodf) == IDvar ) == 1, msg = paste0( IDvar, ' must be a unique column name in ', platesetupfile ) )

      assertthat::assert_that( anyDuplicated( annodf[[IDvar]] ) == 0, msg = paste0( platesetupfile, ' ', IDvar, ' contains duplicates.' ) )

      filelist <- vector( mode = 'list', length(csvfilenames) )

      dfout <- data.frame()

      for ( f in 1:length(csvfilenames) ){

            wellIDtemp <- stringr::str_extract( csvfilenames[f], wellIDstring )
            wellID <- stringr::str_replace( wellIDtemp, csvstring, '')

            df <- data.table::fread( file = file.path( dir, csvfilenames[f] )  ) %>%
                  dplyr::mutate(
                        filename = csvfilenames[f],
                        !!sym(IDvar) := wellID )

            if ( dim(df)[1] == 0 ){
                  message( paste0( csvfilenames[f], ' contains no data.'))
            }

            filelist[[f]] <- df

            print( paste0( 'working on ', csvfilenames[f]) )
      }

      dfout <- dplyr::bind_rows( filelist )
      # check that all annotations have a matching csv file
      if ( !(sum( annodf[[IDvar]] %in% dfout[[IDvar]] ) == length(annodf[[IDvar]]) ) ) {
            message( paste0('these ', IDvar, ' in ', platesetupfile, ' do not match any rows in the combined data: ', toString( annodf[[IDvar]][!(annodf[[IDvar]] %in% dfout[[IDvar]] )] ), '; recommend checking if this is expected' ) )
      }

      dfout <- dplyr::inner_join( annodf, dfout, by = IDvar)

      write.table( dfout, file = file.path(dir, fileout), quote = FALSE, sep = '\t', row.names = FALSE )

      return(dfout)
}

