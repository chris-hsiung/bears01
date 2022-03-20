#' Function reads CRISPick results as data frame and remove spacers that 1. contain BsmbI sites, 2. contain TTTT, 3. start with TTT
#'@param CRISPickfile A data frame containing CRISPick results. Minimally must contain 'sgRNA.Sequence' column, which must consist of DNA bases only
#'@author Chris Hsiung
#'@return A data frame after eliminating rows in the input CRISPickfile where 'sgRNA.Sequence' column contains BsmbI sites or TTTT or starts with TTT
#'@export

removeBadCRISPickSpacers <- function( CRISPickfile ){

      df <- read.delim( CRISPickfile )

      assertthat::assert_that(
            'sgRNA.Sequence' %in% names(df), msg = 'column names must include sgRNA.Sequence' )

       # assertion to check all inputs are DNA
      sgRNAsequence <- lapply( df$`sgRNA.Sequence`, Biostrings::DNAString )


      df <- dplyr::rowwise(df) %>%
            dplyr::mutate( countBsmbI = bears01::countBsmBI(`sgRNA.Sequence`) ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate( TTTTstatus = ifelse( grepl( 'TTTT', `sgRNA.Sequence`), TRUE, FALSE ),
                           TTTstart = ifelse( grepl('^TTT', `sgRNA.Sequence`), TRUE, FALSE ),
                           keep = ifelse( countBsmbI > 0 | TTTTstatus == TRUE | TTTstart == TRUE, FALSE, TRUE ),
                           filename = basename( CRISPickfile)
      )

      dfout <- subset( df, keep == TRUE )

      return( dfout )
}


