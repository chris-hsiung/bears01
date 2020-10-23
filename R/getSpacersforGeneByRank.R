#` for given gene retrieve top rows corresponding to (number of rows specified by numbertoget) after sorting by any numerical score in rankcol
#` libdf can be any data frame (each row is a sgRNA protospacer sequence) that contains columns specified by the other input arguments
#` returns dataframe with additional pickrank column to indicate rank after sorting by rankcol
getSpacersForGeneByRank <- function( gene,
                                     libdf,
                                     numbertoget,
                                     rankcol = 'selection rank',
                                     genecol = 'gene',
                                     spacercol = 'protospacer sequence'
){

      libdf <- as.data.frame(libdf)

      assertthat::assert_that( is.numeric( libdf[ , rankcol] ), msg = 'rankcol must be numeric' )

      assertthat::assert_that( is.numeric( numbertoget ), msg = 'numbertoget must be numeric' )

      assertthat::assert_that( gene %in% libdf[ , genecol], msg = paste0( gene, ' is not found in the library data frame') )

      genedf <- libdf[ libdf[ , genecol] == gene,  ]

      topdf <- dplyr::arrange( genedf, get(rankcol) )

      topdf <- genedf[ 1:numbertoget, ]

      topdf <- cbind( topdf, pickrank = data.frame( pickrank = 1:numbertoget) )

      return(topdf)
}
