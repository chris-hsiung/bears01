#' function for getting the hamming distance between 2 strings
#' @export
gethammingdist <- function( str1, str2 ){
      assertthat::assert_that( nchar(str1) == nchar(str2), msg = 'nchar for str and str2 must be equal' )

      str1vec <- unlist( str_split( str1, pattern = '' ) )
      str2vec <- unlist( str_split( str2, pattern = '' ) )

      hammingdist <- sum( str1vec != str2vec )
      return(hammingdist)
}
