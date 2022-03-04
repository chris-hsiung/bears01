#' wrapper for seqinr::write.fasta that takes data frame as input
#'@name writedf2fasta
#'@export
#'@return writes FASTA file
writedf2fasta <- function( df,
                           seqvar,
                           namevar,
                           fileout ){

      assertthat::assert_that(
            sum( c(seqvar, namevar) %in% names(df) ) == 2,
            msg = paste0( seqvar, ' and ', namevar, ' must be column names in data frame' )
      )

      assertthat::assert_that( is.character(fileout),
                               msg = 'file name must be character' )

      seqlistlist <- lapply( df[seqvar], strsplit, '')
      seqlist <- base::unlist( seqlistlist , recursive=FALSE)
      names <- base::unlist( df[namevar] )

      seqinr::write.fasta( sequences = seqlist,
                           names = names,
                           file.out = fileout )

}
