#' wrapper for seqinr::read.fasta
#'@name fasta2df
#'@return Returns data frame with columns 'ID' and 'sequence'
#'@export
fasta2df <- function( fastafile ){

      library(seqinr)

      char <- unlist( seqinr::read.fasta( fastafile, as.string = TRUE ) )
      ID <- names(char)

      dfout <- data.frame( ID = ID, sequence = char, stringsAsFactors = FALSE )

      return(dfout)
}
