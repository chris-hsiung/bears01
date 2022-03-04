## Take DNA as input (assumed to come from single strand of DNA) and obtains its reverse complement. Useful for finding all PAM sites in dsDNA.
#' by Chris Hsiung, updated 2022-02-15
#'@name getBothStrandsFASTA
#'@export
#'@return Return data frame containing both strands. Also writes a FASTA file by default.
getBothStrandsFASTA <- function( fastafile, writefasta = TRUE, outputfile = paste0( fastafile, '_bothstrands')) {

strand1df <- fasta2df( fastafile )

ID <- strand1df$ID

strand2df <- data.frame( ID = paste0( ID,"::strand2" ),
                          sequence = sapply( strand1df$sequence, getReverseComplement )
                        )

strand1df <- dplyr::mutate( strand1df, ID = paste0( ID, "::strand1") )

duplexdf <- rbind( strand1df, strand2df )

if ( writefasta == TRUE ){
      writedf2fasta( df = duplexdf,
                     seqvar = 'sequence',
                     namevar = 'ID',
                     fileout = outputfile )
}

return( duplexdf )

}


