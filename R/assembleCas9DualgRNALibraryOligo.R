#' Function to facilitate assembly of oligo pool synthesis for Cas9 dual gRNA expression as in Replogle et al 2020
#' BsmBIstuffer defaults to sequence for containing BsmBI sitesß
#' REseq5p defaults to contain BstxI site
#' REseq3p defaults to contain BlpI site
#'@export
#'@return character containing assembled oligo sequence
assembleCas9DualgRNALibraryOligo <- function( spacer1,
                                            spacer2,
                                            BsmBIstuffer = 'gtttcagagcgagacgtgcctgcaggatacgtctcagaaacatg',
                                            REseq5p = 'ccaccttgttg',
                                            REseq3p = 'gtttaagagctaagctg',
                                            Adapter5p,
                                            Adapter3p){
      # use the DNAString function to check the input arguments are DNA sequences
      spacer1 <- as.character( Biostrings::DNAString(spacer1) )
      spacer2 <- as.character( Biostrings::DNAString(spacer2) )
      BsmBIstuffer <- as.character( Biostrings::DNAString(BsmBIstuffer))
      REseq5p <- as.character( Biostrings::DNAString(REseq5p))
      REseq3p <- as.character( Biostrings::DNAString(REseq3p))
      Adapter5p <- as.character(Biostrings::DNAString(Adapter5p))
      Adapter3p <- as.character(Biostrings::DNAString(Adapter3p))

      oligo <- str_c( Adapter5p, REseq5p, spacer1, BsmBIstuffer, spacer2, REseq3p, Adapter3p, sep = '' )

      return(oligo)
}
