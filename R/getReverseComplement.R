#' Wrapper for Biostring::reverseComplement()
#'@export
getReverseComplement <- function( sequence ){
      assertthat::assert_that( is.character(sequence), msg = 'sequence must be character')
      # additional checks done by call to DNAString below
      rc_sequence <- Biostrings::DNAString(sequence) %>% Biostrings::reverseComplement() %>% as.character()
      return(rc_sequence)
}
