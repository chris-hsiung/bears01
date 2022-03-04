#' Function to extract target sites for each sequence
#' sequence input must be character of nucleotide sequence
#' pam and altpams must be character consisting of IUPAC codes. getTargetSites loops through all pam and altpams to search for match in the sequence. if pam or altpams contain redundantly defined patterns (e.g. TTTN and TTTV), the sequence will be matched twice (once for each pattern).
#' target site defined as 34mer consisting of 4bp + PAM (4bp) + 23bp protospacer + 3bp, as described by Supp Fig. 1 of Kim et al., Nat Biotech 2018. The entire 34mer must fit within the input sequence.
#' returns data frame with columns --
#' mer34 contains the target site 34mer original sequence
#' mer27 contains sequence to be used as input for bowtie that can subsequently be intersected with ENCODE DNase-seq narrow peaks, as describe in Kim et al., Nat Biotech 2018.
#' convertmer34 contains 34mer where any alternative PAM target sites were converted to 'TTTC' (hard-coded). This is because DeepCpf1 was only trained on 'TTTV' PAMs. This strategy of modifying the input for DeepCpf1 is described by Sanson et al., bioRxiv 2019.
#' @name getTargetSites
#'@return data frame with row containing NA if no target sites found
#'@export
getTargetSites <- function( sequence, pam, altpams ){

      # use DNAString to check inputs are in IUPAC code (will throw error if not)
      allpams <- as.character( Biostrings::DNAStringSet( c(pam, altpams) ) )
      bioseq <- Biostrings::DNAString( sequence )

      pamcoord <- sapply( allpams, Biostrings::matchPattern,
                          subject = bioseq,
                          fixed = FALSE ) ## Match with IUPAC ambiguities

      pamstart <- sapply( pamcoord, Biostrings::start )

      pamend <- sapply( pamcoord, Biostrings::end ) %>% unlist()

      assertthat::assert_that( sum( str_replace_all( names( pamend ), '[0-9]', '' ) %in% allpams )   == length( names( pamend ) ) ,
                               msg = "PAMid is not formatted correctly" )

      if ( sum( pamend ) == 0 ){

            df <- data.frame( PAMid = NA,
                              pamend = NA,
                              mer34 = NA,
                              mer27 = NA,
                              convertmer34 = NA
            )
      } else {

            df <- data.frame( pamend = pamend ) %>%
                  tibble::rownames_to_column( var = "PAMid") %>%
                  dplyr::mutate(
                        mer34 = stringr::str_sub( sequence, pamend-7, pamend+26 ),
                        mer27 = stringr::str_sub( mer34, 5, 31 )
                  ) %>%
                  dplyr::filter( stringr::str_length( mer34 ) == 34 ) %>% #This step removes any sites where the PAM resides in the sequence but the 34mer extends beyond the input sequence
                  dplyr::mutate(
                        convertmer34 = mer34
                  )

            if ( !( dim(df)[1] == 0 ) ){

                  ## convert non-canonical PAMs to 'TTTC'
                  for ( i in 1:dim(df)[1] ){

                        if ( !( any( c('TTTA', 'TTTC', 'TTTG') %in% df$PAMid[i] ) ) ) {
                              stringr::str_sub( df$convertmer34[i], 5, 8 ) <- 'TTTC'
                        }

                  }
            } else {

                  df <- data.frame( PAMid = NA,
                                    pamend = NA,
                                    mer34 = NA,
                                    mer27 = NA,
                                    convertmer34 = NA )
            }

      }

      return( df )
}
