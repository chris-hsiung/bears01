### Function to extract all target sites from fasta file by looping through each sequence entry and applying getTargetSites(), returning a data frame.
## pam argument defaults to 'TTTV'
## altpams argument defaults to tier 1 non-canonical PAMs from Kleinstiver et al Nat Biotech 2019
#' By Chris Hsiung 2022-02-15
#'@name fasta2targetsites
#'@export
#'@return data frame with target sites
fasta2targetsites <- function(
      fastafile,
      pam = 'TTTV',
      altpams = c('TTYN','CTTV', 'RTTC', 'TATM', 'CTCC', 'TCCC','TACA', 'TTTT')

){

      fastadf <- fasta2df( fastafile )

      sequence <- fastadf$sequence[1]

      total <- dim(fastadf)[1]

      seqlist <- vector( mode = 'list', total )

      for ( k in 1:total ){

            print( paste0('working on sequence ', k, ' out of ', total ) )

            df <- getTargetSites( sequence = fastadf$sequence[k],
                                  pam = pam,
                                  altpams = altpams ) %>%
                  dplyr::mutate(
                        ID = fastadf$ID[k],
                        targetID = paste0( ID, '_', PAMid )
                  )

            seqlist[[k]] <- df
      }

      seqdf <- do.call('rbind', seqlist)

      return( seqdf )
}
