#' Get Best CRISPick Spacers. As written is currently intended for CRIPRko output from CRISPick.
#'@param CRISPickfile A data frame containing CRISPick results. Minimally must contain 'sgRNA Sequence' column, which must consist of DNA bases only
#'@param TTTVonly logical indicates whether to keep only TTTV PAM spacers; defaults to TRUE
#'@param checkBsmbI logical indicates whether to remove spacers containing BsmbI sites; defaults to TRUE
#'@param removeTTTT logical indicates whether to remove spacers containing TTTT or start with TTT; defaults to TRUE
#'@author Chris Hsiung
#'@return After filtering out TTTT, TTTstart, BsmbI sites (optional), Offtarget matches to 'MAX',  returns a data frame sorted by On-Target Efficacy Score.
#'@export
getBestCRISPickSpacers <- function(CRISPickfile, TTTVonly = TRUE, checkBsmbI = TRUE, removeTTTT = TRUE ){
  df <- fread( CRISPickfile )


assertthat::assert_that(
  'sgRNA Sequence' %in% names(df), msg = 'column names must include sgRNA Sequence' )
assertthat::assert_that(
  'On-Target Efficacy Score' %in% names(df), msg = 'column names must include On-Target Efficacy Score' )



# assertion to check all inputs are DNA
sgRNAsequence <- lapply( df$`sgRNA Sequence`, Biostrings::DNAString )

df <- dplyr::rowwise(df) %>%
  dplyr::mutate( countBsmbI = bears01::countBsmBI(`sgRNA Sequence`) ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( TTTTstatus = ifelse( grepl( 'TTTT', `sgRNA Sequence`), TRUE, FALSE ),
                 TTTstart = ifelse( grepl('^TTT', `sgRNA Sequence`), TRUE, FALSE ),
                 keep = ifelse( countBsmbI > 0 | TTTTstatus == TRUE | TTTstart == TRUE, FALSE, TRUE ),
                 filename = basename( CRISPickfile)
  ) %>%
  dplyr::arrange( desc(`On-Target Efficacy Score`) ) %>%
  subset( !(`# Off-Target Tier I Match Bin I Matches` == 'MAX') &
          !(`# Off-Target Tier II Match Bin I Matches` == 'MAX') &
          !(`# Off-Target Tier III Match Bin I Matches` == 'MAX') &
          !(`# Off-Target Tier I Match Bin II Matches` == 'MAX') &
          !(`# Off-Target Tier II Match Bin II Matches` == 'MAX') &
          !(`# Off-Target Tier III Match Bin II Matches` == 'MAX') &
          !(`# Off-Target Tier I Match Bin III Matches` == 'MAX') &
          !(`# Off-Target Tier II Match Bin III Matches` == 'MAX') &
          !(`# Off-Target Tier III Match Bin III Matches` == 'MAX')
  )


dfout <- df

if( TTTVonly == TRUE ){
  assertthat::assert_that(
    'PAM Sequence' %in% names(dfout), msg = 'PAM sequence is not in column names'
  )

  dfout <- subset( dfout, grepl('TTT[C|G|A]', `PAM Sequence`) )

}

if( removeTTTT == TRUE ){
 dfout <- subset( dfout, TTTTstatus == FALSE & TTTstart == FALSE )
}

if( checkBsmbI == TRUE ){
    dfout <- subset( dfout, countBsmbI == 0 )
}

return(dfout)
}




