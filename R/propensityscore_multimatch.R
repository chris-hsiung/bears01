#' Propensity Score Matching for Multiple Control Groups
#'
#' This function performs propensity score matching for multiple control groups
#' against a single treatment group using the `MatchIt` package.
#'
#' @param df A data frame containing the necessary variables for propensity score matching.
#' @param treatmentID A character indicating the ID for the treatment group.
#' @param controlID A vector of characters indicating the IDs for the control groups.
#' @param treatmentvar A character string specifying the column name in `df` which identifies the treatment or control IDs.
#' @param match_var A character vector specifying the column names in `df` used for matching in the propensity score model.
#'
#' @return A data frame containing the matched data.
#'
#' @examples
#' \dontrun{
#' matched_data <- propensityscore_multimatch(
#'   df = your_data_frame,
#'   treatmentID = "treatID",
#'   controlID = c("controlID1", "controlID2"),
#'   treatmentvar = "treatment_column",
#'   group_var = "group_column",
#'   match_var = c("var1", "var2")
#' )
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate filter
#' @importFrom MatchIt matchit match.data
#' @importFrom rlang sym
#'
#' @export
propensityscore_multimatch <- function( df, treatmentID, controlID, treatmentvar, match_var ){

      # Ensure df is a data frame
      assertthat::assert_that(is.data.frame(df),
                              msg = "Input df must be a data frame.")

      # Check if treatmentvar and match_var columns exist in df
      assertthat::assert_that(all(c(treatmentvar, match_var) %in% names(df)),
                              msg = "treatmentvar and/or match_var are not valid column names in df.")

      # Ensure treatmentvar is a character
      assertthat::assert_that(is.character(treatmentvar),
                              msg = "treatmentvar must be a character string.")

      # Ensure match_var is a character
      assertthat::assert_that(is.character(match_var),
                              msg = "match_var must be a character vector.")

      # Ensure treatmentID is a character
      assertthat::assert_that(is.character(treatmentID),
                              msg = "treatmentID must be a character string.")

      # Ensure controlID is a character
      assertthat::assert_that(is.character(controlID),
                              msg = "controlID must be a character vector.")

      # Ensure controlID doesn't have duplicates
      assertthat::assert_that(!(any(duplicated(controlID))),
                              msg = "controlID must have unique values.")

      # Ensure treatmentID is not in controlID
      assertthat::assert_that(!treatmentID %in% controlID,
                              msg = "treatmentID should not be part of controlID.")

      # Check if controlID is not empty
      assertthat::assert_that(length(controlID) > 0,
                              msg = "controlID cannot be empty.")

      # Check if treatmentID is not empty
      assertthat::assert_that(length(treatmentID) > 0,
                              msg = "treatmentID cannot be empty.")

      # Ensure values in treatmentvar column are among treatmentID and controlID
      assertthat::assert_that( all( c(treatmentID, controlID) %in% unique(df[[treatmentvar]]) ),
                              msg = "treatmentID and controlID must be in treatmentvar")


  df <- dplyr::mutate( df,
                       PStreatgroup = ifelse( !!sym(treatmentvar) == treatmentID, 1, 0 )
  )


  controllist <- vector("list", length(controlID) )
  names(controllist) <- unique(controlID)

  treatmentdf <- dplyr::filter( df, !!sym( treatmentvar ) == treatmentID )

  for ( cid in controlID ){
    controllist[[cid]] <- rbind( dplyr::filter( df, !!sym(treatmentvar) == cid ), treatmentdf )
  }

  psmatchobjlist <- vector('list', length(controlID))

  psmatchdflist <- vector('list', length(controlID))

  matchformula <- as.formula( paste0( 'PStreatgroup ~ ', paste( match_var, sep = ' + ')) )

  for ( cid in controlID ){

    psmatchobjlist[[cid]] <- MatchIt::matchit( formula = matchformula, data = controllist[[cid]], method = 'nearest', distance = 'glm')

    psmatchdflist[[cid]] <- MatchIt::match.data( psmatchobjlist[[cid]] )

  }

  dfout <- subset( psmatchdflist[[controlID[1]]], PStreatgroup == 1 )

  for (cid in controlID ){
    dfout <- rbind( dfout, subset( psmatchdflist[[cid]], PStreatgroup == 0 ) )
  }

  return(dfout)
}
