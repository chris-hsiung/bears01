#' Wilcoxon Test for Two Groups in a DataFrame
#'
#' Performs a Wilcoxon test on two groups within a dataframe based on a grouping variable and returns the p-value.
#'
#' @param dataframe A data frame containing the data.
#' @param groupvar1 The first group to be compared.
#' @param groupvar2 The second group to be compared.
#' @param groupname_var The column name in dataframe that represents the grouping variable.
#' @param value_var The column name in dataframe representing the values on which the Wilcoxon test is performed.
#' @param alternative_var A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#'
#' @return A data frame with the p-value of the Wilcoxon test. If there's insufficient data for either group, the result will be "Insufficient data".
#'
#' @examples
#' \dontrun{
#' df <- data.frame(group = c("A", "A", "B", "B"), value = c(1,2,3,4))
#' wilcoxtestfordf(df, "A", "B", "group", "value", "less")
#' }
#'
#' @export
wilcoxtestfordf <- function(dataframe, groupvar1, groupvar2, groupname_var, value_var, alternative_var, round_var = 2, sciformat_var = FALSE){

      assertthat::assert_that(is.data.frame(dataframe), msg = "dataframe is not a data frame.")
      assertthat::assert_that(groupname_var %in% names(dataframe), msg = paste0(groupname_var, " is not a valid column in dataframe."))
      assertthat::assert_that(value_var %in% names(dataframe), msg = paste0(value_var, " is not a valid column in dataframe."))
      assertthat::assert_that(alternative_var %in% c("two.sided", "less", "greater"), msg = "alternative_var must be 'two.sided', 'less', or 'greater'.")

  groupvar1df <- dplyr::filter( dataframe, !!sym(groupname_var) == groupvar1 )

  groupvar2df <- dplyr::filter( dataframe, !!sym(groupname_var) == groupvar2 )

  if ( sum( !(is.na(groupvar1df[[value_var]])) ) < 2 | sum( !(is.na(groupvar2df[[value_var]])) ) < 2) {
    pvalue <- NA
  } else{
    pvalue <- format(wilcox.test(groupvar1df[[value_var]], groupvar2df[[value_var]], alternative = alternative_var)$p.value, scientific = sciformat_var, digits = round_var)
  }

  dfout <- data.frame( wilcox_pvalue = as.numeric(pvalue))
  return(dfout)

 }

