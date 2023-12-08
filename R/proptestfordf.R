#' Proportion Test for Two Groups in a DataFrame
#'
#' This function performs a proportion test between two specified groups in a given dataframe.
#' It calculates the number of successes for each group and then conducts a proportion test.
#'
#' @param data A dataframe containing the data to be tested.
#' @param response_col The name of the column in the dataframe that contains the binary response variable.
#' @param group_col The name of the column in the dataframe that specifies the group.
#' @param group1_label The label for the first group to be tested.
#' @param group2_label The label for the second group to be tested.
#' 
#' @return A dataframe containing the labels for the two groups and the p-value from the proportion test.
#' 
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   response = c(1, 0, 1, 0, 1, 0, 1, 0),
#'   group = c('A', 'A', 'B', 'B', 'A', 'A', 'B', 'B')
#' )
#' result <- proptestfordf(df, 'response', 'group', 'A', 'B')
#' print(result)
#' }
#' 
#' @export

proptestfordf <- function(data, response_col, group_col, group1_label, group2_label ){
  
  # Assertion checks using assertthat
  assert_that(is.data.frame(data))
  assert_that(response_col %in% names(data))
  assert_that(group_col %in% names(data))
  # assert_that(group1_label %in% unique(data[[group_col]]))
  # assert_that(group2_label %in% unique(data[[group_col]]))
  
  
  dfout <- data.frame( group1 = NULL, group2 = NULL, proppvalue = NULL )
  
  group1df <- data[ data[group_col] == group1_label, ]
  group2df <- data[ data[group_col] == group2_label, ]
  
  # Calculate the number of successes for each group
  group1_success <- sum(data[[response_col]][data[[group_col]] == group1_label], na.rm = TRUE )
  group2_success <- sum(data[[response_col]][data[[group_col]] == group2_label], na.rm = TRUE)
  
  # Calculate the total observations for each group
  group1_nobs <- nrow(subset(data, data[[group_col]] == group1_label))
  group2_nobs <- nrow(subset(data, data[[group_col]] == group2_label))    
  
  dfout <- data.frame( 
    group1 =  group1_label, 
    group2 = group2_label, 
    proppvalue = prop.test(c(group1_success, group2_success), c(group1_nobs, group2_nobs))$p.value 
  )
  
  return( dfout )
}