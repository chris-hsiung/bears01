
#' Clean Name
#'
#' Remove specified characters or patterns from column names of a data frame.
#'
#' @param df A data frame.
#' @param strtoremove A character or pattern to be removed from column names.
#'
#' @return A data frame with modified column names.
#'
#' @examples
#' df <- data.frame(First.Name = c("John", "Jane"),
#' Last.Name = c("Doe", "Smith"))
#' clean_name(df, "\.")
#' # Output:
#' # FirstName LastName
#' # 1 John Doe
#' # 2 Jane Smith
#'
#' @importFrom stringr str_remove_all
#'
#' @export
clean_name <- function( df, strtoremove ){
      names(df) <- stringr::str_remove_all( names(df), strtoremove)
      return(df)
}
