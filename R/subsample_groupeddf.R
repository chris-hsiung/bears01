#' Subsample a Data Frame
#'
#' The `subsample_groupeddf` function returns a subsampled version of the input data frame. If the number of rows
#' in a group of the input data frame is less than the specified threshold (`Nthres`), all rows of that
#' group are returned. Otherwise, a random subset of rows, of size `Nthres`, from each group is returned.
#'
#' @param df A data frame to be subsampled. Can be grouped or ungrouped.
#' @param Nthres An integer threshold for subsampling. If a group in `df` (or the entire `df` if it's not grouped)
#'   has fewer rows than `Nthres`, all rows of that group (or the entire data frame) are returned.
#'   Otherwise, a random subset of rows, of size `Nthres`, is returned.
#'
#' @return A subsampled data frame.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(group = rep(1:2, each = 100), value = rnorm(200))
#' subsampled_data <- subsample_groupeddf(data, 50)
#' }
#'
#' @importFrom dplyr group_modify slice_sample
#' @importFrom assertthat assert_that
#'
#' @export
subsample_groupeddf <- function(df, Nthres) {

      # Assertions to check input arguments
      assertthat::assert_that(
            is.data.frame(df) || is.grouped_df(df),
            msg = "Input df must be a data frame or a grouped data frame."
      )

      assertthat::assert_that(
            is.numeric(Nthres),
            length(Nthres) == 1,
            Nthres > 0,
            Nthres == as.integer(Nthres),
            msg = "Nthres must be a positive integer."
      )

      # This condition might be redundant, but if you want to be explicitly sure, you can include it
      assertthat::assert_that(
            dim(df)[1] >= 0,
            msg = "df must have a non-negative number of rows."
      )

      setseed = 1

      dfout <- df %>%
            dplyr::group_modify(~ {
                  group_size <- nrow(.x)
                  if (group_size < Nthres) {
                        .x
                  } else {
                        dplyr::slice_sample(.x, n = Nthres)
                  }
            })

      return(dfout)
}
