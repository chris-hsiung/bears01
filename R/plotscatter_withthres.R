#' Scatter Plot with Thresholds and Quadrant Statistics
#'
#' This function creates a scatter plot using the ggplot2 package. The scatter plot is divided
#' into four quadrants using specified x and y thresholds. The percentage of data points in
#' each quadrant is calculated and displayed within the respective quadrant.
#'
#' @param df A data frame containing the data to be plotted.
#' @param facetformula_var A character string specifying the faceting formula for the plot.
#' @param x_var A character string specifying the column name for the x-axis variable in df.
#' @param y_var A character string specifying the column name for the y-axis variable in df.
#' @param xthres_var A character string specifying the column name for the x-axis threshold in df.
#' @param ythres_var A character string specifying the column name for the y-axis threshold in df.
#' @param quadrantx_var A numeric vector of length 4 specifying the x-coordinates for displaying quadrant statistics. Defaults to c(4.5, 4.5, 2, 2).
#' @param quadranty_var A numeric vector of length 4 specifying the y-coordinates for displaying quadrant statistics. Defaults to c(5.2, 0.4, 0.4, 5.2).
#' @param color_var A character string specifying the column name for point colors in df.
#' @param alpha_var A numeric value between 0 and 1 specifying the transparency of the scatter points. Defaults to 0.2.
#' @param basesize_var A numeric value greater than 0 specifying the base font size for the plot. Defaults to 15.
#' @param title_var A character string specifying the title for the plot. Defaults to an empty string.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{dfout}: A data frame summarizing the percentage of points in each quadrant.
#'   \item \code{plotout}: The ggplot2 object of the scatter plot.
#' }
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(x = rnorm(100), y = rnorm(100), color = sample(c('red', 'blue'), 100, replace = TRUE))
#'   result <- plotscatter_withthres(df, "~.", "x", "y", "0", "0", color_var = "color")
#'   print(result$plotout)
#' }
#'
#' @importFrom ggplot2 ggplot geom_point geom_text geom_hline geom_vline facet_grid theme_bw ggtitle
#' @importFrom dplyr group_by mutate summarise ungroup left_join
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_split str_remove
#' @importFrom assertthat assert_that
#' @export

plotscatter_withthres <- function( df, facetformula_var, x_var, y_var, xthres_var, ythres_var, quadrantx_var = c( 4.5, 4.5, 2, 2), quadranty_var = c( 5.2, 0.4, 0.4, 5.2), color_var, alpha_var = 0.2, basesize_var = 15, title_var = '' ){

      # Assert input arguments
      assertthat::assert_that(!is.null(df), msg = "Data frame 'df' is missing")
      assertthat::assert_that(is.data.frame(df), msg = "Input 'df' must be a data frame")
      assertthat::assert_that(!is.null(facetformula_var), msg = "Argument 'facetformula_var' is missing")
      assertthat::assert_that(is.character(facetformula_var), msg = "Argument 'facetformula_var' must be a character")
      assertthat::assert_that(!is.null(x_var), msg = "Argument 'x_var' is missing")
      assertthat::assert_that(is.character(x_var), msg = "Argument 'x_var' must be a character")
      assertthat::assert_that(!is.null(y_var), msg = "Argument 'y_var' is missing")
      assertthat::assert_that(is.character(y_var), msg = "Argument 'y_var' must be a character")
      assertthat::assert_that(!is.null(xthres_var), msg = "Argument 'xthres_var' is missing")
      assertthat::assert_that(is.character(xthres_var), msg = "Argument 'xthres_var' must be a character")
      assertthat::assert_that(!is.null(ythres_var), msg = "Argument 'ythres_var' is missing")
      assertthat::assert_that(is.character(ythres_var), msg = "Argument 'ythres_var' must be a character")
      assertthat::assert_that(!is.null(quadrantx_var), msg = "Argument 'quadrantx_var' is missing")
      assertthat::assert_that(is.numeric(quadrantx_var), msg = "Argument 'quadrantx_var' must be numeric")
      assertthat::assert_that(length(quadrantx_var) == 4, msg = "Argument 'quadrantx_var' must have length 4")
      assertthat::assert_that(!is.null(quadranty_var), msg = "Argument 'quadranty_var' is missing")
      assertthat::assert_that(is.numeric(quadranty_var), msg = "Argument 'quadranty_var' must be numeric")
      assertthat::assert_that(length(quadranty_var) == 4, msg = "Argument 'quadranty_var' must have length 4")
      assertthat::assert_that(!is.null(color_var), msg = "Argument 'color_var' is missing")
      assertthat::assert_that(is.character(color_var), msg = "Argument 'color_var' must be a character")
      assertthat::assert_that(is.numeric(alpha_var) && alpha_var >= 0 && alpha_var <= 1, msg = "Argument 'alpha_var' must be a numeric value between 0 and 1")
      assertthat::assert_that(is.numeric(basesize_var) && basesize_var > 0, msg = "Argument 'basesize_var' must be a numeric value greater than 0")
      assertthat::assert_that(is.character(title_var), msg = "Argument 'title_var' must be a character")

      coorddf <- data.frame( quadrant = c('upperR', 'lowerR', 'lowerL', 'upperL'),
                             quadrantx = quadrantx_var,
                             quadranty = quadranty_var )


      facet_strvar <- unlist(stringr::str_split(facetformula_var, '\\+|\\~'))
      if (any(facet_strvar == '.')) {
            facet_strvar <- facet_strvar[!(facet_strvar == '.')]
      }

      dfout <- df %>%
            dplyr::group_by( across(all_of(facet_strvar)) ) %>%
            dplyr::mutate(
                  upperR = ifelse( !!sym(y_var) >= !!sym(ythres_var) & !!sym(x_var) >= !!sym(xthres_var), TRUE, FALSE ),
                  upperL = ifelse( !!sym(y_var) >= !!sym(ythres_var) & !!sym(x_var) < !!sym(xthres_var), TRUE, FALSE ),
                  lowerL = ifelse( !!sym(y_var) < !!sym(ythres_var) & !!sym(x_var) < !!sym(xthres_var), TRUE, FALSE ),
                  lowerR = ifelse( !!sym(y_var) < !!sym(ythres_var) & !!sym(x_var) >= !!sym(xthres_var), TRUE, FALSE )
            ) %>%
            dplyr::summarise(
                  N = n(),
                  fracupperR = round( sum(upperR)/n()*100, 1),
                  fracupperL = round( sum(upperL)/n()*100, 1),
                  fraclowerL = round( sum(lowerL)/n()*100, 1),
                  fraclowerR = round( sum(lowerR)/n()*100, 1)
            ) %>%
            ungroup() %>%
            tidyr::pivot_longer( cols = c('fracupperR', 'fracupperL', 'fraclowerL', 'fraclowerR'), names_to = 'quadrant', values_to = 'quadrantfrac' ) %>%
            dplyr::mutate(
                  quadrant = str_remove( quadrant, 'frac'),
                  quadrantfrac = paste0( quadrantfrac, '%' )
            ) %>%
            dplyr::left_join( coorddf, by = 'quadrant')

      plotout <- ggplot() +
            geom_point( data = df, aes_string( x = x_var, y = y_var, color = color_var ), alpha = alpha_var ) +
            geom_text( data = dfout, aes_string( x = 'quadrantx', y = 'quadranty', label = 'quadrantfrac') ) +
            geom_hline( data = df, aes_string( yintercept = ythres_var ), linetype = 'dashed') +
            geom_vline( data = df, aes_string( xintercept = xthres_var ), linetype = 'dashed') +
            facet_grid( as.formula(facetformula_var) ) +
            theme_bw( base_size = basesize_var ) +
            ggtitle(title_var)

      return( list(dfout = dfout, plotout = plotout))
}


