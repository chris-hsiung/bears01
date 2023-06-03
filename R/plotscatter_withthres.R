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


