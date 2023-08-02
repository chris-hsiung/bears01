#' Plot Half Violin with Thresholds
#'
#' This function creates a half violin plot with thresholds using ggplot2.
#'
#' @param data A data frame containing the variables used for plotting.
#' @param x_var The variable name for the x-axis.
#' @param y_var The variable name for the y-axis.
#' @param fill_var The variable name for the fill color.
#' @param dodge_width The width of the dodge for grouped violins (default = 0.8).
#' @param color_palette A vector of colors to use for the plot (default = cbbPalette).
#' @param facetformula_var The formula specifying the faceting variables.
#' @param facet_strvar The variable name for facet labels.
#' @param title_var The title for the plot.
#' @param thres_var The variable name for the threshold.
#' @param perc_var The variable name for the percentage label.
#' @param perclabelx The x-coordinate adjustment for the percentage label (default = 1).
#' @param facet_var The variable name for faceting (optional).
#' @param labelsize The font size for the percentage label (default = 6).
#' @param basesize_var The base font size for the plot (default = 18).
#'
#' @return A ggplot object representing the half violin plot with thresholds.
#'
#' @import ggplot2
#' @import dplyr
#' @import ggridges
#'
#' @export


plothalfviolin_withthres <- function(data, x_var, y_var, summaryfn_var = 'median', fill_var, dodge_width = 0.8, color_palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), facetformula_var, title_var, thres_var, perc_var, perclabelx = 1, labelsize = 6, basesize_var = 18, nudge_var = 0.5) {

      assertthat::assert_that( is.factor(data[[y_var]]) == TRUE, msg = paste0( y_var, ' must be factor') )

      facet_strvar <- unlist(stringr::str_split(facetformula_var, '\\+|\\~'))
      if (any(facet_strvar == '.')) {
            facet_strvar <- facet_strvar[!(facet_strvar == '.')]
      }

      thresdf <- dplyr::select(data, all_of(c(y_var, thres_var, facet_strvar, fill_var))) %>%
            unique()

      yvarnumeric <- seq( 1, length(unique(data[[y_var]])), 1)

      yvarnumericdf <- data.frame( levels(data[[y_var]]), yvarnumeric, yvarnumeric+1 )

      names( yvarnumericdf) <-  c(y_var, 'ystart', 'yend' )

      thresdf <- dplyr::left_join( thresdf, yvarnumericdf, by = y_var )

      labeldf <- dplyr::select(data, all_of(c(y_var, facet_strvar, perc_var, fill_var))) %>%
            unique()

      plotout <- ggplot() +
            ggridges::geom_density_ridges(data = data, aes_string(y = y_var, x = x_var, fill = fill_var), alpha = 0.2) +
            stat_summary(data = data, aes_string(y = y_var, x = x_var, color = fill_var), fun = summaryfn_var,
                         fun.max = function(x) { quantile(x, 0.25) },
                         fun.min = function(x) { quantile(x, 0.75) },
                         geom = "pointrange", show.legend = FALSE, size = 0.3, position = position_nudge(x = 0, y = 0.35 ) ) +
            geom_segment(data = thresdf, aes_string(x = thres_var, xend = thres_var, y = 'ystart' , yend = 'yend' ), linetype = 'dotted' ) +
            geom_text(data = labeldf, aes_string(x = perclabelx, y = y_var, label = perc_var), size = labelsize, nudge_y = nudge_var) +
            scale_fill_manual(values = color_palette) +
            scale_color_manual(values = color_palette) +
            facet_grid(as.formula(facetformula_var), scales = 'free') +
            theme_bw(base_size = basesize_var) +
            theme(axis.text.x = element_text(angle = 90)) +
            ggtitle(title_var)

      return(plotout)
}
