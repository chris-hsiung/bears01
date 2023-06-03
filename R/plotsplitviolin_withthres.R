#' Plot Split Violin with Thresholds
#'
#' This function creates a split violin plot with thresholds using ggplot2.
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


plotsplitviolin_withthres <- function(data, x_var, y_var, fill_var, dodge_width = 0.8, color_palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), facetformula_var, title_var, thres_var, perc_var, perclabely = 2, labelsize = 6, basesize_var = 18 ) {

      facet_strvar <- unlist( stringr::str_split( facetformula_var, '\\+|\\~' ))

      facet_strvar <- facet_strvar[ !(facet_strvar == '.')]

      thresdf <- dplyr::select( data, all_of( c(y_var, thres_var, facet_strvar, fill_var) ) ) %>%
            unique()

      labeldf <- dplyr::select( data, all_of( c(facet_strvar, perc_var, fill_var) ) ) %>%
            unique()

      plotout <- ggplot() +
            introdataviz::geom_split_violin( data = data, aes( x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var)), alpha = .3, scale = 'width', trim = FALSE ) +
            stat_summary( data =  data, aes( x = !!sym(x_var), y = !!sym(y_var), color = !!sym(fill_var)), fun = mean, fun.max = function(x){quantile(x,0.25)}, fun.min = function(x){quantile(x,0.75)}, geom = "pointrange", show.legend = F, size = 1.5, fatten = 2, position_dodge( width = 0.4)) +
            geom_hline( data = thresdf, aes_string( yintercept = thres_var, color = fill_var), linetype = 'dashed' ) +
            geom_text( data = labeldf, aes_string( x = x_var, y = perclabely, label= perc_var), size = labelsize, position = position_dodge( width = 0.4) ) +
            scale_fill_manual(values = color_palette) +
            scale_color_manual(values = color_palette) +
            facet_grid(as.formula(facetformula_var), scales = 'free' ) +
            theme_bw(base_size = basesize_var) +
            theme(axis.text.x = element_text(angle = 90)) +
            # coord_flip() +
            ggtitle( title_var )

      return(plotout)
}
