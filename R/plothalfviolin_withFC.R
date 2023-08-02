# ## in progress
# ## get summary prior to transformation
# ## then transform data for plotting
#
# data = manucd81df
# x_var = 'YL1A'
# y_var = 'Description2'
# facetformula_var = 'RepID~Spacer'
# fill_var = 'Description2'
# title_var = 'CD81-PE 2022-04-01 Day 6 piggyBAC'
# thres_var = 'YL1A_0.5_thres'
# label_var = 'YL1A_0.5_meanFC'
# nudge_var = 0.5
# labelsize = 4
# basesize_var = 18
#
# plotridge_log10trans_withFC <- function(data, x_var, y_var, summaryfn_var = 'median', fill_var, dodge_width = 0.8, color_palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), facetformula_var, title_var, thres_var, label_var, perclabelx = 1, labelsize = 6, basesize_var = 18, nudge_var = 0.5) {
#
#       assertthat::assert_that( is.factor(data[[y_var]]) == TRUE, msg = paste0( y_var, ' must be factor') )
#
#       facet_strvar <- unlist(stringr::str_split(facetformula_var, '\\+|\\~'))
#       if (any(facet_strvar == '.')) {
#             facet_strvar <- facet_strvar[!(facet_strvar == '.')]
#       }
#
#       thresdf <- dplyr::select(data, all_of(c(y_var, thres_var, facet_strvar, fill_var))) %>%
#             unique()
#
#       yvarnumeric <- seq( 1, length(unique(data[[y_var]])), 1)
#
#       yvarnumericdf <- data.frame( levels(data[[y_var]]), yvarnumeric, yvarnumeric+1 )
#
#       names( yvarnumericdf) <-  c(y_var, 'ystart', 'yend' )
#
#       thresdf <- dplyr::left_join( thresdf, yvarnumericdf, by = y_var )
#
#       labeldf <- dplyr::select(data, all_of(c(y_var, facet_strvar, label_var, fill_var))) %>%
#             unique()
#
#       plotout <- ggplot() +
#             ggridges::geom_density_ridges(data = data, aes_string(y = y_var, x = x_var, fill = fill_var), alpha = 0.2) +
#             stat_summary(data = data, aes_string(y = y_var, x = x_var, color = fill_var), fun = summaryfn_var,
#                          fun.max = function(x) { quantile(x, 0.25) },
#                          fun.min = function(x) { quantile(x, 0.75) },
#                          geom = "pointrange", show.legend = FALSE, size = 0.3, position = position_nudge(x = 0, y = 0.35 ) ) +
#             geom_segment(data = thresdf, aes_string(x = thres_var, xend = thres_var, y = 'ystart' , yend = 'yend' ), linetype = 'dotted' ) +
#             geom_text(data = labeldf, aes_string(x = perclabelx, y = y_var, label = perc_var), size = labelsize, nudge_y = nudge_var) +
#             scale_fill_manual(values = color_palette) +
#             scale_color_manual(values = color_palette) +
#             facet_grid(as.formula(facetformula_var), scales = 'free') +
#             theme_bw(base_size = basesize_var) +
#             theme(axis.text.x = element_text(angle = 90)) +
#             ggtitle(title_var)
#
#       return(plotout)
# }
#
#
# manucd81df <- subset( cd55cd81gfpdf, grepl('CD81-1|NT-3', Spacer)) %>%
#       subset( RepID == 'Rep1') %>%
#       subset( gCHID %in% c('gCH128','gCH29', 'gCH130','gCH132', 'gCH134')) %>%
#       dplyr::mutate(
#             Spacer = factor( Spacer, levels = c('NT-3', 'CD81-1', 'CD55-4_B2M-1_KIT-2_CD81-1', 'CD55-4_B2M-1_KIT-2_KIT-3_CD81-1', 'CD55-4_B2M-1_B2M-3_KIT-2_KIT-3_CD81-1' )),
#             Description2 = factor( Description2, levels = c('denAsCas12a-KRAB', 'multiAsCas12a-KRAB', 'multiAsCas12a'))
#       ) %>%
#       subset( !(is.na(Description2)))
# sv2 <- plothalfviolin_withFC( data = manucd81df,
#                                           x_var = 'YL1A',
#                                           y_var = 'Description2',
#                                           facetformula_var = 'RepID~Spacer',
#                                           fill_var = 'Description2',
#                                           title_var = 'CD81-PE 2022-04-01 Day 6 piggyBAC',
#                                           thres_var = 'YL1A_0.5_thres',
#                                           perc_var = 'YL1A_0.5_meanFC',
#                                           nudge_var = 0.5,
#                                           labelsize = 4 ) +
#       theme( legend.position = 'none', strip.text.x = element_text(size = 7))
# #coord_trans( x = 'log10') ## this transformation occurs after calculation of summary stats
# print(sv2)
#
#
#
