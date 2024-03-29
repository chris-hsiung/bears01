#' Calculate Quantile Threshold Percentages And Counts
#'
#' This function calculates the quantile threshold percentages based on the input data.
#' Warning: roxygen documentation generated by chatgpt
#' @param data A data frame containing the input data.
#' @param group_var A character vector specifying the grouping variable(s).
#' @param thres_var A character vector specifying the threshold variable (default is 'Spacer').
#' @param thresname_var A character vector specifying the threshold name variable (default is 'NT-3').
#' @param value_var A character vector specifying the value variable.
#' @param quantilethres_var A character vector specifying the quantile threshold variable (default is '0.05').
#' @param round_var An integer specifying the number of decimal places to round the percentages (default is 1).
#` 2023-07-13 fixed bug involving joining thresdf and data
#' @export

calculate_quantilethresperc <- function(data, group_var, thres_var = 'Spacer', thresname_var = 'NT-3', value_var, quantilethres_var = '0.05', round_var = 1 ) {

      # Check if input arguments are valid
      assertthat::assert_that(is.data.frame(data))
      assertthat::assert_that(is.character(group_var))
      assertthat::assert_that(is.character(thres_var))
      assertthat::assert_that(is.character(thresname_var))
      assertthat::assert_that(is.character(value_var))
      assertthat::assert_that(is.character(quantilethres_var))

      valuenumon_var = paste0( value_var,'_',quantilethres_var,'_numon')
      valuenumoff_var = paste0( value_var,'_', quantilethres_var, '_numoff')
      valuethres_var = paste0(value_var,'_', quantilethres_var,'_thres')
      valuethreslog2FC_var = paste0( value_var, '_', quantilethres_var, '_threslog2FC')
      valueperc_var = paste0( value_var, '_', quantilethres_var,'_perc')
      meanthres_var = paste0( value_var, '_meanthres')
      medianthres_var = paste0( value_var, '_medianthres')
      valuemeanFCperc_var = paste0( value_var, '_meanFCperc')
      valuemedianFCperc_var = paste0( value_var, '_medianFCperc')
      valuemedianlog2FC_var = paste0( value_var, '_medianlog2FC')
      valuemedianlog10FC_var = paste0( value_var, '_medianlog10FC')

      individvaluemedianlog2FC_var = paste0( value_var,'_individmedianlog2FC')

      join_var <- group_var[ !( group_var %in% thres_var ) ]

      thresdf <- data %>%
            dplyr::filter(.data[[thres_var]] == thresname_var) %>%
            dplyr::group_by(across(all_of(group_var))) %>%
            dplyr::summarise(
                  !!sym(valuethres_var) := quantile(.data[[value_var]], as.numeric(quantilethres_var)),
                  !!sym(meanthres_var) := mean(.data[[value_var]]),
                  !!sym(medianthres_var) := median(.data[[value_var]]),
                  !!sym(valuethreslog2FC_var) := log2( !!sym(valuethres_var)/!!sym(medianthres_var))
                  ) %>%
            ungroup() %>%
            dplyr::select( all_of(join_var), contains('_meanthres'), contains('_medianthres'), contains('_thres'), contains('_threslog2FC') )

      dfout <- left_join(data, thresdf, by = join_var) %>%
            dplyr::group_by( across(all_of(thres_var)), across(all_of(group_var)) )  %>%
            dplyr::mutate(
                  # N = n(),
                  !!sym( individvaluemedianlog2FC_var ) := log2(.data[[value_var]]/unique( .data[[medianthres_var]])),

                  !!sym(valuenumon_var) := sum( .data[[value_var]] > unique( .data[[valuethres_var]] ) ) ,
                  !!sym(valuenumoff_var) := sum( .data[[value_var]] <= unique(.data[[valuethres_var]]) ),

                  !!sym( valueperc_var ) := paste0( round(sum(.data[[value_var]] <= !!sym(valuethres_var)) / n()*100,round_var), '%' ),

                  !!sym( valuemeanFCperc_var ) := round( mean( .data[[value_var]] )/unique( .data[[meanthres_var]])*100, round_var ),

                  !!sym( valuemedianFCperc_var ) := round( median( .data[[value_var]])/unique( .data[[medianthres_var]])*100, round_var),

                  !!sym( valuemedianlog2FC_var ) := log2( median( .data[[value_var]])/unique(.data[[medianthres_var]]) ),

                  !!sym( valuemedianlog10FC_var ) := log10( median( .data[[value_var]])/unique(.data[[medianthres_var]]) )
                  ) %>%
            dplyr::ungroup()

      return(dfout)
}
