#' Calculate Quantile Threshold Percentages
#'
#' This function calculates the quantile threshold percentages based on the input data.
#'
#' @param data A data frame containing the input data.
#' @param group_var A character vector specifying the grouping variable(s).
#' @param thres_var A character vector specifying the threshold variable (default is 'Spacer').
#' @param thresname_var A character vector specifying the threshold name variable (default is 'NT-3').
#' @param value_var A character vector specifying the value variable.
#' @param quantilethres_var A character vector specifying the quantile threshold variable (default is '0.05').
#' @param round_var An integer specifying the number of decimal places to round the percentages (default is 1).


calculate_quantilethresperc <- function(data, group_var, thres_var = 'Spacer', thresname_var = 'NT-3', value_var, quantilethres_var = '0.05', round_var = 1) {

      # Check if input arguments are valid
      assertthat::assert_that(is.data.frame(data))
      assertthat::assert_that(is.character(group_var))
      assertthat::assert_that(is.character(thres_var))
      assertthat::assert_that(is.character(thresname_var))
      assertthat::assert_that(is.character(value_var))
      assertthat::assert_that(is.character(quantilethres_var))


      valuethres_var = paste0(value_var,'_', quantilethres_var,'_thres')
      valueperc_var = paste0( value_var, '_', quantilethres_var,'_perc')

      thresdf <- data %>%
            dplyr::filter(.data[[thres_var]] == thresname_var) %>%
            dplyr::group_by(across(all_of(group_var))) %>%
            dplyr::summarise( !!sym(valuethres_var) := quantile(.data[[value_var]], as.numeric(quantilethres_var))) %>%
            ungroup()

      dfout <- left_join(data, thresdf, by = group_var) %>%
            dplyr::group_by(across(all_of(thres_var)), across(all_of(group_var))) %>%
            dplyr::mutate( !!sym( valueperc_var ) := paste0( round(sum(.data[[value_var]] <= !!sym(valuethres_var)) / n()*100,round_var), '%' ) ) %>%
            dplyr::ungroup()

      return(dfout)
}
