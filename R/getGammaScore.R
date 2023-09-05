#' This function calculates gamma from screen raw read counts
# Updated 2023-08-17 fixed negctrlmedianRPM_init and negctrlmedianRPM_final by dividing both by 1e6, should not change any any gamma score calculations.
#' @param df is a data frame, must contain column names matching initcount, finalcount, and spacertype
#' @param initcount is a string for name of column in df containing raw read counts of initial condition (e.g. T0 in growth screen)
#' @param finalcount is a string for name of a column in df containing raw read counts of final condition (e.g. Tfinal in growth screen)
#' @param doublings is a string for name of numeric column in df containing total number of cell population doublings between start and end of screen
#' @param spacertype string corresponding to column name in df that categorizes which rows are negative controls
#' @param negctrllabel string corresponding to label given to negative controls in the column specified by spacertype
#' @return a data frame with gammascore added as column
#' @export

getGammaScore <- function( df, initcount, finalcount, spacertype = 'spacertype', negctrllabel = 'negctrl', doublings = 'totaldoublings' ){
      df <- as.data.frame(df)

      assertthat::assert_that( sum( c(initcount, finalcount, spacertype, doublings ) %in% names(df) ) == 4, msg = 'column names incorrect' )

      assertthat::assert_that( negctrllabel %in% unique(df[[spacertype]]), msg = paste0('negctrllabel does not exist in ', spacertype, ' column' ) )

      negctrldf <- df[ df[[spacertype]] == negctrllabel, ]

      negctrlmediancount_init <- median( negctrldf[ , initcount ] )

      negctrlmediancount_final <- median( negctrldf[ , finalcount] )


      dfout <- df %>% dplyr::mutate(
                  totalcount_init = sum(.data[[initcount]]),
                  totalcount_final = sum(.data[[finalcount]]),
                  negctrlmedianRPM_init = negctrlmediancount_init/(totalcount_init/1e6),
                  negctrlmedianRPM_final = negctrlmediancount_final/(totalcount_final/1e6),
                  targetRPM_init = .data[[initcount]]/(totalcount_init/1e6),
                  targetRPM_final = .data[[finalcount]]/(totalcount_final/1e6),
                  gammascore = log2( (targetRPM_final/negctrlmedianRPM_final)/(targetRPM_init/negctrlmedianRPM_init) )/unique(.data[[doublings]])
      )

      return(dfout)
}
