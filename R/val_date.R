#' Return the current valuation date
#'
#' @return a date object
#' @export
#'
#' @examples
#' val_date()
val_date <- function(){

suppressMessages(bizdays::load_quantlib_calendars(c('UnitedStates/NYSE'),
                                                  from='2016-01-01', to=lubridate::today(),
                                                  financial = TRUE))

val_date <- bizdays::offset(lubridate::today(), -1, 'QuantLib/UnitedStates/NYSE')

return(val_date)

}


# Funktionsideen: Valdate, ggplot theme, gt theme
# Abfolge Package Development:
# New File
# -> Write Function
# -> Insert Roxygen2 skeleton with strg+shift+alt+r and document the variables and output
# -> Load the function and test strg + shift + l
# -> Document Package strg+shift+d. this creates the helpfile and adds dependencies to namespace file
# -> Check Package strg+shift+e this is the cmd check. any error in development will pop up here.
# -> Restart and Rebuild strg+shift+b
