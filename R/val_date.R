#' Return the current valuation date
#'
#' @param offset A negative integer defining the number of business days to offset from today
#' @param location Which holiday calendar to use. Either "us" for NYSE or "de" for Boerse Frankfurt
#'
#' @return An object of type \code{date}
#' @export
#'
#' @examples
#' val_date()
#' val_date(offset = -5)
#' val_date(offset = -3, location = "de")
val_date <- function(offset = -1, location = "us"){

stopifnot("Value must be a negative integer or zero!" = offset <= 0)
suppressMessages(bizdays::load_quantlib_calendars(c('UnitedStates/NYSE', "Germany/FrankfurtStockExchange"),
                                                  from='2016-01-01',
                                                  to=lubridate::today(),
                                                  financial = TRUE))

cal_vec <- c("us" = "QuantLib/UnitedStates/NYSE",
             "de" = "QuantLib/Germany/FrankfurtStockExchange")

val_date <- bizdays::offset(lubridate::today(),
                            offset,
                            cal_vec[[location]])

return(val_date)

}
