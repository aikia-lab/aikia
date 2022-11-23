



#' Update Ticker Sensis of different returns and volas history in DB
#'
#' @param ticker_list A list of ticker_yh symbol in DB
#' @param check_duplicates if c deletes duplicates in sensi table
#' @param verbose if \code{TRUE} provides instant feedback
#' @param start_date a 1st valuation date
#' @param backwards number of days to calculate history geginning from start date
#'
#' @return directly saved to DB
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#' update_tic_sensis_in_db(ticker_list = c("AAPL","AMZN"),valuation_date = aikia::val_date(), backwards = 10)
#' }
update_tic_sensis_history_in_db <- function(ticker_list = NULL, start_date = FALSE, check_duplicates = TRUE, verbose = FALSE, backwards=0){


  success_cat <- crayon::bold $ green
  info_cat <- crayon::cyan $ bold
  warning_cat <- crayon::yellow $ bold
  red_cat <- crayon::red $ bold

    #load calendar
  suppressMessages(bizdays::load_quantlib_calendars(ql_calendars = "UnitedStates/NYSE",
                                                    from= lubridate::as_date('2010-01-01'),
                                                    to=lubridate::today(),
                                                    financial = TRUE))


  # set valuation date
  if(is.null(start_date)){
    start_date <- aikia::val_date()
    if(verbose){cat(warning_cat(paste0("valation date is set to ", aikia::val_date(),"\n")))}
  }

  if(lubridate::as_date(start_date) >= lubridate::today()){
    if(verbose){cat(warning_cat(paste0("valation date is set to ", aikia::val_date(),"\n")))}
    start_date <- aikia::val_date()
  }

  # Check for correct ticker list
  if(is.null(ticker_list) | any(stringi::stri_count(ticker_list,regex="\\S+") > 1)){
    stop(red_cat("please provide ticker_yh for sensi calculation\n"))
  }

  for(i in 0:backwards){

    valuation_date <- bizdays::offset(lubridate::as_date(start_date),
                                      -i,
                                      "QuantLib/UnitedStates/NYSE")

    end_date <- lubridate::as_date(valuation_date)-(400+i)

    displ_date <- as.character(valuation_date)

    if(verbose){cat(i,info_cat(" calculating sensis for date "),displ_date,"\n")}

    tic_sql <- ticker_list %>%
      stringr::str_c(collapse = "','") # first and last ' willl be added in sql query !!

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    # Get ticker history out of Data Base
    tic_history <-  DBI::dbGetQuery(mydb, stringr::str_c("SELECT *
                                  FROM fin_ticker_history
                                  WHERE ticker_yh IN ('",tic_sql,"')
                                  AND date <= '",valuation_date,"'
                                  AND date >= '",end_date,"'")) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(date = lubridate::as_date(date)) %>%
      dplyr::arrange(desc(date))

    DBI::dbDisconnect(mydb)

    if(verbose){cat(info_cat("starting p&l vola calculation\n"))}

    ticker_vola <- purrr::map_df(unique(ticker_list), ticker_vola_fun, data = tic_history, required_date = valuation_date, verbose = verbose)

    # connect to db
    mydb <- aikia::connect_to_db(user="ceilert",password = "ceilert")

    DBI::dbWriteTable(conn = mydb,
                      name = "fin_ticker_sensi_history",
                      value = ticker_vola,
                      row.names = FALSE,
                      append = TRUE)

    DBI::dbDisconnect(mydb)

  }

  # Check for duplicates
  if(check_duplicates==TRUE){

    if(verbose){cat(info_cat("deleting all duplicates in 'fin_ticker_sensi_history'\n"))}

    mydb <- aikia::connect_to_db(user="ceilert",password = "ceilert")

    # Delete dublicate rows
    DBI::dbSendQuery(mydb,"DELETE
                          FROM fin_ticker_sensi_history
                          WHERE uid IN (
                            SELECT uid
                            FROM (
                              SELECT *,
                              RANK() OVER (PARTITION BY date, ticker_yh
                                           ORDER BY retrieval_time DESC) AS oldest
                              FROM fin_ticker_sensi_history
                              ORDER BY date) AS get_values
                            WHERE get_values.oldest = 2)")

    DBI::dbDisconnect(mydb)
  }

}







