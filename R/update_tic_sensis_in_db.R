



#' Update Ticker Sensis in DB
#'
#' @param ticker_list A list of ticker_yh symbol in DB
#' @param valuation_date A Date to calculate Volas & Returns for
#' @param check_duplicates if \code{TRUE} deletes duplicates in sensi table
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#' update_tic_sensis_in_db(ticker_list = c("AAPL","AMZN"),valuation_date = aikia::val_date())
#' }
update_tic_sensis_in_db <- function(ticker_list = NULL, valuation_date = NULL,  check_duplicates = FALSE, verbose = FALSE){

  # Check for correct ticker list
  if(is.null(ticker_list) | any(stringi::stri_count(ticker_list,regex="\\S+") > 1)){
    stop(crayon::red("please provide ticker_yh for sensi calculation\n"))
  }


  # set valuation date
  if(is.null(valuation_date)){
    if(verbose){cat(crayon::yellow(paste0("valation date is set to ", aikia::val_date(),"\n")))}
    valuation_date <- aikia::val_date()
  }

  if(lubridate::as_date(valuation_date) >= lubridate::today()){
    if(verbose){cat(crayon::yellow(paste0("valation date is set to ", aikia::val_date(),"\n")))}
    valuation_date <- aikia::val_date()
  }


  end_date <- aikia::val_date(offset = -400)

  tic_sql <- ticker_list %>%
    stringr::str_c(collapse = "','") # first and last ' willl be added in sql query !!


  # Get ticker history out of Data Base
  tic_history <-  DBI::dbGetQuery(mydb, stringr::str_c("SELECT *
                                FROM fin_ticker_history
                                WHERE ticker_yh IN ('",tic_sql,"')
                                AND date <= '",valuation_date,"'
                                AND date >= '",end_date,"'")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(desc(date))


  if(verbose){cat(crayon::blue("starting p&l vola calculation\n"))}

  ticker_vola <- purrr::map_df(unique(ticker_list), ticker_vola_fun, data = tic_history, required_date = valuation_date, verbose = FALSE)

   # connect to db
  mydb <- aikia::connect_to_db(user="ceilert",password = "ceilert")

  DBI::dbWriteTable(conn = mydb,
                    name = "fin_ticker_sensi_history",
                    value = ticker_vola,
                    row.names = FALSE,
                    append = TRUE)

  DBI::dbDisconnect(mydb)


  # Check for duplicates
  if(check_duplicates==TRUE){

    if(verbose){cat(crayon::blue("deleting all duplicates in 'fin_ticker_sensi_history'\n"))}

    mydb <- aikia::connect_to_db(user="ceilert",password = "ceilert")

    all_sensis <- DBI::dbReadTable(mydb,"fin_ticker_sensi_history") %>%
      dplyr::distinct(ticker_yh,date)

    DBI::dbWriteTable(conn = mydb,
                      name = "fin_ticker_sensi_history",
                      value = ticker_vola,
                      row.names = FALSE,
                      append = TRUE)

    DBI::dbDisconnect(mydb)

  }



}
