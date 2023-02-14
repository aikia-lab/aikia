

#' Completely setup of new ticker in DB's ticker_meta_data, ticker_history and ticker_sensi_history tables
#'
#' @param ticker_bb a Bloomberg ticker
#' @param provide_tic_yh set ticker yahoo or search by ISIN
#' @param provide_tic_fv set ticker finviz for news scraping
#' @param verbose \code{TRUE} provides instand feedback
#' @param start_history start date of new ticker's price history
#'
#'
#' @return NULL. Saves directly to DB
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#' setup_new_ticker_in_db("AAPL UW", provide_tic_yh = "AAPL")
#' }
#'
setup_new_ticker_in_db <- function(ticker_bb, provide_tic_yh = NULL, provide_tic_fv = NULL, start_history = aikia::val_date(), verbose = FALSE){


  info_cat <- crayon::cyan $ bold
  warning_cat <- crayon::yellow $ bold
  stop_cat <- crayon::red $ bold
  success_cat <- crayon::green $ bold

  cat(info_cat("Ensure an existing bloomberg connection\n\n"))

  bbcon <- Rblpapi::blpConnect()


  tictoc::tic()
  # Get new ticker sectors and isin
  tic_isin <- Rblpapi::bdp(paste(ticker_bb," Equity"), c("ID_ISIN", "NAME", "COUNTRY", "INDUSTRY_GROUP","ISSUER_INDUSTRY",
                                                                                         "BICS_LEVEL_1_SECTOR_NAME","BICS_LEVEL_2_INDUSTRY_GROUP_NAME",
                                                                                         "BICS_LEVEL_3_INDUSTRY_NAME","BICS_LEVEL_4_SUB_INDUSTRY_NAME",
                                                                                         "BICS_LEVEL_5_SEGMENT_NAME","CRNCY")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(ticker_bb = ticker_bb) %>%
    dplyr::rename(bb_ccy = CRNCY,
                  isin = ID_ISIN) %>%
    dplyr::rename_all(tolower)

  Rblpapi::blpDisconnect(bbcon)

  cat(info_cat("retrieved all sector data for"),warning_cat(ticker_bb,"\n"),info_cat("Checking if ticker ISIN already exists in db!\n\n"))


  #Check if already exists in DB
  con <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

  # get old ticker meta data
  meta_data <- DBI::dbGetQuery(con, stringr::str_c("SELECT *
                                     FROM fin_ticker_meta_data
                                     WHERE isin = '",tic_isin$isin,"'
                                     OR ticker_bb = '",tic_isin$ticker_bb ,"'")) %>% dplyr::as_tibble()

  DBI::dbDisconnect(con)

  # Check if ticker and isin already exists in meta data table
  if(nrow(meta_data)>0){

    cat(stop_cat("\n\n",tic_isin$ticker_bb,"isin",tic_isin$isin,"is already available in meta data with ticker", meta_data$ticker_bb, "\n"))
    short_return <- meta_data %>% dplyr::select(isin, ticker_bb,ticker_yh, name)
    return(short_return)

  } else {

# 1.update ticker meta data -------------------------------------------------

    cat(info_cat("setting up",tic_isin$ticker_bb,"in meta data table\n"))
    # if no yahoo ticker is provided one is search via BB ticker's isin
#    if(is.null(provide_tic_yh) & is.null(provide_tic_fv)){
#      tic_isin <- tic_isin %>%
#        dplyr::mutate(ticker_yh = aikia::get_yh_ticker_by_isin(isin,verbose = F)$symbol,
#                      ticker_fv = stringr::word(ticker_bb,1))

#    } else {
      # if yahoo ticker is provided it is used directly
      tic_isin <- tic_isin %>%
        dplyr::mutate(ticker_yh = ifelse(!is.null(provide_tic_yh),as.character(provide_tic_yh), aikia::get_yh_ticker_by_isin(isin,verbose = F)$symbol),
                      ticker_fv = ifelse(!is.null(provide_tic_fv),as.character(provide_tic_fv), stringr::word(ticker_bb,1)))
 #   }

    cat(info_cat(" \ with yahoo ticker:",tic_isin$ticker_yh,"\n \ and finviz:",tic_isin$ticker_fv,"\nin meta data table\n\n"))

    # update new tickers to db
    con <- aikia::connect_to_db(user = "ceilert", password = "ceilert")
    DBI::dbWriteTable(con,
                      name= "fin_ticker_meta_data",
                      value = tic_isin,
                      row.names = FALSE,
                      header = TRUE,
                      append = TRUE,
                      pg.update.seq=TRUE)

    DBI::dbDisconnect(con)


# 2.Update fin_ticker_history for new ISINs ---------------------------------

    cat(info_cat("Updating ticker history starting",as.character(start_history),"and save in db\n"))

    new_hist <- aikia::get_bb_ticker_hist_data(stock = ticker_bb, start = start_history, change_ticker = TRUE)

    con <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    # append new history
    DBI::dbWriteTable(con,
                      name = "fin_ticker_history",
                      value = new_hist,
                      row.names = FALSE,
                      append = TRUE)

    DBI::dbDisconnect(con)


# 3.Update Sensis --------------------------------------------------------

    cat(info_cat("\nUpdating last 60 days of ticker's sensi history\n"))

    if(start_history == aikia::val_date()){backward=0
    } else {backward=60}

    # direct db update in function
    aikia::update_tic_sensis_history_in_db(ticker_list = tic_isin$ticker_yh, start_date = aikia::val_date(), verbose = verbose, backwards=backward)

    cat(success_cat(stringr::str_c("\n\nAll successfull...!\n")))
    tictoc::toc()
    }

}



