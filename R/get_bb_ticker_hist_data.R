

#' Update ticker history from Bloomberg
#'
#' @param stock a bloomberg ticker
#' @param start a start date
#' @param end an end date
#' @param check_max_date_end \code{TRUE} checks the maximum available date in DB nd resets "END"
#' @param check_min_date_end \code{TRUE} checks the minimum available date in DB and resets "END"
#' @param check_min_date_start \code{TRUE} checks the minimum available date in DB and resets "START"
#' @param check_max_date_start \code{TRUE} checks the maximum available date in DB and resets "START"
#' @param change_ticker switch ticker from bloomberg to yahoo (needs to be in meta data)
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#' get_stock_hist_data_bb("AAPL UW", start = "2022-01-01", end = aikia::val_date())
#' }
get_bb_ticker_hist_data <- function(stock, start = NULL, end = aikia::val_date(),
                          check_max_date_end = FALSE,
                          check_min_date_end = FALSE,
                          check_min_date_start = FALSE,
                          check_max_date_start = FALSE,
                          change_ticker = FALSE){


  ticker <- NULL
  cat(crayon::blue(stringr::str_c("Start Ticker ",stock, "\n")))
  if(is.null(start)){ stop("no starting date provided")}

  if(check_min_date_start == TRUE){ # checks the minimum available date in DB and resets "START"

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    start_new <- DBI::dbGetQuery(mydb, stringr::str_c("SELECT
                                                     MIN(fin_ticker_history.date) AS min_date,
                                                     fin_ticker_history.ticker_yh,
                                                     fin_ticker_meta_data.ticker_bb
                                                  FROM fin_ticker_history
                                                  LEFT JOIN fin_ticker_meta_data
                                                    ON fin_ticker_history.ticker_yh = fin_ticker_meta_data.ticker_yh
                                                  WHERE ticker_bb = '",stock,"'")) %>%
      dplyr::pull(min_date) %>%
      lubridate::as_date(.)+1

    if(is.na(start_new)){
      cat(crayon::yellow("no min date found. keep initial 'start' one\n"))
    } else {
      start <- start_new
    }

    DBI::dbDisconnect(mydb)
  }

  if(check_max_date_start == TRUE){ # checks the maximum available date in DB and resets "START"

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    start_new <- DBI::dbGetQuery(mydb, stringr::str_c("SELECT
                                                     MAX(fin_ticker_history.date) AS max_date,
                                                     fin_ticker_history.ticker_yh,
                                                     fin_ticker_meta_data.ticker_bb
                                                  FROM fin_ticker_history
                                                  LEFT JOIN fin_ticker_meta_data
                                                    ON fin_ticker_history.ticker_yh = fin_ticker_meta_data.ticker_yh
                                                  WHERE ticker_bb = '",stock,"'")) %>%
      dplyr::pull(max_date) %>%
      lubridate::as_date(.) + 1

    if(is.na(start_new)){
      cat(crayon::yellow("no max date found in DB. keep initial 'start' one\n"))
    } else {
      start <- start_new
    }

    DBI::dbDisconnect(mydb)
  }

  if(check_max_date_end == TRUE){ # checks the maximum available date in DB nd resets "END"

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    end_new <- DBI::dbGetQuery(mydb, stringr::str_c("SELECT
                                                     MAX(fin_ticker_history.date) AS max_date,
                                                     fin_ticker_history.ticker_yh,
                                                     fin_ticker_meta_data.ticker_bb
                                                  FROM fin_ticker_history
                                                  LEFT JOIN fin_ticker_meta_data
                                                    ON fin_ticker_history.ticker_yh = fin_ticker_meta_data.ticker_yh
                                                  WHERE ticker_bb = '",stock,"'")) %>%
      dplyr::pull(max_date) %>%
      lubridate::as_date(.) - 1

    if(is.na(end_new)){
      cat(crayon::yellow("no max date found. keep initial 'end' one\n"))
    } else {
      end <- end_new
    }

    DBI::dbDisconnect(mydb)
  }

  if(check_min_date_end == TRUE){ # checks the minimum available date in DB nd resets "END"

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    end_new <- DBI::dbGetQuery(mydb, stringr::str_c("SELECT
                                                     MIN(fin_ticker_history.date) AS min_date,
                                                     fin_ticker_history.ticker_yh,
                                                     fin_ticker_meta_data.ticker_bb
                                                  FROM fin_ticker_history
                                                  LEFT JOIN fin_ticker_meta_data
                                                    ON fin_ticker_history.ticker_yh = fin_ticker_meta_data.ticker_yh
                                                  WHERE ticker_bb = '",stock,"'")) %>%
      dplyr::pull(min_date) %>%
      lubridate::as_date(.) - 1

    if(is.na(end_new)){
      cat(crayon::yellow("no min date found. keep initial 'end' one\n"))
    } else {
      end <- end_new
    }

    DBI::dbDisconnect(mydb)
  }

  cat(crayon::blue(stringr::str_c("Start Date is ",start ," and end Date is ", end,"\n")))


  bbcon <- Rblpapi::blpConnect()

  history <- Rblpapi::bdh(paste0(stock," Equity"),c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "PX_VOLUME"),
                          start.date = lubridate::as_date(start),
                          end.date = lubridate::as_date(end),
                          int.as.double=TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ticker_bb = stock,
                  adjusted = .data$PX_LAST) %>%
    dplyr::rename(open = .data$PX_OPEN,
                  high = .data$PX_HIGH,
                  low = .data$PX_LOW,
                  close = .data$PX_LAST,
                  volume = .data$PX_VOLUME)

  Rblpapi::blpDisconnect(bbcon)

  if(change_ticker == TRUE){

    mydb <- aikia::connect_to_db(user = "ceilert", password = "ceilert")

    ticker <- DBI::dbGetQuery(mydb, stringr::str_c("SELECT ticker_bb, ticker_yh
                                                    FROM fin_ticker_meta_data
                                                    WHERE ticker_bb = '",stock,"'")) %>%
      dplyr::pull(ticker_yh)

    DBI::dbDisconnect(mydb)

    if(length(ticker)==0|is.na(ticker)){
      cat(crayon::red(paste0("no ticker match for ", stock,"\n")))
      return() # return empty data frame
    } else {
      cat(crayon::blue("yahoo ticker matched to:",ticker,"\n"))
    }

    history <- history %>% dplyr::mutate(ticker_yh = ticker) %>%
      dplyr::select(-ticker_bb)

  }

  return(history)
}

