#' Add Stock Splits to DB table
#'
#' @param ticker search string like "AAPL" or "Henry Hub"
#' @param exact either search like or exact string
#' @param ratio split ratio (see details)
#' @param date date of stock split
#'
#' @details
#' The ratio is a old to new one. 1/3 means, for 1 old stock of 900$ you will get 3 new ones for 300$.
#'
#' The function aikiaTrade::adjust_ticker_history_stocksplit() needs to be run after the split has been added to
#' adjust the fin_ticker_history table accordingly.
#'
#' @importFrom magrittr %>%
#'
#' @return none
#' @export
#'
#' @examples \dontrun{
#'   add_ticker_stocksplit("AMZN", ratio = 1/20, date = '2022-06-06')
#' }
add_ticker_stocksplit <- function(ticker, exact = TRUE, ratio, date = lubridate::today()){

  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::red $ bold


  if(is.null(ticker)|is.null(ratio)){
    cat(error_logger("check input!\n"))
  }

  result <- find_in_db_tables(ticker,exact = exact)

  if(nrow(result)>1){
    cat(warning_logger("\nmore than 1 match to your query:\n"))
    result_test <- result %>% dplyr::select(.data$symbol,.data$name)
    print(result_test)

    answer <- readline("select rownumber to select ticker.\nType '0' for none\n")

    # no TS is returned
    if(as.numeric(answer) == 0){
      cat(warning_logger("No stocksplit inserted\n"))
      stop()
    } else {
      # Select TS
      result <- as.numeric(answer)
    }

  }

  new_split <- tibble::tibble(split_date = date,
                              ticker_yh = result$symbol,
                              ratio = ratio,
                              hist_adj = 0)

  con <- aikia::connect_to_db()
  DBI::dbWriteTable(con,
                    name= "fin_ticker_stocksplit",
                    value = new_split,
                    row.names = FALSE,
                    header = FALSE,
                    append = TRUE,
                    overwrite = FALSE)

  DBI::dbDisconnect(con)

  cat(script_logger("New stocksplit updated for"),result$symbol,"\n")

}
