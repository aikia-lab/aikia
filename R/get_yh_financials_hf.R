


get_yh_financials_hf <- function (symbol = NULL, period = 'annual', verbose = FALSE){

  if(verbose == TRUE){
    cat(crayon::blue("retrieving data for ",symbol,"\n"))
  }

  if(period == "annual"){
    timespan <- "incomeStatementHistory,balanceSheetHistory,cashflowStatementHistory"
  } else {
    timespan <- "incomeStatementHistoryQuarterly,balanceSheetHistoryQuarterly,cashflowStatementHistoryQuarterly"
  }
  # compose the request
  url <- glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules={timespan}")

  res <- tryCatch(jsonlite::fromJSON(url)$quoteSummary$result,
             error=function(e) e)

  if(inherits(res, 'error')){
    stop(crayon::red(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
  }

  flat <- lapply(res, function(x) {
    x[[1]][[1]] %>% dplyr::select(-.data$maxAge)
  })

  df <- suppressMessages(Reduce(dplyr::full_join, flat)) %>% jsonlite::flatten() %>%
    dplyr::select(-dplyr::contains(".fmt"), -dplyr::contains(".longFmt")) %>%
    dplyr::mutate(endDate.raw = lubridate::as_date(.data$endDate.raw))

  colnames(df) <- gsub(".raw", "", colnames(df))
  if ("capitalExpenditures" %in% names(df)) {
    df <- df %>% dplyr::mutate(freeCashflow = .data$totalCashFromOperatingActivities +
                                 .data$capitalExpenditures)
  }
  else {
    df <- df %>% dplyr::mutate(freeCashflow = .data$totalCashFromOperatingActivities)
  }
  df$symbol <- symbol
  return(dplyr::as_tibble(df))
}
