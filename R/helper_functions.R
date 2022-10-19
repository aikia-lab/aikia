


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





ticker_vola_fun <- function(ticker_tic, data, required_date = NULL, verbose = FALSE){

  if(is.null(required_date)|required_date >= lubridate::today()){
    stop(crayon::red("wrong date provided. Please check!\n"))
  }

  if(verbose){cat(paste0(crayon::blue("starting calculation of ticker ", ticker_tic,"\n")))}

  date_max <- data %>%
    dplyr::filter(ticker_yh == ticker_tic,
                  date == max(date)) %>%
    dplyr::pull(date)

  ts <- data %>%
    dplyr::filter(ticker_yh ==  ticker_tic) %>%
    dplyr::select(date, close) %>%
    timetk::tk_xts(close, date_var = date)

  # error handling if time line not long enough
  #  if(nrow(ts) > 84 & date_max == required_date){ # only ticker histories > 80d are valid otherwise 0 is stored (12 weekly figures for vola_w)
  if(nrow(ts)<20){
    stop(crayon::red("price history with "),crayon::yellow(nrow(ts)),crayon::red(" dates for "),crayon::yellow(ticker_tic),crayon::red(" not long enough! PLease check\n"))
  }
  if(date_max != required_date){
    stop(crayon::red("max available date for "),crayon::yellow(ticker_tic), crayon::red(" does not match required date! PLease check\n"))
  }

  d_return <- quantmod::periodReturn(ts, period = "daily") %>%
    na.omit()

  w_return <- quantmod::periodReturn(ts, period = "weekly") %>%
    na.omit()

  qtd_return <- quantmod::periodReturn(ts, period = "quarterly") %>%
    na.omit() %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))

  ytd_return <- quantmod::periodReturn(ts, period = "yearly") %>%
    na.omit() %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))


  vola_daily <- xts::xts(apply(d_return, 2, TTR::runSD, n = min(260, nrow(d_return))),
                         zoo::index(d_return)) * sqrt(365)

  vola_weekly <- xts::xts(apply(w_return, 2, TTR::runSD, n = min(52, nrow(w_return))),
                          zoo::index(w_return)) * sqrt(52)


  vola_result_daily <- vola_daily %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))

  vola_result_weekly <- vola_weekly %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))

  vola_lt_avg <- vola_daily %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(!is.na(daily.returns)) %>%
    dplyr::slice_max(date, n = 20) %>%
    dplyr::summarise(vola_avg_20 = mean(daily.returns))

  single_d <- d_return %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))

  single_w <- w_return %>%
    zoo::fortify.zoo() %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(date = Index) %>%
    dplyr::filter(date == max(date))


  idx_returns <- tibble::tibble(ticker_yh = ticker_tic,
                                date = required_date,
                                vola_d = vola_result_daily$daily.returns,
                                vola_w = vola_result_weekly$weekly.returns,
                                vola_avg_20 = vola_lt_avg$vola_avg_20,
                                dtd_return = single_d$daily.returns,
                                wtd_return = single_w$weekly.returns,
                                qtd_return = qtd_return$quarterly.returns,
                                ytd_return = ytd_return$yearly.returns)

  return(idx_returns)

}
