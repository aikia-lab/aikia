

# Liste an allen modules
# https://github.com/pilwon/node-yahoo-finance/issues/52

get_yh_financials_hf <- function (symbol = NULL, period = 'annual', as_pivot_long = FALSE, verbose = FALSE){

  script_logger <- crayon::bold $ bold
  error_logger <- crayon::magenta $ bold

  if(verbose == TRUE){
    cat(script_logger("retrieving data for ",symbol,"\n"))
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
    cat(error_logger(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  flat <- tryCatch(lapply(res, function(x) {
      x[[1]][[1]] %>% dplyr::select(-.data$maxAge)
    }), error=function(e) e)

  if(inherits(flat, 'error')){
    cat(error_logger(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  df <- suppressMessages(Reduce(dplyr::full_join, flat)) %>% jsonlite::flatten() %>%
    dplyr::mutate(endDate.raw = lubridate::as_datetime(.data$endDate.raw)) %>%
    dplyr::select(dplyr::contains(".raw"))

  colnames(df) <- gsub(".raw", "", colnames(df))

  if ("capitalExpenditures" %in% names(df)) {
    df <- df %>% dplyr::mutate(freeCashflow = .data$totalCashFromOperatingActivities +
                                  .data$capitalExpenditures)
  } else if("totalCashFromOperatingActivities" %in% names(df)){
    df <- df %>% dplyr::mutate(freeCashflow = .data$totalCashFromOperatingActivities)
  } else {
    df <- df %>% dplyr::mutate(freeCashflow = NA)
  }


  if(as_pivot_long == TRUE){
    df <- df %>%
      dplyr::arrange(desc(endDate)) %>%
      tidyr::pivot_longer(-1,names_to = "position", values_to = "values") %>%
      tidyr::pivot_wider(names_from = endDate,values_from = values) %>%
      dplyr::mutate(symbol = symbol) %>%
      dplyr::relocate(symbol,.before = "position")
  } else {
    df <- df %>% dplyr::mutate(symbol = symbol) %>%
      dplyr::relocate(symbol,.before = "endDate")
  }

  return(dplyr::as_tibble(df))
}



get_yh_estimates_hf <- function (symbol = NULL, as_pivot_long = FALSE, verbose = FALSE){

  script_logger <- crayon::bold $ bold
  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::magenta $ bold

  if(verbose){
    cat(script_logger("retrieving data for ",symbol,"\n"))
  }


  # compose the request
  url <- glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules=earningsTrend&ssl=true")

  res <- tryCatch(jsonlite::fromJSON(url)$quoteSummary$result,
                  error=function(e) e)

  if(inherits(res, 'error')){
    cat(error_logger(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  flat <- tryCatch(lapply(res, function(x) {
    x[[1]][[1]] %>% dplyr::select(-.data$maxAge)
  }), error=function(e) e)

  if(inherits(flat, 'error')){
    cat(error_logger(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  df <- tryCatch(suppressMessages(Reduce(dplyr::full_join, flat)) %>% jsonlite::flatten() %>%
    dplyr::mutate(endDate = lubridate::as_datetime(.data$endDate)) %>%
    dplyr::select(growth_period = period,endDate,dplyr::contains(".raw")) %>%
    dplyr::relocate('growth.raw',.after = 'growth_period') %>%
    janitor::clean_names(), error=function(e) e)

  if(inherits(df, 'error')){
    cat(error_logger(paste0("no financial data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  df <- cbind(df,est_period = c("actual_qtr","next_qtr","actual_year","next_year",NA,NA)) %>%
    dplyr::relocate('est_period',.after = 'end_date')
  colnames(df) <- gsub("_raw", "", colnames(df))


  if(as_pivot_long){

    if(verbose){
      cat(warning_logger("Pivot long requires to exclude >growth< estimation!\n"))
    }

    df <- df %>%
      dplyr::arrange(desc(end_date)) %>%
      dplyr::select(-growth_period,-growth,-end_date) %>%
      tidyr::drop_na(est_period) %>%
      tidyr::pivot_longer(-est_period,names_to = "position", values_to = "values") %>%
      tidyr::pivot_wider(names_from = est_period,values_from = values) %>%
      dplyr::mutate(ticker_yh = symbol) %>%
      dplyr::relocate(ticker_yh,.before = "position")
  } else {
    df <- df %>% dplyr::mutate(ticker_yh = symbol) %>%
      dplyr::relocate(ticker_yh,.before = "end_date")
  }

  return(dplyr::as_tibble(df))
}



ticker_vola_fun <- function(ticker_tic, data, required_date = NULL, verbose = FALSE){

  success_cat <- crayon::bold $ green
  info_cat <- crayon::cyan $ bold
  warning_cat <- crayon::yellow $ bold
  red_cat <- crayon::red $ bold

  if(is.null(required_date)|required_date >= lubridate::today()){
    stop(red_cat("wrong date provided. Please check!\n"))
  }

  if(verbose){cat(info_cat("starting calculation of ticker ", ticker_tic,"\n"))}

  date_max <- data %>%
    dplyr::filter(ticker_yh == ticker_tic,
                  date == max(date)) %>%
    dplyr::pull(date)


  # error handling if max(date return empty data frame)
  if(length(date_max)==0){
    if(verbose){cat(red_cat("No max date. Empty return for ticker ", ticker_tic,"\n"))}
    idx_returns <- tibble::tibble(ticker_yh = ticker_tic,
                                  date = required_date,
                                  vola_d = 0,
                                  vola_w = 0,
                                  vola_avg_20 = 0,
                                  dtd_return = 0,
                                  wtd_return = 0,
                                  qtd_return = 0,
                                  ytd_return = 0,
                                  retrieval_time = lubridate::now())


    return(idx_returns)
  }

  ts <- data %>%
    dplyr::filter(ticker_yh ==  ticker_tic) %>%
    dplyr::select(date, close) %>%
    timetk::tk_xts(close, date_var = date)

  # error handling if time line not long enough
  #  if(nrow(ts) > 60 & date_max == required_date){ # only ticker histories > 80d are valid otherwise 0 is stored (12 weekly figures for vola_w)
  if(nrow(ts)<60){
    cat(red_cat("price history with "),warning_cat(nrow(ts)),red_cat(" dates for "),warning_cat(ticker_tic),red_cat(" not long enough! PLease check\n"))

    idx_returns <- tibble::tibble(ticker_yh = ticker_tic,
                                  date = required_date,
                                  vola_d = 0,
                                  vola_w = 0,
                                  vola_avg_20 = 0,
                                  dtd_return = 0,
                                  wtd_return = 0,
                                  qtd_return = 0,
                                  ytd_return = 0,
                                  retrieval_time = lubridate::now())


    return(idx_returns)

  }
  if(date_max != required_date){
    cat(red_cat("max available date for "),warning_cat(ticker_tic), red_cat(" does not match required date! PLease check\n"))
    idx_returns <- tibble::tibble(ticker_yh = ticker_tic,
                                  date = required_date,
                                  vola_d = 0,
                                  vola_w = 0,
                                  vola_avg_20 = 0,
                                  dtd_return = 0,
                                  wtd_return = 0,
                                  qtd_return = 0,
                                  ytd_return = 0,
                                  retrieval_time = lubridate::now())


    return(idx_returns)
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
                                ytd_return = ytd_return$yearly.returns,
                                retrieval_time = lubridate::now())

  return(idx_returns)

}
