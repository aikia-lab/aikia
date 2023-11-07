

# Liste an allen modules
# https://github.com/pilwon/node-yahoo-finance/issues/52
# Auflistung
# https://rdrr.io/github/n0Trader/TDI/man/yahooAPI.html

get_crumb <- function(){
  # Unbale to obtain yahoo crumb. If this is being called from a GDPR country, Yahoo requires GDPR consent, which cannot be scripted
  ses <- list()
  ses$h <- curl::new_handle()
  # yahoo finance doesn't seem to set cookies without these headers
  # and the cookies are needed to get the crumb
  curl::handle_setheaders(ses$h,
                          accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
                          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36 Edg/115.0.1901.183")
  URL <- "https://finance.yahoo.com/"
  r <- curl::curl_fetch_memory(URL, handle = ses$h)
  # yahoo redirects to a consent form w/ a single cookie for GDPR:
  # detecting the redirect seems very brittle as its sensitive to the trailing "/"
  ses$can.crumb <- ((r$status_code == 200) && (URL == r$url) && (NROW(curl::handle_cookies(ses$h)) > 1))

  if (ses$can.crumb) {
    # get a crumb so that downstream callers don't have to handle invalid sessions.
    # this is a network hop, but very lightweight payload
    n <- if (unclass(Sys.time()) %% 1L >= 0.5) 1L else 2L

    query.srv <- paste0("https://query", n, ".finance.yahoo.com/v1/test/getcrumb")
    r <- curl::curl_fetch_memory(query.srv, handle = ses$h)

    if ((r$status_code == 200) && (length(r$content) > 0)) {
      ses$crumb <- rawToChar(r$content)

      # assign the crumb to the global environment
      assign("ses",ses,envir = .GlobalEnv)
  #    list2env(ses, envir = .GlobalEnv) # probably need to assign a list ???

    }
  }
}


get_yh_financials_hf <- function (symbol = NULL, period = 'annual', as_pivot_long = FALSE, verbose = FALSE){

  script_logger <- crayon::bold $ bold
  error_logger <- crayon::magenta $ bold


  if(verbose){
    cat(script_logger("retrieving data for ",symbol,"\n"))
  }

  if(period == "annual"){
    timespan_vec <- c("incomeStatementHistory","balanceSheetHistory","cashflowStatementHistory")
  } else {
    timespan_vec <- c("incomeStatementHistoryQuarterly","balanceSheetHistoryQuarterly","cashflowStatementHistoryQuarterly")
  }

  df_list_raw <- purrr::map(timespan_vec,get_yh_single_financials_hf,symbol=symbol,verbose=verbose)

  # retun 0 if all lists are empty
  if(all(!lengths(df_list_raw))){
    cat(error_logger("all financials are empty for"),symbol,"\n")
    return()
  }

  df <- merge_list_of_df_w_same_columns_hf(df_list_raw)

  if(as_pivot_long){
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


get_yh_single_financials_hf <- function(timespan,symbol,verbose){


  # cookie needed first as yahoo can only be fetched outside GDPR countries
  if(!exists("ses")){
    if(verbose){
      cat(script_logger("setup new crumb\n"))
    }
    get_crumb()
  }

  # compose the request
  url <- glue::glue('https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules={timespan}&ssl=true&crumb={ses$crumb}')


  if(verbose){
    cat(script_logger("used url call:\n"))
    url
    cat("\n")
  }

    res <-suppressWarnings(tryCatch(jsonlite::fromJSON(curl::curl(url, handle = ses$h))$quoteSummary$result,
                    error=function(e) e)
    )

    if(inherits(res, 'error')||ncol(res)==0){
      if(verbose){
        cat(as.character(res))
      }
      if(inherits(res,"handle is dead")){
        rm(ses)
        cat(script_logger("previous crumb deleted\n"))
      }
      return()
    }


    flat <- tryCatch(lapply(res, function(x) {
      x[[1]][[1]] %>% dplyr::select(-.data$maxAge)
    }), error=function(e) e)

    if(inherits(flat, 'error')){
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

    return(df)

}




get_yh_estimates_hf <- function (symbol = NULL, as_pivot_long = FALSE, verbose = FALSE){

  script_logger <- crayon::bold $ bold
  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::magenta $ bold

  if(verbose){
    cat(script_logger("retrieving data for ",symbol,"\n"))
  }

  # cookie needed first as yahoo can only be fetched outside GDPR countries
  if(!exists("ses")){
    if(verbose){
      cat(script_logger("setup new crumb\n"))
    }
    get_crumb()
  }

  # compose the request
  url <- glue::glue('https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules=earningsTrend&ssl=true&crumb={ses$crumb}')

  res <- suppressWarnings(tryCatch(jsonlite::fromJSON(curl::curl(url, handle = ses$h))$quoteSummary$result,
                                   error=function(e) e))

  if(inherits(res, 'error')||ncol(res)==0){
    if(verbose){
      cat(as.character(res))
    }
    if(inherits(res,"handle is dead")){
      rm(ses)
      cat(script_logger("previous crumb deleted\n"))
    }
    cat(error_logger(paste0("no estimate data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }


  flat <- tryCatch(lapply(res, function(x) {
    x[[1]][[1]] %>% dplyr::select(-.data$maxAge)
  }), error=function(e) e)

  if(inherits(flat, 'error')){
    cat(error_logger(paste0("no estimate data avialble for '", symbol,"'! Please check symbol\n")))
    return()
  }

  df <- tryCatch(suppressMessages(Reduce(dplyr::full_join, flat)) %>% jsonlite::flatten() %>%
                   dplyr::mutate(endDate = lubridate::as_datetime(.data$endDate)) %>%
                   dplyr::select(growth_period = period,endDate,dplyr::contains(".raw")) %>%
                   dplyr::relocate('growth.raw',.after = 'growth_period') %>%
                   janitor::clean_names(), error=function(e) e)

  if(inherits(df, 'error')){
    cat(error_logger(paste0("no estimate data avialble for '", symbol,"'! Please check symbol\n")))
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



get_yh_hist_hf <- function(symbol = NULL, verbose = FALSE){

  script_logger <- crayon::bold $ bold
  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::magenta $ bold

  if(verbose){
    cat(script_logger("retrieving data for ",symbol,"\n"))
  }


  tic_hist <- glue::glue("https://finance.yahoo.com/quote/{symbol}/history?p={symbol}") %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    .[[1]] %>%
    janitor::clean_names() %>%
    dplyr::filter(!stringr::str_detect(close,"Dividend|splits")) %>%
    dplyr::mutate(ticker_yh = symbol) %>%
    dplyr::as_tibble()


  return(tic_hist)

}




# helper function for ticker_pe_px_indication function
fill_column <- function(df,gtobj, columns){

  for(column in columns){

    if(as.numeric(df[7,column]) < as.numeric(df[8,column])){
      gtobj <- gtobj %>%
        gt::tab_style(style = gt::cell_fill(color = aikia::aikia_palette_eight()[5],alpha = 0.5),
                      locations = gt::cells_body(columns = column, rows = c(8:9)))
    } else {
      gtobj <- gtobj %>%
        gt::tab_style(style = gt::cell_fill(color = aikia::aikia_palette_eight()[8],alpha = 0.6),
                      locations = gt::cells_body(columns = column, rows = c(8:9)))
    }

    if(as.numeric(df[7,column]) < as.numeric(df[10,column])){
      gtobj <- gtobj %>%
        gt::tab_style(style = gt::cell_fill(color = aikia::aikia_palette_eight()[5],alpha = 0.6),
                      locations = gt::cells_body(columns = column, rows = c(10:11)))
    } else {
      gtobj <- gtobj %>%
        gt::tab_style(style = gt::cell_fill(color = aikia::aikia_palette_eight()[8],alpha = 0.5),
                      locations = gt::cells_body(columns = column, rows = c(10:11)))
    }

  }
  return(gtobj)
}



# helper function to merge columns with same name by fewer NAs
merge_list_of_df_w_same_columns_hf <- function(df_list) {
  # Get the unique column names of list
  unique_cols <- unique(unlist(lapply(df_list, names)))

  # Initialize an empty list to store selected columns
  selected_cols <- list()

  for (col_name in unique_cols) {

    # Extract columns with the same name from all data frames
    col_data <- lapply(df_list, function(df) df[[col_name]])

    # Find the index of the column with fewer NAs
    min_na_index <- which.max(sapply(col_data, function(col) sum(!is.na(col))))

    # Add the selected column to the list
    selected_cols[[col_name]] <- col_data[[min_na_index]]

  }

  # Combine the selected columns into a data frame
  result_df <- as.data.frame(selected_cols)

  return(result_df)
}





