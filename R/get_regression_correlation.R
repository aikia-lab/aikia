

#' Calculate Regression or Plot Correlation
#'
#' @param symbols provide {ticker} or {eco} symbols
#' @param start_date default value today -180
#' @param end_date default vlaue today -1
#' @param prediction the symbol to predict the regression for
#' @param plot_correlation no regression but plot correlation
#' @param incl_cor_histo include histogram in correlation plot
#' @param ticker_vs_eco offset returns to see lagging effects
#' @param verbose retrieve instant feedback
#'
#' @return a Plot
#' @export
#'
#' @examples \dontrun{
#'  get_regression_correlation(symbols = c("AAPL","ASML"))
#'}
get_regression_correlation <- function(symbols = c("AAPL","DCOILWTICO","T10Y2Y","VIXCLS"),
                                       start_date = lubridate::today() - 180,
                                       end_date = lubridate::today() - 1,
                                       prediction = "AAPL",
                                       plot_correlation = FALSE,
                                       incl_cor_histo = TRUE,
                                       ticker_vs_eco = 0,
                                       verbose = TRUE){


  script_logger <- crayon::cyan $ bold
  script_error <- crayon::red $ bold


  if(is.null(symbols)){
    cat(script_error("no symbols provided!\n"))
    stop()
  }


  con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")

  all_sorted <- tibble::tibble()
  for(i in symbols){
    tab_group <- DBI::dbGetQuery(con, stringr::str_c("SELECT CASE
      WHEN EXISTS(SELECT 1 FROM fin_ticker_meta_data WHERE ticker_yh = '",i,"') THEN 1
      WHEN EXISTS(SELECT 2 FROM eco_ts_meta_data WHERE series = '",i,"') THEN 2
      ELSE NULL
      END AS result;"))

    all_sorted <- rbind(all_sorted,
                        tibble::tibble(symbol = i,
                                       type = tab_group$result))

  }

  if(any(is.na(all_sorted$type))){
    cat(script_warning("unmatched:\n"))
    dplyr::filter(all_sorted, is.na(type))
  }


  tics <- all_sorted %>% dplyr::filter(type == 1) %>%
    dplyr::pull(symbol) %>%
    stringr::str_c(collapse = "','")


  ecos <- all_sorted %>% dplyr::filter(type == 2) %>%
    dplyr::pull(symbol) %>%
    stringr::str_c(collapse = "','")


  if(verbose){
    cat(script_logger("retrieving"),nrow(all_sorted[all_sorted$type == 1,]), "ticker", script_logger("history\n"))
  }
  ticker_hist <- DBI::dbGetQuery(con, stringr::str_c("SELECT date,
                                                     ticker_yh,
                                                     high,
                                                     low,
                                                     adjusted as price,
                                                     volume
      FROM fin_ticker_history
      WHERE ticker_yh IN ('",tics,"')
      AND date >= '",start_date,"'
      AND date <= '",end_date,"'"))


  if(verbose){
    cat(script_logger("retrieving"),nrow(all_sorted[all_sorted$type == 2,]), "eco", script_logger("history\n"))
  }
  eco_hist <- DBI::dbGetQuery(con, stringr::str_c("SELECT *
      FROM eco_fred_raw_data
      WHERE symbol IN ('",ecos,"')
      AND date >= '",start_date,"'
      AND date <= '",end_date,"'"))

  DBI::dbDisconnect(con)


  eco_perf <- eco_hist %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(return = price/dplyr::lead(price)-1) %>%
    dplyr::ungroup() %>%
    dplyr::select(date,symbol,return) %>%
    dplyr::filter(date <= max(date)-ticker_vs_eco) %>% # cut off laging NA days before union call
    tidyr::pivot_wider(names_from = "symbol",values_from = "return")


  tics_perf <- ticker_hist %>%
    dplyr::group_by(ticker_yh) %>%
    dplyr::mutate(return = price/dplyr::lead(price)-1,
                  return = dplyr::lead(return,ticker_vs_eco)) %>% # lead as tic's today's reaction should be measured with eco's yesterday move
    dplyr::ungroup() %>%
    dplyr::select(date,ticker_yh,return) %>%
    dplyr::filter(date <= max(date)-ticker_vs_eco) %>% # cut off laging NA days before union call
    tidyr::pivot_wider(names_from = "ticker_yh",values_from = "return")


  if(verbose){
    cat(script_logger("creating 'xts' time series\n"))
  }
  all_ret <- tics_perf %>%
    dplyr::left_join(eco_perf, by = 'date') %>%
    tidyr::drop_na() %>%
    xts::as.xts()

  # instead to loose eco dates (trouble with monthly data)
  # all dates are kept
  # INSTEAD OF FILL, SPLINE OR LINEARE FILL WOULD BE BETTER PROXY
  #all_ret <- dplyr::union(tics_perf,eco_perf) %>%
  #  tidyr::fill(return, .direction = "updown")



  if(plot_correlation==FALSE){
    if(verbose){
      cat(script_logger("calculating linear regression\n"))
    }
    variable_ <- all_ret[,-which(names(all_ret) %in% c(as.character(prediction)))]
    regr <- lm(all_ret[,as.character(prediction)] ~ variable_)
    print(summary(regr))
    plot(regr)
  } else {
    if(verbose){
      cat(script_logger("plot corrleation\n"))
    }
    PerformanceAnalytics::chart.Correlation(all_ret,
                                            histogram=incl_cor_histo,
                                            pch="+")
  }

}



























