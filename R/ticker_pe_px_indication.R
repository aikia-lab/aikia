

#' A simple Ffuture Price Indication using the estimated eps and PE Ratio
#'
#' @param stock A yahoo ticker symbol
#' @param set_pe Optionally set an own PE ratio
#' @param as_gt if \code{FALSE} returns a tibble otherwise a gt table
#'
#' @return a tibble or gt table
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' ticker_pe_px_indication("AAPL")
#' }
#'
ticker_pe_px_indication <- function(stock = NULL, set_pe = NULL, as_gt = TRUE){


  script_logger <- crayon::bold $ cyan


  con <- aikia::connect_to_db()

  vals <- DBI::dbGetQuery(con,stringr::str_c(
    "SELECT
                      CAST(retrieval_time as date) as date,
                      ticker_yh,
                      regular_market_price as price,
                      trailing_pe,
                      trailing_eps,
                      forward_eps,
                      forward_pe,
                      price_to_book,
                      book_value,
                      float_shares
                  FROM fin_ticker_market_snapshot
                  WHERE ticker_yh = '",stock,"'
                  ORDER BY retrieval_time DESC
                  LIMIT 1")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(impl_px_eps_pe = forward_eps * forward_pe,
                  impl_px_pbv = book_value * price_to_book)


  histo <- DBI::dbGetQuery(con,stringr::str_c(
    "SELECT
                  ticker_yh,
                  LONG_TERM_PRICE_EARNINGS_RATIO as pe_lt,
                  FIVE_YR_AVG_PRICE_EARNINGS as pe_5y
                  FROM fin_ticker_statics_valuations
                  WHERE ticker_yh = '",stock,"'"))

  DBI::dbDisconnect(con)

  # not working since GDPR is in place
  #  est <- aikia::get_yh_estimates(symbols = stock,as_pivot_long = T)

  con <- aikia::connect_to_db(database = "tony_data")
  est <- DBI::dbGetQuery(con,stringr::str_c("SELECT *
                                     FROM yahoo_financials_estimates
                                     WHERE symbol = '",stock,"'
                                     AND pull_date = (SELECT max(pull_date)
                                                      FROM yahoo_financials_estimates
                                                      WHERE symbol = '",stock,"')"))
  DBI::dbDisconnect(con)

  if(nrow(est)==0){
    cat(warning("no estimation available\n"))
    return(est)
  }

  est <- est %>%
    dplyr::arrange(desc(end_date)) %>%
    dplyr::select(-growth_period,-growth,-end_date) %>%
    tidyr::drop_na(est_period) %>%
    dplyr::mutate(dplyr::across(.col = dplyr::everything(),.fns = as.character)) %>%
    tidyr::pivot_longer(-c(symbol,est_period),names_to = "position", values_to = "values") %>%
    tidyr::pivot_wider(names_from = est_period,values_from = values) %>%
    dplyr::rename(ticker_yh = symbol) %>%
    dplyr::relocate(ticker_yh,.before = "position")

  if(!is.null(set_pe)){
    cat(script_logger("PE ratio has been manually set to"), set_pe,"\n")
  }

  est <-
    est %>% dplyr::filter(stringr::str_detect(position,"eps_trend_current|eps_trend_30days_ago|ps_trend_60days_ago|eps_trend_90days_ago")) %>%
    dplyr::select(ticker_yh,position,actual_year,next_year) %>%
    dplyr::left_join(vals[,c("price","ticker_yh","trailing_pe","forward_pe")], by = "ticker_yh") %>%
    dplyr::left_join(histo, by = "ticker_yh") %>%
    dplyr::rename(eps_act_yr = actual_year,
                  eps_nxt_yr =next_year) %>%
    dplyr::mutate(dplyr::across(.col = c("eps_act_yr","eps_nxt_yr"),.fns = as.numeric)) %>%
    dplyr::mutate(indic_px_act_yr = eps_act_yr * ifelse(is.null(set_pe),forward_pe,set_pe),
                  indic_px_nxt_yr = eps_nxt_yr * ifelse(is.null(set_pe),forward_pe,set_pe),
                  price_dif = indic_px_act_yr / price -1,
                  price_dif2 = indic_px_nxt_yr / price -1) %>%
    dplyr::select(ticker_yh,position,
                  `PE long term` = pe_lt,
                  `PE last 5 Yr` = pe_5y,
                  `Trailing PE` = trailing_pe,
                  `Forward PE` = forward_pe,
                  `Estimation eps (act yr)` = eps_act_yr,
                  `Estimation eps (nxt yr)` = eps_nxt_yr,
                  `Last Price` = price,
                  `Price Indication (act yr)` = indic_px_act_yr,
                  `Price Difference (act yr)` = price_dif,
                  `Price Indication (nxt yr)` = indic_px_nxt_yr,
                  `Price Difference (nxt yr)` = price_dif2) %>%
    tidyr::pivot_longer(c(-ticker_yh,-position),names_to = 'name',values_to = 'values') %>%
    tidyr::pivot_wider(id_cols = dplyr::everything(),names_from = "position",values_from = c("values"))


  if(as_gt == FALSE){
    return(est)
  }

  # Create base GT table
  gt_est <- est %>%
    dplyr::select(-ticker_yh) %>%
    gt::gt() %>%
    gt::tab_header(title = gt::md(paste0("<font style='color:red;font-size:30px'> ",stock,"</font>"))) %>%
    gt::tab_spanner(label = "Estimation",
                    columns = c(
                      eps_trend_current,eps_trend_30days_ago,eps_trend_60days_ago,eps_trend_90days_ago)) %>%
    gt::cols_label(
      eps_trend_current = "Current",
      eps_trend_30days_ago = "30 Days ago",
      eps_trend_60days_ago = "60 Days ago",
      eps_trend_90days_ago = "90 Days ago") %>%
    gt::cols_align(align = "center") %>%
    gt::fmt_number(columns = is.numeric,decimals = 2) %>%
    gt::fmt_percent(columns = is.numeric,
                    rows =  c(9,11),
                    decimals = 2) %>%
    gt::tab_style(style = gt::cell_text(size = gt::px(9)),
                  locations = gt::cells_body(columns = dplyr::everything(), rows = c(9,11))) %>%
    aikia::gt_theme_aikia() %>%
    gt::tab_footnote(footnote = gt::md(stringr::str_c("Market Data retrieval as of ",vals$date)),
                     placement = "left")

  gt_est <- fill_column(est,gt_est,c("eps_trend_current","eps_trend_30days_ago","eps_trend_60days_ago","eps_trend_90days_ago"))

  # add footnote
  if(!is.null(set_pe)){
    gt_est <- gt_est %>%
      gt::tab_footnote(footnote = gt::md(stringr::str_c("<b>PE ratio manually set to '",set_pe,"' !</b>")),
                       placement = "left")
  } else {
    gt_est <- gt_est %>%
      gt::tab_footnote(footnote = gt::md(stringr::str_c("'Forward PE' is used to calculate Price Indication")),
                       placement = "left")
  }


  return(gt_est)



}
