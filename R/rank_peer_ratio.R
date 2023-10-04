
#' Rank Peers of same Sectors by different Ratios
#'
#' @param industry_level_prio select an industry level
#' @param sector_prio select a sector within the industry
#' @param industry_level_sec optional select a 2nd industry level
#' @param sector_sec optional select a sector within the 2nd industry
#' @param country optional select a vector of countries
#'
#' @return tibble of multiple financial ratios
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#'  rank_peer_ratio(industry_level_prio = "bics_level_2_industry_group_name", sector_prio = "Software & Tech Services")
#'}
rank_peer_ratio <- function(industry_level_prio = "industry_group", sector_prio = "software",
                            industry_level_sec = NULL, sector_sec = NULL,
                            country = NULL){

  error_logger <- crayon::red $ bold
  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  success_logger <- crayon::green $ bold

  # define settings ---------------------------------------------------------
  if(!industry_level_prio %in% c("industry_group",
                                 "issuer_industry",                  "bics_level_1_sector_name",
                                 "bics_level_2_industry_group_name", "bics_level_3_industry_name",
                                 "bics_level_4_sub_industry_name",   "bics_level_5_segment_name")){
    cat(error_logger("Industry Level not found! Please check!\n"))
    return()
  }



  needed_ratios <- tibble::tribble(
    ~ratios, ~pref_direction,~groups, ~comment,
    'trailingPE', 'lower','Valuations', "last 12 month Price/Earnings Ratio",
    'forwardPE',  'lower','Valuations',  "expected next 12 month Price/Earnings Ratio",
    'pegRatio',   'lower','Ratios', "Price/Earnings to Growth Ratio (Relates the PE ratio of a company to its expected earnings growth.
                                              Particularly in the case of growth stocks, whose PE ratio tends to be higher and can therefore be
                                               perceived as overvalued, the PEG ratio offers the possibility of achieving more meaningful results.",
    'shortRatio', 'lower','Ratios',    "proportion of securities sold short divided by the average daily trading volume. The ratio can be used,
                                                for example, as a sentiment indicator.",
    'quickRatio', 'higher','Ratios',   "ratio of the company's available financial assets plus shares and current receivables to current liabilities.",
    'currentRatio', 'higher','Ratios', "The current ratio is a liquidity ratio that measures whether a firm has enough resources to meet its short-term obligations.
                                                It compares a firm's current assets to its current liabilities",
    'earningsGrowth', 'higher','Growth', "Earnings growth rate is a key value that is needed when the Discounted cash flow model.",
    'ebitdaMargins', 'higher','Margins', "The EBITDA margin is calculated by dividing EBITDA by revenue.",
    'grossMargins', 'higher', 'Margins',"Gross margin measures a company's gross profit compared to its revenues as a percentage.",
    'operatingMargins', 'higher','Margins', "The operating margin represents how efficiently a company is able to generate profit through its core operations.",
    'profitMargins', 'higher','Margins', "Expressed as a percentage, profit margin indicates how many cents of profit has been generated for each dollar of sale.",
    'revenueGrowth', 'higher','Growth', "revenue growth is an increase in a company's sales in one year to the previous year. For an accurate picture of growth, investors should look at the growth of several quarters and how consistent it is.",
    'returnOnAssets', 'higher','Valuations', "indicates how profitable a company is in relation to its total assets by dividing its net income by its total assets.")



  # get data ----------------------------------------------------------------


  con <- aikia::connect_to_db()

  if(is.null(country)){
    region <- ""
  } else {
    region <- stringr::str_c("AND country IN ('",stringr::str_c(country,collapse = "','"),"')")
  }


  if(is.null(industry_level_sec)){
    sec_filter <- ""
  } else {
    sec_filter <- stringr::str_c("AND ",industry_level_sec," = '",sector_sec,"'")
  }

  peers <- DBI::dbGetQuery(con, stringr::str_c("SELECT ticker_yh, name
                                    FROM fin_ticker_meta_data
                                    WHERE ",industry_level_prio," = '",sector_prio,"'",
                                    sec_filter,
                                    region)) %>%
    tidyr::drop_na(.data$ticker_yh)

  peer_sql <- peers %>%
    dplyr::pull(.data$ticker_yh) %>%
    stringr::str_c(.,collapse = "','")

  needed_ratios_sql <- stringr::str_c(needed_ratios$ratios,collapse = ",")

  tic_ratios <- DBI::dbGetQuery(con, stringr::str_c("SELECT *
                            FROM (SELECT
                            		CAST(regularMarketTime AS date) as date,
                                    ticker_yh,
                                    longName AS name,",
                            needed_ratios_sql,",
                                    ROW_NUMBER() OVER (PARTITION BY ticker_yh ORDER BY regularMarketTime DESC) AS row_num
                                  FROM fin_ticker_market_snapshot) as t
                            WHERE t.row_num = 1
                            AND ticker_yh IN ('",peer_sql,"')
                            AND date >= (SELECT DATE_SUB(CAST(MAX(regularMarketTime) as DATE), INTERVAL 1 DAY) as max_date
                                           FROM fin_ticker_market_snapshot)")) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-.data$row_num)



  # get last 2 quarter rsults from peers
  tic_qtr_results_raw <- DBI::dbGetQuery(con, stringr::str_c("SELECT *
                                      FROM (
                                        SELECT
                                            endDate as date,
                                            ticker_yh,
                                            totalRevenue,
                                            accountsPayable,
                                            capitalExpenditures,
                                            researchDevelopment,
                                            ROW_NUMBER() OVER (PARTITION BY ticker_yh ORDER BY endDate DESC) AS row_num
                                        FROM fin_ticker_qtr_results_yh
                                      ) AS t
                                      WHERE t.row_num <= 2
                                      AND ticker_yh IN ('",peer_sql,"')")) %>%
    dplyr::as_tibble()


  DBI::dbDisconnect(con)


  # Add Revenue qtr1-qtr2
  tic_qtr_results <- tic_qtr_results_raw %>%
    dplyr::group_by(.data$ticker_yh) %>%
    dplyr::arrange(.data$ticker_yh,.data$date) %>%
    dplyr::mutate(revenue_diff = (.data$totalRevenue-dplyr::lag(.data$totalRevenue))*4) %>%
    dplyr::ungroup() %>%
    dplyr::select(date,ticker_yh,accountsPayable,capitalExpenditures,researchDevelopment,revenue_diff)




  # Adding some addtional Key Metrics if feasable ---------------------------

  # RULE OF 40
  if("profitMargins" %in% needed_ratios$ratios & "revenueGrowth" %in% needed_ratios$ratios){
    tic_ratios <- tic_ratios %>%
      dplyr::mutate(rule_40 = profitMargins + revenueGrowth)

    needed_ratios <- needed_ratios %>% tibble::add_row(
      ratios = 'rule_40',
      pref_direction = 'higher',
      groups = 'SaaS KPIs',
      comment = 'a principle that states a software companys combined revenue growth rate and profit margin should equal or exceed 40%')
  }

  # Magic Number

  # (Umsatz der Periode MINUS Umsatz der Vorperiode) * 4 DIV Marketingausgaben der Vorperiode
  tic_ratios <- tic_ratios %>%
    dplyr::distinct(ticker_yh,.keep_all = T) %>%
    dplyr::left_join(
      tic_qtr_results[,c("date","ticker_yh","revenue_diff")] %>% dplyr::left_join(
        tic_qtr_results %>%
          dplyr::group_by(ticker_yh) %>%
          dplyr::summarise(min_date = min(date),
                           accountsPayable = dplyr::first(accountsPayable),
                           capitalExpenditures = dplyr::first(capitalExpenditures),
                           researchDevelopment = dplyr::first(researchDevelopment),.groups = "drop") %>%
          dplyr::ungroup(),
        by = "ticker_yh") %>%
        dplyr::select(date,ticker_yh,accountsPayable,capitalExpenditures,researchDevelopment,revenue_diff) %>%
        tidyr::drop_na(revenue_diff) %>%
        dplyr::mutate(magic_number_accounts = revenue_diff/accountsPayable,
                      magic_number_capexpends = revenue_diff/capitalExpenditures,
                      magic_number_research_devs = revenue_diff/researchDevelopment) %>%
        dplyr::select(ticker_yh,magic_number_research_devs,magic_number_capexpends,magic_number_accounts),
      by = "ticker_yh")


  needed_ratios <- needed_ratios %>% tibble::add_row(
    ratios = c('magic_number_accounts','magic_number_capexpends','magic_number_research_devs'),
    pref_direction = c('higher','higher','higher'),
    groups = c('SaaS KPIs','SaaS KPIs','SaaS KPIs'),
    comment = c('(revenue for the period MINUS revenue for the previous period) * 4 DIV accountsPayable',
                '(revenue for the period MINUS revenue for the previous period) * 4 DIV capitalExpenditures',
                '(revenue for the period MINUS revenue for the previous period) * 4 DIV researchDevelopment'))

  #tic_ratios[(tic_ratios$ticker_yh == "AAPL" & tic_ratios$date == max(tic_ratios$date)),"pegRatio"]$pegRatio




  # Calculate Ratios --------------------------------------------------------

  # tic_ratios <- tic_ratios %>% dplyr::filter(!ticker_yh %in% c("AI"))


  cat(script_logger("Calculating"),warning_logger(nrow(peers)),script_logger("ticker(s) from industry sector"),warning_logger(sector_prio),"\n")

  val_ratios <- function(ticker,ratio){

    direction <- needed_ratios[needed_ratios$ratios == rlang::sym(ratio),"pref_direction"]$pref_direction

    cleaned_ratio <- tic_ratios %>%
      tidyr::drop_na(!!ratio)

    cur_ratio <- tryCatch(cleaned_ratio %>%
                            dplyr::add_count(name = 'count') %>%
                            dplyr::mutate(calc_ratio = !!ratio,
                                          # which(sort(.[[ratio]],decreasing=T) == cleaned_ratio[(cleaned_ratio$ticker_yh == ticker & cleaned_ratio$date == max(cleaned_ratio$date)),rlang::sym(ratio)][[ratio]])
                                          pos = which(sort(.[[ratio]],decreasing=T) == cleaned_ratio[(cleaned_ratio$ticker_yh == ticker),rlang::sym(ratio)][[ratio]]),
                                          rank = ifelse(direction == 'lower',stringr::str_c(length(count)+1-pos,"/",count),stringr::str_c(pos,"/",count)),
                                          quantile = pos/count,
                                          quantile = dplyr::case_when(
                                            direction == "lower" & quantile >= 0.75 ~ '1.Qtl',
                                            direction == "lower" & quantile >= 0.5 ~ '2.Qtl',
                                            direction == "lower" & quantile >= 0.25 ~ '3.Qtl',
                                            direction == "lower" & quantile >  0    ~ '4.Qtl',
                                            direction == "higher" & quantile >= 0.75 ~ '4.Qtl',
                                            direction == "higher" & quantile >= 0.5 ~ '3.Qtl',
                                            direction == "higher" & quantile >= 0.25 ~ '2.Qtl',
                                            direction == "higher" & quantile >  0    ~ '1.Qtl',
                                            TRUE ~ 'NA'
                                          ),
                                          assessment = ifelse(quantile=='1.Qtl',1,0),
                                          min = min(get(ratio),na.rm = T),
                                          max = max(get(ratio),na.rm = T),
                                          median = median(get(ratio),na.rm = T)
                            ) %>%
                            dplyr::filter(ticker_yh == ticker) %>%
                            dplyr::select(date, ticker_yh,name,calc_ratio,!!ratio,rank,quantile,assessment, min,median,max) %>%
                            dplyr::rename(level = 5),
                          error = function(e) e)

    if(inherits(cur_ratio,"error")){return(tibble::tibble(date = lubridate::as_date(max(cleaned_ratio$date)),
                                                          ticker_yh = ticker,
                                                          name = NA,
                                                          level = 0,
                                                          calc_ratio = ratio,
                                                          rank = NA,
                                                          assessment = 0,
                                                          quantile = NA,
                                                          min = NA,
                                                          median = NA,
                                                          max = NA))}

    return(cur_ratio)
  }


  all_ratios <- tibble::tibble()
  for(i in 1:length(needed_ratios$ratios)){
    cat(script_logger(i,"/",length(needed_ratios$ratios),"calculating ratio: " ,needed_ratios$ratios[i],"\n"))
    new_ratio <- purrr::map_df(unique(tic_ratios$ticker_yh), val_ratios, ratio = as.character(needed_ratios$ratios[i])) %>%
      dplyr::arrange(desc(.[,3]))

    # adding quantiles to ratio levels
    new_ratio <- new_ratio %>%
      dplyr::mutate(q_10pct = as.numeric(stats::quantile(new_ratio$level,c(0.10))),
                    q_25pct = as.numeric(stats::quantile(new_ratio$level,c(0.25))),
                    q_50pct = as.numeric(stats::quantile(new_ratio$level,c(0.50))),
                    q_75pct = as.numeric(stats::quantile(new_ratio$level,c(0.75))),
                    q_90pct = as.numeric(stats::quantile(new_ratio$level,c(0.90))))

    all_ratios <- rbind(all_ratios,new_ratio)
  }

  # Adding Ratio Comments
  all_ratios <- all_ratios %>%
    dplyr::left_join(needed_ratios, by = c("calc_ratio" = "ratios")) %>%
    tibble::add_column(sector = sector_prio,
                       ratio_rank_object = TRUE)

  return(all_ratios)

  success_logger("All Done!\n")
}


