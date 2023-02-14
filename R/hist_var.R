



#' Historical Value at Risk
#'
#' @param name a vector of yahoo tickers
#' @param weights weights for each stock
#' @param start_date historical start date
#' @param end_date end date of time series
#' @param confidence confidence intervall
#' @param observation_points number of historical observations, e.g. 1y = 261
#' @param half_life in years. until half of the distribution is filled
#' @param verbose provides instant feedback
#'
#' @return list of VaR, Time Series and VaR Plot
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#' hist_Value_at_Risk(name = c("AAPL","F"),weights = c(0.5,0.5))
#' }
historical_VaR <- function(name = c('AAPL','F'),
                               weights = c(0.5,0.5),
                               start_date = lubridate::today()-365.25*3.5,
                               end_date = lubridate::today()-1,
                               confidence = 99,
                               observation_points = 783,
                               half_life = 0.5,
                               verbose = TRUE){


# preprocessing constraints -----------------------------------------------

  success_cat <- crayon::bold $ green
  info_cat <- crayon::cyan $ bold
  warning_cat <- crayon::yellow $ bold
  red_cat <- crayon::red $ bold


  end_date <- lubridate::as_date(end_date) # ensure date variable
  start_date <- lubridate::as_date(start_date) # ensure date variable
  hldays <- 261 * half_life # half life in days
  startdecay <- exp(-log(2) / hldays) # scaleing of exp Decay
  percentile_threshold <- function(x){ # helper function to cut off VaR
    1 - (x/100)
  }
  hline <- function(y = 0, color = "red") { # helper function for plotting
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color, dash="dot")
    )
  }

# Distinction between single & multiple equities --------------------------

# 1) Multiple Equities
  if(length(name) > 1){

    if(length(weights)!=length(name)){
      stop(red_cat("adjust portfolio weights!"))
    } else if(sum(weights)!=1){
      stop(red_cat("sum of weights not 1!"))
    } else {
      eqwgt <- tibble::tibble(name,weights)
    }

    if(verbose){cat(info_cat("Starting calculation\n  Fetching historical data\n"))}

    # Fetching History
    raw_data <- name %>%
      tidyquant::tq_get(get  = "stock.prices",
                        from = start_date,
                        to   = end_date)

    if(verbose){cat(info_cat("  Fetching done!\n"))}

    raw_data <- raw_data %>%
      dplyr::rename(name = symbol,
                    px_last = adjusted) %>%
      dplyr::arrange(name, dplyr::desc(date))

    if(max(raw_data$date)<end_date){
      cat(warning_cat("  Max received date", max(raw_data$date),"is not matching end date", end_date,"\n"))
    }

    raw_wgt <- dplyr::left_join(raw_data, eqwgt, by = 'name') %>%
      dplyr::mutate(px_wgt = px_last * weights)


    portfolio_composition <- raw_wgt %>%
      dplyr::select(name,date,px_wgt) %>%
      tidyr::pivot_wider(names_from = 'name',values_from = px_wgt) %>%
      dplyr::distinct(date,.keep_all = T) %>%
      tidyr::fill(names(.)) %>%
      dplyr::mutate(singleTS = dplyr::select(.,2:(length(name)+1)) %>% rowSums(na.rm = T)) %>%
      dplyr::select(date, singleTS) %>%
      dplyr::arrange(desc(date))

# 2) Single Equity
  } else{

    eqwgt <- 1

    if(verbose){cat(info_cat("Starting calculation\n  Fetching historical data\n"))}

    # Fetching History
    raw_data <- name %>%
      tidyquant::tq_get(get  = "stock.prices",
                        from = start_date,
                        to   = end_date)

    if(verbose){cat(info_cat("  Fetching done!\n"))}

    raw_data <- raw_data %>%
      dplyr::rename(name = symbol,
                    px_last = adjusted) %>%
      dplyr::arrange(desc(date))


    if(max(raw_data$date)<end_date){
      cat(warning_cat("  Max received date", max(raw_data$date),"is not matching end date", end_date,"\n"))
    }


    portfolio_composition <- raw_data %>%
      dplyr::select(date, singleTS = px_last) %>%
      dplyr::arrange(desc(date))

  }


# Value at Risk Calculation -----------------------------------------------

  if(verbose){cat(info_cat("  Calculating historical Value of Risk\n"))}
  if(nrow(portfolio_composition)<observation_points){observation_points <- nrow(portfolio_composition)}

  portfolio_calc <- portfolio_composition %>%
    dplyr::slice(1:observation_points) %>%
    dplyr::mutate(px_lead = dplyr::lead(singleTS), # for PnL calculation
                  n = 0:(observation_points-1)) # for decay calculations


  # get latest price for scaling
  p1 <- portfolio_calc %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(singleTS)

  var_prep <- portfolio_calc %>%
    dplyr::mutate(px_ref = p1,
                  pnl_sc = singleTS / px_lead - 1, # % change
                  pnl_sc = pnl_sc * px_ref) # $ change scaled to latest price

  # decay factoring
  decay <- var_prep %>%
    dplyr::mutate(nt = startdecay ^ n,
                  decay = startdecay ^ n / sum(nt),
                  rank = rank(pnl_sc)) %>%
    dplyr::arrange(rank) %>%
    dplyr::mutate(cumulative_wgt = cumsum(decay))

  VaR <- decay %>%
    dplyr::filter(cumulative_wgt >= percentile_threshold(confidence),
                  dplyr::lag(cumulative_wgt) <= percentile_threshold(confidence)) %>%
    dplyr::mutate(var_percent = pnl_sc/px_ref * 100)


  VaR_view <- VaR %>%
    dplyr::select(pnl_sc, var_percent)

  # Interpolation of VaR really sets to percentile_threshold
  interpol_result <- tibble::tibble("VaR Nominal"= stats::approx(decay$cumulative_wgt,decay$pnl_sc, xout = percentile_threshold(confidence))$y,
                                    "VaR Percent" = stats::approx(decay$cumulative_wgt,decay$pnl_sc, xout = percentile_threshold(confidence))$y/p1 *100)

  if(verbose){cat(success_cat("VaR calculation done\n"))}

  # Plotting VaR
  pnl_visio <- decay %>%
      plotly::plot_ly(x = ~date, y = ~pnl_sc,
                      type = 'bar',
                      name = 'PnL History in Crncy',
                      hovertemplate = paste0("PnL %{y} <br>",
                                             "PnL Perc ",scales::percent(decay$pnl_sc/p1,accuracy = 0.01),"<br>",
                                             "at: %{x} <extra></extra>"),
                      color = I("#248A8A")) %>%
      plotly::add_markers(data = VaR,
                          x = ~date, y = ~pnl_sc,
                          mode = 'markers',
                          hovertemplate = paste0("VaR Nom %{y} <br>",
                                                 "VaR Perc ", scales::percent(VaR$var_percent,scale = 1,accuracy = 0.01),"<br>",
                                                 "at: %{x} <extra></extra>"),
                          name = paste0('Value at Risk\n', confidence,'% confidence'),
                          color = I('red')) %>%
    plotly::layout(annotations = list(
      list(x = 0.5 , y = 1.0, text = "<b>Profit and Loss timeline</b>", showarrow = F, xref='paper', yref='paper',
           font = list(size = 24)),
      list(x = 0.5 , y = 0.93, text = paste0("with a half life factor of ",half_life," years"), showarrow = F, xref='paper', yref='paper',
           font = list(size = 14))),
      xaxis = list(title = "<b>Date</b>"),
      yaxis = list(title = paste0("<b>PnL in Crncy</b>")),
      shapes = list(hline(VaR$pnl_sc))) %>%
    plotly::add_text(showlegend = FALSE, x = start_date +lubridate::days(500), y = c(VaR$pnl_sc*1.1),
                     text = paste0("HVaR line as of valuation date ",max(portfolio_calc$date)),color = I("red"))


    output <- list(HVaR = interpol_result, TimeSeries = decay, PnL = pnl_visio)

    return(output)
}


























