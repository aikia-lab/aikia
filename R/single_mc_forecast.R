
#' Title
#'
#' @param s_ticker a symbol part of the 'df_hist' tibble
#' @param df_hist a price history table
#' @param hist_length which history length is used for the params estimation
#' @param no_sim number of simulations performed by MC
#' @param days_forecast how many (working) days to be forecasted
#' @param combine_ts combine the history ts with the forecast ts
#' @param lambda Poisson jump intensity (default 0.1)
#' @param scale forecast the 'price' or 'norm' it to 1
#' @param coridor probability corridor
#'
#' @details
#' Perform a Monte Carlo based forecast based on the 'get_any_history' function.
#' Tickers are saved in the 'symbol' column and last values in the 'price' column.
#' You also need to have a 'date' column
#'
#' @importFrom magrittr %>%
#'
#' @return a tibble
#' @export
#'
#' @examples \dontrun{
#' single_mc_forecast("AAPL",df,scale = 'price')
#' }
single_mc_forecast <- function(s_ticker,
                               df_hist = NULL,
                               hist_length = 120,
                               no_sim = 1000,
                               days_forecast = 40,
                               combine_ts = TRUE,
                               lambda = 0.1, # Poisson jump intensity, adjust as needed
                               scale = c("price", "norm"),
                               coridor = c(0:0.9999)){

  logger <- crayon::cyan $ bold

  if(is.null(df_hist)){
    cat(logger("no df_hist provided\n"))
  }

  # Calender needed for forecast function
  bizdays::load_quantlib_calendars('UnitedStates/NYSE',
                                   from='2016-01-01', to=lubridate::today()+100)


    single_ticker <- df_hist %>%
      dplyr::filter(symbol == s_ticker) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(log_ret = log(price/dplyr::lag(price))) %>%
      tidyr::drop_na() %>%
      tail(hist_length)


  # Get parameter for distribution sampling
  params <<- tryCatch(sn::cp2dp(c(mean(single_ticker$log_ret),
                                 sd(single_ticker$log_ret),
                                 moments::skewness(single_ticker$log_ret),
                                 moments::kurtosis(single_ticker$log_ret)), # keine ?berkurtosis
                               "ST"),
                     error = function(e) e)

  if(inherits(params, "error")){
    print(paste("Quit ", s_ticker))
    return()
  } # cp2dp funktion ist restriktiert auf niedrige Werte -> einfache L?sung >NEXT<



  #start simulating prices
  mc_matrix<-matrix(nrow=days_forecast,ncol=no_sim) # no of simulation (+1 as 1 value is last close)
  mc_matrix[1,1]<- ifelse(scale == "price", as.numeric(single_ticker[nrow(single_ticker),"price"]), 100)
  for(j in 1:ncol(mc_matrix)){
    mc_matrix[1,j] <- ifelse(scale == "price", as.numeric(single_ticker[nrow(single_ticker),"price"]), 100)
    for(i in 2:nrow(mc_matrix)){
      # mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(sn::rst(1, dp = params)[["nu"]])
      jump <- rpois(1, lambda) # Simulate price for this day (using skew-normal + Poisson jump)
      mc_matrix[i,j] <- mc_matrix[i-1,j]*exp(sn::rst(1, dp = params)[["nu"]]) + ifelse(jump > 0, runif(1, min = -0.02, max = 0.02), 0)
    }
  }
  name<- stringr::str_c("Sim ",seq(1,no_sim))
  name<-c("Day",name)
  final_mat<-cbind(1:days_forecast,mc_matrix)
  final_mat<-dplyr::as_tibble(final_mat)
  colnames(final_mat)<-name


  new_ts <- final_mat %>%
    tidyr::pivot_longer(cols = 2:(no_sim+1),
                        names_to = "Simulation",
                        values_to = "Price") %>%
    dplyr::group_by(Day) %>%
    dplyr::mutate(mean = round(median(Price),4),
                  percentile_up = round(quantile(Price, (1-coridor)),4),
                  percentile_down = round(quantile(Price, coridor),4)) %>%
    dplyr::distinct(Day, .keep_all = T) %>%
    dplyr::ungroup() %>%
    dplyr::select(mean,percentile_up, percentile_down) %>%
    dplyr::mutate(date = bizdays::bizseq(lubridate::as_date(single_ticker[nrow(single_ticker),"date"]$date),
                                         bizdays::offset(single_ticker[nrow(single_ticker),"date"]$date,
                                                         (days_forecast-1),
                                                         'QuantLib/UnitedStates/NYSE'),
                                         'QuantLib/UnitedStates/NYSE'),
                  symbol = s_ticker)

  if(combine_ts){
    new_ts <- rbind(
      single_ticker |> dplyr::select(date,symbol,price) |> dplyr::mutate(percentile_up=0, percentile_down=0),
      new_ts |> dplyr::rename(price = mean)) |>
      dplyr::arrange(desc(date)) |>
      dplyr::distinct(date,.keep_all = T)
  }

  return(new_ts)



}



