
#' Get Ticker Sensis from Portfoliolab
#'
#' @param symbols Yahoo ticker symbol(s)
#' @param allocation weight of ticker(s) in theroretical prt (see details!)
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @details
#' Retrieve Ticker and Portfolio wise Sensis like Sharp Ratio, Sortino Ratio and Drawdowns from the Portfoliolabs.com website.
#' If allocation is left empty, the weight for each ticker is set automatically to 100
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' get_prtlabs_sensis("AAPL",100)
#' }
get_prtlabs_sensis <- function(symbols = NULL, allocation = NULL, verbose=FALSE){

  if(is.null(symbols)){
    cat("Check Input data\n")
    stop()
  }


  if(is.null(allocation)){
    if(verbose){cat("Allocation manuualy set to 100 for each ticker\n")}
    allocation <- rep(100,length(symbols))
  }



  # Prepare Headers
  headers <- c(
    `accept` = "application/json, text/plain, */*",
    `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
    `content-type` = "application/json",
    # Relativ egal welches WP hier im referer steht
    `referer` = "https://portfolioslab.com/symbol/AMZN"
  )

  # Prepare Portfolio Allocation
  alloc <- tibble::tibble(
    symbolName = symbols,
    allocation = allocation
  )  %>%
    dplyr::mutate(
      string = glue::glue(
        '{{"symbolName":"{symbols}", "allocation":{allocation}}}'
      )
    )  %>%
    dplyr::pull(
      string
    )  %>%
    paste0(collapse = ",")

  # Insert Portfolio Allocation in cURL Body
  data <- glue::glue('{{"portfolio":
          {{
          "currency":"USD",
          "type":"static",
          "positions":
            [
              {alloc}
              ],
          "rebalance":"never"
          }},
        "settings":{{}},
        "range":"MAX"}}'
  )

  # POST Request via cURL
  res <- httr::POST(
    url = "https://portfolioslab.com/api/v2/tools/portfolio-analysis",
    httr::add_headers(.headers = headers),
    # httr::set_cookies(.cookies = cookies),
    body = data
  )
  content <- httr::content(res)
  perf <- content$indicators$table
  perf_data <- perf$data
  names(perf_data) <- unlist(perf$index)

  # Parse results
  result_set <- dplyr::as_tibble(perf_data)  %>%
    tidyr::unnest(
      cols = tidyselect::everything()
    )  %>%
    dplyr::bind_cols(
      dplyr::tibble(
        symbol = unlist(perf$columns)
      )
    )  %>%
    tidyr::pivot_longer(
      -symbol,
      names_to = "metric",
      values_to = "value",
      names_repair = "minimal"
    )

  cli::cli_alert_success(
    "Retrieved Data for {.val {unique(result_set$symbol)}}"
  )

  return(result_set)


}
