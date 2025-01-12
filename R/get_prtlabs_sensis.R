
#' Get Ticker Sensis from Portfoliolab
#'
#' @param symbol Yahoo ticker symbol
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @details
#' Retrieve Ticker and Portfolio-wise Sensitivities like Sharp Ratio, Sortino Ratio and Drawdowns from the Portfoliolabs.com website.
#'
#' @return a list
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' get_prtlabs_sensis("AAPL")
#' }
get_prtlabs_sensis <- function(symbol = NULL, verbose = FALSE){

  if(is.null(symbol)|length(symbol)!=1){
    cat("Check Input data\n")
    stop()
  }

    # Base URL of the API
    base_url <- "https://portfolioslab.com/api"

    # Endpoint for symbol information
    endpoint <- sprintf("%s/symbol/%s", base_url, symbol)

    # Send GET request
    response <- httr::GET(endpoint)

    # Check for successful response
    if (httr::status_code(response) == 200) {
      # Parse JSON response
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content, flatten = TRUE)

      if(verbose){
        cli::cli_alert_success("Retrieved Data for {symbol}")
      }

      return(parsed)
    } else {
      cli::cli_alert_danger(
        "Failed to retrieve data for {symbol}: {httr::http_status(response)$message}"
      )
    }

}



