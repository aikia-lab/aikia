


#' Match yahoo finance ticker by ISINs
#'
#' @param q ISIN to request for yahoo ticker
#' @param verbose if\code{TRUE} provides instant feedback
#'
#' @return A tibble with several company & exchange info
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' get_yh_ticker_by_isin("DE0007100000", verbose = TRUE)
#' }
#'
get_yh_ticker_by_isin <- function (q = NULL, verbose = TRUE){

  if(length(q)>1 | is.null(q)){
    stop(crayon::red("Please provide 1 ISIN \n"))
  }

  url <- glue::glue("https://query2.finance.yahoo.com/v1/finance/search?q={q}&newsCount=0")

  plain <- jsonlite::fromJSON(url)$quotes %>% dplyr::as_tibble()

  plain$isin <- q

  # inn case multiple tickers are available for same ISIN
  if(nrow(plain)>1){
    if (verbose){
      cat(crayon::yellow(paste0("multiple tickers for: ", q,"\n")))
      print(plain)
    }

    plain$rowsum <- rowSums(is.na(plain))
    plain <- plain %>% dplyr::filter(rowsum == min(rowsum)) %>% dplyr::select(-rowsum)
  }


  if (verbose) {
    cat(crayon::green(paste0("returning ticker: ", plain$symbol,"\n")))
  }


  return(plain)
}







