
#' Get Company Descriptions from Yahoo Finance
#'
#' @param symbols Yahoo ticker symbol(s)
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @details
#' Retrieve Company's profile and operating descriptions from the Yahoo Finance website.
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' get_yh_comp_description(c("AAPL","MSFT))
#' }
get_yh_comp_description <- function(symbols = NULL, verbose = FALSE){


  if(is.null(symbols)){stop(crayon::red("No Symbol provided\n"))}

  # cookie needed first as yahoo can only be fetched outside GDPR countries
  if(!exists("ses")){
    if(verbose){
      cat(crayon::cyan("setup new crumb\n"))
    }
    get_crumb()
  }

  # initialize final tibble
  all <- tibble::tibble()


  # loop through symbols
  for(tic in symbols){

    if(verbose){
      cat(crayon::cyan("get info for ticker",tic ,"\n"))
    }
    # compose the request
    url <- glue::glue('https://query2.finance.yahoo.com/v10/finance/quoteSummary/{tic}?modules=assetProfile&ssl=true&crumb={ses$crumb}')

    res <- suppressWarnings(tryCatch(jsonlite::fromJSON(curl::curl(url, handle = ses$h))$quoteSummary$result,
                                     error=function(e) e))

    if(inherits(res, 'error')){
      if(verbose){
          cat(crayon::yellow("no data for ", tic,"\n"))
        }
      next
    }

    new1 <- tibble::tibble(ticker_yh = tic,
                           industry = ifelse(is.null(res$assetProfile$industry),"NA",res$assetProfile$industry),
                           industryKey = ifelse(is.null(res$assetProfile$industryKey),"NA",res$assetProfile$industryKey),
                           industryDisp = ifelse(is.null(res$assetProfile$industryDisp),"NA",res$assetProfile$industryDisp),
                           sector = ifelse(is.null(res$assetProfile$sector),"NA",res$assetProfile$sector),
                           sectorKey = ifelse(is.null(res$assetProfile$sectorKey),"NA",res$assetProfile$sectorKey),
                           sectorDisp = ifelse(is.null(res$assetProfile$sectorDisp),"NA",res$assetProfile$sectorDisp),
                           longBusinessSummary = ifelse(is.null(res$assetProfile$longBusinessSummary),"NA",res$assetProfile$longBusinessSummary),
                           auditRisk = ifelse(is.null(res$assetProfile$auditRisk),"NA",res$assetProfile$auditRisk),
                           boardRisk = ifelse(is.null(res$assetProfile$boardRisk),"NA",res$assetProfile$boardRisk),
                           compensationRisk = ifelse(is.null(res$assetProfile$compensationRisk),"NA",res$assetProfile$compensationRisk),
                           shareHolderRightsRisk = ifelse(is.null(res$assetProfile$shareHolderRightsRisk),"NA",res$assetProfile$shareHolderRightsRisk),
                           overallRisk = ifelse(is.null(res$assetProfile$overallRisk),"NA",res$assetProfile$overallRisk))

    all <- rbind(all, new1)

  }

  return(all)

}
