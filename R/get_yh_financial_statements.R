

#' Get yahoo company's financial statements
#'
#' @param symbols A yahoo ticker symbol
#' @param period Income statement: Either annual or quarterly
#' @param verbose if\code{TRUE} provides instant feedback
#'
#' @return a tibble with corporate financials
#' @export
#'
#' @examples\dontrun{
#' get_yh_financial_statements("AAPL")
#' }
#'
get_yh_financial_statements <- function(symbols = NULL, period = "quarterly", verbose = FALSE){

  if(is.null(symbols)){stop(crayon::red("No Symbol provided\n"))}

  if(verbose == TRUE){
    cat(crayon::green("getting ",period," data for ",length(symbols)," Symbol(s)\n"))
  }

  single <- purrr::map(.x = symbols, .f = ~get_yh_financials_hf(.x, period, verbose))
  all_symbols <- dplyr::bind_rows(single)

  if(verbose == TRUE){
    cat(crayon::green("all done\n"))
  }
  return(all_symbols)
}
