

#' Get yahoo company's financial statements
#'
#' @param symbols A yahoo ticker symbol
#' @param period Income statement: Either annual or quarterly
#' @param as_pivot_long if \code{TRUE} returns result as pivot long
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @return a tibble with corporate financials
#' @export
#'
#' @examples\dontrun{
#' get_yh_financial_statements("AAPL")
#' }
#'
get_yh_financial_statements <- function(symbols = NULL, period = "quarterly", as_pivot_long = FALSE, verbose = FALSE){

  if(is.null(symbols)){stop(crayon::red("No Symbol provided\n"))}

  if(verbose == TRUE){
    cat(crayon::green("getting ",period," data for ",length(symbols)," Symbol(s)\n"))
  }

  single <- purrr::map(.x = symbols, .f = ~get_yh_financials_hf(.x, period, verbose, as_pivot_long = as_pivot_long))
  all_symbols <- dplyr::bind_rows(single)

  if(verbose == TRUE){
    cat(crayon::green("all done\n"))
  }
  return(all_symbols)
}
