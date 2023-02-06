
#' Update ticker history from Yahoo Finance
#'
#' @param symbols a Yahoo ticker symbol
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @return a tibble with available price history
#' @export
#'
#' @examples \dontrun{
#' get_yh_hist_data("AAPL")
#' }
#'
get_yh_ticker_hist_data <- function(symbols = NULL, verbose = FALSE){

  if(is.null(symbols)){stop(crayon::red("No Symbol provided\n"))}


  single <- purrr::map(.x = symbols, .f = ~get_yh_hist_hf(.x, verbose))
  all_symbols <- dplyr::bind_rows(single)

  if(verbose == TRUE){
    cat(crayon::green("all done\n"))
  }


  return(all_symbols)
}
