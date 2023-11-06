
#' Get yh EPS and Revenue estimates and trend
#'
#' @param symbols A yahoo ticker symbol
#' @param as_pivot_long if \code{TRUE} returns result as pivot long
#' @param verbose if \code{TRUE} provides instant feedback
#'
#' @return a tibble with corporate estimates
#' @export
#'
#' @examples\dontrun{
#' get_yh_estimates("AAPL", verbose = TRUE)
#' }
#'
get_yh_estimates <- function(symbols = NULL, as_pivot_long = FALSE, verbose = FALSE){

  if(is.null(symbols)){stop(crayon::red("No Symbol provided\n"))}

  if(verbose == TRUE){
    cat(crayon::green("getting estimates data for ",length(symbols)," Symbol(s)\n"))
  }

  single <- purrr::map(.x = symbols, .f = ~get_yh_estimates_hf(.x, verbose = verbose, as_pivot_long = as_pivot_long))
  all_symbols <- dplyr::bind_rows(single)

  if(verbose == TRUE){
    cat(crayon::green("all done\n"))
  }
  return(all_symbols)
}


