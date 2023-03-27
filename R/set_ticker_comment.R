
#' Add a personal comment to symbols
#'
#' @param symbols provide (multiple) ticker_yh symbols
#' @param comments provide a comment to add. OR keep empty to remove
#'
#' @return no return
#' @export
#'
#' @examples\dontrun{
#'  set_ticker_comment(symbols = c("AAPL","ASML","MSFT"), comment =c("treated as value than growth tec"))
#'}
set_ticker_comment <- function(symbols = NULL, comments = NULL){

  error_logger <- crayon::red $ bold
  success_logger <- crayon::green $ bold


  if(is.null(symbols)){
    cat(error_logger("No ticker provided! PLease check input\n"))
  } else{

    tickers_sql <- stringr::str_c(symbols,collapse = "','")

    mydb <- aikia::connect_to_db(user = "ceilert",password = "ceilert")

    comment <- DBI::dbSendQuery(mydb,stringr::str_c("UPDATE fin_ticker_meta_data
                                   SET comment = '",comments,"'
                                   WHERE ticker_yh IN ('",tickers_sql,"')"))
    DBI::dbClearResult(comment)

    DBI::dbDisconnect(mydb)

    cat(success_logger("new comment set for"),symbols)
  }


}
