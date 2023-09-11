

#' Add and Remove Ticker from Watchlist table
#'
#' @param add provide ticker_yh symbols
#' @param remove provide ticker_yh symbols
#' @param threshold provide a price limit to get noticed
#'
#' @return no return
#' @export
#'
#' @importFrom magrittr %>%
#' @examples\dontrun{
#'  adjust_ticker_watchlist(add = c("AAPL","ASML","MSFT"))
#'}
adjust_ticker_watchlist <- function(add = NULL, remove = NULL, threshold = 0){

  error_logger <- crayon::red $ bold
  success_logger <- crayon::green $ bold
  warning_logger <- crayon::yellow $ bold
  script_logger <- crayon::cyan $ bold


  if(length(add) > 1 | length(remove) > 1){

    cat(error_logger("PLease provide 1 ticker at a time\n"))

  } else {

    # ADD
    if(!is.null(add)){


      con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")
      check_tic <- DBI::dbGetQuery(con,stringr::str_c("SELECT name
                                         FROM fin_ticker_meta_data
                                         WHERE ticker_yh = '",add,"'")) %>%
        dplyr::as_tibble()
      DBI::dbDisconnect(con)

      if(nrow(check_tic)==0){
        cat(paste0(warning_logger(add),error_logger(" is not a valid ticker! Please check\n")))
        return()
      }

      add_sql <- data.frame(ticker_yh = add,
                            threshold = threshold)

      con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")
      DBI::dbWriteTable(con,
                        name = "fin_ticker_watchlist",
                        value = add_sql,
                        append = TRUE)


      dubs <- DBI::dbSendQuery(con, "DELETE FROM fin_ticker_watchlist
          WHERE uid IN (
            SELECT uid
            FROM (
              SELECT *,
              RANK() OVER (PARTITION BY ticker_yh
                            ORDER BY updated_at) AS oldest
              FROM fin_ticker_watchlist
              ORDER BY updated_at) AS get_values
            WHERE get_values.oldest = 2)")

      DBI::dbClearResult(dubs) # needed in DBSendQuery as otherwise connection will remains open und not finalized
      DBI::dbDisconnect(con)

      cat(paste0(script_logger("successfully "),success_logger("added "),script_logger("ticker "),warning_logger(stringr::str_c(add,collapse = ",")),"\n"))
    }

    # REMOVE
    if(!is.null(remove)){

      if(length(remove)==1){
        remove_sql <- remove
      } else if(length(remove)>1){
        remove_sql <- stringr::str_c(remove,collapse = "','")
      }

      con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")

      update <- DBI::dbSendQuery(con,
                                 stringr::str_c("DELETE
                                        FROM fin_ticker_watchlist
                                        WHERE ticker_yh IN ('",remove_sql,"')"))

      DBI::dbClearResult(update) # needed in DBSendQuery as otherwise connection will remains open und not finalized
      DBI::dbDisconnect(con)

      cat(paste0(script_logger("successfully "),error_logger("removed "),script_logger("ticker "),warning_logger(stringr::str_c(remove,collapse = ",")),"\n"))
    }

  }

}





