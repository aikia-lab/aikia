
#' Add a personal comment to symbols
#'
#' @param symbols provide (multiple) ticker_yh symbols
#' @param comments provide a comment to add. OR keep empty to remove
#'
#' @return no return
#' @export
#'
#' @examples\dontrun{
#'  set_ticker_comment("AAPL", comment = c("new comment"))
#'}
set_ticker_comment <- function(symbols = NULL, comments = NULL){

  script_logger <- crayon::cyan $ bold
  error_logger <- crayon::red $ bold
  success_logger <- crayon::green $ bold
  warning_logger <- crayon::yellow $ bold

  if(is.null(symbols)){
    cat(error_logger("No ticker provided! PLease check input\n"))
  }

  for(check_tic in symbols){

    mydb <- aikia::connect_to_db()
    check_comm <- DBI::dbGetQuery(mydb,stringr::str_c("SELECT comment
                                  FROM fin_ticker_meta_data
                                  WHERE ticker_yh = '",check_tic,"'")) %>%
      dplyr::pull(comment)
    DBI::dbDisconnect(mydb)

    # removed comments are not NULL
    if(is.na(check_comm)){
      check_comm <- ""
    }


    if(nchar(check_comm)>0){

      cat(warning_logger("Already comments available for"),check_tic,":\n")
      cat(check_comm,"\n\n")
      cat(warning_logger("Please choose from following:\n  1 - keep current Comment\n  2 - take new Comment\n  3 - add new Comment\n"))
      case <- readline(prompt = "Please insert Number of Choice:")


      if(case == 1){
        cat(success_logger("keep current comment for"),check_tic,"\n")
      }

      else if(case == 2){
        mydb <- aikia::connect_to_db()

        comment_send <- DBI::dbSendQuery(mydb,stringr::str_c("UPDATE fin_ticker_meta_data
                                   SET comment = '",comments,"'
                                   WHERE ticker_yh = ('",check_tic,"')"))
        DBI::dbClearResult(comment_send)

        DBI::dbDisconnect(mydb)

        cat(success_logger("new comment saved for"),check_tic,"\n")
      }

      else if(case == 3){

        new_comment <- stringr::str_c(check_comm,comments,sep = ". ")

        mydb <- aikia::connect_to_db()

        comment_send <- DBI::dbSendQuery(mydb,stringr::str_c("UPDATE fin_ticker_meta_data
                                   SET comment = '",new_comment,"'
                                   WHERE ticker_yh = ('",check_tic,"')"))
        DBI::dbClearResult(comment_send)

        DBI::dbDisconnect(mydb)

        cat(success_logger("new comment:",new_comment,"\n\n"))

      }
      else {
        cat(error_logger("wrong input. Retry\n"))

      }


    } else {
      cat(script_logger("No previous comment noted for"),check_tic,script_logger(". Saving new comment:\n",comments,"\n"))

      mydb <- aikia::connect_to_db()

      comment_send <- DBI::dbSendQuery(mydb,stringr::str_c("UPDATE fin_ticker_meta_data
                                   SET comment = '",comments,"'
                                   WHERE ticker_yh = ('",check_tic,"')"))
      DBI::dbClearResult(comment_send)

      DBI::dbDisconnect(mydb)

    }

  }


}
