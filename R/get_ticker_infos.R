
#' Get ticker infos from Meta Data Table
#'
#' @param search_term searches in {name} and {ticker_*}  cols
#' @param exact if {TRUE} filters for exact matches
#'
#' @return a tibble
#' @export
#'
#' @examples\dontrun{
#' get_ticker_infos("DBK.DE")
#' }
#'
get_ticker_infos <- function(search_term = NULL, exact = FALSE){

  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::red $ bold

  if(is.null(search_term)){
    stop(error_logger("Specify Search Term\n"))
  }


  con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")
  result <- DBI::dbGetQuery(con,stringr::str_c("SELECT *
                              FROM fin_ticker_meta_data
                              WHERE ticker_yh LIKE CONCAT('%','",search_term,"','%')
                              OR ticker_bb LIKE CONCAT('%','",search_term,"','%')
                              OR ticker_fv LIKE CONCAT('%','",search_term,"','%')
                              OR name LIKE CONCAT('%','",search_term,"','%')")) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-.data$retrieval_time) %>%
    dplyr::relocate(.data$name,.before = 1) %>%
    dplyr::mutate_all(as.character)

  DBI::dbDisconnect(con)

  if(exact){
    result <- result %>%
      dplyr::filter(.data$ticker_yh == search_term |
                    .data$ticker_bb == search_term |
                    .data$ticker_fv == search_term |
                    .data$name == search_term)
  }

  if(nrow(result)>1){
    cat(warning_logger("\nmore than 1 match to your query:\n"))
    result <- result %>% dplyr::select(ticker_yh,name)
    return(result)
  }


  result <- result %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = 'names',
                        values_to = 'values')

  return(result)

}
