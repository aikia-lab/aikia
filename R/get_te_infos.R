
#' Get economic variable infos from trading economic
#'
#' @param search_term searches in {type} col
#' @param country select country - default is {all}
#' @param category select category - default is {all}
#' @param exact if {TRUE} filters for exact matches
#' @param list_all if {TRUE} retuens all available series
#'
#' @details
#' search the trading economic table by country and category for potential matches:
#' the idea is to find future expectations to currently leading indicators.
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples{
#' get_te_infos("pmi",country = "states",category = "Busi")
#' }
get_te_infos <- function(search_term =NULL,
                         country = "all",
                         category = "all",
                         exact = FALSE,
                         list_all = FALSE){


  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::red $ bold
  script_logger <- crayon::cyan $ bold

  if(list_all){

    cat(script_logger("for simplicity, returning fields from US only\n"))
    con <- aikia::connect_to_db()

    all <- DBI::dbGetQuery(con,"SELECT country, category, type
                                FROM eco_forecasts_te
                                WHERE country = 'united-states'
                                AND DATE(retrieval_date) >
                                  (SELECT MAX(DATE(retrieval_date))-3 as date
                                      FROM eco_forecasts_te)
                                GROUP BY category, type") %>%
      dplyr::as_tibble()

    DBI::dbDisconnect(con)

    return(all)

  }


  if(is.null(search_term)){
    stop(error_logger("Specify Search Term\n"))
  }

  if(country != 'all'){
    ctry <- paste0("AND country LIKE CONCAT('%','",country,"','%')")
  }else {
    ctry <- ""
  }

  if(category != 'all'){
    ctgry <- paste0("AND category LIKE CONCAT('%','",category,"','%')")
  }else{
    ctgry <- ""
  }

  con <- aikia::connect_to_db()
  result <- DBI::dbGetQuery(con,stringr::str_c("SELECT country, category, type, unit
                              FROM eco_forecasts_te
                              WHERE type LIKE CONCAT('%','",search_term,"','%')",
                              ctry,
                              ctgry,
                              "AND period = 'actual'
                              GROUP BY country, category, type, unit")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate_all(as.character)

  DBI::dbDisconnect(con)

  if(exact){
    result <- result %>%
      dplyr::filter(.data$type == search_term)
  }

  if(nrow(result)>1){
    cat(warning_logger("\nmore than 1 match to your query:\n"))
    result <- result %>% dplyr::select(country,category,type,unit)
    return(result)
  }

  result <- result %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = 'names',
                        values_to = 'values')

  return(result)

}



