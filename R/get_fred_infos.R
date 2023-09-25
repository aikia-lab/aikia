


get_fred_infos <- function(search_term = "oil", exact = FALSE){


  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::red $ bold

  if(is.null(search_term)){
    stop(error_logger("Specify Search Term\n"))
  }


#  con <- aikia::connect_to_db(user = "ceilert",password = "ceilert")
  con <- aikiaTrade::connect_to_db(group = "fin_data")
#  DBI::dbDisconnect(con)
#  DBI::dbListFields(con,"eco_ts_meta_data")

  result <- DBI::dbGetQuery(con,stringr::str_c("SELECT *
                              FROM eco_ts_meta_data
                              WHERE series LIKE CONCAT('%','",search_term,"','%')
                              OR name LIKE CONCAT('%','",search_term,"','%')
                              OR name_short LIKE CONCAT('%','",search_term,"','%')
                              OR comment LIKE CONCAT('%','",search_term,"','%')")) %>%
    dplyr::as_tibble() %>%
    dplyr::relocate(.data$name,.before = 1) %>%
    dplyr::mutate_all(as.character)

  DBI::dbDisconnect(con)

  if(exact){
    result <- result %>%
      dplyr::filter(.data$series == search_term |
                      .data$name == search_term |
                      .data$name_short == search_term |
                      .data$comment == search_term)
  }

  if(nrow(result)>1){
    cat(warning_logger("\nmore than 1 match to your query:\n"))
    result <- result %>% dplyr::select(name,series,name_short)
    return(result)
  }


  result <- result %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = 'names',
                        values_to = 'values')

  return(result)


}
