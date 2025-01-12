#' Adjust the history table for stock splits
#'
#' @param check_1st if {false} no check will be performed
#'
#' @return none
#' @export
#'
#' @details
#' Checks if the DB table fin_ticker_stocksplit contains tickers whith a '0' flag
#' and performs an adjustment of the tickers' history in the original history table
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#'   adjust_ticker_history_stocksplit()
#' }
adjust_ticker_history_stocksplit <- function(check_1st = TRUE){

  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  error_logger <- crayon::red $ bold


  # get all tickers with stock splits
  con <- aikia::connect_to_db()
  sst <- DBI::dbReadTable(con,"fin_ticker_stocksplit") |>
    dplyr::as_tibble() |>
    dplyr::filter(hist_adj == 0)
  DBI::dbDisconnect(con)

  sst_sql <- paste0(sst$ticker_yh,collapse = "','")

  # Get all respective history data
  con <- aikia::connect_to_db()
  all_hist <- DBI::dbGetQuery(con,paste0(
    "SELECT *
    FROM fin_ticker_history
    WHERE ticker_yh IN ('",sst_sql,"')
    ORDER BY date"
  )) |> dplyr::as_tibble() |>
    dplyr::select(-uid,-retrieval_time)
  DBI::dbDisconnect(con)


  all_new <- all_hist %>%
    dplyr::arrange(date) |>
    dplyr::left_join(sst, by = c("ticker_yh")) %>%
    dplyr::mutate(
      # If the trade date is bevore the split date, adjust by the split ratio
      open = ifelse(!is.na(split_date) & date >= split_date,open, open * (ratio)),
      close = ifelse(!is.na(split_date) & date >= split_date,close, close * (ratio)),
      high = ifelse(!is.na(split_date) & date >= split_date,high, high * (ratio)),
      low = ifelse(!is.na(split_date) & date >= split_date,low, low * (ratio)),
      adjusted = ifelse(!is.na(split_date) & date >= split_date,adjusted, adjusted * (ratio))
    )

  if(check_1st){
    print(sst)

    check_diff <- dplyr::left_join(all_new, all_hist, by = c("date","ticker_yh")) |>
      dplyr::mutate(retX = adjusted.x / dplyr::lag(adjusted.x)-1,
                    retY = adjusted.y / dplyr::lag(adjusted.y)-1, .by = ticker_yh) |>
      dplyr::mutate(diff = retX - retY) |>
      dplyr::select(date, ticker_yh, diff,retX,retY) |>
      dplyr::arrange(desc(diff))
    cat(script_logger("\ncheck highest diviations:\n"))
    print(check_diff)

    answer <- readline("Want to proceed adjusting history in DB? (y/n)\n")

    # TS is adjusted
    if(answer == 'Y'|answer == 'y'|answer == 'Yes'|answer == 'yes'){

      for(tic in sst$ticker_yh){

        cat(script_logger("inserting new history for"),tic,"\n")
        new_hist <- all_new |>
          dplyr::filter(ticker_yh == tic) |>
          dplyr::select(date, ticker_yh,open,close, high, low, adjusted, volume)

        con <- aikia::connect_to_db()
        DBI::dbWriteTable(con,
                          name= "fin_ticker_history",
                          value = new_hist,
                          row.names = FALSE,
                          header = FALSE,
                          append = TRUE,
                          overwrite = FALSE)

        upd <- DBI::dbSendQuery(con, paste0(
          "UPDATE fin_data.fin_ticker_stocksplit
           SET hist_adj = 1
           WHERE ticker_yh = '",tic,"'"
        ))
        DBI::dbClearResult(upd)




        DBI::dbDisconnect(con)


      }

       cat(script_logger("deleting duplicates for all tickers from table\ntakes up to a minute!\n"))
       con <- aikia::connect_to_db()
       del <- DBI::dbSendQuery(con,"DELETE
                                FROM fin_data.fin_ticker_history
                                WHERE uid IN (
                                  SELECT uid
                                  FROM (
                                    SELECT *,
                                    RANK() OVER (PARTITION BY ticker_yh, date
                                                 ORDER BY uid DESC) AS oldest
                                    FROM fin_data.fin_ticker_history
                                    ORDER BY cast(retrieval_time as date)) AS get_values
                                  WHERE get_values.oldest > 1)")
       DBI::dbClearResult(del)

    } else {
      cat(error_logger("STOP adjusting TS\n"))
      stop()
    }

    # if no 1st check is done
  } else {

    for(tic in sst$ticker_yh){
      cat(script_logger("deleting old history data for"),tic,script_logger("from table\n"))

      mydb <- aikia::connect_to_db()
      dele <- DBI::dbSendQuery(mydb, paste0(
        "DELETE FROM fin_ticker_history WHERE ticker_yh IN ('", tic, "')"
      ))
      DBI::dbClearResult(dele)
      DBI::dbDisconnect(mydb)

      cat(script_logger("inserting new history for"),tic,"\n")
      new_hist <- all_new |>
        dplyr::filter(ticker_yh == tic) |>
        dplyr::select(date, ticker_yh,open,close, high, low, adjusted, volume)

      con <- aikia::connect_to_db()
      DBI::dbWriteTable(con,
                        name= "fin_ticker_history",
                        value = new_hist,
                        row.names = FALSE,
                        header = FALSE,
                        append = TRUE,
                        overwrite = FALSE)

      upd <- DBI::dbSendQuery(con, paste0(
        "UPDATE fin_data.fin_ticker_stocksplit
           SET hist_adj = 1
           WHERE ticker_yh = '",tic,"'"
      ))
      DBI::dbClearResult(upd)

      DBI::dbDisconnect(con)


    }

  }



  cat(script_logger("All done!\n"))
}
