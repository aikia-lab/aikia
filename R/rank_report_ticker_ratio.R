
#' Ratio Rank of single Ticker
#'
#' @param ratio_rank_object result file of 'rank_peer_ratio' function
#' @param ex_ticker Select a Yahoo ticker to list all Ratios
#'
#' @return gt object of Ticker Rank in all Ratios
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#'  rank_report_ticker_ratio(ratio_rank_object = NULL, ex_ticker = NULL)
#'}
rank_report_ticker_ratio <- function(ratio_rank_object, ex_ticker = NULL){

  error_logger <- crayon::red $ bold
  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  success_logger <- crayon::green $ bold

  if(!'ratio_rank_object' %in% names(ratio_rank_object)){
    cat(error_logger("PLease check data frame! Not a 'Ratio Rank Object'\n"))
    cat(error_logger("FIRST run function"), warning_logger("aikia::rank_peer_ratio()\n"))
    return()
  }
  ticker_info <- ratio_rank_object %>%
    tidyr::drop_na(name) %>%
    dplyr::group_by(calc_ratio) %>%
    dplyr::mutate(level_max = max(level),
                  level_bar = 100*(level/level_max),
                  `Level Range` = 100) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ticker_yh == ex_ticker) %>%
    dplyr::mutate(score = sum(assessment)) %>%
    dplyr::select(calc_ratio,rank,level,`Level Range`,level_bar,groups,score,sector,name,comment) %>%
    dplyr::group_by(groups)

  tic_gt <- ticker_info %>% gt::gt() %>%
    gt::tab_header(
      title = gt::md(paste0("**",unique(ticker_info$name),"**<br>",
                            "Overall Score: <font style='color:red;font-size:30px'> ",unique(ticker_info$score),"</font>")),
      #"<p style ='color: red; font-size: 30px;'>", unique(ticker_info$score),"</p>")),
      #<font color='red',size='1'>",unique(ticker_info$score),"</font>")),
      subtitle = gt::md(paste0("**",ex_ticker," - ",unique(ticker_info$sector),"**"))) %>%
    gtExtras::gt_plt_bullet(column = `Level Range`, target = level_bar, width = 45,
                            palette = c(aikia::aikia_palette_eight()[4],"black")) %>%
    gt::fmt_number(columns = c("level"),rows = level > 1 ,decimals = 2) %>%
    gt::fmt_percent(columns = c("level"),rows = level < 1 ,decimals = 2) %>%
    gt::cols_label(
      calc_ratio = "Sensitivity",
      rank = "Rank",
      level = "Level") %>%
    gt::tab_options(row_group.font.size = "16px",
                    row_group.font.weight = "bold") %>%
    gt::cols_hide(columns = c(name,sector,score)) %>%
    gt::cols_align(align = "center") %>%
    aikia::gt_theme_aikia()

  return(tic_gt)
}
