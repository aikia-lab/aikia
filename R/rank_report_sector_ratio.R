
#' Rank Peers in General OR by Specific Ratio
#'
#' @param ratio_rank_object result file of 'rank_peer_ratio' function
#' @param ratio keep {NULL} to Rank in General OR select a Ratio
#'
#' @return gt object of peer rank (by ratios)
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#'  rank_report_sector_ratio(ratio_rank_object = NULL, ratio = NULL)
#'}
rank_report_sector_ratio <- function(ratio_rank_object, ratio = NULL){

  error_logger <- crayon::red $ bold
  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  success_logger <- crayon::green $ bold

  if(!'ratio_rank_object' %in% names(ratio_rank_object)){
    cat(error_logger("PLease check data frame! Not a 'Ratio Rank Object'\n"))
    cat(error_logger("FIRST run function"), warning_logger("aikia::rank_peer_ratio()\n"))
    return()
  }

  if(is.null(ratio)){
    # plain all ticker assessments
    all_ranks <- ratio_rank_object %>%
      dplyr::group_by(ticker_yh,name) %>%
      dplyr::summarise(all_counts = sum(assessment), .groups = "drop") %>%
      dplyr::arrange(desc(all_counts)) %>%
      dplyr::mutate(Score = all_counts,
                    count_max = max(all_counts)) %>%
      dplyr::select(ticker_yh,name,Score,all_counts,count_max) %>%
      gt::gt()  %>%
      gt::tab_header(
        title = gt::md(paste0("Comapny's Rank by **Total Scores** within<br>",
                              "<font style='color:red;font-size:30px'> ",unique(ratio_rank_object$sector),"</font>"))) %>%
      gtExtras::gt_plt_bullet(column = all_counts, target = count_max, width = 45,
                              palette = c(aikia::aikia_palette_eight()[4],"black")) %>%
      gt::cols_label(
        ticker_yh = "Ticker",
        name = "Name",
        all_counts = "Score Range") %>%
      gt::cols_align(align = "center") %>%
      aikia::gt_theme_aikia()

    return(all_ranks)
  } else {
    # all ticker assessments by SECTOR
    ratio_rank_object %>%
      dplyr::filter(stringr::str_detect(calc_ratio,!!ratio)) %>%
      tidyr::drop_na(name) %>%
      dplyr::mutate(level_max = max(level),
                    levels = level) %>%
      dplyr::select(ticker_yh,name,levels,rank,level, level_max) %>%
      dplyr::arrange(desc(levels)) %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md(paste0("**Rank Comapnies by Ratio<br>",
                              "<font style='color:red;font-size:30px'> ",ratio,"</font>**<br>"))) %>%
      gtExtras::gt_plt_bullet(column = level, target = level_max, width = 45,
                              palette = c(aikia::aikia_palette_eight()[4],"black")) %>%
      gt::fmt_number(columns = c("levels"),rows = levels > 1 ,decimals = 2) %>%
      gt::fmt_percent(columns = c("levels"),rows = levels < 1 ,decimals = 2) %>%
      gt::cols_label(
        ticker_yh = "Ticker",
        name = "Name",
        rank = "Rank",
        levels = "Level",
        level = "Level Range") %>%
      gt::cols_align(align = "center") %>%v
    aikia::gt_theme_aikia()

  }

}
