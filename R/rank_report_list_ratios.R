
#' List All Possible Ratios
#'
#' @param ratio_rank_object result file of 'rank_peer_ratio' function
#'
#' @return gt object of possible ratios to use
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples\dontrun{
#'  rank_report_list_ratios(ratio_rank_object = NULL)
#'}
rank_report_list_ratios <- function(ratio_rank_object = NULL){

  error_logger <- crayon::red $ bold
  script_logger <- crayon::cyan $ bold
  warning_logger <- crayon::yellow $ bold
  success_logger <- crayon::green $ bold

  if(!'ratio_rank_object' %in% names(ratio_rank_object)){
    cat(error_logger("PLease check data frame! Not a 'Ratio Rank Object'\n"))
    cat(error_logger("FIRST run function"), warning_logger("aikia::rank_peer_ratio()\n"))
    return()
  }

  ratios <- tibble::tibble(Selection = unique(ratio_rank_object$calc_ratio)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("all possible **RATIOS**<br>"))  %>%
    gt::cols_align(align = "center") %>%
    aikia::gt_theme_aikia()

  return(ratios)
}







