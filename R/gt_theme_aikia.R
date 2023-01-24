
#' aikia gt theme
#'
#' @param gt_object a \code{gt}-object
#'
#' @return a customized gt object
#' @export
#'
#' @examples\dontrun{
#' dplyr::tibble(a = c(1:4), b = c(5:8)) %>%
#' gt::gt() %>%
#' gt_theme_aikia()
#'}
gt_theme_aikia <- function(gt_object){

  gt_object %>%
    gt::tab_style(style = list(gt::cell_borders(sides = "bottom",
                                                color = "black",
                                                weight = gt::px(3))),
                  locations = list(gt::cells_column_labels(
                    columns = gt::everything()))) %>%
    gt::opt_row_striping() %>%
    gt::opt_table_outline(color = '#484B42') %>%
    gt::tab_options(table.background.color = scales::alpha("#CAC2B0",0.2), # obam beige
                    table.font.size = 12,
                    column_labels.font.weight = "bold",
                    column_labels.background.color = '#484B42', # obam grey
                    row.striping.background_color = scales::alpha(aikia::aikia_palette_eight()[3],0.2),
                    table.border.left.color = "#484B42",
                    table.border.left.width = 1)

}







