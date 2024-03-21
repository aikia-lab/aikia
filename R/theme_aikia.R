
#' aikia ggplot2 theme
#'
#' @family aikia themes
#' @param base_size double
#' @param base_family character
#' @param base_line_size double
#' @param base_rect_size double
#'
#' @return function
#' @export
#' @importFrom ggplot2 %+replace%
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Length)) +
#' geom_histogram()+
#' theme_aikia()
theme_aikia <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170
){
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace% ggplot2::theme(
    plot.title = ggtext::element_markdown(
      colour = aikia_main(),
  #    face = "bold",
      size = base_size + 24,
      hjust = 0,
      vjust = 2,
      lineheight = 0.9
    ),
    plot.subtitle = ggtext::element_markdown(
      colour = aikia_main_light(),
  #    face = "bold",
      size = base_size + 9,
      hjust = 0,
      vjust = 2,
      lineheight = 0.9
    ),
    axis.title = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size + 3,
      #hjust = 0,
      lineheight = 0.9
    ),
    legend.text = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 1,
      #hjust = 0,
      lineheight = 0.9
    ),
    legend.title = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size + 3,
      #hjust = 0,
      lineheight = 0.9
    ),
    plot.caption = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 2,
      hjust = 1
    ),
    # Facet Wrap Title
    strip.text = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 2,
      vjust = 1,
      lineheight = 0.9,
      margin = ggplot2::margin(4,0,4,0, unit = "pt")
    ),
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = NA),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = aikia_main()),
    #axis.line = ggplot2::element_line(colour = main_color),
    axis.line.x.top = ggplot2::element_blank(),
    complete = TRUE)

}


#' aikia ggplot2 theme light
#'
#' @family aikia themes
#' @param base_size double
#' @param base_family character
#' @param base_line_size double
#' @param base_rect_size double
#'
#' @return function
#' @export
#' @importFrom ggplot2 %+replace%
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Length)) +
#' geom_histogram()+
#' theme_aikia_light()
theme_aikia_light <- function(
    base_size = 11,
    base_family = "",
    base_line_size = base_size / 170,
    base_rect_size = base_size / 170
){
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace% ggplot2::theme(
    plot.title = ggtext::element_markdown(
      colour = aikia_main(),
      #    face = "bold",
      size = base_size + 24,
      hjust = 0,
      vjust = 2,
      lineheight = 0.9
    ),
    plot.subtitle = ggtext::element_markdown(
      colour = aikia_main_light(),
      #    face = "bold",
      size = base_size + 9,
      hjust = 0,
      vjust = 2,
      lineheight = 0.9
    ),
    axis.title = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size + 3,
      #hjust = 0,
      lineheight = 0.9
    ),
    legend.text = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 1,
      #hjust = 0,
      lineheight = 0.9
    ),
    legend.title = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size + 3,
      #hjust = 0,
      lineheight = 0.9
    ),
    plot.caption = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 2,
      hjust = 1
    ),
    # Facet Wrap Title
    strip.text = ggplot2::element_text(
      colour = aikia_main(),
      face = "bold",
      size = base_size - 2,
      vjust = 1,
      lineheight = 0.9,
      margin = ggplot2::margin(4,0,4,0, unit = "pt")
    ),
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = NA),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = aikia_main()),
    #axis.line = ggplot2::element_line(colour = main_color),
    axis.line.x.top = ggplot2::element_blank(),
    complete = TRUE)

}


#' @name aikia_main
#' @title aikia colours
#' @description Collection of aikia colors
#' @return The functions return the hexcodes of colours
#' @export
#'
#' @examples
#' aikia_main()
aikia_main <- function(){"#248A8A"}

#' @name aikia_main_light
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_main_light()
aikia_main_light <- function(){"#61B7B7"}

#' @name aikia_secondary
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_secondary()
aikia_secondary <- function(){"#374B9D"}

#' @name aikia_palette_main
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_palette_main()
aikia_palette_main <- function(){c("#248A8A", "#374B9D", "#E6B33C", "#E6893C")}

#' @name aikia_palette_eight
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_palette_eight()
aikia_palette_eight <- function(){c("#0F7B7B",
                   "#20358B",
                   "#CC9719",
                   "#CC6A19",
                   "#61B7B7",
                   "#7484C6",
                   "#FFDB88",
                   "#FFBE88")}

#' @name aikia_palette_light
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_palette_light()
aikia_palette_light <- function(){c("#61B7B7",
                   "#7484C6",
                   "#FFDB88",
                   "#FFBE88")}

#' @name aikia_palette_eleven
#' @rdname aikia_main
#' @export
#' @examples
#' aikia_palette_eleven()
aikia_palette_eleven <- function(){c("#1F525B",
                   "#4964AB",
                   "#707070",
                   "#6893A1",
                   "#976021",
                   "#95D0DB",
                   "#9BABD4",
                   "#B8B8B8",
                   "#A7C0C8",
                   "#E5BB8A",
                   "#1A444C")}
# Scale Functions ----------------------------------------------------------

#' @name scale_fill_aikia_four
#' @family aikia themes
#' @title aikia scales
#' @description Collection of aikia ggplot scale functions
#' @return The functions return ggplot scales
#' @export
#'
#' @examples \dontrun{scale_fill_aikia_four()}
#'
scale_fill_aikia_four <- function(){
  ggplot2::scale_fill_manual(values = aikia_palette_main())
}


#' @name scale_fill_aikia_eight
#' @rdname scale_fill_aikia_four
#' @export
#' @examples
#' \dontrun{scale_fill_aikia_eight()}
scale_fill_aikia_eight <- function(){
  ggplot2::scale_fill_manual(values = aikia_palette_eight())
}

#' @name scale_fill_aikia_eleven
#' @rdname scale_fill_aikia_four
#' @export
#' @examples
#' \dontrun{scale_fill_aikia_eleven()}
scale_fill_aikia_eleven <- function(){
  ggplot2::scale_fill_manual(values = aikia_palette_eleven())
}

#' @name scale_color_aikia_four
#' @rdname scale_fill_aikia_four
#' @export
#' @examples
#' \dontrun{scale_color_aikia_four()}
scale_color_aikia_four <- function(){
  ggplot2::scale_color_manual(values = aikia_palette_main())
}

#' @name scale_color_aikia_eight
#' @rdname scale_fill_aikia_four
#' @export
#' @examples
#' \dontrun{scale_color_aikia_eight()}
scale_color_aikia_eight <- function(){
  ggplot2::scale_color_manual(values = aikia_palette_eight())
}



#' Save a ggplot for LinkedIn
#'
#' @param plot a ggplot object
#' @param filename the path and filename to write to
#' @param format the output format
#'
#' @return saves the plot to the specified path
#' @export
#'
#' @examples
#' \dontrun{
#' p <- iris %>%
#' ggplot(aes(Sepal.Length, Sepal.Width)) +
#' geom_point(aes(color = Species)) +
#' theme_aikia() +
#' scale_color_aikia_four()
#'
#' save_linkedin(p, "testplot")}
save_linkedin <- function(plot, filename, format = "png") {
  ggplot2::ggsave(
    plot = plot,
    filename = stringr::str_c(filename, ".", format),
    width = 1142,
    height = 682,
    units = "px",
    scale = 3
  )
}
