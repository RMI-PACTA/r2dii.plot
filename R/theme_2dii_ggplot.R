#' Returns a custom 2dii ggplot theme
#'
#' Returns a ggplot theme which can be applied to all graphs to appear according
#' to 2DII plotting aesthetics, like removed gridlines, grey axis lines etc.
#'
#' @param font_family argument specifying the font that should be used in a
#'   graph (character string taking the same values as standard ggplot font
#'   families).
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' class(theme_2dii_ggplot())
#'
#' ggplot(mtcars) +
#'   geom_histogram(aes(mpg)) +
#'   theme_2dii_ggplot()
theme_2dii_ggplot <- function(font_family = "Helvetica") {
  font_size_ticks <- 10
  font_size_axis_titles <- 12
  supporting_elts_color <- "#C0C0C0"

  theme_classic() %+replace%
    theme(
      plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
      axis.line = element_line(colour = supporting_elts_color),
      axis.ticks = element_line(colour = supporting_elts_color),
      plot.title = element_text(
        hjust = 0.5, vjust = 0.5, face = "bold",
        family = font_family, size = 14,
        margin = margin(20, 2, 12, 2)
      ),
      axis.text = element_text(
        family = font_family, size = font_size_ticks,
        margin = margin(5, 5, 5, 5)
      ),
      axis.title = element_text(
        family = font_family,
        size = font_size_axis_titles,
        margin = margin(5, 5, 5, 5)
      ),
      legend.text = element_text(
        family = font_family, size = 9,
        margin = margin(5, 5, 5, 5)
      ),
      legend.title = element_blank()
    )
}
