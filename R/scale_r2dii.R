#' Custom 2DII colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII palette.
#'
#' @param labels A character vector. Specifies colour labels to use and their
#'   order. Run `unique(r2dii.plot:::palette_colours$label)` to see available
#'   labels. Similar to `value` parameter in `ggplot::scale_colour_manual()`.
#' @param ... Other parameters passed on to `ggplot2::discrete_scale()`.
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @aliases scale_color_r2dii
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(data = mpg) +
#'  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'  scale_colour_r2dii()
#'
#' ggplot(data = mpg) +
#'  geom_histogram(mapping = aes(x = cyl, fill = class), position = "dodge") +
#'  scale_fill_r2dii()
scale_colour_r2dii <- function(labels = NULL, ...) {
  discrete_scale("colour", "r2dii", r2dii_pal(labels), ...)
}

#' @rdname scale_colour_r2dii
#' @export
scale_fill_r2dii <- function(labels = NULL, ...) {
  discrete_scale("fill", "r2dii", r2dii_pal(labels), ...)
}

#' @noRd
r2dii_pal <- function(labels = NULL) {
  labels <- labels %||% palette_colours$label
  values <- tibble(label = labels) %>%
    inner_join(palette_colours, by = "label") %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}
