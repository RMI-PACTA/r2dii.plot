#' Custom 2DII colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII palette.
#'
#' @param colour_labels A character vector. Specifies colour labels to use and their
#'   order. Run `unique(r2dii.plot:::palette_colours$label)` to see available
#'   colours. Similar to `value` parameter in [ggplot2::scale_colour_manual()].
#' @param ... Other parameters passed on to [ggplot2::discrete_scale()].
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @aliases scale_color_r2dii
#' @family r2dii scales
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(mpg) +
#'   geom_point(aes(displ, hwy, color = class)) +
#'   scale_colour_r2dii()
#'
#' ggplot(mpg) +
#'   geom_histogram(aes(cyl, fill = class), position = "dodge", bins = 5) +
#'   scale_fill_r2dii()
scale_colour_r2dii <- function(colour_labels = NULL, ...) {
  discrete_scale("colour", "r2dii", r2dii_pal(colour_labels), ...)
}

#' @rdname scale_colour_r2dii
#' @export
scale_fill_r2dii <- function(colour_labels = NULL, ...) {
  discrete_scale("fill", "r2dii", r2dii_pal(colour_labels), ...)
}

r2dii_pal <- function(colour_labels = NULL) {
  abort_if_unknown_values(colour_labels, palette_colours, column = "label")
  r2dii_pal_impl(colour_labels, column = "label", data = palette_colours)
}
