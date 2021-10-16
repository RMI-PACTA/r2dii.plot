#' Custom 2DII sector colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII sector
#' palette.
#'
#' @param sectors A character vector. Specifies sector colours to use and their
#'   order. Run `unique(r2dii.plot:::sector_colours$label)` to see available
#'   labels. Similar to `value` parameter in [ggplot2::scale_colour_manual()].
#' @param ... Other parameters passed on to [ggplot2::discrete_scale()].
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @aliases scale_color_r2dii_sector
#' @family r2dii scales
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(mpg) +
#'   geom_point(aes(displ, hwy, color = class)) +
#'   scale_colour_r2dii_sector()
#'
#' ggplot(mpg) +
#'   geom_histogram(aes(cyl, fill = class), position = "dodge", bins = 5) +
#'   scale_fill_r2dii_sector()
scale_colour_r2dii_sector <- function(sectors = NULL, ...) {
  discrete_scale("colour", "r2dii_sector", r2dii_sec_pal(sectors), ...)
}

#' @rdname scale_colour_r2dii_sector
#' @export
scale_fill_r2dii_sector <- function(sectors = NULL, ...) {
  discrete_scale("fill", "r2dii_sector", r2dii_sec_pal(sectors), ...)
}

r2dii_sec_pal <- function(sectors = NULL) {
  abort_if_unknown_values(sectors, data = sector_colours, column = "label")
  r2dii_pal_impl(sectors, data = sector_colours, column = "label")
}
