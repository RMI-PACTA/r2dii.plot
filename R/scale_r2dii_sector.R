#' Custom 2DII sector colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII sector
#' palette.
#'
#' @param sectors A character vector. Specifies sector colours to use and their
#'   order. Run `unique(r2dii.plot:::sector_colours$label)` to see available
#'   labels. Similar to `value` parameter in `ggplot::scale_colour_manual()`.
#' @param ... Other parameters passed on to `ggplot2::discrete_scale()`.
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @aliases scale_color_r2dii_sector
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(data = mpg) +
#'  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'  scale_colour_r2dii_sector()
#'
#'  ggplot(data = mpg) +
#'  geom_histogram(mapping = aes(x = cyl, fill = class), position = "dodge", bins = 5) +
#'  scale_fill_r2dii_sector()
scale_colour_r2dii_sector <- function(sectors = NULL, ...) {
  discrete_scale("colour", "r2dii_sector", r2dii_sec_pal(sectors), ...)
}

#' @rdname scale_colour_r2dii_sector
#' @export
scale_fill_r2dii_sector <- function(sectors = NULL, ...) {
  discrete_scale("fill", "r2dii_sector", r2dii_sec_pal(sectors), ...)
}

#' @noRd
r2dii_sec_pal <- function(sectors = NULL) {
  check_sectors(sectors)
  r2dii_pal_impl(sectors, data = sector_colours, column = "label")
}

check_sectors <- function(sectors) {
  available_sectors <- unique(sector_colours$label)
  if(!is.null(sectors)) {
    if (!all((sectors %in% available_sectors))) {
      bad_sectors <- sort(setdiff(sectors, available_sectors))
      abort(
        c(glue("`sectors` must be in sector_colours data set."),
          i = glue("Run `unique(r2dii.plot:::sector_colours$label)` to see all available sectors:
            {toString(available_sectors)}."),
          x = glue("You passed: {toString(bad_sectors)}.")
        )
      )
    }
  }
}

