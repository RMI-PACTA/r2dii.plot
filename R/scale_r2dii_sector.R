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
  check_sectors(sectors)

  sectors <- sectors %||%
    sector_colours$label
  values <- tibble(label = sectors) %>%
    inner_join(
      sector_colours,
      by = "label") %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
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

