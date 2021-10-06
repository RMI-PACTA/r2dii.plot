#' Custom 2DII technology colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII technology
#' palette.
#'
#' @param sector A string. Sector name specifying a colour palette. Run
#'   `unique(r2dii.plot:::technology_colours$sector)` to see available sectors.
#' @param technologies A character vector. Specifies technologies to use as
#'   colours and their order. Run
#'   `unique(r2dii.plot:::technology_colours$technology)` to see available
#'   technologies (pay attention if they match the `sector`). Similar to `value`
#'   parameter in `ggplot::scale_colour_manual()`.
#' @param ... Other parameters passed on to `ggplot2::discrete_scale()`.
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(data = mpg) +
#'  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'  scale_colour_r2dii_tech("automotive")
#'
#'  ggplot(data = mpg) +
#'  geom_histogram(mapping = aes(x = cyl, fill = class), position = "dodge") +
#'  scale_fill_r2dii_tech("automotive")
scale_colour_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("colour", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

#' @rdname scale_colour_r2dii_tech
scale_color_r2dii_tech <- scale_colour_r2dii_tech

#' @rdname scale_colour_r2dii_tech
scale_fill_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("fill", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

#' @noRd
r2dii_tech_pal <- function(sector, technologies = NULL) {
  technologies <- technologies %||%
    technology_colours$technology
  values <- tibble(technology = technologies) %>%
    inner_join(
      technology_colours %>% filter(.data$sector == .env$sector),
      by = "technology") %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

