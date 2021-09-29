#' Colour scale
#'
#' A custom ggplot colour scale that can be applied to all graphs with colour
#' aesthetic specified by a discrete variable to apply colours corresponding to
#' 2DII technology colour palette.
#'
#' @param sector A string. Sector name which technology colour palette should be
#'   used. Run `unique(r2dii.plot:::technology_colours$sector)` to see available
#'   sectors.
#' @param technologies A character vector. Selected technologies which colours should be used
#' @inheritParams ggplot2::discrete_scale
#'
#' @return
#' @export
#'
#' @examples
scale_colour_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("colour", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

scale_color_r2dii_tech <- scale_colour_r2dii_tech

scale_fill_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("fill", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

r2dii_tech_pal <- function(sector, technologies = NULL) {
  technologies <- technologies %||%
    technology_colours$technology
  values <- tibble(technology = technologies) %>%
    inner_join(
      technology_colours %>% filter(.data$sector == .env$sector),
      by = "technology") %>%
    pull(hex)
  max_n <- length(values)
  f <- scales:::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

