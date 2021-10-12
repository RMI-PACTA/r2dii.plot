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
#' @aliases scale_color_r2dii_tech
#' @family r2dii scales
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(mpg) +
#'   geom_point(aes(displ, hwy, color = class)) +
#'   scale_colour_r2dii_tech("automotive")
#'
#' ggplot(mpg) +
#'   geom_histogram(aes(cyl, fill = class), position = "dodge", bins = 5) +
#'   scale_fill_r2dii_tech("automotive")
scale_colour_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("colour", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

#' @rdname scale_colour_r2dii_tech
#' @export
scale_fill_r2dii_tech <- function(sector, technologies = NULL, ...) {
  discrete_scale("fill", "r2dii_tech", r2dii_tech_pal(sector, technologies), ...)
}

r2dii_tech_pal <- function(sector, technologies = NULL) {
  check_sector(sector)
  check_technologies(sector, technologies)

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

check_sector <- function(sector) {
  available_sectors <- unique(technology_colours$sector)
  if (!(sector %in% available_sectors)) {
    abort(
      c(glue("`sector` must be one of sectors in technology_colours data set."),
        i = glue("Run `unique(r2dii.plot:::technology_colours$sector)` to see a list of available sectors:
                 {toString(available_sectors)}."),
        x = glue("You passed: {sector}.")
      )
    )
  }
}

check_technologies <- function(sector, technologies) {
  available_technologies <- unique(technology_colours[technology_colours$sector == sector,]$technology)
  if(!is.null(technologies)) {
    if (!all((technologies %in% available_technologies))) {
      bad_technologies <- sort(setdiff(technologies, available_technologies))
      abort(
        c(glue("`technologies` must be technologies from technology_colours data set for the given `sector`."),
          i = "Run `unique(r2dii.plot:::technology_colours$technology)` to see all available technologies.",
          i = glue("Technologies for the {sector} are: {toString(available_technologies)}."),
          x = glue("You passed: {toString(bad_technologies)}.")
        )
      )
    }
  }
}

