#' Custom PACTA technology colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from the PACTA technology
#' palette.
#'
#' @param sector A string. Sector name specifying a colour palette. Run
#'   `unique(r2dii.plot:::technology_colours$sector)` to see available sectors.
#' @param technologies A character vector. Specifies technologies to use as
#'   colours and their order. Run
#'   `unique(r2dii.plot:::technology_colours$technology)` to see available
#'   technologies (pay attention if they match the `sector`). Similar to `value`
#'   parameter in [ggplot2::scale_colour_manual()].
#' @param ... Other parameters passed on to [ggplot2::discrete_scale()].
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
  if (utils::packageVersion("ggplot2") >= numeric_version("3.5.0")) {
    discrete_scale(
      aesthetics = "colour",
      palette = r2dii_tech_pal(sector, technologies),
      ...
    )
  } else {
    discrete_scale(
      aesthetics = "colour",
      scale_name = "r2dii_tech",
      palette = r2dii_tech_pal(sector, technologies),
      ...
    )
  }
}

#' @rdname scale_colour_r2dii_tech
#' @export
scale_fill_r2dii_tech <- function(sector, technologies = NULL, ...) {
  if (utils::packageVersion("ggplot2") >= numeric_version("3.5.0")) {
    discrete_scale(
      aesthetics = "fill",
      palette = r2dii_tech_pal(sector, technologies),
      ...
    )
  } else {
    discrete_scale(
      aesthetics = "fill",
      scale_name = "r2dii_tech",
      palette = r2dii_tech_pal(sector, technologies),
      ...
    )
  }
}

r2dii_tech_pal <- function(sector, technologies = NULL) {
  abort_if_unknown_values(sector, technology_colours, "sector")

  some_sector <- sector # nudge users to replace `some_sector` with their own
  abort_if_unknown_values(
    technologies,
    # This expression appears in the error message
    data = filter(technology_colours, .data$sector == some_sector),
    column = "technology"
  )

  data <- filter(technology_colours, .data$sector == .env$sector)
  r2dii_pal_impl(technologies, data, column = "technology")
}
