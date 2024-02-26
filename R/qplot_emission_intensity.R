#' Create a quick emission intensity plot
#'
#' @inherit plot_emission_intensity
#' @seealso plot_emission_intensity
#'
#' @description
#' Compared to [plot_emission_intensity()] this function:
#' * is restricted to plotting future as 5 years from the start year,
#' * outputs formatted labels, based on emission metric column,
#' * outputs a title,
#' * outputs formatted axis labels.
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda, sector == "cement" & region == "global")
#'
#' qplot_emission_intensity(data)
qplot_emission_intensity <- function(data) {
  env <- list(data = substitute(data))
  check_prep_emission_intensity(
    data,
    convert_label = to_title,
    span_5yr = TRUE,
    env = env
  )

  data <- prep_emission_intensity(
    data,
    convert_label = to_title,
    span_5yr = TRUE
  )

  check_plot_emission_intensity(data, env = env)

  data %>%
    plot_emission_intensity() %>%
    labs_emission_intensity()
}

labs_emission_intensity <- function(p) {
  sector <- tools::toTitleCase(p[["data"]][["sector"]][[1]])

  p + labs(
    title = glue("Emission Intensity Trend for the {sector} Sector"),
    x = "Year",
    y = "Tons of CO2 per Ton of Production Unit"
  )
}
