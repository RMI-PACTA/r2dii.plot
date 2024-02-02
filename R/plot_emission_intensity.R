#' Create an emission intensity plot
#'
#' @param data A data frame like the output of `prep_emission_intensity()`.
#'
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # plot with `qplot_emission_intensity()` parameters
#' data <- subset(sda, sector == "cement" & region == "global") %>%
#'   prep_emission_intensity(span_5yr = TRUE, convert_label = to_title)
#'
#' plot_emission_intensity(data)
plot_emission_intensity <- function(data) {
  env <- list(data = substitute(data))
  check_emission_intensity(data, env = env)
  plot_emission_intensity_impl(data)
}

plot_emission_intensity_impl <- function(data) {
  ggplot(
    data = data,
    aes(
      x = .data$year,
      y = .data$emission_factor_value,
      colour = .data$label
    )
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    theme_2dii()
}
