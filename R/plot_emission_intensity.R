#' Create an emission intensity plot
#'
#' @param data A data frame like the output of `prep_emission_intensity()`.
#'
#' @seealso [sda_demo].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # plot with `qplot_emission_intensity()` parameters
#' data <- subset(sda_demo, sector == "cement" & region == "global")
#' data <- prep_emission_intensity(data, span_5yr = TRUE, convert_label = to_title)
#'
#' plot_emission_intensity(data)
plot_emission_intensity <- function(data) {
  check_plot_emission_intensity(data, env = list(data = substitute(data)))

  metrics <- distinct(data, .data$emission_factor_metric)
  colours <- palette_colours[seq_len(nrow(metrics)), "hex", drop = FALSE]
  specs <- dplyr::bind_cols(metrics, colours)

  data <- left_join(data, specs, by = metric(data))

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

check_plot_emission_intensity <- function(data, env) {
  stopifnot(is.data.frame(data))
  crucial <- c(prep_emission_factor_crucial, "label")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "sda_demo")
  enforce_single_value <- "sector"
  abort_if_multiple(data, enforce_single_value)
  abort_if_has_zero_rows(data, env = env)
  abort_if_too_many_lines(data, max = 7)

  invisible(data)
}
