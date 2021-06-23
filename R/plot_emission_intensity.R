#' Create an emission intensity plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @param convert_label A symbol. The unquoted name of a function to apply to
#'   legend labels. For example, to convert labels to
#'   uppercase use `convert_label = toupper`.
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda, sector == "cement")
#' plot_emission_intensity(data)
#' plot_emission_intensity(data, convert_label = toupper)
plot_emission_intensity <- function(data, convert_label = format_label) {
  ggplot_emission_intensity(data, convert_label = convert_label)
}

check_plot_emission_intensity <- function(data, env = parent.frame()) {
  stopifnot(is.data.frame(data))

  template <- sda
  crucial <- c("sector", "year", metric(template), emission_factor(template))
  hint_if_missing_names(abort_if_missing_names(data, crucial), "sda")

  enforce_single_value <- "sector"
  abort_if_multiple(data, enforce_single_value)

  abort_if_has_zero_rows(data, env = env)
  abort_if_too_many_lines(data, max = 7)

  invisible(data)
}

prep_emission_intensity <- function(data, convert_label) {
  prep <- data %>%
    prep_common() %>%
    mutate(year = lubridate::make_date(.data$year)) %>%
    mutate(label = convert_label(.data$label))

  labels <- distinct(prep, .data[[metric(prep)]])
  colours <- palette_colours[seq_len(nrow(labels)), "hex", drop = FALSE]
  specs <- dplyr::bind_cols(labels, colours)

  left_join(prep, specs, by = metric(prep))
}

plot_emission_intensity_impl <- function(data) {
  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data[[emission_factor(data)]],
        colour = match_lines_order(data)
      )
    ) +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    theme_2dii()
}

match_lines_order <- function(data) {
  # TODO: Ask if we should use data$metric instead. What would happen if `label`
  # holds a lump version of `metric`? e.g. metric: a, b, c; label: a, other.
  forcats::fct_reorder2(
    data$label, data$year, data[[emission_factor(data)]]
  )
}
