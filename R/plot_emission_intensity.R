#' Create an emission intensity plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#'
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda, sector == "cement")
#' plot_emission_intensity(data)
plot_emission_intensity <- function(data) {
  check_plot_emission_intensity(data)

  prep <- prep_emission_intensity(data)
  plot_emission_intensity_impl(prep)
}

check_plot_emission_intensity <- function(data, env = parent.frame()) {
  stopifnot(is.data.frame(data))
  crucial <- c("sector", "year", glue("emission_factor_{c('metric', 'value')}"))
  hint_if_missing_names(abort_if_missing_names(data, crucial), "sda")
  enforce_single_value <- "sector"
  abort_if_multiple(data, enforce_single_value)
  abort_if_has_zero_rows(data, env = env)
  abort_if_too_many_lines(data)

  invisible(data)
}

prep_emission_intensity <- function(data) {
  prep <- data %>%
    beautify("emission_factor_metric") %>%
    drop_rows_before_sart_year("emission_factor_metric") %>%
    mutate(year = lubridate::make_date(.data$year))

  metrics <- distinct(prep, .data$emission_factor_metric)
  colours <- palette_colours[seq_len(nrow(metrics)), "hex", drop = FALSE]
  specs <- dplyr::bind_cols(metrics, colours)

  left_join(prep, specs, by = "emission_factor_metric")
}

plot_emission_intensity_impl <- function(data) {
  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data$emission_factor_value,
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
  forcats::fct_reorder2(
    data$emission_factor_metric,
    data$year,
    data$emission_factor_value
  )
}

abort_if_too_many_lines <- function(data) {
  lines <- unique(data$emission_factor_metric)
  n <- length(lines)
  max <- 7
  if (n > max) {
    abort(c(
      glue("Can't plot more than {max} lines in one plot."),
      x = glue("Found {n} lines: {toString(lines)}."),
      i = "Do you need to split the data over multiple plots?"
    ))
  }

  invisible(data)
}
