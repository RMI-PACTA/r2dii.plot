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
  stopifnot(is.data.frame(data))
  crucial <- c(
    "sector", "year", "emission_factor_metric", "emission_factor_value"
  )
  abort_if_missing_names(data, crucial)
  abort_if_multiple(data, "sector")
  abort_if_has_zero_rows(data)

  data <- data %>%
    mutate(emission_factor_metric = to_title(.data$emission_factor_metric))

  prep <- hint_if_missing_names(prep_emission_intensity(data))
  line_names <- unique(prep$line_name)
  specs <- tibble(line_name = line_names, label = line_names) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_emission_intensity_impl(prep, specs = specs)
}

prep_emission_intensity <- function(data,
                                    value = "emission_factor_value",
                                    metric = "emission_factor_metric") {
  data %>%
    drop_before_start_year(metric) %>%
    mutate(
      line_name = .data[[metric]],
      value = .data[[value]],
      year = lubridate::make_date(.data$year)
    )
}

plot_emission_intensity_impl <- function(data, specs) {
  data <- left_join(data, specs, by = "line_name")

  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data$value,
        colour = forcats::fct_reorder2(.data$label, .data$year, .data$value)
      )
    ) +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    scale_linetype_manual(values = "solid") +
    guides(linetype = "none") +
    theme_2dii()
}

abort_if_too_many_lines <- function(data) {
  n_lines <- nrow(data)
  max_n_lines <- 7
  if (n_lines > max_n_lines) {
    abort(glue(
      "Can't plot more than {max_n_lines} lines in one plot.
      Found {n_lines} lines: {toString(data$line_name)}.
      Consider splitting the data over multiple plots."
    ))
  }

  invisible(data)
}

add_r2dii_colours <- function(specs) {
  n <- seq_len(nrow(specs))
  specs$r2dii_colour_name <- palette_colours$label[n]

  specs %>%
    left_join(palette_colours, by = c("r2dii_colour_name" = "label")) %>%
    select(-.data$r2dii_colour_name)
}
