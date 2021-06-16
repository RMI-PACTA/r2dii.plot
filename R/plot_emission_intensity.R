#' Create a timeline plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
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
plot_emission_intensity <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data), is.logical(extrapolate))
  crucial <- c(
    "sector", "year", "emission_factor_metric", "emission_factor_value"
  )
  abort_if_missing_names(data, crucial)
  abort_if_multiple(data, "sector")
  abort_if_has_zero_rows(data)

  data <- data %>%
    mutate(emission_factor_metric = to_title(.data$emission_factor_metric))

  prep <- hint_if_missing_names(prep_timeline(data, extrapolate = extrapolate))
  line_names <- unique(prep$line_name)
  specs <- tibble(line_name = line_names, label = line_names) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timeline_impl(prep, specs = specs)
}

prep_timeline <- function(data,
                          value = "emission_factor_value",
                          metric = "emission_factor_metric",
                          extrapolate = FALSE) {
  start_year <- get_common_start_year(data, metric)
  data <- filter(data, .data$year >= start_year)

  out <- data %>%
    mutate(
      line_name = .data[[metric]],
      value = .data[[value]],
      extrapolated = FALSE
    )

  if (extrapolate) {
    max_year <- max(out$year, na.rm = TRUE)

    to_extrapolate <- out %>%
      group_by(.data$line_name) %>%
      arrange(desc(.data$year)) %>%
      dplyr::slice(1) %>%
      filter(.data$year != max_year)

    if (nrow(to_extrapolate) != 0) {
      extrapolated <- to_extrapolate
      extrapolated$year <- max_year
      extrapolated <- bind_rows(to_extrapolate, extrapolated)
      extrapolated$extrapolated <- TRUE

      out <- bind_rows(out, extrapolated)
    }
  }

  out$year <- lubridate::make_date(out$year)
  out
}

get_common_start_year <- function(data, metric) {
  year <- max(
    data %>%
      group_by(.data[[metric]]) %>%
      summarise(year = min(.data$year)) %>%
      pull(.data$year)
  )
  year
}

plot_timeline_impl <- function(data, specs) {
  data <- left_join(data, specs, by = "line_name")

  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data$value,
        colour = forcats::fct_reorder2(.data$label, .data$year, .data$value),
        linetype = .data$extrapolated
      )
    ) +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    scale_linetype_manual(
      values = if (any(data$extrapolated)) c("solid", "dashed") else "solid"
    ) +
    guides(linetype = FALSE) +
    theme_2dii()
}

# TODO: Is this dead code? How can we test it?
abort_if_too_many_lines <- function(data) {
  n_lines <- nrow(data)
  max_n_lines <- 7
  if (n_lines > max_n_lines) {
    abort(glue(
      "Can't plot more than {max_n_lines} lines in one plot. Found {n_lines} lines:
        {toString(data$line_name)}. Consider splitting over multiple plots."
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
