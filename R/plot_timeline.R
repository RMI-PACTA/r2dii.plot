#' Create a timeline plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
#'
#' @family functions with minimal arguments
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # `data` must meet documented "Requirements"
#' data <- filter(sda, sector == "cement")
#' plot_timeline(data)
plot_timeline <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data))
  abort_if_has_cero_rows(data)
  prep <- hint_if_missing_names(prep_timeline(data, extrapolate = extrapolate))

  plot_timelineB(prep)
}

prep_timeline <- function(data,
                          value = "emission_factor_value",
                          metric = "emission_factor_metric",
                          extrapolate = FALSE) {
  browser()
  check_prep_timeline(data, value, metric, extrapolate)
  abort_if_multiple(data, "sector")

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

check_prep_timeline <- function(data, value, metric, extrapolate) {
  stopifnot(
    is.data.frame(data),
    is.character(value),
    is.character(metric),
    is.logical(extrapolate)
  )
  abort_if_missing_names(data, c("sector", "year", metric, value))

  invisible(data)
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

plot_timelineB <- function(data) {
  abort_if_missing_names(data, "line_name")
  specs <- tibble(line_name = unique(data$line_name), label = line_name) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineY(data = data, specs = specs)
}

plot_timelineY <- function(data, specs) {
  abort_if_multiple(data, "sector")
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
  n_colours <- nrow(palette_colours)
  if (n_lines > n_colours) {
    abort(glue(
      "Can't plot more than {n_colours} lines. Found {n_lines} lines:
        {toString(data$line_name)}."
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
