#' Prepare data for a trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @template convert_label
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_trajectory()`), or use
#'   `FALSE` to impose no restriction.
#' @param center_y Logical. Use `TRUE` to center the y-axis around start value
#'   (the default behavior of `qplot_trajectory()`), or use `FALSE` to not
#'   center.
#' @param value_col Character. Name of the column to be used as a value to be
#'   plotted.
#'
#' @seealso [market_share].
#'
#' @return A data-frame ready to be plotted using `plot_trajectory()`.
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#'
#' prep_trajectory(data)
prep_trajectory <- function(data,
                            convert_label = identity,
                            span_5yr = FALSE,
                            center_y = FALSE,
                            value_col = "percentage_of_initial_production_by_scope") {
  out <- data %>%
    prep_common() %>%
    mutate(value = !!as.name(value_col)) %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    out <- span_5yr(out)
  }

  start_year <- min(out$year, na.rm = TRUE)

  cols <- c("year", "metric", "label", "technology", "value", "sector")
  out <- select(out, all_of(cols))

  scenarios <- scenario(out, center_y)
  not_scenarios <- out %>%
    filter(!is_scenario(.data$metric)) %>%
    mutate(value_low = .data$value)
  bind_rows(scenarios, not_scenarios)
}

scenario <- function(data, center_y = FALSE) {
  area_borders <- get_area_borders(data, center_y)

  data_worse_than_scenarios <- tibble(
    year = unique(data$year),
    technology = unique(data$technology),
    sector = unique(data$sector)
  )

  technology_kind <- get_tech_kind(data)

  if (technology_kind == "increasing") {
    data_scenarios <- data %>%
      filter(is_scenario(.data$metric))

    data_worse_than_scenarios$value <- area_borders$lower
    data_worse_than_scenarios$metric <- "target_worse"
    data_worse_than_scenarios$label <- "target_worse"

    data_scenarios <- bind_rows(data_scenarios, data_worse_than_scenarios)

    data_scenarios <- data_scenarios %>%
      group_by(.data$year, .data$technology, .data$sector) %>%
      mutate(metric = factor(.data$metric,
                             levels = rev(get_ordered_scenarios(data_scenarios))
      )) %>%
      arrange(.data$year, .data$metric) %>%
      rename(value_low = "value") %>%
      mutate(value = lead(.data$value_low,
                          n = 1,
                          default = area_borders$upper
      ))
  } else {
    data_worse_than_scenarios$value <- area_borders$upper
    data_worse_than_scenarios$metric <- "target_worse"
    data_worse_than_scenarios$label <- "target_worse"

    data_scenarios <- data %>%
      filter(is_scenario(.data$metric))

    data_scenarios <- bind_rows(data_scenarios, data_worse_than_scenarios)

    data_scenarios <- data_scenarios %>%
      group_by(.data$year, .data$technology, .data$sector) %>%
      mutate(
        metric = factor(
          .data$metric,
          levels = rev(get_ordered_scenarios(data_scenarios))
        )
      ) %>%
      arrange(.data$year, .data$metric) %>%
      mutate(value_low = lag(.data$value, n = 1, default = area_borders$lower))
  }

  data_scenarios
}

get_area_borders <- function(data, center_y = FALSE) {
  lower <- min(data$value, na.rm = TRUE)
  upper <- max(data$value, na.rm = TRUE)
  span <- upper - lower
  lower <- lower - 0.1 * span
  upper <- upper + 0.1 * span

  upper_distance <- distance_from_start_value(data, upper) / span
  lower_distance <- distance_from_start_value(data, lower) / span

  if (center_y) {
    # Center the starting point of the lines
    distance <- abs(upper_distance - lower_distance)
    max_distance <- 0.1
    if (distance > max_distance) {
      lower <- lower - max(0, upper_distance - lower_distance) * span
      upper <- upper + max(0, lower_distance - lower_distance) * span
    }
  }
  list(lower = lower, upper = upper)
}

get_tech_kind <- function(data) {
  technology_kind <- r2dii.data::increasing_or_decreasing %>%
    filter(.data$technology == unique(data$technology)) %>%
    pull(.data$increasing_or_decreasing) %>%
    unique()

  technology_kind
}

distance_from_start_value <- function(data, value) {
  abs(value - start_value_portfolio(data))
}

start_value_portfolio <- function(data) {
  data %>%
    filter(.data$year == min(data$year, na.rm = TRUE), is_portfolio(.data$metric)) %>%
    pull(.data$value)
}
