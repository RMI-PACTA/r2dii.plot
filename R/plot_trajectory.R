#' Create a trajectory plot
#'
#' @param data A data frame like the outputs of `prep_trajectory()`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @param center_y Logical. Use `TRUE` to center the y-axis around start value
#'   (the default behavior of `qplot_trajectory()`), or use `FALSE` to not
#'   center.
#' @param perc_y_scale Logical. `FALSE` defaults to using no label conversion.
#'   Use `TRUE` to convert labels on y-axis to percentage using
#'   `scales::percent` (the default behavior of `qplot_trajectory()`).
#'
#' @seealso [market_share].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # plot with `qplot_trajectory()` parameters
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' ) %>%
#'   prep_trajectory()
#'
#' plot_trajectory(
#'   data,
#'   center_y = TRUE,
#'   perc_y_scale = TRUE
#' )
plot_trajectory <- function(data,
                            center_y = FALSE,
                            perc_y_scale = FALSE) {
  env <- list(data = substitute(data))
  check_plot_trajectory(data, env = env)

  start_year <- min(data$year, na.rm = TRUE)

  cols <- c("year", "metric", "label", "technology", "value", "sector")
  data <- select(data, all_of(cols))

  scenarios <- scenario(data, center_y)
  not_scenarios <- data %>%
    filter(!is_scenario(.data$metric)) %>%
    mutate(value_low = .data$value)

  data <- bind_rows(scenarios, not_scenarios)

  stopifnot(is.logical(perc_y_scale))

  p <- ggplot(order_trajectory(data), aes(x = .data$year, y = .data$value))

  scenarios <- data %>% filter(is_scenario(metric))
  p <- p + geom_ribbon(
    data = scenarios,
    aes(
      ymin = .data$value_low,
      ymax = .data$value,
      fill = factor(
        .data$metric,
        levels = scenario_colour(scenarios)$scenario
      ),
      alpha = 0.9
    )
  )

  p <- p + geom_line(
    data = order_trajectory(data),
    aes(linetype = .data$metric, color = .data$metric)
  )

  lines_end <- filter(order_trajectory(data), .data$year == max(data$year))
  year_span <- max(data$year, na.rm = TRUE) - min(data$year, na.rm = TRUE)
  p <- p + ggrepel::geom_text_repel(
    data = lines_end,
    aes(
      y = .data$value,
      label = .data$label,
      segment.color = .data$metric
    ),
    direction = "y",
    color = "black",
    size = 3.5,
    alpha = 1,
    nudge_x = if_else(
      is_scenario(lines_end$metric),
      0.06 * year_span,
      0.01 * year_span
    ),
    nudge_y = if_else(
      is_scenario(lines_end$metric),
      0.01 * value_span(data),
      0
    ),
    hjust = 0,
    segment.size = if_else(is_scenario(lines_end$metric), 0.4, 0),
    xlim = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE) + 0.7 * year_span)
  )

  p <- p +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_x_continuous(breaks = integer_breaks()) +
    scale_fill_manual(values = scenario_colour(data)$colour) +
    # Calling `scale_fill_manual()` twice is intentional (https://git.io/JnDPc)
    scale_fill_manual(aesthetics = "segment.color", values = line_colours(data)) +
    scale_linetype_manual(values = line_types(data)) +
    scale_color_manual(values = line_colours(data))

  if (perc_y_scale) {
    p <- p +
      scale_y_continuous(labels = percent)
  }

  p +
    theme_2dii() +
    theme(axis.line = element_blank(), legend.position = "none") %+replace%
    theme(plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm"))
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

check_plot_trajectory <- function(data, env) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), "label")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  abort_if_has_zero_rows(data, env = env)
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_invalid_scenarios_number(data)
  abort_if_too_many_lines(max = 4, summarise_max_year_by_scenario(data))
  abort_if_too_many_lines(max = 5, summarise_max_year_by_traj_metric(data))

  invisible(data)
}

summarise_max_year_by_scenario <- function(data) {
  data %>%
    filter(is_scenario(.data$metric)) %>%
    group_by(.data$metric) %>%
    summarise(year = max(.data$year))
}

summarise_max_year_by_traj_metric <- function(data) {
  data %>%
    filter(!is_scenario(.data$metric)) %>%
    group_by(.data$metric) %>%
    summarise(year = max(.data$year))
}

value_span <- function(data) {
  max(data$value, na.rm = TRUE) - min(data$value_low, na.rm = TRUE)
}

line_colours <- function(data) {
  linecolours <- c("black", "black", "grey46", "black", "grey46")
  c(scenario_lines(data)$colour, rev(linecolours[1:lines_n(data)]))
}

line_types <- function(data) {
  linetypes <- c("solid", "dashed", "solid", "twodash", "longdash")
  c(rep("solid", nrow(scenario_lines(data))), rev(linetypes[1:lines_n(data)]))
}

lines_n <- function(data) {
  length(unique(order_trajectory(data)$metric)) - nrow(scenario_lines(data))
}

scenario_lines <- function(data) {
  filter(scenario_colour(data), .data$scenario != "target_worse")
}

abort_if_invalid_scenarios_number <- function(data) {
  scenarios <- extract_scenarios(data$metric)
  n <- length(scenarios)

  if (n < 1 || n > 4) {
    abort(c(
      glue("`metric` must have between 1 and 4 scenarios."),
      x = glue("Provided {n} scenarios: {toString(scenarios)}")
    ))
  }

  invisible(data)
}

order_trajectory <- function(data) {
  order_add_lines <- data %>%
    filter(!is_scenario(.data$metric), .data$metric != main_line()) %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()

  lines_ordered <- c(scenario_lines(data)$scenario, order_add_lines, main_line())

  data <- data %>%
    filter(
      .data$metric %in% lines_ordered
    ) %>%
    mutate(
      metric = factor(
        .data$metric,
        levels = lines_ordered
      )
    ) %>%
    arrange(.data$year, .data$metric)

  technology_kind <- get_tech_kind(data)

  if (technology_kind == "increasing") {
    data <- data %>%
      rename(
        value = "value_low",
        value_high = "value"
      )
  }
  data
}

scenario_colour <- function(data) {
  ordered_scenarios <- get_ordered_scenarios(data)
  num_scen_areas <- length(ordered_scenarios)
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)

  technology_kind <- get_tech_kind(data)

  switch(technology_kind,
    "increasing" = reverse_rows(tibble(
      scenario = ordered_scenarios,
      colour = scenario_colours$hex
    )),
    "decreasing" = tibble(
      scenario = rev(ordered_scenarios),
      colour = scenario_colours$hex
    ),
    abort( # nocov start
      c("Each `technology` must only be either 'green' or 'brown'.",
        i = "Is `r2dii.data::green_or_brown` as expected?",
        x = glue("`technology` is {toString(technology_kind)}.")
      )
    ) # nocov end
  )
}

reverse_rows <- function(x) {
  x[sort(rownames(x), decreasing = TRUE), , drop = FALSE]
}

get_ordered_scenario_colours <- function(n) {
  pick <- function(cols) filter(scenario_colours, .data$label %in% cols)

  switch(as.character(n),
         "2" = pick(c("light_green", "red")),
         "3" = pick(c("light_green", "light_yellow", "red")),
         "4" = pick(c("light_green", "dark_yellow", "light_yellow", "red")),
         "5" = scenario_colours,
         abort(c("`n` must be between 2 and 5.", x = glue("Provided: {n}."))) # nocov
  )
}
