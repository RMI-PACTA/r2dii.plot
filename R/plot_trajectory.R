#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_trajectory()`), or use
#'   `FALSE` to impose no restriction.
#' @template convert_label
#' @templateVar fun qplot_trajectory
#' @templateVar value recode_metric_trajectory
#' @param center_y Logical. Use `TRUE` to center the y-axis around start value
#'   (the default behavior of `qplot_trajectory()`), or use `FALSE` to not
#'   center.
#' @param value_col Character. Name of the column to be used as a value to be
#'   plotted.
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
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#'
#' plot_trajectory(data)
#'
#' # plot with `qplot_trajectory()` parameters
#' plot_trajectory(
#'   data,
#'   span_5yr = TRUE,
#'   convert_label = recode_metric_trajectory,
#'   center_y = TRUE,
#'   value_col = "percentage_of_initial_production_by_scope",
#'   perc_y_scale = TRUE
#' )
plot_trajectory <- function(data,
                            span_5yr = FALSE,
                            convert_label = identity,
                            center_y = FALSE,
                            value_col = "percentage_of_initial_production_by_scope",
                            perc_y_scale = FALSE) {
  lifecycle::deprecate_soft(
      when = "0.4.0",
      what = "plot_trajectory(data = 'must be prepped already')",
      details = api_warning_details("plot_trajectory")
  )
  env <- list(data = substitute(data))
  check_plot_trajectory(data, value_col = value_col, env = env)

  data %>%
    prep_trajectory(
      convert_label = convert_label,
      span_5yr = span_5yr,
      center_y = center_y,
      value_col = value_col
    ) %>%
    plot_trajectory_impl(perc_y_scale)
}

check_plot_trajectory <- function(data, value_col, env) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  abort_if_has_zero_rows(data, env = env)
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_invalid_scenarios_number(data)
  abort_if_too_many_lines(max = 4, summarise_max_year_by_scenario(data))
  abort_if_too_many_lines(max = 5, summarise_max_year_by_traj_metric(data))

  invisible(data)
}

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

plot_trajectory_impl <- function(data, perc_y_scale = FALSE) {
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

  if (technology_kind == "green") {
    data <- data %>%
      rename(
        value = .data$value_low,
        value_high = .data$value
      )
  }
  data
}

start_value_portfolio <- function(data) {
  start_value_portfolio <- data %>%
    filter(.data$year == min(data$year, na.rm = TRUE), is_portfolio(.data$metric)) %>%
    pull(.data$value)
}

distance_from_start_value <- function(data, value) {
  abs(value - start_value_portfolio(data))
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

get_ordered_scenarios <- function(data) {
  ordered_scenarios <- data %>%
    filter(is_scenario(.data$metric), .data$year == max(data$year)) %>%
    arrange(desc(.data$value)) %>%
    pull(.data$metric) %>%
    as.character()

  ordered_scenarios
}

get_tech_kind <- function(data) {
  technology_kind <- r2dii.data::green_or_brown %>%
    filter(.data$technology == unique(data$technology)) %>%
    pull(.data$green_or_brown) %>%
    unique()

  technology_kind
}

scenario_colour <- function(data) {
  ordered_scenarios <- get_ordered_scenarios(data)
  num_scen_areas <- length(ordered_scenarios)
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)

  technology_kind <- get_tech_kind(data)

  switch(technology_kind,
    "green" = reverse_rows(tibble(
      scenario = ordered_scenarios,
      colour = scenario_colours$hex
    )),
    "brown" = tibble(
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

scenario <- function(data, center_y = FALSE) {
  area_borders <- get_area_borders(data, center_y)

  data_worse_than_scenarios <- tibble(
    year = unique(data$year),
    technology = unique(data$technology),
    sector = unique(data$sector)
  )

  technology_kind <- get_tech_kind(data)

  if (technology_kind == "green") {
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
      rename(value_low = .data$value) %>%
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
