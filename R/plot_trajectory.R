#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
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
plot_trajectory <- function(data) {
  check_plot_trajectory(data)

  prep <- prep_trajectory(data)
  plot_trajectory_impl(prep)
}

check_plot_trajectory <- function(data, env = parent.frame()) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), "production")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  abort_if_has_zero_rows(data, env = env)
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_invalid_scenarios_number(data)
  abort_if_too_many_lines(max = 5, summarise_max_year_by_metric(data))

  invisible(data)
}

summarise_max_year_by_metric <- function(data) {
  data %>%
    filter(is_scenario(.data$metric)) %>%
    group_by(.data$metric) %>%
    summarise(year = max(.data$year))
}

plot_trajectory_impl <- function(data) {
  p <- ggplot(order_trajectory(data), aes(x = .data$year, y = .data$value))

  p <- p + geom_ribbon(
    data = scenario(data),
    aes(
      ymin = .data$value_low,
      ymax = .data$value,
      fill = .data$metric,
      alpha = 0.9
    )
  )

  p <- p + geom_line(
    data = order_trajectory(data),
    aes(linetype = .data$metric, color = .data$metric)
  )

  lines_end <- filter(order_trajectory(data), .data$year == max(data$year))
  p <- p + ggrepel::geom_text_repel(
    data = lines_end,
    aes(label = .data$metric, segment.color = .data$metric),
    direction = "y",
    color = "black",
    size = 3.5,
    alpha = 1,
    nudge_x = if_else(is_scenario(lines_end$metric0), 0.6, 0.1),
    nudge_y = 0.01 * value_span(data),
    hjust = 0,
    segment.size = if_else(is_scenario(lines_end$metric0), 0.4, 0),
    # ASK: Does `6` have a meaning? e.g. `some_space <- 6`. I changed the
    # value to 1-30 and noticed no effect on the plot. Strange.
    xlim = c(min(data$year), max(data$year) + 6)
  )

  p +
    coord_cartesian(expand = FALSE, clip = "off") +
    # ASK: We call `scale_fill_manual()` twice with `value`. I don't get it.
    scale_fill_manual(values = scenario_colour(data)$colour) +
    scale_fill_manual(aesthetics = "segment.color", values = line_colours(data)) +

    scale_linetype_manual(values = line_types(data)) +
    scale_color_manual(values = line_colours(data)) +
    theme_2dii() +
    theme(axis.line = element_blank(), legend.position = "none") %+replace%
    theme(plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm"))
}

value_span <- function(data) {
  scen <- scenario(data)
  max(scen$value) - min(scen$value_low)
}

line_colours <- function(data) {
  linecolours <- c("black", "black", "gray", "grey46", "black")
  c(scenario_lines(data)$colour, linecolours[1:lines_n(data)])
}

line_types <- function(data) {
  linetypes <- c("solid", "dashed", "solid", "solid", "twodash")
  c(rep("solid", nrow(scenario_lines(data))), linetypes[1:lines_n(data)])
}

lines_n <- function(data) {
  length(unique(order_trajectory(data)$metric)) - nrow(scenario_lines(data))
}

scenario_lines <- function(data) {
  filter(scenario_colour(data), .data$scenario != "worse")
}

abort_if_invalid_scenarios_number <- function(data) {
  scenarios <- extract_scenarios(data$metric)
  n <- length(scenarios)

  if (n < 1 || n > 4) {
    abort(c(
      glue("`metric` must have between 1 and 4 scenarios, not {n}."),
      x = glue("Provided: {toString(scenarios)}")
    ))
  }

  invisible(data)
}

order_trajectory <- function(data) {
  order_add_lines <- data %>%
    filter(!is_scenario(.data$metric0), .data$metric != main_line()) %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()

  data %>%
    mutate(
      metric = factor(
        .data$metric,
              levels = c(scenario_lines(data)$scenario, order_add_lines, main_line())
      )
    ) %>%
    arrange(.data$year, .data$metric)
}

distance_from_start_value_portfolio <- function(data, value) {
  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year), is_portfolio(.data$metric0)) %>%
    pull(.data$value)

  abs(value - start_value_portfolio)
}

get_area_borders <- function(data) {
  max_delta_distance <- 0.1
  lower_area_border <- 0.9 * min(data$value)
  upper_area_border <- 1.1 * max(data$value)
  value_span <- upper_area_border - lower_area_border

  perc_distance_upper_border <-
    distance_from_start_value_portfolio(data, upper_area_border) / value_span
  perc_distance_lower_border <-
    distance_from_start_value_portfolio(data, lower_area_border) / value_span

  # adjusting the area border to center the starting point of the lines
  delta_distance <- abs(perc_distance_upper_border - perc_distance_lower_border)
  if (delta_distance > max_delta_distance) {
    lower_area_border <- lower_area_border -
      max(0, perc_distance_upper_border - perc_distance_lower_border) * value_span
    upper_area_border <- upper_area_border +
      max(0, perc_distance_lower_border - perc_distance_lower_border) * value_span
  }

  area_borders <- list(lower = lower_area_border, upper = upper_area_border)
  area_borders
}

scenario_colour <- function(data) {
  ordered_scenarios <- data %>%
    filter(is_scenario(.data$metric0), .data$year == max(.data$year)) %>%
    arrange(desc(.data$value)) %>%
    pull(.data$metric) %>%
    as.character()
  num_scen_areas <- length(ordered_scenarios) + 1
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)

  technology_kind <- r2dii.data::green_or_brown %>%
    filter(.data$technology == unique(data$technology)) %>%
    pull(.data$green_or_brown) %>%
    unique()

  switch(technology_kind,
    "green" = reverse_rows(tibble(
      scenario = c(ordered_scenarios, c("worse")),
      colour = scenario_colours$hex
    )),
    "brown" = tibble(
      scenario = rev(c("worse", ordered_scenarios)),
      colour = scenario_colours$hex
    ),
    abort("The kind of technology must only be 'green' or 'brown'.") # nocov
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

prep_trajectory <- function(data) {
  # Store original values to detect metric type from metric
  data$metric0 <- data$metric

  out <- data %>%
    drop_before_start_year() %>%
    mutate(
      value = .data$production,
      metric = sub("target_", "", .data$metric),
      metric = case_when(
        is_scenario(.data$metric0) ~ toupper(as.character(.data$metric)),
        TRUE ~ to_title(as.character(.data$metric))
      )
    )

  start_year <- min(out$year)
  if (!quiet()) {
    inform(glue(
      "Normalizing `production` values to {start_year} -- the start year."
    ))
  }
  by <- c("metric", "metric0")
  out <- left_join(out, filter(out, .data$year == start_year), by = by) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    )

  cols <- c("year", "metric", "metric0", "technology", "value")
  select(out, all_of(cols))
}

scenario <- function(data) {
  specs <- scenario_colour(data)
  area_borders <- get_area_borders(data)

  data_worse_than_scenarios <- tibble(year = unique(data$year))
  if (specs$scenario[1] == "worse") {
    data_scenarios <- data %>%
      filter(is_scenario(.data$metric0)) %>%
      select(.data$year, .data$metric, value_low = .data$value)

    data_worse_than_scenarios$value_low <- area_borders$lower
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      mutate(metric = factor(.data$metric,
        levels = specs$scenario
      )) %>%
      arrange(.data$year, .data$metric) %>%
      mutate(value = lead(.data$value_low,
        n = 1,
        default = area_borders$upper
      ))
  } else {
    data_worse_than_scenarios$value <- area_borders$upper
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- data %>%
      filter(is_scenario(.data$metric0)) %>%
      select(.data$year, .data$metric, .data$value)

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      mutate(metric = factor(.data$metric, levels = specs$scenario)) %>%
      arrange(.data$year, .data$metric) %>%
      mutate(value_low = lag(.data$value, n = 1, default = area_borders$lower))
  }

  data_scenarios
}
