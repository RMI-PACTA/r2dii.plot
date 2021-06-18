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
  stopifnot(is.data.frame(data))
  hint_if_missing_names(
    abort_if_missing_names(data, common_crucial_market_share_columns())
  )
  abort_if_has_zero_rows(data)
  cols <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, cols)

  prep <- prep_trajectory(data)
  plot_trajectory_impl(prep)
}

plot_trajectory_impl <- function(data) {
  abort_if_invalid_scenarios_number(data)

  data <- mutate_pretty_labels(data, name = "metric")

  # plot scenario areas
  scenario_specs_areas <- get_ordered_scenario_specs(data)
  data_scenarios <- get_scenario_data(data, scenario_specs_areas)
  p_trajectory <- ggplot() +
    geom_ribbon(
      data = data_scenarios,
      aes(
        x = .data$year,
        ymin = .data$value_low,
        ymax = .data$value,
        fill = .data$metric,
        alpha = 0.9
      )
    ) +
    scale_fill_manual(values = scenario_specs_areas$colour)

  # plot trajectory and scenario lines
  scenario_specs_lines <- scenario_specs_areas %>%
    filter(.data$scenario != "worse")
  data_lines <- order_for_trajectory(data, scenario_specs_lines)

  n_scenarios <- nrow(scenario_specs_lines)
  n_lines_traj <- length(unique(data_lines$metric)) - n_scenarios
  linetypes_trajectory <- c("solid", "dashed", "solid", "solid", "twodash")
  linecolours_trajectory <- c("black", "black", "gray", "grey46", "black")
  line_types <- c(linetypes_trajectory[1:n_lines_traj], rep("solid", n_scenarios))
  line_colours <- c(linecolours_trajectory[1:n_lines_traj], scenario_specs_lines$colour)

  p_trajectory <- p_trajectory +
    geom_line(
      data = data_lines,
      aes(
        x = .data$year,
        y = .data$value,
        linetype = .data$metric,
        color = .data$metric
      )
    )

  p_trajectory <- p_trajectory +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_linetype_manual(values = line_types) +
    scale_color_manual(values = line_colours)

  # annotate trajectory and scenario lines
  last_year <- max(data$year)
  value_span <- max(data_scenarios$value) - min(data_scenarios$value_low)
  data_lines_end <- data_lines %>%
    filter(
      .data$year == last_year
    ) %>%
    mutate_pretty_labels(name = "label")

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines_end,
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$label,
        segment.color = .data$metric
      ),
      direction = "y",
      color = "black",
      size = 3.5,
      alpha = 1,
      nudge_x = if_else(data_lines_end$metric_type == "scenario", 0.6, 0.1),
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = if_else(data_lines_end$metric_type == "scenario", 0.4, 0),
      xlim = c(min(data$year), last_year + 6)
    ) +
    scale_fill_manual(
      aesthetics = "segment.color",
      values = line_colours
    )

  p_trajectory <- p_trajectory +
    theme_2dii() +
    theme(
      axis.line = element_blank(),
      legend.position = "none"
    ) %+replace%
    theme(
      plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm")
    )

  p_trajectory
}

abort_if_invalid_scenarios_number <- function(data) {
  abort_if_missing_names(data, "metric_type")
  unique_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    pull(.data$metric) %>%
    unique()
  n <- length(unique_scenarios)
  if (n < 1 || n > 4) {
    abort(glue(
      "`metric` must have between 1 and 4 scenarios, not {n}: \\
      {toString(unique_scenarios)}"
    ))
  }

  invisible(data)
}

order_for_trajectory <- function(data, scenario_specs) {
  order_add_lines <- data %>%
    filter(.data$metric_type != "scenario", .data$metric != main_line()) %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()

  order_scenarios <- scenario_specs$scenario

  data_ordered <- data %>%
    mutate(metric = factor(
      .data$metric,
      levels = c(main_line(), order_add_lines, order_scenarios)
    )) %>%
    arrange(.data$year, .data$metric)

  data_ordered
}

distance_from_start_value_portfolio <- function(data, value) {
  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year)) %>%
    filter(.data$metric_type == "portfolio") %>%
    pull(.data$value)

  distance <- abs(value - start_value_portfolio)
  distance
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

get_ordered_scenario_specs <- function(data) {
  ordered_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    filter(.data$year == max(.data$year)) %>%
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
    abort("The kind of technology must only be 'green' or 'brown'") # nocov
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
    abort("`n` must be between 2 and 5 not {n}.") # nocov
  )
}

prep_trajectory <- function(data,
                            value = "production",
                            metric = "metric") {
  cols <- c("year", "metric_type", "metric", "technology", "value")

  out <- data %>%
    check_prep_trajectory(value) %>%
    recode_metric_and_metric_type(metric) %>%
    drop_before_start_year(metric) %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  start_year <- min(out$year)
  # TODO: Extract and move to r2dii.analysis
  if (!quiet()) {
    inform(glue(
      "Normalizing `{value}` values to {start_year} -- the start year."
    ))
  }
  left_join(
    out, filter(out, .data$year == start_year),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectory <- function(data, value) {
  crucial <- c(common_crucial_market_share_columns(), value)
  abort_if_missing_names(data, crucial)

  cols <- c("sector", "technology", "region", "scenario_source")
  lapply(cols, function(x) abort_if_multiple(data, x))

  invisible(data)
}

get_scenario_data <- function(data, scenario_specs) {
  area_borders <- get_area_borders(data)

  data_worse_than_scenarios <- tibble(year = unique(data$year))
  if (scenario_specs$scenario[1] == "worse") {
    data_scenarios <- data %>%
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, value_low = .data$value)

    data_worse_than_scenarios$value_low <- area_borders$lower
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      mutate(metric = factor(.data$metric,
        levels = scenario_specs$scenario
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
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, .data$value)

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      mutate(metric = factor(.data$metric,
        levels = scenario_specs$scenario
      )) %>%
      arrange(.data$year, .data$metric) %>%
      mutate(value_low = lag(.data$value,
        n = 1,
        default = area_borders$lower
      ))
  }
}
