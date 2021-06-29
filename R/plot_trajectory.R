#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
#'   * (Optional) If present, the column `label` is used for data labels.
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

  data %>%
    prep_trajectory(convert_label = identity, span_5yr = FALSE) %>%
    plot_trajectory_impl()
}

# The `env` argument supports non-standard evaluation to print informative error
# messages that mention the symbol passed to `data` (e.g. "my_data") rather than
# the name of the argument (i.e. `data`). Although tests should warn you,
# breaking this functionality is easy, for example, by wrapping this function
# and moving it deeper into the caller stack.
check_plot_trajectory <- function(data, env = parent.frame()) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), "production")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  abort_if_has_zero_rows(data, env = env)
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_invalid_scenarios_number(data)
  abort_if_too_many_lines(max = 4, summarise_max_year_by_scenario(data))
  abort_if_too_many_lines(max = 5, summarise_max_year_by_traj_metric(data))

  invisible(data)
}

#' @param convert_label A symbol. The unquoted name of a function to apply to
#'   legend labels. For example, to convert labels to
#'   uppercase use `convert_label = toupper`.
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year, or use `FALSE` to impose no restriction.
#' @noRd
prep_trajectory <- function(data,
                            convert_label = identity,
                            span_5yr = FALSE) {
  out <- data %>%
    prep_common() %>%
    mutate(value = .data$production) %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    out <- span_5yr(out)
  }

  start_year <- min(out$year)
  if (!quiet()) {
    inform(glue(
      "Normalizing `production` values to {start_year} -- the start year."
    ))
  }
  by <- c("metric", "label")
  out <- left_join(out, filter(out, .data$year == start_year), by = by) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    rename(sector = .data$sector.x)

  cols <- c("year", "metric", "label", "technology", "value", "sector")
  select(out, all_of(cols))
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
  year_span <- max(data$year) - min(data$year)
  p <- p + ggrepel::geom_text_repel(
    data = lines_end,
    aes(
      y = .data$value,
      label = .data$label,
      segment.color = .data$metric),
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
    xlim = c(min(data$year), max(data$year) + 0.7 * year_span)
  )

  p +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_fill_manual(values = scenario_colour(data)$colour) +
    # Calling `scale_fill_manual()` twice is intentional (https://git.io/JnDPc)
    scale_fill_manual(aesthetics = "segment.color", values = line_colours(data)) +
    scale_linetype_manual(values = line_types(data)) +
    scale_color_manual(values = line_colours(data)) +
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
  scen <- scenario(data)
  max(scen$value) - min(scen$value_low)
}

line_colours <- function(data) {
  linecolours <- c("black", "black", "gray", "grey46", "black")
  c(scenario_lines(data)$colour, rev(linecolours[1:lines_n(data)]))
}

line_types <- function(data) {
  linetypes <- c("solid", "dashed", "solid", "solid", "twodash")
  c(rep("solid", nrow(scenario_lines(data))), rev(linetypes[1:lines_n(data)]))
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

  data %>%
    mutate(
      metric = factor(
        .data$metric,
        levels = c(scenario_lines(data)$scenario, order_add_lines, main_line())
      )
    ) %>%
    arrange(.data$year, .data$metric)
}

distance_from_start_value <- function(data, value) {
  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year), is_portfolio(.data$metric)) %>%
    pull(.data$value)

  abs(value - start_value_portfolio)
}

get_area_borders <- function(data) {
  lower <- 0.9 * min(data$value)
  upper <- 1.1 * max(data$value)
  span <- upper - lower

  upper_distance <- distance_from_start_value(data, upper) / span
  lower_distance <- distance_from_start_value(data, lower) / span

  # Center the starting point of the lines
  distance <- abs(upper_distance - lower_distance)
  max_distance <- 0.1
  if (distance > max_distance) {
    lower <- lower - max(0, upper_distance - lower_distance) * span
    upper <- upper + max(0, lower_distance - lower_distance) * span
  }

  list(lower = lower, upper = upper)
}

scenario_colour <- function(data) {
  ordered_scenarios <- data %>%
    filter(is_scenario(.data$metric), .data$year == max(.data$year)) %>%
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

scenario <- function(data) {
  specs <- scenario_colour(data)
  area_borders <- get_area_borders(data)

  data_worse_than_scenarios <- tibble(year = unique(data$year))
  if (specs$scenario[1] == "worse") {
    data_scenarios <- data %>%
      filter(is_scenario(.data$metric)) %>%
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
      filter(is_scenario(.data$metric)) %>%
      select(.data$year, .data$metric, .data$value)

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      mutate(metric = factor(.data$metric, levels = specs$scenario)) %>%
      arrange(.data$year, .data$metric) %>%
      mutate(value_low = lag(.data$value, n = 1, default = area_borders$lower))
  }

  data_scenarios
}
