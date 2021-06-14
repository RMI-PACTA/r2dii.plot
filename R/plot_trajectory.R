#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
#' @param main_line String of length 1. The `metric` to plot as the line with
#'   the most visual salience (solid black line). `NULL` defaults to
#'   "projected".
#' @param normalize Logical of length-1. `TRUE` normalizes to the start year.
#'
#' @family functions with minimal arguments
#' @seealso [market_share].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # `data` must meet documented "Requirements"
#' data <- market_share %>%
#'   filter(
#'     sector == "power",
#'     technology == "renewablescap",
#'     region == "global",
#'     scenario_source == "demo_2020"
#'   )
#' plot_trajectory(data)
#'
#' plot_trajectory(data, normalize = FALSE)
plot_trajectory <- function(data, normalize = TRUE, main_line = NULL) {
  stopifnot(is.data.frame(data))
  hint_if_missing_names(
    abort_if_missing_names(data, common_crucial_market_share_columns())
  )
  abort_if_has_cero_rows(data)

  abort_if_multiple(data, "sector")
  abort_if_multiple(data, "technology")
  abort_if_multiple(data, "region")
  abort_if_multiple(data, "scenario_source")
  if (is.null(main_line)) {
    stopifnot("projected" %in% tolower(data$metric))
    main <- "projected"
  } else {
    abort_if_invalid_main_line(data, main_line)
    main <- main_line
  }

  prep <- prep_trajectoryB(data, normalize = normalize)

  plot_trajectoryB(prep, main_line = main)
}

plot_trajectoryB <- function(data, main_line = NULL) {
  main_line <- main_line %||%
    (data %>%
      filter(.data$metric_type != "scenario") %>%
      slice_head(n = 1) %>%
      pull(.data$metric))
  abort_if_invalid_main_line(data, main_line)
  abort_if_invalid_scenarios_number(data)

  # plot scenario areas
  scenario_specs_areas <- get_ordered_scenario_specsB(data)
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
  data_lines <- order_for_trajectoryB(data, scenario_specs_lines, main_line)

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
    )

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines_end,
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$metric,
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

abort_if_invalid_main_line <- function(data, main_line) {
  abort_if_invalid_length(main_line)

  metrics <- unique(data$metric)
  if (!main_line %in% metrics) {
    rlang::abort(glue(
      "`main_line` must be one value of `data$metric`.
      * Valid: {toString(metrics)}.
      * Provided: {toString(main_line)}."
    ))
  }

  invisible(data)
}

order_for_trajectoryB <- function(data, scenario_specs, main_line) {
  order_add_lines <- data %>%
    filter(
      .data$metric_type != "scenario",
      .data$metric != .env$main_line
    ) %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()

  order_scenarios <- scenario_specs$scenario

  data_ordered <- data %>%
    mutate(metric = factor(
      .data$metric,
      levels = c(main_line, order_add_lines, order_scenarios)
    )) %>%
    arrange(.data$year, .data$metric)

  data_ordered
}

get_area_borders <- function(data) {
  lower_area_border <- 0.9 * min(data$value)
  upper_area_border <- 1.1 * max(data$value)
  value_span <- upper_area_border - lower_area_border

  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year)) %>%
    filter(.data$metric_type == "portfolio") %>%
    pull(.data$value)

  perc_distance_upper_border <-
    (upper_area_border - start_value_portfolio) / value_span
  perc_distance_lower_border <-
    (start_value_portfolio - lower_area_border) / value_span

  # adjusting the area border to center the starting point of the lines
  max_delta_distance <- 0.1
  delta_distance <- abs(perc_distance_upper_border - perc_distance_lower_border)
  if (delta_distance > max_delta_distance) {
    if (perc_distance_upper_border > perc_distance_lower_border) {
      lower_area_border <-
        start_value_portfolio - perc_distance_upper_border * value_span
      value_span <- upper_area_border - lower_area_border
    } else {
      upper_area_border <-
        perc_distance_lower_border * value_span + start_value_portfolio
      value_span <- upper_area_border - lower_area_border
    }
  }

  area_borders <- list(lower = lower_area_border, upper = upper_area_border)
  area_borders
}

get_ordered_scenario_specsB <- function(data) {
  ordered_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    filter(.data$year == max(.data$year)) %>%
    arrange(desc(.data$value)) %>%
    pull(.data$metric) %>%
    as.character()
  num_scen_areas <- length(ordered_scenarios) + 1
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)

  green_or_brown <- r2dii.data::green_or_brown
  tech_green_or_brown <- green_or_brown %>%
    filter(.data$technology == data$technology[1]) %>%
    pull(.data$green_or_brown)

  if (tech_green_or_brown == "brown") {
    ordered_scenarios_good_to_bad <- tibble(
      scenario = rev(c("worse", ordered_scenarios)),
      colour = scenario_colours$hex
    )
    scenario_specs <- ordered_scenarios_good_to_bad
  } else if (tech_green_or_brown == "green") {
    ordered_scenarios_good_to_bad <- tibble(
      scenario = c(ordered_scenarios, c("worse")),
      colour = scenario_colours$hex
    )
    scenario_specs <- reverse_rows(ordered_scenarios_good_to_bad)
  }
  scenario_specs
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
    "5" = assert_5_rows(scenario_colours)
  )
}

assert_5_rows <- function(data) {
  stopifnot(nrow(data) == 5L)
  invisible(data)
}

prep_trajectoryB <- function(data,
                             value = "production",
                             metric = "metric",
                             normalize = TRUE) {
  check_prep_trajectoryB(data, value, normalize)
  data <- recode_metric_and_metric_type(data, metric)

  cols <- c("year", "metric_type", "metric", "technology", "value")
  out <- data %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  if (!normalize) {
    return(out)
  }

  left_join(
    out, filter(out, .data$year == min(.data$year)),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectoryB <- function(data, value, normalize) {
  crucial <- c(common_crucial_market_share_columns(), value)
  abort_if_missing_names(data, crucial)

  abort_if_invalid_length(normalize)
  stopifnot(is.logical(normalize))

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
