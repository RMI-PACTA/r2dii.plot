#' Create a trajectory alignment chart in a ggplot object
#'
#' @description We are exploring different interfaces before release. We are
#'   keen to hear feedback from beta-testers like you. Please try these
#'   alternative interfaces and let us know which one you prefer. The main
#'   difference between them is the number of arguments and how the input data
#'   is used:
#'
#'   * `plot_trajectoryA()` requires input arguments such as (at the minimum)
#'   `scenario_specs_good_to_bad` and `main_line_metric` for specifying the
#'   order and labels of scenario data and trajectory lines.
#'
#' @param data Filtered input data; with columns: year, metric_type, metric and
#'   value.
#' @param scenario_specs_good_to_bad Data frame containing scenario
#'   specifications like name label, ordered from the most to least sustainable;
#'   with columns: scenario, label.
#' @param main_line_metric Data frame containing information about metric that
#'   should be plotted as the main line; with columns: metric, label.
#' @param additional_line_metrics Data frame containing information about
#'   additional metrics that should be plotted as lines; with columns: metric,
#'   label).
#'
#' @family plotting functions
#'
#' @export
#' @examples
#' # `plot_trajectoryA()` -------------------------------------------------------
#' data <- prep_trajectory(
#'   market_share,
#'   sector_filter = "power",
#'   technology_filter = "renewablescap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value = "production"
#' )
#'
#' scenario_specs <- dplyr::tibble(
#'   scenario = c("sds", "sps", "cps"),
#'   label = c("SDS", "STEPS", "CPS")
#' )
#'
#' main_line_metric <- dplyr::tibble(metric = "projected", label = "Portfolio")
#'
#' additional_line_metrics <- dplyr::tibble(
#'   metric = "corporate_economy",
#'   label = "Corporate Economy"
#' )
#'
#' p <- plot_trajectoryA(data,
#'   scenario_specs_good_to_bad = scenario_specs,
#'   main_line_metric = main_line_metric,
#'   additional_line_metrics = additional_line_metrics
#' )
#'
#' p
plot_trajectoryA <- function(data,
                             scenario_specs_good_to_bad,
                             main_line_metric,
                             additional_line_metrics = NULL) {
  check_number_scenarios(scenario_specs_good_to_bad)

  # plot scenario areas
  scenario_specs_areas <- get_ordered_scenario_specs_with_colours(
    scenario_specs_good_to_bad, data$technology[1]
  )
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
  scenario_specs_lines <- scenario_specs_areas %>% filter(.data$scenario != "worse")
  if (!is.null(additional_line_metrics)) {
    line_metrics <- c(
      main_line_metric$metric,
      additional_line_metrics$metric,
      scenario_specs_lines$scenario
    )
    line_labels <- c(
      main_line_metric$label,
      additional_line_metrics$label,
      scenario_specs_lines$label
    )
  } else {
    line_metrics <- c(main_line_metric$metric, scenario_specs_lines$scenario)
    line_labels <- c(main_line_metric$label, scenario_specs_lines$label)
  }
  names(line_labels) <- line_metrics

  n_scenarios <- nrow(scenario_specs_lines)
  n_lines_traj <- length(line_metrics) - n_scenarios
  linetypes_trajectory <- c("solid", "dashed", "solid", "solid", "twodash")
  linecolours_trajectory <- c("black", "black", "gray", "grey46", "black")
  line_types <- c(linetypes_trajectory[1:n_lines_traj], rep("solid", n_scenarios))
  line_colours <- c(linecolours_trajectory[1:n_lines_traj], scenario_specs_lines$colour)

  data_lines <- data %>%
    filter(.data$metric %in% line_metrics) %>%
    mutate(label = dplyr::recode(.data$metric, !!!line_labels)) %>%
    mutate(metric = factor(.data$metric, levels = line_metrics)) %>%
    arrange(.data$year, .data$metric)

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

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type != "scenario"
        ),
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$label
      ),
      direction = "y",
      size = 3.5,
      nudge_x = 0.15,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0,
      xlim = c(min(data$year), last_year + 3)
    )

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type == "scenario"
        ),
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
      nudge_x = 0.6,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0.3,
      xlim = c(min(data$year), last_year + 5)
    ) +
    scale_fill_manual(
      aesthetics = "segment.color",
      values = scenario_specs_lines$colour
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

# For backward compatibility until we decide which version to keep
#' @export
#' @rdname plot_trajectoryA
plot_trajectory <- plot_trajectoryA

#' @rdname plot_trajectoryA
#' @description * `plot_trajectoryB()` derives the main and additional lines as
#'   well as scenario order from the data. The lines are plotted according to
#'   the order of the input data. The scenario order is inferred from the order
#'   of values on the last year. For the labels the `data` column `metric` is
#'   used. You may recode `metric` before passing the data with, for example,
#'   `dplyr::recode()`.
#'
#' @family plotting functions
#'
#' @export
#' @examples
#'
#' # `plot_trajectoryB()` ------------------------------------------------------
#' library(dplyr)
#'
#' data <- prep_trajectory(
#'   market_share,
#'   sector_filter = "power",
#'   technology_filter = "renewablescap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value = "production"
#' )
#'
#' # Order metric: First main trajectory line, then benchmarks, then scenarios
#' lines_order <- c("projected", "corporate_economy", "sds", "sps", "cps")
#' ordered <- data %>%
#'   mutate(metric = factor(.data$metric, levels = lines_order)) %>%
#'   arrange(.data$year, .data$metric)
#'
#' plot_trajectoryB(ordered)
#'
#' # You may recode `metric` with `dplyr::recode()`
#' recoded <- ordered %>%
#'   mutate(
#'     metric = recode(
#'       .data$metric,
#'       "projected" = "Projected",
#'       "corporate_economy" = "Corporate Economy",
#'       "sds" = "SDS",
#'       "sps" = "SPS",
#'       "cps" = "CPS"
#'     )
#'   )
#'
#' plot_trajectoryB(recoded)
plot_trajectoryB <- function(data) {
  check_number_scenariosB(data)

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
  data_lines <- order_for_trajectoryB(data, scenario_specs_lines)

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

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type != "scenario"
        ),
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$metric
      ),
      direction = "y",
      size = 3.5,
      nudge_x = 0.15,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0,
      xlim = c(min(data$year), last_year + 3)
    )

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type == "scenario"
        ),
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
      nudge_x = 0.6,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0.3,
      xlim = c(min(data$year), last_year + 5)
    ) +
    scale_fill_manual(
      aesthetics = "segment.color",
      values = scenario_specs_lines$colour
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

check_number_scenariosB <- function(data) {
  unique_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    pull(.data$metric) %>%
    unique()

  if (length(unique_scenarios) > 4) {
    rlang::abort(glue(
      "Scenario number for plotting must be between 1 and 4. \\
      You provided {nrow(scenario_specs)} scenarios in 'scenario_specs'."
    ))
  }
}

check_number_scenarios <- function(scenario_specs) {
  if (nrow(scenario_specs) > 4) {
    rlang::abort(glue(
      "Scenario number for plotting must be between 1 and 4. \\
      You provided {nrow(scenario_specs)} scenarios in 'scenario_specs'."
    ))
  }
}

reverse_rows <- function(x) {
  x[sort(rownames(x), decreasing = TRUE), , drop = FALSE]
}

order_for_trajectoryB <- function(data, scenario_specs) {
  order_lines <- data %>%
    mutate(metric = factor(.data$metric, levels = unique(data$metric))) %>%
    filter(.data$metric_type != "scenario") %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()
  order_scenarios <- scenario_specs$scenario

  data_ordered <- data %>%
    mutate(metric = factor(
      .data$metric,
      levels = c(order_lines, order_scenarios)
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

add_scenario_colours <- function(scenario_specs) {
  num_scen_areas <- nrow(scenario_specs)
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)
  scenario_specs$colour <- scenario_colours$hex

  scenario_specs
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

get_ordered_scenario_specs_with_colours <- function(scenario_specs_good_to_bad,
                                                    technology) {
  worse_row <- tibble(scenario = "worse", label = "Worse")
  scenario_specs_good_to_bad <- rbind(scenario_specs_good_to_bad, worse_row)
  scenario_specs_good_to_bad <- add_scenario_colours(scenario_specs_good_to_bad)

  green_or_brown <- r2dii.data::green_or_brown
  tech_green_or_brown <- green_or_brown %>%
    filter(.data$technology == .env$technology) %>%
    pull(.data$green_or_brown)

  if (tech_green_or_brown == "brown") {
    scenario_specs <- scenario_specs_good_to_bad
  } else if (tech_green_or_brown == "green") {
    scenario_specs <- reverse_rows(scenario_specs_good_to_bad)
  }
  scenario_specs
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
