plot_trajectoryY <- function(data,
                             scenario_specs_good_to_bad,
                             main_line_metric,
                             additional_line_metrics = NULL) {
  abort_if_missing_names(scenario_specs_good_to_bad, "scenario")
  check_number_scenarios(scenario_specs_good_to_bad)
  if (!("label" %in% names(scenario_specs_good_to_bad))) {
    scenario_specs_good_to_bad$label <- scenario_specs_good_to_bad$scenario
  }

  abort_if_missing_names(main_line_metric, "metric")
  if (!("label" %in% names(main_line_metric))) {
    main_line_metric$label <- main_line_metric$metric
  }
  if (!is.null(additional_line_metrics)) {
    abort_if_missing_names(additional_line_metrics, "metric")
    if (!("label" %in% names(additional_line_metrics))) {
      additional_line_metrics$label <- additional_line_metrics$metric
    }
  }

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

check_number_scenarios <- function(scenario_specs) {
  if (nrow(scenario_specs) > 4) {
    rlang::abort(glue(
      "Scenario number for plotting must be between 1 and 4. \\
      You provided {nrow(scenario_specs)} scenarios in 'scenario_specs'."
    ))
  }
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

add_scenario_colours <- function(scenario_specs) {
  num_scen_areas <- nrow(scenario_specs)
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)
  scenario_specs$colour <- scenario_colours$hex

  scenario_specs
}
