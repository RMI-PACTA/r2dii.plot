#' Create a trajectory alignment chart in a ggplot object
#'
#' The function returns a ggplot object containing a stacked bar chart showing a
#' technology mix for different categories (portfolio, scenario, benchmark,
#' etc.).
#'
#' @param data Filtered input data (dataframe with columns: year, metric_type,
#'   metric and value).
#' @param plot_title Title of the plot (character string).
#' @param x_title,y_title Title of the x- and y-axis (character string).
#' @param scenario_specs_good_to_bad Dataframe containing scenario
#'   specifications like color or label, ordered from the most to least
#'   sustainable (dataframe with columns: scenario, label, color).
#' @param main_line_metric Dataframe containing information about metric that
#'   should be plotted as the main line (dataframe with columns: metric, label).
#' @param additional_line_metrics Dataframe containing information about
#'   additional metrics that should be plotted as lines (dataframe with columns:
#'   metric, label).
#'
#' @export

plot_trajectory <- function(data,
                            plot_title = "",
                            x_title = "",
                            y_title = "",
                            # FIXME: obligatory arguments come before optional
                            scenario_specs_good_to_bad,
                            # FIXME: obligatory arguments come before optional
                            main_line_metric,
                            additional_line_metrics = data.frame()) {
  p_trajectory <- ggplot() +
    theme_2dii_ggplot() +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme(axis.line = element_blank()) +
    xlab(x_title) +
    ylab(y_title) +
    labs(title = plot_title) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  # adjusting the area border to center the starting point of the lines
  lower_area_border <- min(data$value)
  upper_area_border <- max(data$value)
  value_span <- upper_area_border - lower_area_border

  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year)) %>%
    filter(.data$metric_type == "portfolio") %>%
    pull(.data$value)

  perc_distance_upper_border <-
    (upper_area_border - start_value_portfolio) / value_span
  perc_distance_lower_border <-
    (start_value_portfolio - lower_area_border) / value_span

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

  year <- unique(data$year)
  data_worse_than_scenarios <- data.frame(year)

  green_or_brown <- r2dii.data::green_or_brown
  tech_green_or_brown <- green_or_brown[
    green_or_brown$technology == data$technology[1],
  ]$green_or_brown

  if (tech_green_or_brown == "brown") {
    scenario_specs <- scenario_specs_good_to_bad

    data_worse_than_scenarios$value <- upper_area_border
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- data %>%
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, .data$value)

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      arrange(.data$year, factor(.data$metric,
        levels = scenario_specs$scenario
      )) %>%
      mutate(value_low = dplyr::lag(.data$value,
        n = 1,
        default = lower_area_border
      ))
  } else if (tech_green_or_brown == "green") {
    scenario_specs <- reverse_rows(scenario_specs_good_to_bad)

    data_scenarios <- data %>%
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, value_low = .data$value)

    data_worse_than_scenarios$value_low <- lower_area_border
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      arrange(.data$year, factor(.data$metric,
        levels = scenario_specs$scenario
      )) %>%
      mutate(value = dplyr::lead(.data$value_low,
        n = 1,
        default = upper_area_border
      ))
  }

  for (i in seq_along(scenario_specs$scenario)) {
    scen <- scenario_specs$scenario[i]
    color <- scenario_specs$color[i]
    data_scen <- data_scenarios %>% filter(.data$metric == scen)
    p_trajectory <- p_trajectory +
      geom_ribbon(
        data = data_scen, aes(
          ymin = .data$value_low,
          ymax = .data$value, x = year, group = 1
        ),
        fill = color, alpha = 0.75
      )

    if (scen != "worse") {
      if (tech_green_or_brown == "brown") {
        p_trajectory <- p_trajectory +
          geom_line(
            data = data_scen, aes(x = year, y = .data$value),
            color = color
          )
      } else if (tech_green_or_brown == "green") {
        p_trajectory <- p_trajectory +
          geom_line(
            data = data_scen, aes(x = year, y = .data$value_low),
            color = color
          )
      }
    }
  }

  linetypes_ordered <- c("solid", "dashed", "solid", "solid", "twodash")
  linecolors_ordered <- c("black", "black", "gray", "grey46", "black")

  if (length(additional_line_metrics) >= 1) {
    line_metrics <- c(main_line_metric$metric, additional_line_metrics$metric)
    line_labels <- c(main_line_metric$label, additional_line_metrics$label)
  } else {
    line_metrics <- c(main_line_metric$metric)
    line_labels <- c(main_line_metric$label)
  }

  data_metrics <- data %>% filter(.data$metric %in% line_metrics)
  n_lines <- length(line_metrics)

  p_trajectory <- p_trajectory +
    geom_line(
      data = data_metrics,
      aes(
        x = .data$year,
        y = .data$value,
        linetype = .data$metric,
        color = .data$metric
      )
    ) +
    scale_linetype_manual(
      values = linetypes_ordered[1:n_lines],
      labels = line_labels) +
    scale_color_manual(
      values = linecolors_ordered[1:n_lines],
      labels = line_labels
    )

  p_trajectory
}

reverse_rows <- function(x) {
  x[sort(rownames(x), decreasing = TRUE), , drop = FALSE]
}
