#' Create a trajectory alignment chart in a ggplot object
#'
#' The function returns a ggplot object containing a stacked bar chart showing a
#' technology mix for different categories (portfolio, scenario, benchmark,
#' etc.).
#'
#' @param data Filtered input data (dataframe with columns: year, metric_type,
#'   metric and value).
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
#' @examples
#' data <- prepare_for_trajectory_chart(
#'   process_input_data(example_data),
#'   sector_filter = "power",
#'   technology_filter = "renewablescap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value_name = "production"
#' )
#'
#' scenario_specs <- dplyr::tibble(
#'   scenario = c("sds", "sps", "cps", "worse"),
#'   color = c("#9CAB7C", "#FFFFCC", "#FDE291", "#E07B73"),
#'   label = c("SDS", "STEPS", "CPS", "worse")
#' )
#'
#' main_line_metric <- dplyr::tibble(metric = "projected", label = "Portfolio")
#'
#' additional_line_metrics <- dplyr::tibble(
#'   metric = "corporate_economy",
#'   label = "Corporate Economy"
#' )
#'
#' p <- plot_trajectory(data,
#'   scenario_specs_good_to_bad = scenario_specs,
#'   main_line_metric = main_line_metric
#' )
#'
#' p
plot_trajectory <- function(data,
                            scenario_specs_good_to_bad,
                            main_line_metric,
                            additional_line_metrics = NULL) {
  # plot scenario areas
  scenario_specs <- get_ordered_scenario_specs(
    scenario_specs_good_to_bad, data$technology[1]
  )
  data_scenarios <- get_scenario_data(data, scenario_specs)
  colours_scenarios <- get_adjusted_colours(data_scenarios, scenario_specs)
  p_trajectory <- ggplot() +
    geom_ribbon(
      data = data_scenarios,
      aes(
        x = .data$year,
        ymin = .data$value_low,
        ymax = .data$value,
        fill = .data$metric
      )
    ) +
    scale_fill_manual(values = colours_scenarios)

  # plot trajectory lines
  if (!is.null(additional_line_metrics)) {
    line_metrics <- c(main_line_metric$metric, additional_line_metrics$metric)
    line_labels <- c(main_line_metric$label, additional_line_metrics$label)
  } else {
    line_metrics <- c(main_line_metric$metric)
    line_labels <- c(main_line_metric$label)
  }

  data_metrics <- data %>% filter(.data$metric %in% line_metrics)
  n_lines <- length(line_metrics)

  linetypes_ordered <- c("solid", "dashed", "solid", "solid", "twodash")
  linecolours_ordered <- c("black", "black", "gray", "grey46", "black")

  p_trajectory <- p_trajectory +
    geom_line(
      data = data_metrics,
      aes(
        x = .data$year,
        y = .data$value,
        linetype = .data$metric,
        color = .data$metric
      )
    )

  p_trajectory <- p_trajectory +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_linetype_manual(values = linetypes_ordered[1:n_lines]) +
    scale_color_manual(values = linecolours_ordered[1:n_lines])

  p_trajectory <- p_trajectory +
    theme_2dii() +
    theme(
      axis.line = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.position = NULL
    ) +
    guides(linetype = FALSE, colour = FALSE)  # remove legend for "projected"

  legend <- plot_trajectory_legend(
    p_trajectory,
    data_scenarios,
    scenario_specs,
    data_metrics,
    linetypes_ordered,
    linecolours_ordered,
    line_labels
  )

  add_grobs(p_trajectory, get_legend(legend))
}

reverse_rows <- function(x) {
  x[sort(rownames(x), decreasing = TRUE), , drop = FALSE]
}

get_area_borders <- function(data) {
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

get_ordered_scenario_specs <- function(scenario_specs_good_to_bad, technology) {
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

  data_worse_than_scenarios <- data.frame(year = unique(data$year))
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

get_adjusted_colours <- function(data_scenarios,
                                 scenario_specs) {
  p_colors <- help_plot_area_colors(
    data_scenarios,
    scenario_specs
  )

  g <- ggplot_build(p_colors)
  colors <- unique(g$plot$scales$scales[[1]]$palette.cache)

  colors
}

plot_trajectory_legend <- function(plot,
                                   data_scenarios,
                                   scenario_specs,
                                   data_metrics,
                                   linetypes_ordered,
                                   linecolours_ordered,
                                   line_labels) {
  p_legend <- help_plot_area_colors(data_scenarios, scenario_specs)

  n_lines <- length(line_labels)
  p_legend <- p_legend +
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
      labels = line_labels
    ) +
    scale_color_manual(
      values = linecolours_ordered[1:n_lines],
      labels = line_labels
    )

  p_legend
}

help_plot_area_colors <- function(data_scenarios,
                                  scenario_specs) {
  lower_area_border <- min(data_scenarios$value)
  upper_area_border <- max(data_scenarios$value)
  num_scen <- nrow(scenario_specs)
  value_span <- upper_area_border - lower_area_border

  p_legend <- ggplot() +
    theme_2dii() +
    geom_point(
      data = data_scenarios,
      aes(
        x = .data$year,
        y = .data$value,
        fill = .data$value
      )
    ) +
    scale_fill_stepsn(
      colours = scenario_specs$color,
      guide = "coloursteps",
      breaks = seq(
        from = lower_area_border + value_span / num_scen,
        to = upper_area_border - value_span / num_scen,
        by = value_span / num_scen
      ),
      labels = scenario_specs$label[scenario_specs$scenario != "worse"]
    )

  p_legend
}

add_grobs <- function(plot, legend) {
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
      plot + theme(legend.position = "none"),
      nrow = 1
    ),
    legend,
    ncol = 2,
    widths = c(7, 3)
  )
}

# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}
