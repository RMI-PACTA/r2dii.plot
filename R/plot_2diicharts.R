#' Create a generic ggplot object with default aesthetics
#'
#' @description
#' Returns a ggplot with common aesthetics, like removed gridlines, grey axis lines etc.
#'
#' @import ggplot2
#' @export

create_general_plot_with_default_settings <- function() {
  font_family <- "Helvetica"
  font_size_ticks <- 10
  font_size_axis_titles <- 12
  supporting_elts_color <- "#C0C0C0"

  p_general <- ggplot() +
    theme_classic() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) +
    theme(axis.line = element_line(colour = supporting_elts_color)) +
    theme(axis.ticks = element_line(colour = supporting_elts_color)) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", family = font_family, size = 14, margin = margin(25, 2, 8, 2))) +
    theme(axis.text = element_text(family = font_family, size = font_size_ticks, margin = margin(5, 5, 5, 5))) +
    theme(axis.title = element_text(family = font_family, size = font_size_axis_titles, margin = margin(5, 5, 5, 5)))

  return(p_general)
}

#' Create a trajectory alignment chart in a ggplot object
#'
#' @param data filtered input data (dataframe with columns: year, metric_type, metric and value)
#' @param plotTitle title of the plot (character string; default = "")
#' @param xTitle title of the x-axis (character string; default = "")
#' @param yTitle title of the y-axis (character string; default = "")
#' @param annotateData flag indicating whether the data should be annotated (boolean; default = FALSE)
#' @param scenario_specs_good_to_bad dataframe containing scenario specifications like color or label, ordered from the most to least sustainable (dataframe with columns: scenario, label, color)
#' @param mainLineMetric dataframe containing information about metric that should be plotted as the main line (datframe with columns: metric, label)
#' @param additionalLineMetrics dataframe containing information about additional metrics that should be plotted as lines (datframe with columns: metric, label; default = data.frame())
#'
#' @description
#' The function returns a ggplot object containing a stacked bar chart showing a technology mix for different categories (portfolio, scenario, benchmark, etc.)
#'
#' @import ggplot2
#' @import dplyr
#' @import r2dii.data
#' @export

plot_trajectory_chart <- function(data, plotTitle = "", xTitle = "", yTitle = "", annotateData = FALSE,
                                  scenario_specs_good_to_bad, mainLineMetric,
                                  additionalLineMetrics = data.frame()) {
  p_general <- create_general_plot_with_default_settings()

  p_trajectory <- p_general +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme(axis.line = element_blank()) +
    xlab(xTitle) +
    ylab(yTitle) +
    labs(title = plotTitle)

  if (annotateData) {
    p_trajectory <- p_trajectory +
      theme(plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm"))
  } else {
    p_trajectory <- p_trajectory +
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  }

  lower_area_border <- min(data$value)
  upper_area_border <- max(data$value)
  last_year <- max(data$year)

  year <- unique(data$year)
  data_worse_than_scenarios <- data.frame(year)

  green_or_brown <- r2dii.data::green_or_brown
  tech_green_or_brown <- green_or_brown[green_or_brown$technology == data$technology[1], ]$green_or_brown

  if (tech_green_or_brown == "brown") {
    scenario_specs <- scenario_specs_good_to_bad

    data_worse_than_scenarios$value <- upper_area_border
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- data %>%
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, .data$value)

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      arrange(.data$year, factor(.data$metric, levels = scenario_specs$scenario)) %>%
      mutate(value_low = dplyr::lag(.data$value, n = 1, default = lower_area_border))
  } else if (tech_green_or_brown == "green") {
    scenario_specs <- scenario_specs_good_to_bad[nrow(scenario_specs_good_to_bad):1, ]

    data_scenarios <- data %>%
      filter(.data$metric_type == "scenario") %>%
      select(.data$year, .data$metric, value_low = .data$value)

    data_worse_than_scenarios$value_low <- lower_area_border
    data_worse_than_scenarios$metric <- "worse"

    data_scenarios <- rbind(data_scenarios, data_worse_than_scenarios) %>%
      group_by(.data$year) %>%
      arrange(.data$year, factor(.data$metric, levels = scenario_specs$scenario)) %>%
      mutate(value = dplyr::lead(.data$value_low, n = 1, default = upper_area_border))
  }

  for (i in 1:length(scenario_specs$scenario)) {
    scen <- scenario_specs$scenario[i]
    color <- scenario_specs$color[i]
    data_scen <- data_scenarios %>% filter(.data$metric == scen)
    p_trajectory <- p_trajectory +
      geom_ribbon(data = data_scen, aes(ymin = .data$value_low, ymax = .data$value, x = year, group = 1), fill = color, alpha = 0.75)

    if (scen != "worse") {
      if (tech_green_or_brown == "brown") {
        p_trajectory <- p_trajectory +
          geom_line(data = data_scen, aes(x = year, y = .data$value), color = color)
      } else if (tech_green_or_brown == "green") {
        p_trajectory <- p_trajectory +
          geom_line(data = data_scen, aes(x = year, y = .data$value_low), color = color)
      }
    }

    if (annotateData) {
      p_trajectory <- p_trajectory +
        annotate("segment",
          x = last_year, xend = last_year + 0.75, y = data_scen[data_scen$year == last_year, ]$value,
          yend = data_scen[data_scen$year == last_year, ]$value, colour = color
        ) +
        annotate("text",
          x = (last_year + 0.85), (y <- data_scen[data_scen$year == last_year, ]$value),
          label = scenario_specs$label[i], hjust = 0, size = 3
        )
    }
  }

  data_mainline <- data %>% filter(.data$metric == mainLineMetric$metric)
  p_trajectory <- p_trajectory +
    geom_line(data = data_mainline, aes(x = year, y = .data$value), linetype = "solid")

  if (annotateData) {
    p_trajectory <- p_trajectory +
      annotate("text",
        x = (last_year + 0.1), (y <- data_mainline[data_mainline$year == last_year, ]$value),
        label = mainLineMetric$label, hjust = 0, size = 3
      )
  }

  if (length(additionalLineMetrics) >= 1) {
    linetypes_supporting <- c("dashed", "solid", "solid", "twodash")
    colors_supporting <- c("black", "gray", "grey46", "black")

    for (i in 1:length(additionalLineMetrics$metric)) {
      metric_line <- additionalLineMetrics$metric[i]
      linetype_metric <- linetypes_supporting[i]
      color_metric <- colors_supporting[i]
      label_metric <- additionalLineMetrics$label[i]
      data_metric <- data %>% filter(.data$metric == metric_line)
      p_trajectory <- p_trajectory +
        geom_line(data = data_metric, aes(x = year, y = .data$value), linetype = linetype_metric, color = color_metric)

      if (annotateData) {
        p_trajectory <- p_trajectory +
          annotate("text",
            x = (last_year + 0.1), (y <- data_metric[data_metric$year == last_year, ]$value),
            label = label_metric, hjust = 0, size = 3
          )
      }
    }
  }

  return(p_trajectory)
}

#' Create a techmix chart in a ggplot object
#'
#' @param data filtered input data (dataframe with columns: technology, metric_type, metric and value)
#' @param plotTitle title of the plot (character string; default = "")
#' @param showLegend flag indicating whether legend should be shown (boolean; default = TRUE)
#' @param df_tech_colors dataframe cotaining colors per technology (dataframe with columns: technology, label, color)
#' @param df_bar_specs dataframe containing order of bars and their labels (datframe with columns: metric_type, label)
#'
#' @description
#' The function returns a ggplot object containing a stacked bar chart showing a technology mix for different categories (portfolio, scenario, benchmark, etc.)
#'
#' @import ggplot2
#' @import dplyr
#' @export

plot_techmix_chart <- function(data, plotTitle = "", showLegend = TRUE, df_tech_colors, df_bar_specs) {
  data_colors <- df_tech_colors %>%
    filter(.data$technology %in% unique(!!data$technology))

  data <- data %>%
    filter(.data$metric_type %in% df_bar_specs$metric_type)

  p_general <- create_general_plot_with_default_settings()

  p_techmix <- p_general +
    xlab("") +
    ylab("") +
    labs(title = plotTitle)

  p_techmix <- p_techmix +
    geom_bar(data = data, aes(
      fill = factor(.data$technology, levels = data_colors$technology),
      x = factor(.data$metric_type, levels = rev(df_bar_specs$metric_type)),
      y = .data$value
    ), position = "fill", stat = "identity", width = .5) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), sec.axis = dup_axis()) +
    scale_x_discrete(labels = rev(df_bar_specs$label)) +
    scale_fill_manual(labels = data_colors$label, values = data_colors$color) +
    coord_flip() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank())

  if (showLegend) {
    p_techmix <- p_techmix +
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(family = "Helvetica", size = 9, margin = margin(5, 5, 5, 5))) +
      theme(legend.title = element_blank()) +
      guides(fill = guide_legend(ncol = 4, byrow = TRUE))
  } else {
    p_techmix <- p_techmix +
      theme(legend.position = "none")
  }

  return(p_techmix)
}

#' Get the predefined technology colors for a sector
#'
#' @param sector sector for which we want to retrieve colors (a character string)
#'
#' @import dplyr
#' @export

get_sector_colors <- function(sector) {
  all_colors <- structure(
    list(
      sector = c(
        "Power", "Power", "Power", "Power",
        "Power", "Power", "Automotive", "Automotive", "Automotive", "Automotive",
        "Automotive", "Automotive", "Automotive", "Oil&Gas", "Oil&Gas",
        "Fossil Fuels", "Fossil Fuels", "Fossil Fuels"
      ),
      technology = c(
        "CoalCap", "OilCap", "GasCap", "NuclearCap", "HydroCap", "RenewablesCap",
        "Electric", "Electric_HDV", "FuelCell", "Hybrid", "Hybrid_HDV",
        "ICE", "ICE_HDV", "Gas", "Oil", "Gas", "Oil", "Coal"
      ),
      label = c(
        "Coal Capacity", "Oil Capacity", "Gas Capacity", "Nuclear Capacity", "Hydro Capacity", "Renewables Capacity", "Electric", "Electric Heavy Duty Vehicles",
        "FuelCell", "Hybrid", "Hybrid Heavy Duty Vehicles", "ICE", "ICE Heavy Duty Vehicles",
        "Gas", "Oil", "Gas", "Oil", "Coal"
      ),
      color_hex = c(
        "#7A2701", "#a63603", "#e6550d", "#fd8d3c", "#fdbe85", "#ffd4ad", "#548995",
        "#609cab", "#6cb0c0", "#78c4d6", "#93cfde", "#aedbe6", "#c9e7ee",
        "#b9b5b0", "#181716", "#b9b5b0", "#181716", "#4e3b37"
      )
    ),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -18L),
    spec = structure(list(cols = list(sector = structure(list(), class = c(
      "collector_character",
      "collector"
    )), technology = structure(list(), class = c(
      "collector_character",
      "collector"
    )), label = structure(list(), class = c(
      "collector_character",
      "collector"
    )), color_hex = structure(list(), class = c(
      "collector_character",
      "collector"
    ))), default = structure(list(), class = c(
      "collector_guess",
      "collector"
    )), skip = 1L), class = "col_spec")
  )

  all_colors <- all_colors %>%
    mutate(sector = tolower(.data$sector), technology = tolower(.data$technology))

  colors <- all_colors %>%
    filter(.data$sector == !!sector) %>%
    select(.data$technology, .data$label, color = .data$color_hex)

  return(colors)
}
