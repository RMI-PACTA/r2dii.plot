#' Create a generic ggplot object with default aesthetics
#'
#' @description
#' Returns a ggplot with common aesthetics, like removed gridlines, grey axis lines etc.
#'
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
    theme(plot.title = element_text(
      hjust = 0.5, vjust = 0.5, face = "bold",
      family = font_family, size = 14,
      margin = margin(25, 2, 8, 2)
    )) +
    theme(axis.text = element_text(
      family = font_family, size = font_size_ticks,
      margin = margin(5, 5, 5, 5)
    )) +
    theme(axis.title = element_text(
      family = font_family,
      size = font_size_axis_titles,
      margin = margin(5, 5, 5, 5)
    ))

  return(p_general)
}

#' Create a trajectory alignment chart in a ggplot object
#'
#' @param data filtered input data (dataframe with columns: year, metric_type, metric and value)
#' @param plot_title title of the plot (character string; default = "")
#' @param x_title title of the x-axis (character string; default = "")
#' @param y_title title of the y-axis (character string; default = "")
#' @param annotate_data flag indicating whether the data should be annotated (boolean; default = FALSE)
#' @param scenario_specs_good_to_bad dataframe containing scenario specifications like color or label, ordered from the most to least sustainable (dataframe with columns: scenario, label, color)
#' @param main_line_metric dataframe containing information about metric that should be plotted as the main line (dataframe with columns: metric, label)
#' @param additional_line_metrics dataframe containing information about additional metrics that should be plotted as lines (dataframe with columns: metric, label; default = data.frame())
#'
#' @description
#' The function returns a ggplot object containing a stacked bar chart showing a technology mix for different categories (portfolio, scenario, benchmark, etc.)
#'
#' @export

plot_trajectory_chart <- function(data, plot_title = "", x_title = "",
                                  y_title = "", annotate_data = FALSE,
                                  scenario_specs_good_to_bad, main_line_metric,
                                  additional_line_metrics = data.frame()) {
  p_general <- create_general_plot_with_default_settings()

  p_trajectory <- p_general +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme(axis.line = element_blank()) +
    xlab(x_title) +
    ylab(y_title) +
    labs(title = plot_title)

  if (annotate_data) {
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
    scenario_specs <- scenario_specs_good_to_bad[nrow(scenario_specs_good_to_bad):1, ]

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

  for (i in 1:length(scenario_specs$scenario)) {
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

    if (annotate_data) {
      p_trajectory <- p_trajectory +
        annotate("segment",
          x = last_year, xend = last_year + 0.75,
          y = data_scen[data_scen$year == last_year, ]$value,
          yend = data_scen[data_scen$year == last_year, ]$value,
          colour = color
        ) +
        annotate("text",
          x = (last_year + 0.85), (y <- data_scen[data_scen$year == last_year, ]$value),
          label = scenario_specs$label[i], hjust = 0, size = 3
        )
    }
  }

  data_mainline <- data %>% filter(.data$metric == main_line_metric$metric)
  p_trajectory <- p_trajectory +
    geom_line(
      data = data_mainline, aes(x = year, y = .data$value),
      linetype = "solid"
    )

  if (annotate_data) {
    p_trajectory <- p_trajectory +
      annotate("text",
        x = (last_year + 0.1), (
          y <- data_mainline[data_mainline$year == last_year, ]$value
        ),
        label = main_line_metric$label, hjust = 0, size = 3
      )
  }

  if (length(additional_line_metrics) >= 1) {
    linetypes_supporting <- c("dashed", "solid", "solid", "twodash")
    colors_supporting <- c("black", "gray", "grey46", "black")

    for (i in 1:length(additional_line_metrics$metric)) {
      metric_line <- additional_line_metrics$metric[i]
      linetype_metric <- linetypes_supporting[i]
      color_metric <- colors_supporting[i]
      label_metric <- additional_line_metrics$label[i]
      data_metric <- data %>% filter(.data$metric == metric_line)
      p_trajectory <- p_trajectory +
        geom_line(
          data = data_metric, aes(x = year, y = .data$value),
          linetype = linetype_metric, color = color_metric
        )

      if (annotate_data) {
        p_trajectory <- p_trajectory +
          annotate("text",
            x = (last_year + 0.1), (
              y <- data_metric[data_metric$year == last_year, ]$value),
            label = label_metric, hjust = 0, size = 3
          )
      }
    }
  }

  return(p_trajectory)
}

#' Create a techmix chart in a ggplot object
#'
#' @param data filtered input data (dataframe with columns: technology,
#'   metric_type, metric and value).
#' @param plot_title title of the plot (character string; default = "").
#' @param show_legend flag indicating whether legend should be shown (boolean;
#'   default = TRUE).
#' @param df_tech_colors dataframe containing colors per technology (dataframe
#'   with columns: technology, label, color).
#' @param df_bar_specs dataframe containing order of bars and their labels
#'   (dataframe with columns: metric_type, label).
#'
#' @description
#' The function returns a ggplot object containing a stacked bar chart showing a
#' technology mix for different categories (portfolio, scenario, benchmark,
#' etc.).
#'
#' @export
#' @examples
#' # TODO create an example or copy-paste an exising one from README or a test.
plot_techmix_chart <- function(data, plot_title = "", show_legend = TRUE,
                               df_tech_colors, df_bar_specs) {
  data_colors <- df_tech_colors %>%
    filter(.data$technology %in% unique(!!data$technology))

  data <- data %>%
    filter(.data$metric_type %in% df_bar_specs$metric_type)

  p_general <- create_general_plot_with_default_settings()

  p_techmix <- p_general +
    xlab("") +
    ylab("") +
    labs(title = plot_title)

  p_techmix <- p_techmix +
    geom_bar(data = data, aes(
      fill = factor(.data$technology, levels = data_colors$technology),
      x = factor(.data$metric_type, levels = rev(df_bar_specs$metric_type)),
      y = .data$value
    ), position = "fill", stat = "identity", width = .5) +
    scale_y_continuous(
      labels = scales::percent_format(), expand = c(0, 0),
      sec.axis = dup_axis()
    ) +
    scale_x_discrete(labels = rev(df_bar_specs$label)) +
    scale_fill_manual(labels = data_colors$label, values = data_colors$color) +
    coord_flip() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank())

  if (show_legend) {
    p_techmix <- p_techmix +
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(
        family = "Helvetica", size = 9,
        margin = margin(5, 5, 5, 5)
      )) +
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
#' @export

get_r2dii_sector_colours <- function(sector) {
  # styler: off
  all_colours <- tribble(
         ~sector,     ~technology,                         ~label, ~colour_hex,
         "Power",       "CoalCap",                "Coal Capacity",  "#7A2701",
         "Power",        "OilCap",                 "Oil Capacity",  "#a63603",
         "Power",        "GasCap",                 "Gas Capacity",  "#e6550d",
         "Power",    "NuclearCap",             "Nuclear Capacity",  "#fd8d3c",
         "Power",      "HydroCap",               "Hydro Capacity",  "#fdbe85",
         "Power", "RenewablesCap",          "Renewables Capacity",  "#ffd4ad",
    "Automotive",      "Electric",                     "Electric",  "#548995",
    "Automotive",  "Electric_HDV", "Electric Heavy Duty Vehicles",  "#609cab",
    "Automotive",      "FuelCell",                     "FuelCell",  "#6cb0c0",
    "Automotive",        "Hybrid",                       "Hybrid",  "#78c4d6",
    "Automotive",    "Hybrid_HDV",   "Hybrid Heavy Duty Vehicles",  "#93cfde",
    "Automotive",           "ICE",                          "ICE",  "#aedbe6",
    "Automotive",       "ICE_HDV",      "ICE Heavy Duty Vehicles",  "#c9e7ee",
       "Oil&Gas",           "Gas",                          "Gas",  "#b9b5b0",
       "Oil&Gas",           "Oil",                          "Oil",  "#181716",
  "Fossil Fuels",           "Gas",                          "Gas",  "#b9b5b0",
  "Fossil Fuels",           "Oil",                          "Oil",  "#181716",
  "Fossil Fuels",          "Coal",                         "Coal",  "#4e3b37"
  )
  # styler: on

  all_colours <- all_colours %>%
    mutate(sector = tolower(.data$sector), technology = tolower(.data$technology))

  colours <- all_colours %>%
    filter(.data$sector == !!sector) %>%
    select(.data$technology, .data$label, colour = .data$colour_hex)
}

#' Get the 2DII colour palette
#'
#' @export

r2dii_palette_colours <- function() {
  # styler: off
  tribble(
    ~label, ~colour_hex,
    "dark_blue",   "#1b324f",
    "green",   "#00c082",
    "orange",   "#ff9623",
    "dark_purple",   "#574099",
    "yellow",   "#f2e06e",
    "soft_blue",   "#78c4d6",
    "ruby_red",   "#a63d57",
    "grey",   "#d0d7e1",
    "moss_green",   "#4a5e54"
  )
  # styler: on
}
