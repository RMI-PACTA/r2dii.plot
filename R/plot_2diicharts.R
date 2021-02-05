#' Returns a custom 2dii ggplot theme
#'
#' @param font_family argument specifying the font that should be used in a
#'   graph (character string taking the same values as standard ggplot font
#'   families).
#'
#' @description Returns a ggplot theme which can be applied to all graphs to
#'   appear according to 2DII plotting aesthetics, like removed gridlines, grey
#'   axis lines etc.
#'
#' @export

theme_2dii_ggplot <- function(font_family = "Helvetica") {
  font_size_ticks <- 10
  font_size_axis_titles <- 12
  supporting_elts_color <- "#C0C0C0"

  theme_classic() %+replace%
    theme(
      plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
      axis.line = element_line(colour = supporting_elts_color),
      axis.ticks = element_line(colour = supporting_elts_color),
      plot.title = element_text(
        hjust = 0.5, vjust = 0.5, face = "bold",
        family = font_family, size = 14,
        margin = margin(25, 2, 8, 2)
      ),
      axis.text = element_text(
        family = font_family, size = font_size_ticks,
        margin = margin(5, 5, 5, 5)
      ),
      axis.title = element_text(
        family = font_family,
        size = font_size_axis_titles,
        margin = margin(5, 5, 5, 5)
      ),
      legend.text = element_text(
        family = font_family, size = 9,
        margin = margin(5, 5, 5, 5)
      ),
      legend.title = element_blank()
    )
}

#' Create a trajectory alignment chart in a ggplot object
#'
#' @param data Filtered input data (dataframe with columns: year, metric_type,
#'   metric and value).
#' @param plot_title Title of the plot (character string).
#' @param x_title,y_title Title of the x- and y-axis (character string).
#' @param annotate_data Flag indicating whether the data should be annotated
#'   (boolean).
#' @param scenario_specs_good_to_bad Dataframe containing scenario
#'   specifications like color or label, ordered from the most to least
#'   sustainable (dataframe with columns: scenario, label, color).
#' @param main_line_metric Dataframe containing information about metric that
#'   should be plotted as the main line (dataframe with columns: metric, label).
#' @param additional_line_metrics Dataframe containing information about
#'   additional metrics that should be plotted as lines (dataframe with columns:
#'   metric, label).
#'
#' @description The function returns a ggplot object containing a stacked bar
#'   chart showing a technology mix for different categories (portfolio,
#'   scenario, benchmark, etc.).
#'
#' @export

plot_trajectory <- function(data,
                            plot_title = "",
                            x_title = "",
                            y_title = "",
                            annotate_data = FALSE,
                            scenario_specs_good_to_bad,
                            main_line_metric,
                            additional_line_metrics = data.frame()) {

  p_trajectory <- ggplot() +
    theme_2dii_ggplot() +
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
    scenario_specs <- scenario_specs_good_to_bad[
      nrow(scenario_specs_good_to_bad):1,
    ]

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
          x = (last_year + 0.85),
          (y <- data_scen[data_scen$year == last_year, ]$value),
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
  p_trajectory
}

#' Create a techmix chart in a ggplot object
#'
#' @param data Filtered input data (dataframe with columns: technology,
#'   metric_type, metric and value).
#' @param plot_title Title of the plot (character string).
#' @param show_legend Flag indicating whether legend should be shown (boolean).
#' @param df_tech_colours Dataframe containing colors per technology (dataframe
#'   with columns: technology, label, color).
#' @param df_bar_specs Dataframe containing order of bars and their labels
#'   (dataframe with columns: metric_type, label).
#'
#' @description The function returns a ggplot object containing a stacked bar
#' chart showing a technology mix for different categories (portfolio, scenario,
#' benchmark, etc.).
#'
#' @export
#' @examples
#' # TODO create an example or copy-paste an existing one from README or a test.

plot_techmix <- function(data,
                         plot_title = "",
                         show_legend = TRUE,
                         df_tech_colours,
                         df_bar_specs) {

  data_colours <- df_tech_colours %>%
    filter(.data$technology %in% unique(!!data$technology))

  data <- data %>%
    filter(.data$metric_type %in% df_bar_specs$metric_type)

  p_techmix <- ggplot() +
    theme_2dii_ggplot() +
    xlab("") +
    ylab("") +
    labs(title = plot_title)

  p_techmix <- p_techmix +
    geom_bar(data = data, aes(
      fill = factor(.data$technology, levels = data_colours$technology),
      x = factor(.data$metric_type, levels = rev(df_bar_specs$metric_type)),
      y = .data$value
    ), position = "fill", stat = "identity", width = .5) +
    scale_y_continuous(
      labels = scales::percent_format(), expand = c(0, 0),
      sec.axis = dup_axis()
    ) +
    scale_x_discrete(labels = rev(df_bar_specs$label)) +
    scale_fill_manual(labels = data_colours$label, values = data_colours$colour) +
    coord_flip() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank())

  if (show_legend) {
    p_techmix <- p_techmix +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(ncol = 3, byrow = TRUE))
  } else {
    p_techmix <- p_techmix +
      theme(legend.position = "none")
  }
}

#' Create a bar chart with overview of asset types per investor type
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, asset_type, share (dataframe).
#' @param bars_asset_type_specs Dataframe with specifications for each asset
#'   type, columns: asset_type, legend label, r2dii_colour_name (dataframe).
#' @param bars_labels_specs Dataframe with labels for investor types, columns:
#'   investor_type, label. If no is specified, investor_type from data is used
#'   as label. (dataframe).
#'
#' @description This function plots a horizontal stacked bar chart with
#'   composition of investors securities per investor type. No need to specify
#'   the colours if the security types in your dataset are only: Equity, Bonds
#'   and Others. Otherwise full specification needs to be passed to the
#'   function, for all security types.
#'
#' @export

plot_metareport_security_types <- function(data,
                                           bars_asset_type_specs =
                                             data.frame(
                                               "asset_type" =
                                                 c("Equity", "Bonds", "Others"),
                                               "label" =
                                                 c("Equity", "Bonds", "Others"),
                                               "r2dii_colour_name" =
                                                 c("dark_blue", "green", "grey")
                                             ),
                                           bars_labels_specs = NULL) {
  r2dii_colors <- r2dii_palette_colours()

  bars_asset_type_specs <- left_join(bars_asset_type_specs, r2dii_colors,
    by = c("r2dii_colour_name" = "label")
  )

  if (is.null(bars_labels_specs)) {
    bars_labels_specs <- data.frame(
      "investor_name" = unique(data$investor_name),
      "label" = unique(data$investor_name)
    )
  }

  p_bar <- ggplot(
    data = data,
    aes(
      fill = factor(.data$asset_type, levels = rev(bars_asset_type_specs$asset_type)),
      x = factor(.data$investor_name, levels = rev(bars_labels_specs$investor_name)),
      y = .data$share
    )
  ) +
    theme_2dii_ggplot() +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    scale_fill_manual(values = rev(bars_asset_type_specs$colour_hex)) +
    scale_x_discrete(labels = rev(bars_labels_specs$label)) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = c(0, 0),
      sec.axis = dup_axis()
    ) +
    ylab("") +
    xlab("") +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(reverse = TRUE))
}

#' Create a small multiples bar chart with portfolio exposure to PACTA sectors
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, asset_type, share_climate_relevant (dataframe).
#' @param bars_labels_specs Dataframe with labels for investor types, columns:
#'   investor_type, label. If no is specified, investor_type from data is used
#'   as label. (dataframe).
#' @param plot_title Title of the plot; if no title is specified, the plot title
#'   is set in the code to a predetermined character string (character string).
#'
#' @return an object of class "ggplot"
#' @export
#'
#' @examples
#' # TODO
plot_metareport_pacta_sectors <- function(data,
                                          bars_labels_specs = NULL,
                                          plot_title = NULL) {
  r2dii_colors <- r2dii_palette_colours()

  asset_types <- c("Equity", "Bonds")
  asset_types_colours <- c("dark_blue", "green")

  ylim_up <- max(data$share_climate_relevant)

  if (is.null(bars_labels_specs)) {
    bars_labels_specs <- data.frame(
      "investor_name" = unique(data$investor_name),
      "label" = unique(data$investor_name)
    )
  }

  if (is.null(plot_title)) {
    plot_title =
      "Percentage of Equity and Bonds Portfolios invested in PACTA sectors"
  }

  subplots <- list()

  for (i in 1:2) {
    asset_type_filter <- asset_types[i]
    data_subplot <- data %>% filter(.data$asset_type == asset_type_filter)

    subplot <- ggplot(
      data = data_subplot,
      aes(
        x = factor(.data$investor_name,
          levels = rev(bars_labels_specs$investor_name)
        ),
        y = .data$share_climate_relevant,
        fill = .data$asset_type
      )
    ) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = r2dii_colors %>%
        filter(.data$label == asset_types_colours[i]) %>%
        pull(.data$colour_hex)) +
      scale_x_discrete(labels = rev(bars_labels_specs$label)) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        limits = c(0, ylim_up)
      ) +
      ylab("") +
      xlab("") +
      labs(title = asset_type_filter) +
      coord_flip() +
      theme_2dii_ggplot() +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(legend.position = "none") %+replace%
      theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm")) %+replace%
      theme(plot.title = element_text(
        hjust = 0.5, vjust = 0.5, face = "plain",
        size = 11,
        margin = margin(4, 2, 4, 2)
      ))

    subplots[[asset_type_filter]] <- subplot
  }

  plot <- ggarrange(subplots[["Equity"]], subplots[["Bonds"]], nrow = 2, ncol = 1)

  plot <- annotate_figure(plot, top = text_grob(plot_title, face = "bold", size = 14))
}

#' Create a small multiples stacked bar chart with percentage investment in
#' PACTA sectors
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, asset_type, sector, share (dataframe).
#' @param plot_title Title of the plot; if no title is specified, the plot title
#'   is set in the code to a predetermined character string (character string).
#' @param df_sectors_order Dataframe with ordered sector names and their labels
#'   (dataframe).
#' @param bars_labels_specs Dataframe with labels for investor types, columns:
#'   investor_type, label. If no is specified, investor_type from data is used
#'   as label. (dataframe).
#'
#' @return an object of class "ggplot"
#' @export
#'
#' @examples
#' # TODO
plot_metareport_pacta_sectors_mix <- function(data,
                                              plot_title = NULL,
                                              df_sectors_order = data.frame(
                                                "sector" = c(
                                                  "steel", "cement", "shipping",
                                                  "aviation", "automotive", "power",
                                                  "coal", "oil&gas"
                                                ),
                                                "label" = c(
                                                  "Steel", "Cement", "Shipping",
                                                  "Aviation", "Automotive", "Power",
                                                  "Coal", "Oil & Gas"
                                                )
                                              ),
                                              bars_labels_specs = NULL) {

  if (is.null(bars_labels_specs)) {
    bars_labels_specs <- data.frame(
      "investor_name" = unique(data$investor_name),
      "label" = unique(data$investor_name)
    )
  }

  if (is.null(plot_title)) {
    plot_title =
      "Investment per sector as percentage of total value invested in PACTA sectors"
  }

  r2dii_sector_colours <- r2dii_sector_colours()

  data_colours <- df_sectors_order %>%
    left_join(r2dii_sector_colours, by = c("sector" = "label"))

  asset_types <- c("Equity", "Bonds")

  subplots <- list()

  for (i in 1:2) {
    asset_type_filter <- asset_types[i]
    data_subplot <- data %>% filter(.data$asset_type == asset_type_filter)

    subplot <- ggplot() +
      xlab("") +
      ylab("") +
      labs(title = asset_type_filter) +
      geom_bar(data = data_subplot, aes(
        fill = factor(tolower(.data$sector), levels = data_colours$sector),
        x = factor(.data$investor_name,
          levels = rev(bars_labels_specs$investor_name)
        ),
        y = .data$share
      ), position = "fill", stat = "identity", width = .7) +
      scale_y_continuous(
        labels = scales::percent_format(), expand = c(0, 0)
      ) +
      scale_x_discrete(labels = rev(bars_labels_specs$label)) +
      scale_fill_manual(
        labels = data_colours$label,
        values = data_colours$colour_hex
      ) +
      coord_flip() +
      theme_2dii_ggplot() +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(legend.position = "none") %+replace%
      theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm")) %+replace%
      theme(plot.title = element_text(
        hjust = 0.5, vjust = 0.5, face = "plain",
        size = 11,
        margin = margin(4, 2, 4, 2)
      ))

    subplots[[asset_type_filter]] <- subplot
  }
  legend <- get_legend(subplots[["Equity"]] +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(
      ncol = 4, byrow = TRUE,
      reverse = TRUE
    )))
  plot <- ggarrange(subplots[["Equity"]], subplots[["Bonds"]],
    nrow = 2, ncol = 1,
    legend.grob = legend, legend = "bottom"
  )
  plot <- annotate_figure(plot, top = text_grob(plot_title,
    face = "bold",
    size = 14
  ))
}

#' Create a metareport distribution chart
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, portfolio_name, value (dataframe).
#' @param plot_title Title of the plot; (character string).
#' @param x_title,y_title x- and y-axis title (character string).
#' @param investor_labels Dataframe with order and labels for investor types,
#'   columns: investor_type, label. Can be used for preserving colour order
#'   between different plots (using the same df will preserve the order). If no
#'   is specified, investor_type from data is used as label. (dataframe).
#'
#' @return an object of class "ggplot"
#' @export
#'
#' @examples
#' #TODO
plot_metareport_distribution <- function(data,
                                         plot_title = "",
                                         x_title = "",
                                         y_title = "",
                                         investor_labels = NULL) {

  if (is.null(investor_labels)) {
    investor_labels <- data.frame(
      "investor_name" = unique(data$investor_name),
      "label" = unique(data$investor_name)
    )
  }

  r2dii_colours <- r2dii_palette_colours()

  p <- ggplot(data,
              aes(
                x = factor(.data$portfolio_name, levels = .data$portfolio_name),
                y = .data$value,
                fill = factor(.data$investor_name,
                levels = investor_labels$investor_name))) +
    geom_bar(stat="identity", width = 0.94) +
    xlab(x_title) +
    ylab(y_title) +
    labs(title = plot_title) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = c(0, 0)) +
    scale_fill_manual(
      values = r2dii_colours$colour_hex[c(1:length(investor_labels$label))],
      labels = investor_labels$label) +
    theme_2dii_ggplot() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "bottom")
}

#' Get the predefined technology colors for a sector
#'
#' @param sector Sector for which we want to retrieve colors (character string).
#'
#' @export

get_r2dii_technology_colours <- function(sector) {
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
         "grey",   "#d0d7e1",
  "dark_purple",   "#574099",
       "yellow",   "#f2e06e",
    "soft_blue",   "#78c4d6",
     "ruby_red",   "#a63d57",
   "moss_green",   "#4a5e54"
  )
  # styler: on
}

#' Get the 2DII sector colour palette
#'
#' @return tribble with sector labels and colour hex strings.
#' @export

r2dii_sector_colours <- function() {
  # styler: off
  tribble(
        ~label, ~colour_hex,
  "automotive",   "#78C4D6",
    "aviation",   "#00c082",
      "cement",   "#c1b358",
        "coal",   "#4e3b37",
     "oil&gas",   "#181716",
       "power",   "#a63603",
    "shipping",   "#574099",
       "steel",   "#a63d57"
  )
  # styler: on
}
