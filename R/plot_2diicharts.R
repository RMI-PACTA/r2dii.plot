#' Create a bar chart with overview of asset types per investor type
#'
#' This function plots a horizontal stacked bar chart with composition of
#' investors securities per investor type. No need to specify the colours if the
#' security types in your dataset are only: Equity, Bonds and Others. Otherwise
#' full specification needs to be passed to the function, for all security
#' types.
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, asset_type, share (dataframe).
#' @param bars_asset_type_specs Dataframe with specifications for each asset
#'   type, columns: asset_type, legend label, r2dii_colour_name (dataframe).
#' @param bars_labels_specs Dataframe with labels for investor types, columns:
#'   investor_type, label. If no is specified, investor_type from data is used
#'   as label. (dataframe).
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

  p_bar
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
#' @return An object of class "ggplot".
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
    plot_title <-
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

  plot
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
#' @return An object of class "ggplot".
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
    plot_title <-
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

  plot
}

#' Create a meta-report distribution chart
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
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
#' # TODO
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

  p <- ggplot(
    data,
    aes(
      x = factor(.data$portfolio_name, levels = .data$portfolio_name),
      y = .data$value,
      fill = factor(.data$investor_name,
        levels = investor_labels$investor_name
      )
    )
  ) +
    geom_bar(stat = "identity", width = 0.94) +
    xlab(x_title) +
    ylab(y_title) +
    labs(title = plot_title) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = r2dii_colours$colour_hex[c(1:length(investor_labels$label))],
      labels = investor_labels$label
    ) +
    theme_2dii_ggplot() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(legend.position = "bottom")

  p
}

#' Create a meta-report bubble chart
#'
#' @param data Dataframe with data processed for the chart. With columns:
#'   investor_name, portfolio_name, value_x, value_y (dataframe).
#' @param plot_title Title of the plot; (character string).
#' @param x_title,y_title x- and y-axis title (character string).
#' @param investor_labels Dataframe with order and labels for investor types,
#'   columns: investor_type, label. Can be used for preserving colour order
#'   between different plots (using the same df will preserve the order). If no
#'   is specified, investor_type from data is used as label. (dataframe).
#' @param colour_investors flag indicating if points should be coloured
#'   (boolean).
#' @param show_legend_when_coloured flag indicating if legend should be showed
#'   if points are coloured (boolean).
#'
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
#' # TODO
plot_metareport_bubble <- function(data,
                                   plot_title = NULL,
                                   x_title = "",
                                   y_title = "",
                                   investor_labels = NULL,
                                   colour_investors = TRUE,
                                   show_legend_when_coloured = TRUE) {
  if (is.null(investor_labels)) {
    investor_labels <- data.frame(
      "investor_name" = unique(data$investor_name),
      "label" = unique(data$investor_name)
    )
  }

  if (colour_investors) {
    r2dii_colours <- r2dii_palette_colours()

    p <- ggplot(
      data,
      aes(
        x = .data$value_x,
        y = .data$value_y,
        colour = factor(.data$investor_name,
          levels = investor_labels$investor_name
        )
      )
    )
  } else {
    p <- ggplot(
      data,
      aes(
        x = .data$value_x,
        y = .data$value_y
      )
    )
  }

  p <- p +
    geom_point(size = 3, alpha = 0.6) +
    xlab(x_title) +
    ylab(y_title) +
    labs(title = plot_title) +
    scale_x_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_2dii_ggplot() +
    theme(legend.position = "none") +
    theme(
      panel.grid.major = element_line(colour = "grey92", linetype = "dashed"),
      panel.grid.minor.x = element_line(colour = "grey92", linetype = "dashed"),
      panel.grid.minor.y = element_line(colour = "grey92", linetype = "dashed")
    )

  if (colour_investors) {
    p <- p +
      scale_colour_manual(
        values = r2dii_colours$colour_hex[c(1:length(investor_labels$label))],
        labels = investor_labels$label
      )

    if (show_legend_when_coloured) {
      p <- p +
        theme(legend.position = "bottom")
    }
  }

  p
}

#' Create a meta-report map chart
#'
#' @param data Dataframe with data processed for the chart. With columns: long,
#'   lat, group, value (dataframe).
#' @param plot_title Title of the plot; (character string).
#' @param legend_title Title of the legend describing value units; (character
#'   string).
#' @param sector Sector to be used for the map colouring. If none is specified,
#'   default dark blue palette is used (character string)
#'
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
plot_metareport_map <- function(data,
                                plot_title = NULL,
                                legend_title = NULL,
                                sector = NULL) {
  if (is.null(sector)) {
    colours <- r2dii_palette_colours()

    dark_colour <- colours %>%
      filter(.data$label == "dark_blue") %>%
      pull(.data$colour_hex)
  } else {
    sec_colours <- r2dii_sector_colours()

    dark_colour <- sec_colours %>%
      filter(.data$label == tolower(!!sector)) %>%
      pull(.data$colour_hex)
  }

  colour_function <- colorRampPalette(c("white", dark_colour))

  data <- data %>%
    replace_na(list(value = 0))

  p <- ggplot(
    data,
    aes(
      x = .data$long,
      y = .data$lat,
      group = .data$group,
      fill = .data$value
    )
  ) +
    scale_fill_gradientn("", colours = colour_function(9)) +
    geom_polygon() +
    coord_cartesian(ylim = c(-55, 85)) +
    ggtitle(plot_title) +
    theme_2dii_ggplot() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    theme(legend.position = "bottom") %+replace%
    theme(legend.title = element_text()) +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    guides(fill = guide_colourbar(
      title = legend_title,
      title.position = "right",
      barwidth = unit(0.2, "npc")
    ))

  p
}
