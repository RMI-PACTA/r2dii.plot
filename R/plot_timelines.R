#' Create a time line plot
#'
#' @param data Pre-processed data for the chart, with columns: year, value,
#'   line_name.
#' @param lines_specs Dataframe containing order of lines, their labels and
#'   (optionally) colour names from the r2dii_colours palette.
#' @param plot_title Title of the plot.
#' @param x_title,y_title x- and y-axis title.
#'
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
#' data_sda_cement <- prepare_for_timeline(sda_target,
#'   sector_filter = "cement",
#'   year_start = 2020,
#'   year_end = 2026,
#'   column_line_names = "emission_factor_metric",
#'   value_to_plot = "emission_factor_value"
#' )
#'
#' lines_specs <- data.frame(
#'   "line_name" = c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo"),
#'   "label" = c("Projected", "Corporate Economy", "Target Demo", "Adjusted Scenario Demo"),
#'   "r2dii_colour_name" = c("dark_blue", "green", "grey", "orange")
#' )
#'
#' plot_timelines(data_sda_cement,
#'   lines_specs = lines_specs,
#'   plot_title = "Emission intensity trend for Cement.",
#'   x_title = "Year",
#'   y_title = "Tons of CO2 per ton"
#' )
plot_timelines <- function(data,
                           lines_specs = NULL,
                           plot_title = NULL,
                           x_title = "Year",
                           y_title = "Value") {
  if (is.null(lines_specs)) {
    lines_specs <- data.frame(
      "line_name" = unique(data$line_name),
      "label" = unique(data$line_name)
    )
  } else if (typeof(lines_specs) != "list") {
    stop("'line_specs' needs to be a dataframe with columns 'line_name', 'label' and (optional) 'r2dii_colour_name'.")
  }

  # input checks
  if (!identical(sort(unique(lines_specs$line_name)), sort(unique(data$line_name)))) {
    stop("The line_name values specified in parameter 'lines_specs' do not match the data.")
  }

  if (nrow(lines_specs) > 9) {
    stop("The maximal number of lines on the plot is 9. Decrease the number of unique 'line_names' in the data to be able to plot.")
  }

  r2dii_colours <- r2dii_palette_colours()

  if (!("r2dii_colour_name" %in% colnames(lines_specs))) {
    lines_specs$r2dii_colour_name <- r2dii_colours$label[1:nrow(lines_specs)]
  } else if (!(all(lines_specs$r2dii_colour_name %in% r2dii_colours$label))) {
    stop("Colour names specified in 'lines_specs' do not match those in r2dii_colours$label.")
  }

  lines_specs <- left_join(lines_specs, r2dii_colours,
    by = c("r2dii_colour_name" = "label")
  )

  plot <- ggplot(
    data = data %>% filter(.data$extrapolated == FALSE),
    aes(
      x = .data$year,
      y = .data$value,
      colour = factor(.data$line_name, levels = lines_specs$line_name)
    ),
    linetype = .data$extrapolated
  ) +
    geom_line() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    expand_limits(y = 0) +
    labs(title = plot_title) +
    xlab(x_title) +
    ylab(y_title) +
    scale_colour_manual(
      values = lines_specs$colour_hex,
      labels = lines_specs$label
    ) +
    theme_2dii_ggplot()

  if (any(data$extrapolated)) {
    plot <- plot +
      geom_line(
        data = data %>% filter(.data$extrapolated == TRUE),
        aes(
          x = .data$year,
          y = .data$value,
          colour = factor(.data$line_name, levels = lines_specs$line_name),
          linetype = .data$extrapolated
        )
      ) +
      scale_linetype_manual(values = "dashed") +
      guides(linetype = FALSE)
  }

  plot
}
