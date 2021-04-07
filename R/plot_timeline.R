#' Create a time line plot
#'
#' @param data Pre-processed data for the chart, with columns: year, value,
#'   line_name.
#' @param lines_specs Dataframe containing order of lines, their labels and
#'   (optionally) colour names from the r2dii_colours palette (column
#'   'r2dii_colour_name').
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
#'   year_end = 2050,
#'   column_line_names = "emission_factor_metric",
#'   value_to_plot = "emission_factor_value",
#'   extrapolate_missing_values = TRUE
#' )
#'
#' lines_specs <- data.frame(
#'   "line_name" = c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo"),
#'   "label" = c("Projected", "Corporate Economy", "Target Demo", "Adjusted Scenario Demo"),
#'   "r2dii_colour_name" = c("dark_blue", "green", "grey", "orange")
#' )
#'
#' plot <- plot_timeline(data_sda_cement,
#'   lines_specs = lines_specs,
#'   plot_title = "Emission intensity trend for Cement.",
#'   x_title = "Year",
#'   y_title = "Tons of CO2 per ton"
#' )
#' plot
plot_timeline <- function(data,
                          lines_specs = NULL,
                          plot_title = NULL,
                          x_title = "Year",
                          y_title = "Value") {

  if (is.null(lines_specs)) {
    lines_specs <- data.frame(
      line_name = unique(data$line_name),
      label = unique(data$line_name),
      stringsAsFactors = FALSE
    )
  } else if (typeof(lines_specs) != "list") {
    msg <- paste0(
      "'line_specs' must be a dataframe.\n",
      paste0("* You've supplied a ", typeof(lines_specs), ".")
    )
    stop(msg)
  }


  # input checks
  if (!all(c("line_name", "label") %in% names(lines_specs))) {
    msg <- paste0(
      "'line_specs' must have columns 'line_name' and 'label'.\n",
      paste0("* Yours has columns: ", toString(names(lines_specs)), ".\n"),
      "* Optionally you could add a column 'r2dii_colour_name'."
    )
    stop(msg)
  }

  if (!identical(sort(unique(lines_specs$line_name)), sort(unique(data$line_name)))) {
    msg <- paste0(
      "Can't find `line_name` values from 'lines_specs' in the data.\n",
      paste0(
        "* Unique `line_name` values in 'data' are: ",
        toString(sort(unique(data$line_name))), ".\n"
      ),
      paste0(
        "* Unique `line_name` values in 'lines_specs' are: ",
        toString(sort(unique(lines_specs$line_name)))
      )
    )
    stop(msg)
  }

  if (nrow(lines_specs) > 9) {
    msg <- paste0(
      "The number of lines on the plot must be lower than 10.\n",
      paste0("* You've supplied 'lines_specs' with ", nrow(lines_specs), " rows.\n"),
      paste0("* Split up your dataset to be able to plot.")
    )
    stop(msg)
  }

  r2dii_colours <- r2dii_palette_colours()

  if (!("r2dii_colour_name" %in% colnames(lines_specs))) {
    lines_specs$r2dii_colour_name <- r2dii_colours$label[1:nrow(lines_specs)]
  } else if (!(all(lines_specs$r2dii_colour_name %in% r2dii_colours$label))) {
    msg <- paste0(
      "Colour names specified in 'lines_specs' must match those in r2dii_colours$label.\n",
      paste0("* The names in r2dii_colours are: ", toString(r2dii_colours$label), ".\n"),
      paste0(
        "* You've supplied: ",
        lines_specs %>%
          filter(!.data$r2dii_colour_name %in% r2dii_colours$label) %>%
          pull(.data$r2dii_colour_name),
        "."
      )
    )
    stop(msg)
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
