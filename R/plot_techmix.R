#' Create a techmix chart in a ggplot object
#'
#' The function returns a ggplot object containing a stacked bar chart showing a
#' technology mix for different categories (portfolio, scenario, benchmark,
#' etc.).
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
#' @export
#' @examples
#' data <- prepare_for_techmix_chart(
#'   process_input_data(example_data),
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_name = "technology_share"
#' )
#' power_colours <- get_r2dii_technology_colours("power")
#' bar_specs <- dplyr::tibble(
#'   metric_type = c(
#'     "portfolio_2020",
#'     "benchmark_2020",
#'     "portfolio_2025",
#'     "benchmark_2025",
#'     "scenario_2025"
#'   ),
#'   label = c(
#'     "Portfolio 2020",
#'     "Benchmark 2020",
#'     "Portfolio 2025",
#'     "Benchmark 2025",
#'     "Target SDS 2025"
#'   )
#' )
#'
#' print(
#'   plot_techmix(
#'     data,
#'     df_tech_colours = power_colours,
#'     df_bar_specs = bar_specs
#'   )
#' )
plot_techmix <- function(data,
                         plot_title = "",
                         show_legend = TRUE,
                         df_tech_colours,
                         df_bar_specs) {
  data_colours <- dplyr::semi_join(df_tech_colours, data, by = "technology")

  data <- data %>%
    filter(.data$metric_type %in% df_bar_specs$metric_type)

  p_techmix <- ggplot() +
    theme_2dii() +
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
    scale_fill_manual(
      labels = data_colours$label, values = data_colours$colour
    ) +
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

  p_techmix
}
