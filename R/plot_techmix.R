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
