#' Create a trajectory plot using default settings
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
#'
#' @seealso [market_share].
#'
#' @return An object of class "ggplot".
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'    technology == "renewablescap" &
#'    region == "global" &
#'    scenario_source == "demo_2020"
#'    )
#'
#' qplot_trajectory(data)
qplot_trajectory <- function(data) {
  check_plot_trajectory(data)

  data <- data %>%
    mutate(label = sub("target_", "", .data$metric),
      label = case_when(
        is_scenario(.data$metric) ~ toupper(as.character(.data$label)),
        TRUE ~ to_title(as.character(.data$label))
      )
    ) %>%
    restrict_to_5_years()

  min_year <- get_common_start_year(data)
  base_size = 12
  plot_trajectory(data) +
    # TODO: move to theme_2dii()
    theme(
      plot.subtitle = element_text(
        hjust = 0.5, vjust = 0.5,
        size = base_size * 10 / 12
      )
    ) +
    labs(
      title = glue("Production trajectory of {to_pretty_label(data$technology[1])} technology
                   in the {to_title(data$sector[1])} sector"),
      subtitle = glue("The coloured areas indicate trajectories in reference to a scenario.
                      The red area indicates trajectories not aligned with any sustainble scenario."),
      x = "Year",
      y = glue("Production rate (normalized to {min_year})")
    )
}
