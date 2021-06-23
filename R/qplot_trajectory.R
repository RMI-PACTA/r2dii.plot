#' @inherit plot_trajectory
#' @seealso plot_trajectory
#'
#' @description
#' Compared to [plot_trajectory()] this function:
#' * is restricted to plotting only 5 years from the start year,
#' * outputs pretty legend labels, based on the column holding metrics,
#' * outputs a title,
#' * outputs a subtitle,
#' * outputs informative axis labels in sentence case.
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#'
#' qplot_trajectory(data)
qplot_trajectory <- function(data) {
  check_plot_trajectory(data)

  data <- data %>%
    mutate(label = format_label(.data$metric)) %>%
    restrict_to_5_years()

  min_year <- get_common_start_year(data)
  base_size <- 12
  plot_trajectory(data) +
    # TODO: move to theme_2dii()
    theme(
      plot.subtitle = element_text(
        hjust = 0.5, vjust = 0.5,
        size = base_size * 10 / 12
      )
    ) +
    labs(
      title = glue(
        "Production trajectory of {to_pretty_label(data$technology[1])} \\
        technology in the {to_title(data$sector[1])} sector"
      ),
      subtitle = glue(
        "The coloured areas indicate trajectories in reference to a scenario.
        The red area indicates trajectories not aligned with any sustainble \\
        scenario."
      ),
      x = "Year",
      y = glue("Production rate (normalized to {min_year})")
    )
}

#' @examples
#' format_label(c("corporate_economy", "target_sds"))
#' # Weird case
#' format_label(c("corporate_._economy", "target_sds_abc"))
#' @noRd
format_label <- function(x) {
  out <- sub("target_", "", x)
  out <- to_title(out)
  if_else(is_scenario(x), toupper(out), out)
}
