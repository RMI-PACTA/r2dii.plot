#' Create a quick trajectory plot
#'
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

  data %>%
    prep_trajectory(convert_label = format_label, span_5yr = TRUE) %>%
    plot_trajectory_impl() %>%
    labs_trajectory()
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

labs_trajectory <- function(p) {
  tech <- tools::toTitleCase(p[["data"]][["technology"]][[1]])
  sector <- tools::toTitleCase(p[["data"]][["sector"]][[1]])
  min_year <- min(p[["data"]][["year"]], na.rm = TRUE)

  p +
    labs(
      title = glue(
        "Production Trajectory of Technology: {tech}
        in the {sector} Sector"
      ),
      subtitle = glue(
        "The coloured areas indicate trajectories in reference to a scenario.
        The red area indicates trajectories not aligned with any sustainble \\
        scenario."
      ),
      x = "Year",
      y = glue("Production Rate (normalized to {min_year})")
    )
}
