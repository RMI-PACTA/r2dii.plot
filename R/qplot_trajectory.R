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
#'   market_share_demo,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#'
#' qplot_trajectory(data)
qplot_trajectory <- function(data) {
  env <- list(data = substitute(data))
  check_qplot_trajectory(
    data,
    value_col = "percentage_of_initial_production_by_scope",
    env = env
  )

  data %>%
    prep_trajectory(
      convert_label = recode_metric_trajectory,
      span_5yr = TRUE,
      value_col = "percentage_of_initial_production_by_scope"
    ) %>%
    plot_trajectory(
      center_y = TRUE,
      perc_y_scale = TRUE
    ) %>%
    labs_trajectory(data)
}

labs_trajectory <- function(p, data) {
  technology <- spell_out_technology(p[["data"]][["technology"]][[1]])
  sector <- tools::toTitleCase(p[["data"]][["sector"]][[1]])
  min_year <- min(p[["data"]][["year"]], na.rm = TRUE)
  scope <- data$scope[1]

  p +
    labs(
      title = glue(
        "Production Trajectory of {technology} Technology
        in the {sector} Sector"
      ),
      subtitle = glue(
        "The coloured areas indicate trajectories in reference to a scenario.
        The red area indicates trajectories not aligned with any sustainable \\
        scenario."
      ),
      x = "Year",
      y = glue("Change in production relative to the total\ninitial production of {eval(parse(text = scope))} {scope} (%)")
    )
}

check_qplot_trajectory <- function(data, value_col, env) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share_demo")
  abort_if_has_zero_rows(data, env = env)
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_invalid_scenarios_number(data)
  abort_if_too_many_lines(max = 4, summarise_max_year_by_scenario(data))
  abort_if_too_many_lines(max = 5, summarise_max_year_by_traj_metric(data))

  invisible(data)
}
