#' Create a quick trajectory plot
#'
#' @seealso plot_trajectory
#'
#' @param data A data frame. Requirements:
#' * Must have columns: `percentage_of_initial_production_by_scope`, `metric`,
#' `year`, `sector`, `technology`, `region`, `scenario_source`, `scope`.
#' * The following columns must be present and have a single value: `sector`,
#' `technology`, `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#'
#' @description
#' Compared to [plot_trajectory()] this function:
#' * accepts [market-share]-like data directly as input,
#' * prepares data with fixed `prep_trajectory()` parameters,
#' * is restricted to plotting only 5 years from the start year,
#' * outputs pretty legend labels, based on the column holding metrics,
#' * outputs a title,
#' * outputs a subtitle,
#' * outputs informative axis labels in sentence case.
#'
#' @return An object of class "ggplot".
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
  check_qplot_trajectory(
    data,
    value_col = c("percentage_of_initial_production_by_scope", "scope")
  )

  env <- list(data = substitute(data))

  data %>%
    prep_trajectory(
      convert_label = format_metric,
      span_5yr = TRUE,
      value_col = "percentage_of_initial_production_by_scope",
      env = env
    ) %>%
    plot_trajectory(center_y = TRUE, perc_y_scale = TRUE, env = env) %>%
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

check_qplot_trajectory <- function(data, value_col) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  invisible(data)
}
