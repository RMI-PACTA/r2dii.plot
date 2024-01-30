#' Prepare data for a trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @template convert_label
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_trajectory()`), or use
#'   `FALSE` to impose no restriction.
#' @param center_y Logical. Use `TRUE` to center the y-axis around start value
#'   (the default behavior of `qplot_trajectory()`), or use `FALSE` to not
#'   center.
#' @param value_col Character. Name of the column to be used as a value to be
#'   plotted.
#'
#' @seealso [market_share].
#'
#' @return A data frame ready to be plot using `plot_trajectory()`.
#' @export
#'
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
#' prep_trajectory(data)
prep_trajectory <- function(data,
                            convert_label = identity,
                            span_5yr = FALSE,
                            center_y = FALSE,
                            value_col = "percentage_of_initial_production_by_scope") {
  out <- data %>%
    prep_common() %>%
    mutate(value = !!as.name(value_col)) %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    out <- span_5yr(out)
  }

  start_year <- min(out$year, na.rm = TRUE)

  cols <- c("year", "metric", "label", "technology", "value", "sector")
  out <- select(out, all_of(cols))

  scenarios <- scenario(out, center_y)
  not_scenarios <- out %>%
    filter(!is_scenario(.data$metric)) %>%
    mutate(value_low = .data$value)
  bind_rows(scenarios, not_scenarios)
}
