#' Prepare data for trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_trajectory()`), or use
#'   `FALSE` to impose no restriction.
#' @template convert_label
#' @templateVar fun qplot_trajectory
#' @templateVar value format_metric
#' @param value_col Character. Name of the column to be used as a value to be
#'   plotted.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # `data` must meet "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#' prep_trajectory(data)
#'
#' # prepare data as in `qplot_trajectory()`
#' prep_trajectory(
#'   data,
#'   span_5yr = TRUE,
#'   convert_label = format_metric,
#'   value_col = "percentage_of_initial_production_by_scope"
#'   )
prep_trajectory <- function(data,
                            convert_label = identity,
                            span_5yr = FALSE,
                            value_col = "percentage_of_initial_production_by_scope") {
  env <- list(data = substitute(data))
  check_prep_trajectory(data, value_col = value_col, env = env)

  out <- data %>%
    prep_common() %>%
    mutate(value = !!as.name(value_col)) %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    out <- span_5yr(out)
  }

  start_year <- min(out$year, na.rm = TRUE)

  cols <- c("year", "metric", "label", "technology", "value", "sector")
  select(out, all_of(cols))
}

check_prep_trajectory <- function(data, value_col, env) {
  check_common_trajectory(data, env)
  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")

  invisible(data)
}

abort_if_invalid_scenarios_number <- function(data) {
  scenarios <- extract_scenarios(data$metric)
  n <- length(scenarios)

  if (n < 1 || n > 4) {
    abort(c(
      glue("`metric` must have between 1 and 4 scenarios."),
      x = glue("Provided {n} scenarios: {toString(scenarios)}")
    ))
  }

  invisible(data)
}

summarise_max_year_by_scenario <- function(data) {
  data %>%
    filter(is_scenario(.data$metric)) %>%
    group_by(.data$metric) %>%
    summarise(year = max(.data$year))
}

summarise_max_year_by_traj_metric <- function(data) {
  data %>%
    filter(!is_scenario(.data$metric)) %>%
    group_by(.data$metric) %>%
    summarise(year = max(.data$year))
}