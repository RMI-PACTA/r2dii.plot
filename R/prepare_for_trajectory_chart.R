#' Prepares pre-processed data for plotting a trajectory chart
#'
#' @param data_preprocessed Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param technology_filter Technology for which to filter the data (character
#'   string).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param value_name The name of the value to be plotted in the trajectory chart
#'   (character string).
#' @param end_year_filter Cut-off year for the chart (an integer).
#' @param normalize_to_start_year Flag indicating whether the values should be
#'   normalized (boolean).
#'
#' @return A data frame.
#'
#' @export
#'
#' @examples
#' raw <- get_example_data()
#' processed <- process_input_data(raw)
#'
#' prepare_for_trajectory_chart(
#'   processed,
#'   sector_filter = "power",
#'   technology_filter = "oilcap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value_name = "production"
#' )
prepare_for_trajectory_chart <- function(data_preprocessed,
                                         sector_filter,
                                         technology_filter,
                                         region_filter,
                                         scenario_source_filter,
                                         value_name,
                                         end_year_filter = 2025,
                                         normalize_to_start_year = TRUE) {
  if (!sector_filter %in% data_preprocessed$sector) {
    warning("Unknown `sector_filter`: '", sector_filter, "'.", call. = FALSE)
  }

  format_unknown_value <- function(x) {
    label <- as.character(substitute(x))
    value <- as.character(eval(substitute(x)))
    sprintf("Unknown `%s`: '%s'", label, value)
  }

  if (!technology_filter %in% data_preprocessed$technology) {
    msg <- format_unknown_value(technology_filter)
    warning(msg, call. = FALSE)
  }

  year_start_projected <- data_preprocessed %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  data_filtered <- data_preprocessed %>%
    # FIXME: `!!` is unnecessary
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$technology == !!technology_filter) %>%
    filter(.data$region == !!region_filter) %>%
    filter(.data$scenario_source == !!scenario_source_filter) %>%
    filter(.data$year >= !!year_start_projected) %>%
    filter(.data$year <= !!end_year_filter) %>%
    select(.data$year, .data$metric_type, .data$metric, .data$technology,
      value = !!value_name
    )

  if (normalize_to_start_year) {
    data_filtered <- left_join(data_filtered,
      data_filtered[data_filtered$year == min(data_filtered$year), ],
      by = c("metric_type", "metric")
    ) %>%
      mutate(value = .data$value.x / .data$value.y) %>%
      select(
        year = .data$year.x, .data$metric_type,
        .data$metric, .data$value, technology = .data$technology.x
      )
  }

  data_filtered
}
