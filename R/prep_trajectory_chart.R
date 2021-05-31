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
#' raw <- market_share
#' processed <- process_input_data(raw)
#' prep_trajectory(
#'   processed,
#'   sector_filter = "power",
#'   technology_filter = "oilcap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value_name = "production"
#' )
prep_trajectory <- function(data_preprocessed,
                                         sector_filter,
                                         technology_filter,
                                         region_filter,
                                         scenario_source_filter,
                                         value_name,
                                         end_year_filter = 2025,
                                         normalize_to_start_year = TRUE) {
  warn_bad_value(sector_filter, data_preprocessed$sector)
  warn_bad_value(technology_filter, data_preprocessed$technology)
  warn_bad_value(region_filter, data_preprocessed$region)
  warn_bad_value(scenario_source_filter, data_preprocessed$scenario_source)
  check_crucial_names(data_preprocessed, "sector")

  year_start_projected <- data_preprocessed %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  data_filtered <- data_preprocessed %>%
    filter(.data$sector == .env$sector_filter) %>%
    filter(.data$technology == .env$technology_filter) %>%
    filter(.data$region == .env$region_filter) %>%
    filter(.data$scenario_source == .env$scenario_source_filter) %>%
    filter(.data$year >= .env$year_start_projected) %>%
    filter(.data$year <= .env$end_year_filter) %>%
    mutate(value = .data[[value_name]]) %>%
    select(
      .data$year,
      .data$metric_type,
      .data$metric,
      .data$technology,
      .data$value
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

warn_bad_value <- function(x, y) {
  if (!x %in% y) {
    warning("`", x, "` matches no data value.", call. = FALSE)
  }
  invisible(x)
}
