#' Prepares pre-processed data for plotting a trajectory chart
#'
#' @param data Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param technology_filter Technology for which to filter the data (character
#'   string).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param value_name,value The name of the value to be plotted in the trajectory chart
#'   (character string).
#' @param end_year_filter Cut-off year for the chart (an integer).
#' @param normalize_to_start_year,noramlize Logical of length-1. `TRUE`
#'   normalized to start year.
#'
#' @return A data frame.
#'
#' @export
#'
#' @examples
#' prep_trajectory(
#'   market_share,
#'   sector_filter = "power",
#'   technology_filter = "oilcap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value_name = "production"
#' )
prep_trajectory <- function(data,
                            sector_filter,
                            technology_filter,
                            region_filter,
                            scenario_source_filter,
                            value_name,
                            end_year_filter = 2025,
                            normalize_to_start_year = TRUE) {
  check_crucial_names(data, "metric")
  data <- recode_metric_and_metric_type(data)

  warn_bad_value(sector_filter, data$sector)
  warn_bad_value(technology_filter, data$technology)
  warn_bad_value(region_filter, data$region)
  warn_bad_value(scenario_source_filter, data$scenario_source)
  check_crucial_names(data, "sector")

  year_start_projected <- data %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  data_filtered <- data %>%
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
        year = .data$year.x,
        .data$metric_type,
        .data$metric,
        technology = .data$technology.x,
        .data$value
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

#' @rdname prep_trajectory
#' @export
#' @examples
#' data <- market_share %>%
#'   filter(
#'     technology == "oilcap",
#'     region == "global",
#'     scenario_source == "demo_2020",
#'     year <= 2025,
#'     sector == "power"
#'   )
#'
#' prep_trajectoryB(data)
prep_trajectoryB <- function(data, value = "production", normalize = TRUE) {
  check_prep_trajectoryB(data, value, normalize)
  data <- recode_metric_and_metric_type(data)

  cols <- c("year", "metric_type", "metric", "technology", "value")
  out <- data %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  if (!normalize) {
    return(out)
  }

  left_join(
    out, filter(out, .data$year == min(.data$year)),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectoryB <- function(data, value, normalize) {
  crucial <- c(
    "metric",
    "sector",
    "technology",
    "region",
    "year",
    "scenario_source",
    value
  )
  check_crucial_names(data, crucial)

  if (!length(normalize) == 1L) abort("`normalize` must be of length 1.")
  stopifnot(is.logical(normalize))

  abort_multiple(data, "sector")
  abort_multiple(data, "technology")
  abort_multiple(data, "region")
  abort_multiple(data, "scenario_source")

  invisible(data)
}
