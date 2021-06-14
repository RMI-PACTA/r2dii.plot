prep_trajectoryY <- function(data,
                             sector_filter,
                             technology_filter,
                             region_filter,
                             scenario_source_filter,
                             value = "production",
                             metric = "metric",
                             end_year_filter = 2025,
                             normalize = TRUE) {
  abort_if_missing_names(data, metric)
  data <- recode_metric_and_metric_type(data, metric)

  warn_bad_value(sector_filter, data$sector)
  warn_bad_value(technology_filter, data$technology)
  warn_bad_value(region_filter, data$region)
  warn_bad_value(scenario_source_filter, data$scenario_source)
  abort_if_missing_names(data, "sector")

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
    mutate(value = .data[[value]]) %>%
    select(.data$metric, .data$metric_type, .data$year, .data$technology, .data$value)

  if (normalize) {
    data_filtered <- left_join(data_filtered,
      data_filtered[data_filtered$year == min(data_filtered$year), ],
      by = c("metric_type", "metric")
    ) %>%
      mutate(
        value = .data$value.x / .data$value.y,
        year = .data$year.x,
        technology = .data$technology.x
      ) %>%
      select(.data$metric, .data$metric_type, .data$year, .data$technology, .data$value)
  }

  data_filtered
}

warn_bad_value <- function(x, y) {
  if (!x %in% y) {
    warning("`", x, "` matches no data value.", call. = FALSE)
  }
  invisible(x)
}
