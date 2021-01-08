#' @import dplyr

process_input_data <- function(data) {
  data <- data %>% mutate(metric_type = case_when(
    .data$metric == "projected" ~ "portfolio",
    grepl("target",.data$metric) ~ "scenario",
    TRUE ~ "benchmark"
  )) %>%
    mutate(metric = if_else(
      grepl("target", .data$metric),
      sub(".*_", "", .data$metric),
      .data$metric
    ))
}

#' @import dplyr

filter_data_for_trajectory_chart <- function(data,sector,technology,
                                             region,scenario_source,
                                             value_name, end_year = 2025,
                                             normalize_to_start_year = TRUE) {

  data_filtered <- data %>%
    filter(.data$sector == !!sector) %>%
    filter(.data$technology == !!technology) %>%
    filter(.data$region == !!region) %>%
    filter(.data$scenario_source == !!scenario_source) %>%
    filter(.data$year <= end_year) %>%
    select(.data$year,.data$metric_type,.data$metric,value = !!value_name)

  if (normalize_to_start_year) {
    data_filtered <- left_join(data_filtered,data_filtered[data_filtered$year == min(data_filtered$year),],
                               by=c("metric_type","metric")) %>%
      mutate(value = .data$value.x/.data$value.y) %>%
      select(year = .data$year.x, .data$metric_type,
             .data$metric,.data$value)
  }

  return(data_filtered)
}

#' @import dplyr

filter_data_for_techmix_chart <- function(data,sector,years,
                                          region,scenario_source,
                                          scenario,value_name) {

  data_filtered <- data %>%
    filter(.data$sector == !!sector) %>%
    filter(.data$region == !!region) %>%
    filter(.data$year %in% !!years) %>%
    filter(.data$scenario_source == !!scenario_source) %>%
    filter(.data$metric_type %in% c("portfolio","benchmark") |
             (.data$metric_type == "scenario" & .data$metric == scenario)) %>%
    mutate(metric_type = paste0(.data$metric_type,"_",as.character(.data$year))) %>%
    select(.data$technology,.data$metric_type,.data$metric,value = !!value_name) %>%

  return(data_filtered)
}
