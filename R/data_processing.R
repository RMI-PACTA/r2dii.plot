#' Performs initial processing on raw input data in banks' format
#'
#' The data is processed so that it can be used later in data filtering
#' functions for charts. 'metric_type' variable is added which depends on
#' 'metric' and the 'metric' values themselves are edited for plotting purposes.
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @return A dataframe with additional column: metric type and modified metric
#' @export
#'
#' @examples
#' # TODO
process_input_data <- function(data) {
  data <- data %>%
    mutate(metric_type = case_when(
      .data$metric == "projected" ~ "portfolio",
      grepl("target", .data$metric) ~ "scenario",
      TRUE ~ "benchmark"
    )) %>%
    mutate(metric = if_else(
      grepl("target", .data$metric),
      sub(".*_", "", .data$metric),
      .data$metric
    ))
}

#' Filters pre-processed data to be ready for plotting a trajectory chart for a technology
#'
#' @param data pre-processed input data
#' @param sector sector for which to filter the data (a character string)
#' @param technology technology for which to filter the data (a character string)
#' @param region region for which to filter the data (a character string)
#' @param scenario_source scenario source for which to filter the data (a character string)
#' @param value_name the name of the value to be plotted in the trajectory chart (a character string)
#' @param end_year cut off year for the chart (an integer; default = 2025)
#' @param normalize_to_start_year flab indicating whether the values should be normalized (boolean; default = TRUE)
#'
#' @export

filter_data_for_trajectory_chart <- function(data, sector, technology,
                                             region, scenario_source,
                                             value_name, end_year = 2025,
                                             normalize_to_start_year = TRUE) {
  data_filtered <- data %>%
    filter(.data$sector == !!sector) %>%
    filter(.data$technology == !!technology) %>%
    filter(.data$region == !!region) %>%
    filter(.data$scenario_source == !!scenario_source) %>%
    filter(.data$year <= end_year) %>%
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

  return(data_filtered)
}

#' Filters pre-processed data to be ready for plotting a techmix chart for a sector
#'
#' @param data pre-processed input data
#' @param sector sector for which to filter the data (a character string)
#' @param years years which we want to plot in the graph (an array of integer values)
#' @param region region for which to filter the data (a character string)
#' @param scenario_source scenario source for which to filter the data (a character string)
#' @param scenario scenario to plot in the graph (a character string)
#' @param value_name the name of the value to be plotted as a bar chart (a character string)
#'
#' @export

filter_data_for_techmix_chart <- function(data, sector, years,
                                          region, scenario_source,
                                          scenario, value_name) {
  data_filtered <- data %>%
    filter(.data$sector == !!sector) %>%
    filter(.data$region == !!region) %>%
    filter(.data$year %in% !!years) %>%
    filter(.data$scenario_source == !!scenario_source) %>%
    filter(.data$metric_type %in% c("portfolio", "benchmark") |
      (.data$metric_type == "scenario" & .data$metric == scenario)) %>%
    mutate(
      metric_type = paste0(.data$metric_type, "_", as.character(.data$year))
    ) %>%
    select(.data$technology, .data$metric_type, .data$metric,
      value = !!value_name
    )
  return(data_filtered)
}

#' Aggregates and filters PACTA analysis total_portfolio data to be an input for metareport security type bar chart
#'
#' @param data_total_portfolio dataframe in the shape of ".._total_portfolio.rda" dataset from PACTA analysis output in "30_Processed_Inputs" folder (dataframe)
#' @param other_asset_types array of character strings that should be summed up as "Other" asset type (array of character strings; default = c("Funds","Others","Unclassifiable"))
#'
#' @description
#' This function filters and aggregates one of PACTA analysis result files ".._total_portfolio.rda" from "30_Processed_Inputs" folder to form an input that can be used for plotting metareport security type coverage per investor type bar chart
#'
#' @export

filter_data_for_metareport_security_type_chart <- function(data_total_portfolio,
                                                           other_asset_types = c(
                                                             "Funds", "Others",
                                                             "Unclassifiable"
                                                           )) {

  data_filtered_all <- data_total_portfolio %>%
    select(.data$investor_name, .data$asset_type, .data$value_usd) %>%
    group_by(.data$investor_name, .data$asset_type) %>%
    summarise(total_value=sum(.data$value_usd, na.rm=T)) %>%
    ungroup() %>%
    group_by(.data$investor_name) %>%
    mutate(total_peergroup=sum(.data$total_value, na.rm=T)) %>%
    ungroup() %>%
    mutate(share=.data$total_value/.data$total_peergroup) %>%
    select(.data$investor_name, .data$asset_type, .data$share)

  data_in_analysis <- data_filtered_all %>%
    filter(!(.data$asset_type %in% other_asset_types))

  data_other <- data_filtered_all %>%
    filter(.data$asset_type %in% other_asset_types) %>%
    group_by(.data$investor_name) %>%
    summarise(share = sum(.data$share)) %>%
    mutate(asset_type = "Other")

  data_filtered <- rbind(data_in_analysis,data_other) %>%
    arrange(.data$investor_name)
}
