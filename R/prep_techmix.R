#' Prepares pre-processed data for plotting a tech-mix chart
#'
#' @param data Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param years_filter Years to plot in the graph (array of integer values).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param scenario_filter Scenario to plot in the graph (character string).
#' @param value_to_plot The name of the value to be plotted as a bar chart
#'   (character string).
#'
#' @export
#' @examples
#' prep_techmix(
#'   market_share,
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_to_plot = "technology_share"
#' )
prep_techmix <- function(data,
                         sector_filter = c(
                           "automotive",
                           "aviation",
                           "cement",
                           "oil and gas",
                           "shipping",
                           "steel",
                           "power"
                         ),
                         years_filter = NULL,
                         region_filter = "global",
                         scenario_source_filter = NULL,
                         scenario_filter = NULL,
                         value_to_plot = "technology_share") {
  data <- process_input_data(data)

  years_filter <- years_filter %||% c(min(data$year), max(data$year))
  scenario_source_filter <- scenario_source_filter %||% data$scenario_source[1]
  scenario_filter <- scenario_filter %||% (data %>%
    filter(
      .data$scenario_source == .env$scenario_source_filter,
      .data$metric_type == "scenario"
    ) %>%
    slice_head(n = 1) %>%
    pull(.data$metric))

  # input checks
  sector_filter <- match.arg(sector_filter)
  check_input_parameters_techmix(
    data,
    years_filter,
    region_filter,
    scenario_source_filter,
    scenario_filter,
    value_to_plot
  )

  data_out <- data %>%
    filter(.data$sector == .env$sector_filter) %>%
    filter(.data$region == .env$region_filter) %>%
    filter(.data$year %in% .env$years_filter) %>%
    filter(.data$scenario_source == .env$scenario_source_filter) %>%
    filter(
      .data$metric_type %in% c("portfolio", "benchmark") |
        (.data$metric_type == "scenario" & .data$metric == scenario_filter)
    ) %>%
    mutate(
      metric_type = paste0(.data$metric_type, "_", as.character(.data$year)),
      value = .data[[value_to_plot]]
    ) %>%
    select(
      .data$sector, .data$technology, .data$metric_type, .data$metric, .data$value,
      .data$scenario_source
    )

  data_out
}

prep_techmixB <- function(data,
                         sector_filter = c(
                           "automotive",
                           "aviation",
                           "cement",
                           "oil and gas",
                           "shipping",
                           "steel",
                           "power"
                         ),
                         years_filter = NULL,
                         region_filter = "global",
                         scenario_source_filter = NULL,
                         scenario_filter = NULL,
                         value_to_plot = "technology_share") {
  data <- process_input_data(data)

  years_filter <- years_filter %||% c(min(data$year), max(data$year))
  scenario_source_filter <- scenario_source_filter %||% data$scenario_source[1]
  scenario_filter <- scenario_filter %||% (data %>%
    filter(
      .data$scenario_source == .env$scenario_source_filter,
      .data$metric_type == "scenario"
    ) %>%
    slice_head(n = 1) %>%
    pull(.data$metric))

  # input checks
  sector_filter <- match.arg(sector_filter)
  check_input_parameters_techmix(
    data,
    years_filter,
    region_filter,
    scenario_source_filter,
    scenario_filter,
    value_to_plot
  )

  data_out <- data %>%
    filter(.data$sector == .env$sector_filter) %>%
    filter(.data$region == .env$region_filter) %>%
    filter(.data$year %in% .env$years_filter) %>%
    filter(.data$scenario_source == .env$scenario_source_filter) %>%
    filter(
      .data$metric_type %in% c("portfolio", "benchmark") |
        (.data$metric_type == "scenario" & .data$metric == scenario_filter)
    ) %>%
    mutate(
      metric_type = paste0(.data$metric_type, "_", as.character(.data$year)),
      value = .data[[value_to_plot]]
    ) %>%
    select(
      .data$sector, .data$technology, .data$metric_type, .data$metric, .data$value,
      .data$scenario_source
    )

  data_out
}

check_input_parameters_techmix <- function(data,
                                           years_filter,
                                           region_filter,
                                           scenario_source_filter,
                                           scenario_filter,
                                           value_to_plot) {
  if (typeof(years_filter) != "double") {
    abort(glue(
      "'years_filter' must be a vector of numbers.
      * You submitted a {typeof(years_filter)}."
    ))
  }

  if (!(region_filter %in% data$region)) {
    abort(glue(
      "'region_filter' must be found in the input data column 'region'.
      * The unique regions in input data are: {toString(unique(data$region))}.
      * You submitted: {region_filter}."
    ))
  }

  if (!(scenario_source_filter %in% data$scenario_source)) {
    abort(glue(
      "'scenario_source_filter' must be found in the input data column 'scenario_source'.
      * The scenario sources in input data are: {toString(unique(data$scenario_source))}.
      * You submitted: {scenario_source_filter}."
    ))
  }

  data_scenario <- data %>% filter(.data$metric_type == "scenario")
  if (!(scenario_filter %in% data_scenario$metric)) {
    abort(glue(
      "'scenario_filter' must be found in the input data for scenarios in column 'metric'.
      * The scenario names in input data are: {toString(unique(data_scenario$metric))}.
      * You submitted: {scenario_filter}."
    ))
  }

  if (!(value_to_plot %in% names(data))) {
    abort(glue(
      "'value_to_plot' must be one of column names in the input data.
      * The input data column names are: {toString(names(data))}.
      * You submitted: {value_to_plot}."
    ))
  }

  invisible(data)
}
