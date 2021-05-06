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
#' out <- prepare_for_techmix_chart(
#'   process_input_data(get_example_data()),
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_to_plot = "technology_share"
#' )
prepare_for_techmix_chart <- function(data,
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
    msg <- sprintf(
      "'years_filter' must be a vector of numbers.
        * You submitted a %s.",
      typeof(years_filter)
    )
    stop(msg, call. = FALSE)
  }

  if (!(region_filter %in% data$region)) {
    msg <- sprintf(
      "'region_filter' must be found in the input data column 'region'.
      * The unique regions in input data are: %s.
      * You submitted: %s.",
      toString(unique(data$region)),
      region_filter
    )
    stop(msg, call. = FALSE)
  }

  if (!(scenario_source_filter %in% data$scenario_source)) {
    msg <- sprintf(
      "'scenario_source_filter' must be found in the input data column 'scenario_source'.
      * The scenario sources in input data are: %s.
      * You submitted: %s.",
      toString(unique(data$scenario_source)),
      scenario_source_filter
    )
    stop(msg, call. = FALSE)
  }

  data_scenario <- data %>% filter(.data$metric_type == "scenario")
  if (!(scenario_filter %in% data_scenario$metric)) {
    msg <- sprintf(
      "'scenario_filter' must be found in the input data for scenarios in column 'metric'.
      * The scenario names in input data are: %s.
      * You submitted: %s.",
      toString(unique(data_scenario$metric)),
      scenario_filter
    )
    stop(msg, call. = FALSE)
  }

  if (!(value_to_plot %in% names(data))) {
    msg <- sprintf(
      "'value_to_plot' must be one of column names in the input data.
      * The input data column names are: %s.
      * You submitted: %s.",
      toString(names(data)),
      value_to_plot
    )
    stop(msg, call. = FALSE)
  }

  invisible(data)
}
