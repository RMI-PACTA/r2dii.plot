#' Prepares pre-processed data for plotting a tech-mix chart
#'
#' @param data_preprocessed Pre-processed input data.
#' @param sector_filter Sector for which to filter the data (character string).
#' @param years_filter Years to plot in the graph (array of integer values).
#' @param region_filter Region for which to filter the data (character string).
#' @param scenario_source_filter Scenario source for which to filter the data
#'   (character string).
#' @param scenario_filter Scenario to plot in the graph (character string).
#' @param value_name The name of the value to be plotted as a bar chart
#'   (character string).
#'
#' @export
#' @examples
#' # FIXME: Returns invisibly
#' out <- prepare_for_techmix_chart(
#'   process_input_data(get_example_data()),
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_name = "technology_share"
#' )
#' out
prepare_for_techmix_chart <- function(data_preprocessed,
                                      sector_filter,
                                      years_filter,
                                      region_filter,
                                      scenario_source_filter,
                                      scenario_filter,
                                      value_name) {
  data_filtered <- data_preprocessed %>%
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$region == !!region_filter) %>%
    filter(.data$year %in% !!years_filter) %>%
    filter(.data$scenario_source == !!scenario_source_filter) %>%
    filter(.data$metric_type %in% c("portfolio", "benchmark") |
      (.data$metric_type == "scenario" & .data$metric == scenario_filter)) %>%
    mutate(
      metric_type = paste0(.data$metric_type, "_", as.character(.data$year))
    ) %>%
    select(.data$technology, .data$metric_type, .data$metric,
      value = !!value_name
    )
}
