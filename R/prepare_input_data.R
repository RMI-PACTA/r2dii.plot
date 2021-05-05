#' Performs the initial processing on raw input data in banks' format
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @description This function processes the data in banks' format so that it can
#'   be used later in data filtering functions for charts.
#'
#' @return A dataframe with additional column `metric_type` and modified
#'   `metric`.
#' @export
#'
#' @examples
#' data <- example_data
#' process_input_data(data)
process_input_data <- function(data) {
  check_crucial_names(data, "metric")

  data %>%
    mutate(
      metric_type = case_when(
        .data$metric == "projected" ~ "portfolio",
        grepl("target", .data$metric) ~ "scenario",
        TRUE ~ "benchmark"
      )
    ) %>%
    mutate(metric = if_else(
      grepl("target", .data$metric),
      sub(".*_", "", .data$metric),
      .data$metric
    ))
}
