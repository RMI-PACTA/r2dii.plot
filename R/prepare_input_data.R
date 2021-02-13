#' Performs the initial processing on raw input data in banks' format
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @description This function processes the data in banks' format so that it can
#' be used later in data filtering functions for charts. 'metric_type' variable
#' is added which depends on 'metric' and the 'metric' values themselves are
#' edited for plotting purposes.
#'
#' @return A dataframe with additional column: metric type and modified metric.
#' @export
#'
#' @examples
#' data <- get_example_data()
#' process_input_data(data)
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

  # FIXME: Here we likely need to return `data`
  # data
}
