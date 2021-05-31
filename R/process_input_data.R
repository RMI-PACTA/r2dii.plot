#' Performs the initial processing on raw input data in banks' format
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @description This function processes the data in banks' format so that it can
#'   be used later in data filtering functions for charts.
#'
#' @return A dataframe with additional column `metric_type` and modified
#'   `metric`.
#'
#' @examples
#' data <- example_data
#' process_input_data(data)
#' @keywords internal
#' @noRd
process_input_data <- function(data) {
  data %>%
    add_metric_type() %>%
    mutate(metric = sub("target_", "", .data$metric))
}

add_metric_type <- function(data) {
  check_crucial_names(data, "metric")

  mutate(
    data,
    metric_type = case_when(
      .data$metric == "projected" ~ "portfolio",
      startsWith(.data$metric, "target") ~ "scenario",
      TRUE ~ "benchmark"
    )
  )
}
