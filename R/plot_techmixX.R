#' Create a techmix plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `region`,
#'   `scenario_source`.
#'   * The column `metric` must have a portfolio (e.g. "projected"), a benchmark
#'   (e.g. "corporate_economy"), and a single `scenario` (e.g. "target_sds").
#'
#' @family functions with minimal arguments
#' @seealso [market_share].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # `data` must meet documented "Requirements"
#' data <- market_share %>%
#'   filter(
#'     scenario_source == "demo_2020",
#'     sector == "power",
#'     region == "global",
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#'   )
#' plot_techmixX(data)
plot_techmixX <- function(data) {
  stopifnot(is.data.frame(data))

  crucial <- c(
    "metric",
    "region",
    "scenario_source",
    "sector",
    "technology",
    "technology_share",
    "year"
  )
  hint_if_missing_names(abort_if_missing_names(data, crucial))

  abort_if_has_cero_rows(data)
  abort_if_multiple(data, "sector")
  abort_if_multiple(data, "region")
  abort_if_multiple(data, "scenario_source")

  prep <- prep_techmixB(data)

  found_scenarios <- pull_scenarios(prep)
  abort_if_invalid_length(found_scenarios)

  plot_techmixY(prep)
}

#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     dplyr::between(year, 2020, 2025),
#'     scenario_source == "demo_2020",
#'     sector == "power",
#'     region == "global",
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#'   )
#'
#' prep <- prep_techmixB(data)
prep_techmixB <- function(data, value = "technology_share", metric = "metric") {
  check_prep_techmixB(data, value)

  data %>%
    recode_metric_and_metric_type(metric) %>%
    pick_extreme_years() %>%
    date_metric_type() %>%
    mutate(value = .data[[value]])
}

pick_extreme_years <- function(data) {
  filter(data, .data$year %in% c(min(data$year), max(data$year)))
}

date_metric_type <- function(data) {
  mutate(data, metric_type = paste0(.data$metric_type, "_", .data$year))
}

check_prep_techmixB <- function(data, value) {
  crucial <- c("metric", "year", "scenario_source", "region", value)
  abort_if_missing_names(data, crucial)

  cols <- c("scenario_source", "sector", "region")
  lapply(cols, function(x) abort_if_multiple(data, x))

  abort_if_bad_metric(data$metric)

  invisible(data)
}
