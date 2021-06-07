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
  lapply(cols, function(x) abort_multiple(data, x))

  abort_bad_metric(data$metric)

  invisible(data)
}

#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     technology == "oilcap",
#'     region == "global",
#'     scenario_source == "demo_2020",
#'     year <= 2025,
#'     sector == "power"
#'   )
#'
#' prep_trajectoryB(data)
prep_trajectoryB <- function(data,
                             value = "production",
                             metric = "metric",
                             normalize = TRUE) {
  check_prep_trajectoryB(data, value, normalize)
  data <- recode_metric_and_metric_type(data, metric)

  cols <- c("year", "metric_type", "metric", "technology", "value")
  out <- data %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  if (!normalize) {
    return(out)
  }

  left_join(
    out, filter(out, .data$year == min(.data$year)),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectoryB <- function(data, value, normalize) {
  crucial <- c(
    "metric", "sector", "technology", "region", "year", "scenario_source", value
  )
  abort_if_missing_names(data, crucial)

  abort_if_invalid_length(normalize)
  stopifnot(is.logical(normalize))

  cols <- c("sector", "technology", "region", "scenario_source")
  lapply(cols, function(x) abort_multiple(data, x))

  invisible(data)
}
