#' Prepare the output of `r2dii.analysis::target_market_share()` for `plot_techmix()`
#'
#' @param data Data frame like the output of `r2dii.analysis::target_market_share()`.
#' @param sector_filter String of length 1. Sector to pick from the `data`.
#' @param years_filter Numeric vector of length 2. Range of years to plot.
#' @param region_filter String of length 1. Region to pick from the `data`.
#' @param scenario_source_filter String of length 1. Value of the column
#'   `scenario_source` to pick from the `data`.
#' @param scenario_filter String of length 1. Scenario to pick from the `data`.
#' @inheritParams prep_timeline
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
#'   value = "technology_share"
#' )
prep_techmix <- function(data,
                         value = "technology_share",
                         metric = "metric",
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
                         scenario_filter = NULL) {
  abort_if_missing_names(data, metric)

  data <- recode_metric_and_metric_type(data, metric)

  years_filter <- years_filter %||% c(min(data$year), max(data$year))
  scenario_source_filter <- scenario_source_filter %||% data$scenario_source[1]
  scenario_filter <- scenario_filter %||% (data %>%
    filter(
      .data$scenario_source == .env$scenario_source_filter,
      .data$metric_type == "scenario"
    ) %>%
    slice_head(n = 1) %>%
    pull(.data$metric))

  sector_filter <- match.arg(sector_filter)
  check_prep_techmix(
    data,
    years_filter,
    region_filter,
    scenario_source_filter,
    scenario_filter,
    value
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
      value = .data[[value]]
    ) %>%
    select(
      .data$sector, .data$technology, .data$metric_type, .data$metric, .data$value,
      .data$scenario_source
    )

  data_out
}

#' @rdname prep_techmix
#' @export
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

abort_multiple <- function(data, colname) {
  values <- unique(data[[colname]])
  if (length(values) != 1L) {
    # FIXME replace 'but has more' with '. 'but it has: ...'
    abort(glue(
      "`{colname}` must have one value but has more: {toString(values)}."
    ))
  }

  invisible(data)
}

check_prep_techmix <- function(data,
                               years_filter,
                               region_filter,
                               scenario_source_filter,
                               scenario_filter,
                               value) {
  if (!is.numeric(years_filter)) {
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

  if (!(value %in% names(data))) {
    abort(glue(
      "'value' must be one of column names in the input data.
      * The input data column names are: {toString(names(data))}.
      * You submitted: {value}."
    ))
  }

  invisible(data)
}
