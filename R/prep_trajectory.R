#' Prepare the output of `r2dii.analysis::target_market_share()` for `plot_trajectory()`
#'
#' @inheritParams prep_techmixY
#' @param technology_filter String of length 1. Technology to pick from the
#'   `data`.
#' @param end_year_filter Numeric of length 1. Cut-off year for the plot.
#' @param normalize Logical of length-1. `TRUE` normalizes to the start year.
#'
#' @return A data frame.
#'
#' @export
#' @examples
#' prep_trajectory(
#'   market_share,
#'   sector_filter = "power",
#'   technology_filter = "oilcap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value = "production"
#' )
prep_trajectory <- function(data,
                            sector_filter,
                            technology_filter,
                            region_filter,
                            scenario_source_filter,
                            value = "production",
                            metric = "metric",
                            end_year_filter = 2025,
                            normalize = TRUE) {
  abort_if_missing_names(data, metric)
  data <- recode_metric_and_metric_type(data, metric)

  warn_bad_value(sector_filter, data$sector)
  warn_bad_value(technology_filter, data$technology)
  warn_bad_value(region_filter, data$region)
  warn_bad_value(scenario_source_filter, data$scenario_source)
  abort_if_missing_names(data, "sector")

  year_start_projected <- data %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  data_filtered <- data %>%
    filter(.data$sector == .env$sector_filter) %>%
    filter(.data$technology == .env$technology_filter) %>%
    filter(.data$region == .env$region_filter) %>%
    filter(.data$scenario_source == .env$scenario_source_filter) %>%
    filter(.data$year >= .env$year_start_projected) %>%
    filter(.data$year <= .env$end_year_filter) %>%
    mutate(value = .data[[value]]) %>%
    select(.data$metric, .data$metric_type, .data$year, .data$technology, .data$value)

  if (normalize) {
    data_filtered <- left_join(data_filtered,
      data_filtered[data_filtered$year == min(data_filtered$year), ],
      by = c("metric_type", "metric")
    ) %>%
      mutate(
        value = .data$value.x / .data$value.y,
        year = .data$year.x,
        technology = .data$technology.x
      ) %>%
      select(.data$metric, .data$metric_type, .data$year, .data$technology, .data$value)
  }

  data_filtered
}

warn_bad_value <- function(x, y) {
  if (!x %in% y) {
    warning("`", x, "` matches no data value.", call. = FALSE)
  }
  invisible(x)
}

abort_multiple <- function(data, colname) {
  values <- unique(data[[colname]])
  if (length(values) != 1L) {
    abort(glue(
      "`{colname}` must have a single value. It has: {toString(values)}."
    ))
  }

  invisible(data)
}
