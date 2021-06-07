#' Prepare the output of `r2dii.analysis::target_market_share()` for `plot_trajectory()`
#'
#' @inheritParams prep_techmix
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
    mutate(value = .data[[value]])

  if (normalize) {
    data_filtered <- left_join(data_filtered,
      data_filtered[data_filtered$year == min(data_filtered$year), ],
      by = c("metric_type", "metric")
    ) %>%
      mutate(
        value = .data$value.x / .data$value.y,
        year = .data$year.x,
        technology = .data$technology.x
      )
  }

  data_filtered
}

warn_bad_value <- function(x, y) {
  if (!x %in% y) {
    warning("`", x, "` matches no data value.", call. = FALSE)
  }
  invisible(x)
}

#' @rdname prep_trajectory
#' @export
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

abort_multiple <- function(data, colname) {
  values <- unique(data[[colname]])
  if (length(values) != 1L) {
    abort(glue(
      "`{colname}` must have a single value. It has: {toString(values)}."
    ))
  }

  invisible(data)
}
