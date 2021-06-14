#' Create a timeline plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @inheritParams prep_timelineY
#' @inheritParams plot_timelineY
#'
#' @family functions with minimal arguments
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # `data` must meet documented "Requirements"
#' data <- filter(sda, sector == "cement")
#' plot_timelineX(data)
plot_timelineX <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data))
  abort_if_has_cero_rows(data)
  prep <- hint_if_missing_names(prep_timelineY(data, extrapolate = extrapolate))

  plot_timelineB(prep)
}

#' Prepare the output of `r2dii.analysis::target_sda()` for `plot_timeline()`
#'
#' @param data Data frame like the output of `r2dii.analysis::target_sda()`.
#' @param value String of length 1. The name of the column holding the value to
#'   plot.
#' @param metric String of length 1. The name of the column holding the metrics
#'   to plot.
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
#' @inheritParams prep_techmixY
#'
#' @seealso [sda].
#'
#' @return Data frame with columns: year, line_name, value, extrapolated.
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Fails
#' try(prep_timelineY(sda))
#'
#' # Works: meets `data` requirements
#' prep_timelineY(sda, sector_filter = "cement")
#'
#' # Same
#' sda %>%
#'   filter(sector == "cement") %>%
#'   prep_timelineY()
#'
#' prep_timelineY(sda, sector_filter = "cement", extrapolate = TRUE)
#'
#' data <- sda %>%
#'   filter(sector == "cement") %>%
#'   rename(
#'     custom_value = "emission_factor_value",
#'     custom_metric = "emission_factor_metric"
#'   )
#' prep_timelineY(data, value = "custom_value", metric = "custom_metric")
prep_timelineY <- function(data,
                           value = "emission_factor_value",
                           metric = "emission_factor_metric",
                           sector_filter = NULL,
                           extrapolate = FALSE) {
  check_prep_timelineY(data, value, metric, extrapolate)
  if (!is.null(sector_filter)) {
    stopifnot(is.character(sector_filter))
    abort_if_invalid_length(sector_filter, 1L)
    data <- filter(data, .data$sector == sector_filter)
  }
  abort_if_multiple(data, "sector")

  start_year <- get_common_start_year(data, metric)
  data <- filter(data, .data$year >= start_year)

  out <- data %>%
    mutate(
      line_name = .data[[metric]],
      value = .data[[value]],
      extrapolated = FALSE
    )

  if (extrapolate) {
    max_year <- max(out$year, na.rm = TRUE)

    to_extrapolate <- out %>%
      group_by(.data$line_name) %>%
      arrange(desc(.data$year)) %>%
      dplyr::slice(1) %>%
      filter(.data$year != max_year)

    if (nrow(to_extrapolate) != 0) {
      extrapolated <- to_extrapolate
      extrapolated$year <- max_year
      extrapolated <- bind_rows(to_extrapolate, extrapolated)
      extrapolated$extrapolated <- TRUE

      out <- bind_rows(out, extrapolated)
    }
  }

  out$year <- lubridate::make_date(out$year)
  out
}

check_prep_timelineY <- function(data, value, metric, extrapolate) {
  stopifnot(
    is.data.frame(data),
    is.character(value),
    is.character(metric),
    is.logical(extrapolate)
  )
  abort_if_missing_names(data, c("sector", "year", metric, value))

  invisible(data)
}

get_common_start_year <- function(data, metric) {
  year <- max(
    data %>%
      group_by(.data[[metric]]) %>%
      summarise(year = min(.data$year)) %>%
      pull(.data$year)
  )
  year
}
