#' Prepare the output of `r2dii.analysis::target_sda()` for `plot_timeline()`
#'
#' @param data Data frame like the output of `r2dii.analysis::target_sda()`.
#' @param value String of length 1. The name of the column holding the value to
#'   plot.
#' @param Metric String of length 1. The name of the column holding the metrics
#'   to plot.
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
#'
#' @seealso [sda].
#'
#' @return Data frame with columns: year, line_name, value, extrapolated.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' tail(prep_timeline(sda))
#' tail(prep_timeline(sda, extrapolate = TRUE))
#'
#' data <- sda %>%
#'   rename(
#'     custom_value = "emission_factor_value",
#'     custom_metric = "emission_factor_metric"
#'   )
#' prep_timeline(data, value = "custom_value", metric = "custom_metric")
prep_timeline <- function(data,
                          value = "emission_factor_value",
                          metric = "emission_factor_metric",
                          extrapolate = FALSE) {
  check_prep_timeline(data, value, metric, extrapolate)

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

check_prep_timeline <- function(data, value, metric, extrapolate) {
  stopifnot(
    is.data.frame(data),
    is.character(value),
    is.character(metric),
    is.logical(extrapolate)
  )
  check_crucial_names(data, c("sector", "year", metric, value))

  invisible(data)
}
get_common_start_year <- function(data, column_line_names) {
  year <- max(
    data %>%
      group_by(.data[[column_line_names]]) %>%
      summarise(year = min(.data$year)) %>%
      pull(.data$year)
  )
  year
}
