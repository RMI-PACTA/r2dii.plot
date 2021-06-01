#' Prepare the output of `r2dii.analysis::target_sda()` for `plot_timeline()`
#'
#' @param data Dataframe like the output of `r2dii.analysis::target_sda()`.
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
#'
#' @seealso [sda].
#'
#' @return Dataframe with columns: year, line_name, value, extrapolated.
#' @export
#'
#' @examples
#' tail(prep_timeline(sda))
#' tail(prep_timeline(sda, extrapolate = TRUE))
prep_timeline <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data), is.logical(extrapolate))
  crucial <- c(
    "sector",
    "year",
    "emission_factor_metric",
    "emission_factor_value"
  )
  check_crucial_names(data, crucial)

  out <- data %>%
    mutate(
      line_name = .data$emission_factor_metric,
      value = .data$emission_factor_value,
      extrapolated = FALSE
    ) %>%
    select(
      .data$year,
      .data$line_name,
      .data$value,
      .data$extrapolated,
      .data$sector
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
