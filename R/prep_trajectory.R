#' Prepare data for a trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @template convert_label
#' @templateVar fun qplot_trajectory
#' @templateVar value recode_metric_trajectory
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_trajectory()`), or use
#'   `FALSE` to impose no restriction.
#' @param value_col Character. Name of the column to be used as a value to be
#'   plotted.
#'
#' @seealso [market_share].
#'
#' @return A data-frame ready to be plotted using `plot_trajectory()`.
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     technology == "renewablescap" &
#'     region == "global" &
#'     scenario_source == "demo_2020"
#' )
#'
#' prep_trajectory(data)
prep_trajectory <- function(data,
                            convert_label = identity,
                            span_5yr = FALSE,
                            value_col = "percentage_of_initial_production_by_scope") {
  env <- list(data = substitute(data))
  check_prep_trajectory(
    data,
    convert_label = convert_label,
    span_5yr = span_5yr,
    value_col = value_col,
    env = env
  )

  data <- data %>%
    prep_common() %>%
    mutate(value = !!as.name(value_col)) %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    data <- span_5yr(data)
  }

  data
}

check_prep_trajectory <- function(data,
                                  convert_label,
                                  span_5yr,
                                  value_col,
                                  env) {
  stopifnot(is.data.frame(data))
  stopifnot(is.function(convert_label))
  stopifnot(is.logical(span_5yr))
  stopifnot(is.character(value_col))

  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  enforce_single_value <- c("sector", "technology", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)

  invisible(data)
}
