#' Prepare data for a trajectory plot
#'
#' @param data A data frame. Requirements:
#' * The structure must be like [market_share].
#' * The following columns must have a single value: `sector`, `technology`,
#' `region`, `scenario_source`.
#' * (Optional) If present, the column `label` is used for data labels.
#' @template convert_label
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
                            value_col = "percentage_of_initial_production_by_scope") {

  env <- list(data = substitute(data))

  check_prep_trajectory(data, value_col = value_col, env = env)

  out <- data %>%
    prep_common() %>%
    mutate(value = !!as.name(value_col)) %>%
    mutate(label = convert_label(.data$label))
}

check_prep_trajectory <- function(data, value_col, env) {
  stopifnot(is.data.frame(data))
  crucial <- c(common_crucial_market_share_columns(), value_col)
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")
  invisible(data)
}
