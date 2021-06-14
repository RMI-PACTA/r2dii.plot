#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
#' @param main_line String of length 1. The `metric` to plot as the line with
#'   the most visual salience (solid black line). `NULL` defaults to
#'   "projected".
#' @inheritParams prep_trajectoryY
#' @inheritParams prep_timelineY
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
#'     sector == "power",
#'     technology == "renewablescap",
#'     region == "global",
#'     scenario_source == "demo_2020"
#'   )
#' plot_trajectoryX(data)
#'
#' plot_trajectoryX(data, normalize = FALSE)
plot_trajectoryX <- function(data, normalize = TRUE, main_line = NULL) {
  stopifnot(is.data.frame(data))
  single_value_columns <- function() {
    c("region", "sector", "scenario_source")
  }
    crucial <- c(
    "metric", single_value_columns(), "technology", "year"
  )
  hint_if_missing_names(abort_if_missing_names(data, crucial))


  abort_if_missing_names(data, "metric")

  if (is.null(main_line)) {
    stopifnot("projected" %in% tolower(data$metric))
    main <- "projected"
  } else {
    stopifnot(main_line %in% tolower(data$metric))
    main <- main_line
  }

  prep <- prep_trajectoryB(data, normalize = normalize)

  plot_trajectoryB(prep, main_line = main)
}

set_first <- function(x, first) {
  stopifnot(first %in% x)
  tail <- setdiff(unique(x), first)
  c(first, tail)
}
