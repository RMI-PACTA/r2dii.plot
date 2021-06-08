#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
#' @param main String of length 1. `metric` value to plot most prominently, as
#'   the main trajectory line.
#' @inheritParams prep_trajectoryY
#' @inheritParams prep_timelineY
#'
#' @return A "ggplot".
#' @export
#'
#' @family functions with minimal arguments
#' @seealso [market_share].
#'
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
plot_trajectoryX <- function(data, normalize = TRUE) {
  stopifnot("projected" %in% tolower(data$metric))
  main <- "projected"

  prep <- prep_trajectoryB(data, normalize = normalize)

  # FIXME: Make it work with values exposed to the user. Now it's not the case.
  # e.g. exposed metric: "target_sds", current metric: "sds".
  .metric <- factor(prep$metric, levels = set_first(prep$metric, first = main))
  ordered <- arrange(mutate(prep, metric = .metric), .data$year, .data$metric)

  plot_trajectoryB(ordered)
}

set_first <- function(x, first) {
  stopifnot(first %in% x)
  tail <- setdiff(unique(x), first)
  c(first, tail)
}
