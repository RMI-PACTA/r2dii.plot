#' Create a trajectory plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `technology`,
#'   `region`, `scenario_source`.
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

  plot_trajectoryB(prep, main_line = main)
}

set_first <- function(x, first) {
  stopifnot(first %in% x)
  tail <- setdiff(unique(x), first)
  c(first, tail)
}
