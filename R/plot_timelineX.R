#' Create a timeline plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @inheritParams prep_timelineY
#' @inheritParams plot_timelineA
#'
#' @return A "ggplot".
#' @export
#'
#' @family functions with minimal arguments
#' @seealso [sda].
#'
#' @examples
#' library(dplyr)
#'
#' # `data` must meet documented "Requirements"
#' data <- filter(sda, sector == "cement")
#' plot_timelineX(data)
plot_timelineX <- function(data, extrapolate = FALSE) {
  prep <- prep_timelineY(data, extrapolate = extrapolate)
  plot_timelineB(prep)
}
