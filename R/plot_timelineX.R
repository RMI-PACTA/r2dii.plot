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
  abort_if_has_cero_rows <- function(data) {
    .data <- deparse_1(substitute(data, env = parent.frame()))
    if (nrow(data) == 0L) {
      abort(glue("`{.data}` must have some rows but has none."))
    }
    invisible(data)
  }
  abort_if_has_cero_rows(data)
  prep <- prep_timelineY(data, extrapolate = extrapolate)
  plot_timelineB(prep)
}
