#' Create a techmix plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `region`,
#'   `scenario_source`.
#'   * The column `metric` must have a portfolio (e.g. "projected"), a benchmark
#'   (e.g. "corporate_economy"), and a single `scenario` (e.g. "target_sds").
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
#'     scenario_source == "demo_2020",
#'     sector == "power",
#'     region == "global",
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#'   )
#' plot_techmixX(data)
plot_techmixX <- function(data) {
  prep <- prep_techmixB(data)
  found_scenarios <- pull_scenarios(prep)
  abort_if_invalid_length(found_scenarios)

  plot_techmixY(prep)
}
