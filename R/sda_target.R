#' Time series of emission factors
#'
#' An example data set containing emission factors for different metric types:
#' projected, benchmark and scenarios and different sectors.
#'
#' Column definitions:
#' * `sector`: Industry sector (as defined in PACTA).
#' * `year`: Year for which emissions are reported.
#' * `emission_factor_metric`: metric type: 'projected' for the test portfolio
#' 'corporate_economy' for the benchmark, 'adjusted_scenario_demo' for a scenario.
#' * `emission_factor_value`: emissions value, a double, units differ depending
#' on the sector.
#'
#' @source <https://github.com/2DegreesInvesting/r2dii.plot.static/issues/55>.
#' @examples
#' sda_target
"sda_target"
