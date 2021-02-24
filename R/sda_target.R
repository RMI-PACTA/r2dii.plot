#' Time series of emission factors
#'
#' An example data set containing emission factors for different metric types:
#' projected, benchmark and scenarios and different sectors.
#'
#' @format A data frame with 208 rows and 4 variables:
#' \describe{
#'   \item{sector}{industry sector}
#'   \item{year}{year for which emisisons are reported}
#'   \item{emission_factor_metric}{metric type: 'projected' for the test
#'   portfolio 'corporate_economy' for the benchmark, 'adjusted_scenario_demo'
#'   for a scenario}
#'   \item{emission_factor_value}{emissions value, a double, units differ
#'   depending on the sector}
#' }
#' @source \url{https://github.com/2DegreesInvesting/r2dii.ggplot/issues/55}
"sda_target"
