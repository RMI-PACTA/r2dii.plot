#' Prepares sda_target-type data for timeline plot
#'
#' @param sda_target_data Dataframe with columns sector, year and two other
#'   columns specifying value to be be plotted as timelines and line names
#'   (dataframe).
#' @param sector_filter Sector to be used for filtering (character string or a
#'   vector of character strings).
#' @param year_start Start year of the plot (double).
#' @param year_end End year of the plot (double).
#' @param column_line_names Column specifying the names of lines to be plotted
#'   (character string).
#' @param value_to_plot Column name of the value to be plotted (character
#'   string).
#'
#' @return Dataframe with columns: year, line_name, value.
#' @export
#'
#' @examples
#' prepare_for_timeline(sda_target,
#' sector_filter = "automotive",
#' year_start = 2020,
#' year_end = 2026,column_line_names = "emission_factor_metric",
#' value_to_plot = "emission_factor_value")
prepare_for_timeline <- function(sda_target_data,
                                 sector_filter = c("automotive",
                                                   "aviation",
                                                   "cement",
                                                   "oil and gas",
                                                   "shipping",
                                                   "steel",
                                                   "power"),
                                 year_start = 0,
                                 year_end = Inf,
                                 column_line_names = "emission_factor_metric",
                                 value_to_plot = "emission_factor_value") {

  # Check inputs
  sector_filter <- match.arg(sector_filter)

  if (typeof(year_start) != "double") {
    stop("'year_start' should be a number.")
  }

  if (typeof(year_start) != "double") {
    stop("'year_end' should be a number.")
  }

  if (!(column_line_names %in% names(sda_target_data))) {
    stop(paste0("'", column_line_names, "' not found as a column in the input data. Please pass an existing column name to 'column_line_names'."))
  }

  if (!(value_to_plot %in% names(sda_target_data))) {
    stop(paste0("'", value_to_plot, "' not found as a column in the input data. Please pass an existing column name to 'value_to_plot'."))
  }

  # Create output data
  data_timeline <- sda_target_data %>%
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$year >= year_start) %>%
    filter(.data$year <= year_end) %>%
    select(.data$year, line_name = !!column_line_names, value = !!value_to_plot)

  data_timeline
}
