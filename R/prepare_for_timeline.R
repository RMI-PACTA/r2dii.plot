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
#' @param extrapolate_missing_values Flag indicating if values should be
#'   extrapolated to match the furthest value in the data set.
#'
#' @return Dataframe with columns: year, line_name, value, extrapolated.
#' @export
#'
#' @examples
#' prepare_for_timeline(sda_target,
#'   sector_filter = "cement",
#'   year_start = 2020,
#'   year_end = 2050, column_line_names = "emission_factor_metric",
#'   value_to_plot = "emission_factor_value",
#'   extrapolate_missing_values = TRUE
#' )
prepare_for_timeline <- function(sda_target_data,
                                 sector_filter = c(
                                   "automotive",
                                   "aviation",
                                   "cement",
                                   "oil and gas",
                                   "shipping",
                                   "steel",
                                   "power"
                                 ),
                                 year_start = 0,
                                 year_end = Inf,
                                 column_line_names = "emission_factor_metric",
                                 value_to_plot = "emission_factor_value",
                                 extrapolate_missing_values = FALSE) {

  # Check inputs
  sector_filter <- match.arg(sector_filter)

  if (typeof(year_start) != "double") {
    msg <- paste0(
      "'year_start' must be a number.\n",
      paste0("* You submitted a ", typeof(year_start), ".")
    )
    stop(msg)
  }

  if (typeof(year_end) != "double") {
    msg <- paste0(
      "'year_end' must be a number.\n",
      paste0("* You submitted a ", typeof(year_end), ".")
    )
    stop(msg)
  }

  if (!(column_line_names %in% names(sda_target_data))) {
    msg <- paste0(
      "'column_line_names' must be one of column names in the input data.\n",
      paste0(
        "The input data column names are: ",
        toString(names(sda_target_data)),
        ".\n"
      ),
      paste0("You submitted: ", column_line_names, ".")
    )
    stop(msg)
  }

  if (!(value_to_plot %in% names(sda_target_data))) {
    msg <- paste0(
      "'value_to_plot' must be one of column names in the input data.\n",
      paste0(
        "The input data column names are: ",
        toString(names(sda_target_data)),
        ".\n"
      ),
      paste0("You submitted: ", value_to_plot, ".")
    )
    stop(msg)
  }

  if (!is.logical(extrapolate_missing_values)) {
    msg <- paste0(
      "'extrapolate_missing_values' must be a logical value.\n",
      paste0("* You submitted a ", typeof(extrapolate_missing_values), ".")
    )
    stop(msg)
  }

  # Create output data
  data_timeline <- sda_target_data %>%
    filter(.data$sector == !!sector_filter) %>%
    filter(.data$year >= year_start) %>%
    filter(.data$year <= year_end) %>%
    select(.data$year, line_name = !!column_line_names, value = !!value_to_plot) %>%
    mutate(extrapolated = FALSE)

  if (extrapolate_missing_values) {
    max_year_dataset <- max(data_timeline$year, na.rm = TRUE)

    data_to_extrapolate <- data_timeline %>%
      group_by(.data$line_name) %>%
      arrange(desc(.data$year)) %>%
      dplyr::slice(1) %>%
      filter(.data$year != max_year_dataset)

    data_extrapolated <- data_to_extrapolate
    data_extrapolated$year <- max_year_dataset

    data_extrapolated <- rbind(data_to_extrapolate, data_extrapolated)
    data_extrapolated$extrapolated <- TRUE

    data_timeline <- rbind(data_timeline, data_extrapolated)
  }

  data_timeline
}
