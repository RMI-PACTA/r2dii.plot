#' Prepares sda_target-type data for timeline plot
#'
#' @param sda_target_data Dataframe with columns sector, year and two other
#'   columns specifying value to be be plotted as timelines and line names
#'   (dataframe).
#' @param sector_filter Sector to be used for filtering (character string of
#'   length 1).
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
  sda_target_data$sector <- tolower(sda_target_data$sector)
  sector_filter <- tolower(sector_filter)
  sector_filter <- match.arg(sector_filter)
  warn_sector(sda_target_data, sector_filter)

  check_input_parameters(
    sda_target_data,
    year_start,
    year_end,
    column_line_names,
    value_to_plot,
    extrapolate_missing_values
  )

  # Create output data
  data_timeline <- sda_target_data %>%
    filter(.data$sector == .env$sector_filter) %>%
    filter(.data$year >= year_start) %>%
    filter(.data$year <= year_end) %>%
    mutate(
      line_name = .data[[column_line_names]],
      value = .data[[value_to_plot]],
      extrapolated = FALSE
    ) %>%
    select(
      .data$year,
      .data$line_name,
      .data$value,
      .data$extrapolated
    )


  if (extrapolate_missing_values) {
    max_year_dataset <- max(data_timeline$year, na.rm = TRUE)

    data_to_extrapolate <- data_timeline %>%
      group_by(.data$line_name) %>%
      arrange(desc(.data$year)) %>%
      dplyr::slice(1) %>%
      filter(.data$year != max_year_dataset)

    if (nrow(data_to_extrapolate) != 0) {
      data_extrapolated <- data_to_extrapolate
      data_extrapolated$year <- max_year_dataset

      data_extrapolated <- dplyr::bind_rows(
        data_to_extrapolate,
        data_extrapolated
      )
      data_extrapolated$extrapolated <- TRUE

      data_timeline <- dplyr::bind_rows(data_timeline, data_extrapolated)
    }
  }

  data_timeline$year <- lubridate::make_date(data_timeline$year)

  data_timeline
}

warn_sector <- function(data, sector_filter) {
  too_long <- length(unique(data$sector)) > 1L
  if (too_long) {
    warn(
      class = "chosen_sector",
      sprintf("Choosing sector: %s", sector_filter)
    )
  }

  missing_sector <- !sector_filter %in% unique(data$sector)
  if (missing_sector) {
    warn(
      class = "missing_sector",
      sprintf("Found no data for sector: %s", sector_filter)
    )
  }

  invisible(data)
}

check_input_parameters <- function(data,
                                   year_start,
                                   year_end,
                                   column_line_names,
                                   value_to_plot,
                                   extrapolate_missing_values) {
  if (typeof(year_start) != "double") {
    msg <- sprintf(
      "'year_start' must be a number.
        * You submitted a %s.",
      typeof(year_start)
    )
    stop(msg, call. = FALSE)
  }

  if (typeof(year_end) != "double") {
    msg <- sprintf(
      "'year_end' must be a number.
        * You submitted a %s.",
      typeof(year_end)
    )
    stop(msg, call. = FALSE)
  }

  if (!(column_line_names %in% names(data))) {
    msg <- sprintf(
      "'column_line_names' must be one of column names in the input data.
      * The input data column names are: %s.
      * You submitted: %s.",
      toString(names(data)),
      column_line_names
    )
    stop(msg, call. = FALSE)
  }

  if (!(value_to_plot %in% names(data))) {
    msg <- sprintf(
      "'value_to_plot' must be one of column names in the input data.
      * The input data column names are: %s.
      * You submitted: %s.",
      toString(names(data)),
      value_to_plot
    )
    stop(msg, call. = FALSE)
  }

  if (!is.logical(extrapolate_missing_values)) {
    msg <- sprintf(
      "'extrapolate_missing_values' must be a logical value.
      * You submitted a %s.",
      typeof(extrapolate_missing_values)
    )
    stop(msg, call. = FALSE)
  }

  invisible(data)
}

