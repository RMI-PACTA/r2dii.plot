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
                                 sector_filter,
                                 year_start = 0,
                                 year_end = Inf,
                                 column_line_names = "emission_factor_metric",
                                 value_to_plot = "emission_factor_value",
                                 extrapolate_missing_values = FALSE) {
  if (length(sector_filter) > 1L) abort("`sector_filter` must be of length 1")
  sda_target_data$sector <- tolower(sda_target_data$sector)
  sector_filter <- tolower(sector_filter)
  valid_sectors <- c("automotive",
                     "aviation",
                     "cement",
                     "oil and gas",
                     "shipping",
                     "steel",
                     "power")
  if (!sector_filter %in% valid_sectors) {
    abort(glue(
      "Invalid `sector_filter`: {sector_filter}.
      Expected one of: {toString(valid_sectors)}."
    ))
  }
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
    msg <- glue::glue(
      "Can only use one sector.
      Using the first of the vector passed to `sector_filter`: {sector_filter}."
    )
    warn(class = "chosen_sector", msg)
  }

  missing_sector <- !sector_filter %in% unique(data$sector)
  if (missing_sector) {
    warn(
      class = "missing_sector",
      glue::glue("Found no data for sector: {sector_filter}.")
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
    abort(glue(
      "'year_start' must be a number.
        * You submitted a {typeof(year_start)}."
    ))
  }

  if (typeof(year_end) != "double") {
    abort(glue(
      "'year_end' must be a number.
        * You submitted a {typeof(year_end)}."
    ))
  }

  if (!(column_line_names %in% names(data))) {
    abort(glue(
      "`column_line_names` must be one of column names in the input `data`:
      * The input `data` column names are: {toString(names(data))}.
      * You submitted: {column_line_names}."
    ))
  }

  if (!(value_to_plot %in% names(data))) {
    abort(glue(
      "`value_to_plot` must be one of column names in the input `data`:
      * The input `data` column names are: {toString(names(data))}.
      * You submitted: {value_to_plot}."
    ))
  }

  if (!is.logical(extrapolate_missing_values)) {
    abort(glue(
      "`extrapolate_missing_values` must be a logical value:
      * You submitted a {typeof(extrapolate_missing_values)}."
    ))
  }

  invisible(data)
}

