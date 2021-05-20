#' Prepares sda_target-type data for timeline plot
#'
#' @param data,sda_target_data Dataframe with columns sector, year and two other
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
#' @param extrapolate,extrapolate_missing_values Flag indicating if values
#'   should be extrapolated to match the furthest value in the data set.
#'
#' @return Dataframe with columns: year, line_name, value, extrapolated.
#' @export
#'
#' @examples
#' # prepare_for_timelineA() --------------------------------------------------
#'
#' data <- sda_target
#' prepare_for_timelineA(
#'   data,
#'   sector_filter = "cement",
#'   year_start = 2020,
#'   year_end = 2050, column_line_names = "emission_factor_metric",
#'   value_to_plot = "emission_factor_value",
#'   extrapolate_missing_values = TRUE
#' )
prepare_for_timelineA <- function(sda_target_data,
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
      .data$extrapolated,
      .data$sector
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

      data_extrapolated <- bind_rows(
        data_to_extrapolate,
        data_extrapolated
      )
      data_extrapolated$extrapolated <- TRUE

      data_timeline <- bind_rows(data_timeline, data_extrapolated)
    }
  }

  data_timeline$year <- lubridate::make_date(data_timeline$year)

  data_timeline
}
# For backward compatibility until we decide which version to keep
prepare_for_timeline <- prepare_for_timelineA

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

#' @export
#' @rdname prepare_for_timelineA
#' @examples
#'
#' # prepare_for_timelineA() --------------------------------------------------
#'
#' data <- sda_target
#' tail(prepare_for_timelineB(data))
#' tail(prepare_for_timelineB(data, extrapolate = TRUE))
prepare_for_timelineB <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data), is.logical(extrapolate))
    crucial <- c("emission_factor_metric", "emission_factor_value", "year")
  check_crucial_names(data, crucial)

  out <- data %>%
    mutate(
      line_name = .data$emission_factor_metric,
      value = .data$emission_factor_value,
      extrapolated = FALSE
    ) %>%
    select(
      .data$year,
      .data$line_name,
      .data$value,
      .data$extrapolated,
      .data$sector
    )

  if (extrapolate) {
    max_year <- max(out$year, na.rm = TRUE)

    to_extrapolate <- out %>%
      group_by(.data$line_name) %>%
      arrange(desc(.data$year)) %>%
      dplyr::slice(1) %>%
      filter(.data$year != max_year)

    if (nrow(to_extrapolate) != 0) {
      extrapolated <- to_extrapolate
      extrapolated$year <- max_year
      extrapolated <- bind_rows(to_extrapolate, extrapolated)
      extrapolated$extrapolated <- TRUE

      out <- bind_rows(out, extrapolated)
    }
  }

  out$year <- lubridate::make_date(out$year)
  out
}
