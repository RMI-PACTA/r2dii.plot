#' Creates a time line plot
#'
#' @param data Pre-processed data for the chart, with columns: year, value,
#'   line_name.
#' @param specs Dataframe containing order of lines, their labels and colour
#'   names from the r2dii_colours palette.
#'
#' @seealso timeline_specs
#' @return An object of class "ggplot".
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Using default preparation and specs
#' data <- prepare_for_timeline(sda_target)
#' plot_timeline(data)
#'
#' # Using custom preparation and specs
#' cement_data <- sda_target %>%
#'   prepare_for_timeline(
#'     sector_filter = "cement",
#'     year_start = 2020,
#'     year_end = 2050,
#'     column_line_names = "emission_factor_metric",
#'     value_to_plot = "emission_factor_value",
#'     extrapolate_missing_values = TRUE
#'   )
#'
#' # Combine `timeline_specs()` and `dput()` or `datapasta::tribble_paste()` to
#' # produce the default `specs`; then adapt it as you wish:
#' custom_specs <- tribble(
#'   ~line_name, ~label, ~colour_hex,
#'   "projected", "Proj.", "#1b324f",
#'   "corporate_economy", "Corp. Economy", "#00c082",
#'   "target_demo", "Target", "#ff9623",
#'   "adjusted_scenario_demo", "Adj. Scenario", "#d0d7e1"
#' )
#'
#' # For reference
#' r2dii_palette_colours()
#'
#' plot_timeline(cement_data, specs = custom_specs)
#'
#' # Customize as usual with ggplot2
#' plot_timeline(cement_data, specs = custom_specs) +
#'   labs(title = "Emission intensity trend for Cement")
plot_timeline <- function(data, specs = timeline_specs(data)) {
  check_specs(specs, data)
  data <- left_join(data, specs, by = "line_name")

  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data$value,
        colour = forcats::fct_reorder2(.data$label, .data$year, .data$value),
        linetype = .data$extrapolated
      )) +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$colour_hex)) +
    scale_linetype_manual(
      values = if (any(data$extrapolated)) c("solid", "dashed") else "solid") +
    guides(linetype = FALSE) +
    theme_2dii()
}

check_specs <- function(specs, data) {
  crucial <- c("line_name", "label", "colour_hex")
  check_crucial_names(specs, crucial)

  specs <- factor_to_character(specs)
  malformed_line_name <- !identical(
    sort(unique(specs$line_name)),
    sort(unique(data$line_name))
  )
  if (malformed_line_name) {
    name_in_data <- toString(sort(unique(data$line_name)))
    name_in_specs <- toString(sort(unique(specs$line_name)))
    abort(glue(
      "Can't find `line_name` values from 'specs' in the data:
      * Unique `line_name` values in 'data' are: {name_in_data}.
      * Unique `line_name` values in 'specs' are: {name_in_specs}."
    ))
  }

  invisible(specs)
}

factor_to_character <- function(data) {
  has_factors <- any(unlist(lapply(data, is.factor)))
  if (is.data.frame(data) && has_factors) {
    data <- mutate(data, across(where(is.factor), as.character))
  }

  data
}

fake_timeline_data <- function(year = NULL,
                               line_name = NULL,
                               value = NULL,
                               extrapolated = NULL,
                               ...) {
  out <- tibble(
    year = year %||% 2002,
    line_name = line_name %||% "projected",
    value = value %||% 0.2,
    extrapolated = extrapolated %||% FALSE,
    ...
  )

  out$year <- lubridate::make_date(out$year)

  out
}
