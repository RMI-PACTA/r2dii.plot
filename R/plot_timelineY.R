#' Creates a time line plot
#'
#' @param data Data frame like the output of [prep_timelineY()].
#' @param specs Data frame containing order of lines, their labels and colour
#'   names from the r2dii_colours palette.
#'
#' @description
#' We are exploring different interfaces before release. We are keen to hear
#' feedback from beta-testers like you. Please try these alternative interfaces
#' and let us know which one you prefer. The main difference between them is if
#' and how they allow recoding the values of `line_name`, which become the
#' labels of the plot legend:
#'
#' * `plot_timelineY()` defaults to recoding `line_name` to title case, and
#' allows custom recoding via a data frame passed to the argument `specs`.
#'
#' @seealso [sda], [timeline_specs].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' data <- prep_timelineY(sda, sector_filter = "cement")
#' plot_timelineY(data)
#'
#' # Customize as usual with ggplot2
#' plot_timelineY(data) +
#'   scale_colour_manual(values = c("red", "blue", "green", "black")) +
#'   labs(title = "Timeline plot")
#'
#' # Customize `line_name` via a data frame passed to `specs`
#' # styler: off
#' custom <- tribble(
#'                 ~line_name,                  ~label,        ~hex,
#'                "projected",                 "Proj.",   "#4a5e54",
#'        "corporate_economy",         "Corp. Economy",   "#a63d57",
#'              "target_demo",         "Target (demo)",   "#78c4d6",
#'   "adjusted_scenario_demo",  "Adj. Scenario (demo)",   "#f2e06e",
#' )
#' # styler: on
#'
#' plot_timelineY(data, specs = custom)
plot_timelineY <- function(data, specs = timeline_specs(data)) {
  check_specs(specs, data)
  abort_if_multiple(data, "sector")
  data <- left_join(data, specs, by = "line_name")

  ggplot() +
    geom_line(
      data = data, aes(
        x = .data$year,
        y = .data$value,
        colour = forcats::fct_reorder2(.data$label, .data$year, .data$value),
        linetype = .data$extrapolated
      )
    ) +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    scale_linetype_manual(
      values = if (any(data$extrapolated)) c("solid", "dashed") else "solid"
    ) +
    guides(linetype = FALSE) +
    theme_2dii()
}

check_specs <- function(specs, data) {
  crucial <- c("line_name", "label", "hex")
  abort_if_missing_names(specs, crucial)

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
                               sector = NULL,
                               ...) {
  out <- tibble(
    year = year %||% 2002,
    line_name = line_name %||% "projected",
    value = value %||% 0.2,
    extrapolated = extrapolated %||% FALSE,
    sector = sector %||% "automotive",
    ...
  )

  out$year <- lubridate::make_date(out$year)

  out
}
