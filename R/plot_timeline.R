#' Creates a time line plot
#'
#' @param data Pre-processed data for the chart, with columns: year, value,
#'   line_name.
#' @param specs Dataframe containing order of lines, their labels and colour
#'   names from the r2dii_colours palette.
#'
#' @description
#' We are exploring different interfaces before release. We are keen to hear
#' feedback from beta-testers like you. Please try these alternative interfaces
#' and let us know which one you prefer. The main difference between them is if
#' and how they allow recoding the values of `line_name`, which become the
#' labels of the plot legend:
#'
#' * `plot_timelineA()` defaults to recoding `line_name` to title case, and
#' allows custom recoding via a data frame passed to the argument `specs`.
#'
#' @seealso timeline_specs
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' data <- sda_target %>%
#'   filter(sector == "cement", between(year, 2020, 2050)) %>%
#'   prepare_for_timelineB(extrapolate = TRUE)
#'
#' # `plot_timelineA()` -------------------------------------------------------
#'
#' data <- prepare_for_timelineA(sda_target, sector_filter = "cement")
#' plot_timelineA(data)
#'
#' # Customize as usual with ggplot2
#' plot_timelineA(data) +
#'   scale_colour_manual(values = c("red", "blue", "green", "black")) +
#'   labs(title = "Timeline plot")
#'
#' # Customize `line_name` via a data frame passed to `specs`
#' # styler: off
#' custom <- tribble(
#'                 ~line_name,                  ~label, ~hex,
#'                "projected",                 "Proj.",   "#4a5e54",
#'        "corporate_economy",         "Corp. Economy",   "#a63d57",
#'              "target_demo",         "Target (demo)",   "#78c4d6",
#'   "adjusted_scenario_demo",  "Adj. Scenario (demo)",   "#f2e06e",
#' )
#' # styler: on
#'
#' plot_timelineA(data, specs = custom)
plot_timelineA <- function(data, specs = timeline_specs(data)) {
  check_specs(specs, data)
  abort_too_many_sectors(data)
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
    scale_colour_manual(values = unique(data$hex)) +
    scale_linetype_manual(
      values = if (any(data$extrapolated)) c("solid", "dashed") else "solid") +
    guides(linetype = FALSE) +
    theme_2dii()
}
# For backward compatibility until we decide which version to keep
plot_timeline <- plot_timelineA

#' @rdname plot_timelineA
#' @description
#' * `plot_timelineB()` plots `line_name` "as is". You may recode `line_name`
#' before passing the `data` with, for example, [dplyr::recode()].
#'
#' @export
#' @examples
#'
#' # `plot_timelineB()` ------------------------------------------------------
#'
#' data <- prepare_for_timelineA(sda_target, sector_filter = "aviation")
#' plot_timelineB(data)
#'
#' # Recode `line_name` with `dplyr::recode()`
#' data %>%
#'   mutate(line_name = recode(line_name,
#'     "corporate_economy" = "Corp. economy",
#'     "projected" = "Proj.",
#'     "target_demo" = "Target (demo)",
#'     "adjusted_scenario_demo" = "Adj. Scenario (demo)",
#'   )) %>%
#'   plot_timelineB()
plot_timelineB <- function(data) {
  check_crucial_names(data, "line_name")

  line_names <- unique(data$line_name)
  labels <- line_names
  specs <- tibble(line_name = line_names, label = labels) %>%
    stop_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineA(data = data, specs = specs)
}

#' @rdname plot_timelineA
#' @description
#' * `plot_timelineC()` defaults to plotting a title case version of `line_name`
#' values, and allows none or other recoding via the argument `recode` (see
#' Arguments).
#' @param recode One of the following:
#' * A function/lambda to apply to `data$line_name`.
#' * A named vector to recode `data$line_name`.
#' * A logical vector of length 1. `TRUE` recodes `data$line_name` to title
#' case. `FALSE` does no recoding and plots `data$line_name` as is.
#' @seealso [dplyr::recode()].
#' @export
#' @examples
#'
#' # `plot_timelineC()` ------------------------------------------------------
#'
#' data <- prepare_for_timelineA(sda_target, sector_filter = "aviation")
#' unique(data$line_name)
#' # Recode to title case
#' plot_timelineC(data, recode = TRUE)
#'
#' # Don't recode
#' plot_timelineC(data, recode = FALSE)
#'
#' # Recode to title case
#' unique(data$line_name)
#' plot_timelineC(data)
#'
#' # Recode using a function
#' plot_timelineC(data, recode = toupper)
#'
#' # Recode using a formula giving a lambda function
#' plot_timelineC(data, recode = ~ toupper(gsub("_", " ", .x)))
#'
#' # Recode via a named vector
#' legend <- c(
#'   "projected" = "Proj.",
#'   "corporate_economy" = "Corp. Economy",
#'   "target_demo" = "Target (demo)",
#'   "adjusted_scenario_demo" = "Adj. Scenario (demo)"
#' )
#' plot_timelineC(data, recode = legend)
plot_timelineC <- function(data, recode = TRUE) {
  if (!is.null(recode)) data$line_name <- recode_lines(recode, data)
  plot_timelineB(data)
}

recode_lines <- function(recode, data) {
  UseMethod("recode_lines")
}
recode_lines.default <- function(recode, data) {
  abort(glue("Can't handle `recode` of class: {class(recode)}"))
}
recode_lines.function <- function(recode, data) {
  recode(data$line_name)
}
recode_lines.formula <- function(recode, data) {
  f <- rlang::as_function(recode)
  f(data$line_name)
}
recode_lines.character <- function(recode, data) {
  dplyr::recode(data$line_name, !!!recode)
}
recode_lines.logical <- function(recode, data) {
  out <- data$line_name
  if (recode) out <- to_title(data$line_name)
  out
}

check_specs <- function(specs, data) {
  crucial <- c("line_name", "label", "hex")
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

abort_too_many_sectors <- function(data) {
  sectors <- unique(data$sector)
  if (length(sectors) > 1L) {
    abort(
      class = "too_many_sectors",
      glue("`data` must have a single sector. It has: {toString(sectors)}")
    )
  }

  invisible(data)
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
