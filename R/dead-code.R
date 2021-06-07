#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     dplyr::between(year, 2020, 2025),
#'     scenario_source == "demo_2020",
#'     sector == "power",
#'     region == "global",
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#'   )
#'
#' prep <- prep_techmixB(data)
prep_techmixB <- function(data, value = "technology_share", metric = "metric") {
  check_prep_techmixB(data, value)

  data %>%
    recode_metric_and_metric_type(metric) %>%
    pick_extreme_years() %>%
    date_metric_type() %>%
    mutate(value = .data[[value]])
}

pick_extreme_years <- function(data) {
  filter(data, .data$year %in% c(min(data$year), max(data$year)))
}

date_metric_type <- function(data) {
  mutate(data, metric_type = paste0(.data$metric_type, "_", .data$year))
}

check_prep_techmixB <- function(data, value) {
  crucial <- c("metric", "year", "scenario_source", "region", value)
  abort_if_missing_names(data, crucial)

  cols <- c("scenario_source", "sector", "region")
  lapply(cols, function(x) abort_multiple(data, x))

  abort_bad_metric(data$metric)

  invisible(data)
}

#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     technology == "oilcap",
#'     region == "global",
#'     scenario_source == "demo_2020",
#'     year <= 2025,
#'     sector == "power"
#'   )
#'
#' prep_trajectoryB(data)
prep_trajectoryB <- function(data,
                             value = "production",
                             metric = "metric",
                             normalize = TRUE) {
  check_prep_trajectoryB(data, value, normalize)
  data <- recode_metric_and_metric_type(data, metric)

  cols <- c("year", "metric_type", "metric", "technology", "value")
  out <- data %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  if (!normalize) {
    return(out)
  }

  left_join(
    out, filter(out, .data$year == min(.data$year)),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectoryB <- function(data, value, normalize) {
  crucial <- c(
    "metric", "sector", "technology", "region", "year", "scenario_source", value
  )
  abort_if_missing_names(data, crucial)

  abort_if_invalid_length(normalize)
  stopifnot(is.logical(normalize))

  cols <- c("sector", "technology", "region", "scenario_source")
  lapply(cols, function(x) abort_multiple(data, x))

  invisible(data)
}

#' @noRd
#' @description
#' * `plot_timelineB()` plots `line_name` "as is". You may recode `line_name`
#' before passing the `data` with, for example, [dplyr::recode()].
#'
#' @family plotting functions
#'
#' @export
#' @examples
#'
#' # `plot_timelineB()` ------------------------------------------------------
#'
#' data <- sda %>%
#'   filter(sector == "aviation") %>%
#'   prep_timeline()
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
  abort_if_missing_names(data, "line_name")

  line_names <- unique(data$line_name)
  labels <- line_names
  specs <- tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineA(data = data, specs = specs)
}

#' @noRd
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
#' data <- sda %>%
#'   filter(sector == "aviation") %>%
#'   prep_timeline()
#'
#' unique(data$line_name)
#'
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
