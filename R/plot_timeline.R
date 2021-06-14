#' Create a timeline plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#' @param extrapolate Logical of length 1. `TRUE` extrapolates to match the
#'   furthest value in the data set.
#'
#' @family functions with minimal arguments
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # `data` must meet documented "Requirements"
#' data <- filter(sda, sector == "cement")
#' plot_timeline(data)
plot_timeline <- function(data, extrapolate = FALSE) {
  stopifnot(is.data.frame(data))
  abort_if_has_cero_rows(data)
  prep <- hint_if_missing_names(prep_timeline(data, extrapolate = extrapolate))

  plot_timelineB(prep)
}

prep_timeline <- function(data,
                          value = "emission_factor_value",
                          metric = "emission_factor_metric",
                          sector_filter = NULL,
                          extrapolate = FALSE) {
  check_prep_timeline(data, value, metric, extrapolate)
  if (!is.null(sector_filter)) {
    stopifnot(is.character(sector_filter))
    abort_if_invalid_length(sector_filter, 1L)
    data <- filter(data, .data$sector == sector_filter)
  }
  abort_if_multiple(data, "sector")

  start_year <- get_common_start_year(data, metric)
  data <- filter(data, .data$year >= start_year)

  out <- data %>%
    mutate(
      line_name = .data[[metric]],
      value = .data[[value]],
      extrapolated = FALSE
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

check_prep_timeline <- function(data, value, metric, extrapolate) {
  stopifnot(
    is.data.frame(data),
    is.character(value),
    is.character(metric),
    is.logical(extrapolate)
  )
  abort_if_missing_names(data, c("sector", "year", metric, value))

  invisible(data)
}

get_common_start_year <- function(data, metric) {
  year <- max(
    data %>%
      group_by(.data[[metric]]) %>%
      summarise(year = min(.data$year)) %>%
      pull(.data$year)
  )
  year
}

plot_timelineB <- function(data) {
  abort_if_missing_names(data, "line_name")

  line_names <- unique(data$line_name)
  labels <- line_names
  specs <- tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineY(data = data, specs = specs)
}

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

timeline_specs <- function(data) {
  abort_if_missing_names(data, "line_name")

  to_title <- function(x) {
    paste(tools::toTitleCase(unlist(strsplit(x, "_"))), collapse = " ")
  }

  line_names <- unique(data$line_name)
  labels <- unlist(lapply(line_names, to_title))
  tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()
}

abort_if_too_many_lines <- function(data) {
  n_lines <- nrow(data)
  n_colours <- nrow(palette_colours)
  if (n_lines > n_colours) {
    abort(glue(
      "Can't plot more than {n_colours} lines. Found {n_lines} lines:
        {toString(data$line_name)}."
    ))
  }

  invisible(data)
}

add_r2dii_colours <- function(specs) {
  n <- seq_len(nrow(specs))
  specs$r2dii_colour_name <- palette_colours$label[n]

  specs %>%
    left_join(palette_colours, by = c("r2dii_colour_name" = "label")) %>%
    select(-.data$r2dii_colour_name)
}
