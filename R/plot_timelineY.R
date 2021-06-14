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
