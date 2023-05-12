#' Create an emission intensity plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#'   * (Optional) If present, the column `label` is used for data labels.
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_emission_intensity()`), or use
#'   `FALSE` to impose no restriction.
#' @template convert_label
#' @templateVar fun qplot_emission_intensity
#' @templateVar value to_title
#'
#' @seealso [sda].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda, sector == "cement" & region == "global")
#' plot_emission_intensity(data)
#'
#' # plot with `qplot_emission_intensity()` parameters
#' plot_emission_intensity(
#'   data,
#'   span_5yr = TRUE,
#'   convert_label = to_title
#' )
plot_emission_intensity <- function(data,
                                    span_5yr = FALSE,
                                    convert_label = identity) {
  lifecycle::deprecate_soft(
    when = "0.4.0",
    what = "plot_emission_intensity(data = 'must be prepped already')",
    details = api_warning_details("plot_emission_intensity")
  )
  env <- list(data = substitute(data))
  check_plot_emission_intensity(data, env = env)

  data %>%
    prep_emission_intensity(
      convert_label = convert_label,
      span_5yr = span_5yr
    ) %>%
    plot_emission_intensity_impl()
}

check_plot_emission_intensity <- function(data, env) {
  stopifnot(is.data.frame(data))
  crucial <- c("sector", "year", glue("emission_factor_{c('metric', 'value')}"))
  hint_if_missing_names(abort_if_missing_names(data, crucial), "sda")
  enforce_single_value <- "sector"
  abort_if_multiple(data, enforce_single_value)
  abort_if_has_zero_rows(data, env = env)
  abort_if_too_many_lines(data, max = 7)

  invisible(data)
}

prep_emission_intensity <- function(data,
                                    convert_label = identity,
                                    span_5yr = FALSE) {
  out <- data %>%
    prep_common() %>%
    mutate(label = convert_label(.data$label))

  if (span_5yr) {
    out <- span_5yr(out)
  }

  out <- out %>%
    mutate(
      year = as.Date(ISOdate(year = .data$year, month = 1L, day = 1L))
    )

  metrics <- distinct(out, .data$emission_factor_metric)
  colours <- palette_colours[seq_len(nrow(metrics)), "hex", drop = FALSE]
  specs <- dplyr::bind_cols(metrics, colours)

  left_join(out, specs, by = metric(data))
}

plot_emission_intensity_impl <- function(data) {
  ggplot(
    data = data,
    aes(
      x = .data$year,
      y = .data$emission_factor_value,
      colour = match_lines_order(data)
    )
  ) +
    geom_line() +
    expand_limits(y = 0) +
    scale_x_date(expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_colour_manual(values = unique(data$hex)) +
    theme_2dii()
}

match_lines_order <- function(data) {
  forcats::fct_reorder2(
    data$label,
    data$year,
    data$emission_factor_value
  )
}
