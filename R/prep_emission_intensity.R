#' Title
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda].
#'   * The column `sector` must have a single value (e.g. "cement").
#'   * (Optional) If present, the column `label` is used for data labels.
#' @template convert_label
#' @templateVar fun qplot_emission_intensity
#' @templateVar value to_title
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_emission_intensity()`), or use
#'   `FALSE` to impose no restriction.
#'
#' @seealso [sda].
#'
#' @return A data-frame ready to be plotted using `plot_emission_intensity()`.
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda, sector == "cement" & region == "global")
#' prep_emission_intensity(data)
prep_emission_intensity <- function(data,
                                    convert_label = identity,
                                    span_5yr = FALSE) {
  out <- data %>%
    prep_common()

  if (is.factor(out$label)) {
    out$label <- factor(
      convert_label(out$label),
      levels = convert_label(levels(out$label))
    )
  } else {
    out$label <- convert_label(out$label)
  }

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
