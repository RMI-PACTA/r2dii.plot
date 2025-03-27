#' Prepare data for a emission intensity plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [sda_demo].
#'   * The column `sector` must have a single value (e.g. "cement").
#'   * (Optional) If present, the column `label` is used for data labels.
#' @param convert_label `r convert_label_docs("qplot_emission_intensity", "to_title")`
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_emission_intensity()`), or use
#'   `FALSE` to impose no restriction.
#'
#' @seealso [sda_demo].
#'
#' @return A data-frame ready to be plotted using `plot_emission_intensity()`.
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(sda_demo, sector == "cement" & region == "global")
#' prep_emission_intensity(data)
prep_emission_intensity <- function(data,
                                    convert_label = identity,
                                    span_5yr = FALSE) {
  check_prep_emission_intensity(
    data,
    convert_label = convert_label,
    span_5yr = span_5yr,
    env = list(data = substitute(data))
  )

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

  out %>%
    mutate(
      year = as.Date(ISOdate(year = .data$year, month = 1L, day = 1L))
    )
}

check_prep_emission_intensity <- function(data, convert_label, span_5yr, env) {
  stopifnot(is.data.frame(data))
  stopifnot(is.function(convert_label))
  stopifnot(is.logical(span_5yr))

  crucial <- prep_emission_factor_crucial
  hint_if_missing_names(abort_if_missing_names(data, crucial), "sda_demo")
  enforce_single_value <- "sector"
  abort_if_multiple(data, enforce_single_value)
  abort_if_has_zero_rows(data, env = env)

  invisible(data)
}

prep_emission_factor_crucial <- c(
  "sector",
  "year",
  glue("emission_factor_{c('metric', 'value')}")
)
