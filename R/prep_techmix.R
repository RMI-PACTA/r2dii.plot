#' Prepare data for plotting technology mix
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `region`,
#'   `scenario_source`.
#'   * The column `metric` must have a portfolio (e.g. "projected"), a benchmark
#'   (e.g. "corporate_economy"), and a single `scenario` (e.g. "target_sds").
#'   * (Optional) If present, the column `label` is used for data labels.
#'   * (Optional) If present, the column `label_tech` is used for technology
#'   labels.
#' @template convert_label
#' @templateVar fun qplot_techmix
#' @templateVar value recode_metric_techmix
#' @param span_5yr Logical. Use `TRUE` to restrict the time span to 5 years from
#'   the start year (the default behavior of `qplot_techmix()`), or use
#'   `FALSE` to impose no restriction.
#' @param convert_tech_label A symbol. The unquoted name of a function to apply
#'   to technology legend labels. For example, to convert labels to uppercase
#'   use `convert_tech_label = toupper`. To get the default behavior of
#'   `qplot_techmix()` use `convert_tech_label = spell_out_technology`.
#'
#' @seealso [market_share].
#'
#' @return A data-frame ready to be plotted using `plot_techmix()`.
#' @export
#'
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   scenario_source == "demo_2020" &
#'     sector == "power" &
#'     region == "global" &
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#' )
#'
#' prep_techmix(data)
prep_techmix <- function(data,
                         convert_label = identity,
                         span_5yr = FALSE,
                         convert_tech_label = identity) {
  env <- list(data = substitute(data))
  check_prep_techmix(
    data,
    convert_label = convert_label,
    convert_tech_label = convert_tech_label,
    span_5yr = span_5yr,
    env = env
  )

  out <- data %>%
    prep_common() %>%
    add_label_tech_if_missing() %>%
    mutate(
      value = .data$technology_share,
      sector = recode_sector(.data$sector),
      label = convert_label(.data$label),
      label_tech = convert_tech_label(.data$label_tech)
    )

  if (span_5yr) {
    out <- span_5yr(out)
  }

  start_year <- min(out$year, na.rm = TRUE)
  future_year <- max(out$year, na.rm = TRUE)
  if (!quiet()) {
    .data <- deparse_1(substitute(data, env = env))
    inform(glue(
      "The `technology_share` values are plotted for extreme years.
       Do you want to plot different years? E.g. filter {.data} with:\\
       `subset({.data}, year %in% c(2020, 2030))`."
    ))
  }
  out <- out %>%
    filter(.data$year %in% c(start_year, future_year)) %>%
    filter(!(is_scenario(.data$metric) & (.data$year == start_year)))
  out
}

recode_sector <- function(x) {
  # styler: off
  case_when(
    grepl("(?i)power(?-i)", x)             ~ "power",
    grepl("(?i)auto(?-i)[a-zA-Z]+", x)     ~ "automotive",
    grepl("(?i)oil(?-i).*(?i)gas(?-i)", x) ~ "oil&gas",
    grepl("(?i)fossil(?-i)[a-zA-Z]+", x)   ~ "fossil fuels",
    TRUE ~ tolower(x)
  )
  # styler: on
}

check_prep_techmix <- function(data, convert_label, convert_tech_label, span_5yr, env) {
  stopifnot(is.data.frame(data))
  stopifnot(is.function(convert_label))
  stopifnot(is.function(convert_tech_label))
  stopifnot(is.logical(span_5yr))

  crucial <- c(common_crucial_market_share_columns(), "technology_share")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")

  abort_if_has_zero_rows(data, env = env)

  abort_if_metric_has_no_projected(data)

  enforce_single_value <- c("sector", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_wrong_number_of_scenarios(data, env = env)

  invisible(data)
}
