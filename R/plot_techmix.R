#' Create a techmix plot
#'
#' @param data A data frame. Requirements:
#'   * The structure must be like [market_share].
#'   * The following columns must have a single value: `sector`, `region`,
#'   `scenario_source`.
#'   * The column `metric` must have a portfolio (e.g. "projected"), a benchmark
#'   (e.g. "corporate_economy"), and a single `scenario` (e.g. "target_sds").
#'
#' @seealso [market_share].
#'
#' @return An object of class "ggplot".
#'
#' @export
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
#' plot_techmix(data)
plot_techmix <- function(data) {
  check_plot_techmix(data)

  prep <- prep_techmix(data)
  plot_techmix_impl(prep)
}

check_plot_techmix <- function(data, env = parent.frame()) {
  stopifnot(is.data.frame(data))
  hint_missing_names_with_hint(
    abort_if_missing_names(
      data, c(common_crucial_market_share_columns(), "technology_share")
    )
  )
  abort_if_has_zero_rows(data, env = env)
  cols <- c("sector", "region", "scenario_source")
  abort_if_multiple(data, cols, env = env)
  abort_if_multiple_scenarios(data, env = env)
}

abort_if_multiple_scenarios <- function(data, env = parent.frame()) {
  abort_if_missing_names(data, "metric")
  .data <- deparse_1(substitute(data, env = env))

  scen <- extract_scenarios(data$metric)
  n <- length(scen)

  if (n == 0L) {
    abort(glue("`{.data}$metric` must have one scenario but has none."))
  }

  if (n > 1L) {
    example <- c(setdiff(unique(data$metric), scen), first(scen))
    abort(glue(
      "`{.data}$metric` must have a single scenario not {n}: {toString(scen)}.
        You may pick one scenario, e.g. '{first(scen)}' with:
          subset({.data}, metric %in% {fmt_vector(fmt_string(example))})"
    ))
  }

  invisible(data)
}

prep_techmix <- function(data) {
  data %>%
    drop_rows_before_sart_year(metric(data)) %>%
    filter(.data$year %in% c(min(.data$year), max(.data$year))) %>%
    mutate(
      metric_type = to_metric_type(.data[[metric(data)]]),
      metric_type = paste0(.data$metric_type, "_", .data$year),
      metric_type = to_title(.data$metric_type),
      metric = sub("target_", "", .data[[metric(data)]]),
      value = .data$technology_share,
      sector = guess_sector(.data$sector)
    )
}

plot_techmix_impl <- function(data) {
  colours <- semi_join(technology_colours, data, by = c("sector", "technology"))

  ggplot() +
    geom_bar(
      data = data,
      aes(
        x = factor(.data$metric_type, levels = rev(unique(.data$metric_type))),
        y = .data$value,
        fill = factor(.data$technology, levels = colours$technology)
      ),
      position = "fill",
      stat = "identity",
      width = .5
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = c(0, 0),
      sec.axis = dup_axis()
    ) +
    scale_x_discrete(labels = rev(unique(.data$metric_type))) +
    scale_fill_manual(
      labels = colours$label,
      values = colours$hex
    ) +
    coord_flip() +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
    theme_2dii() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(legend.position = "bottom") +
    xlab("") +
    ylab("")
}

guess_sector <- function(sector) {
  # styler: off
  case_when(
    grepl("(?i)power(?-i)", sector)             ~ "power",
    grepl("(?i)auto(?-i)[a-zA-Z]+", sector)     ~ "automotive",
    grepl("(?i)oil(?-i).*(?i)gas(?-i)", sector) ~ "oil&gas",
    grepl("(?i)fossil(?-i)[a-zA-Z]+", sector)   ~ "fossil fuels",
    TRUE ~ tolower(sector)
  )
  # styler: on
}

extract_scenarios <- function(x) {
  unique(x[startsWith(x, "target_")])
}
