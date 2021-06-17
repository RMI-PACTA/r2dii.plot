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
  stopifnot(is.data.frame(data))
  hint_if_missing_names(
    abort_if_missing_names(
      data, c(common_crucial_market_share_columns(), "technology_share")
    )
  )
  abort_if_has_zero_rows(data)
  cols <- c("sector", "region", "scenario_source")
  abort_if_multiple(data, cols)
  abort_if_multiple_scenarios(data)

  prep <- prep_techmix(data)
  plot_techmix_impl(prep)
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

prep_techmix <- function(data, value = "technology_share", metric = "metric") {
  data %>%
    check_prep_techmix(value) %>%
    drop_before_start_year(metric) %>%
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

check_prep_techmix <- function(data, value) {
  crucial <- c(common_crucial_market_share_columns(), value)
  abort_if_missing_names(data, crucial)

  cols <- c("scenario_source", "sector", "region")
  lapply(cols, function(x) abort_if_multiple(data, x))

  invisible(data)
}

plot_techmix_impl <- function(data) {
  metric_type_order <- unique(data$metric_type)
  metric_type_labels <- to_title(metric_type_order)

  sector <- data %>%
    pull(.data$sector) %>%
    unique() %>%
    guess_sector()

  tech_colours <- technology_colours %>%
    filter(.data$sector == .env$sector) %>%
    select(.data$technology, .data$label, .data$hex)

  data_colours <- dplyr::semi_join(tech_colours, data, by = "technology")

  data <- data %>%
    filter(.data$metric_type %in% metric_type_order)

  p_techmix <- ggplot() +
    theme_2dii() +
    xlab("") +
    ylab("")

  p_techmix <- p_techmix +
    geom_bar(
      data = data,
      aes(
        x = factor(.data$metric_type, levels = rev(metric_type_order)),
        y = .data$value,
        fill = factor(.data$technology, levels = data_colours$technology)
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
    scale_x_discrete(labels = rev(metric_type_labels)) +
    scale_fill_manual(
      labels = data_colours$label,
      values = data_colours$hex
    ) +
    coord_flip() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank())

  p_techmix <- p_techmix +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))

  p_techmix
}

guess_sector <- function(sector) {
  sector <- case_when(
    grepl("(?i)power(?-i)", sector) ~ "power",
    grepl("(?i)auto(?-i)[a-zA-Z]+", sector) ~ "automotive",
    grepl("(?i)oil(?-i).*(?i)gas(?-i)", sector) ~ "oil&gas",
    grepl("(?i)fossil(?-i)[a-zA-Z]+", sector) ~ "fossil fuels",
    TRUE ~ tolower(sector)
  )
  sector
}

extract_scenarios <- function(x) {
  unique(x[startsWith(x, "target_")])
}
