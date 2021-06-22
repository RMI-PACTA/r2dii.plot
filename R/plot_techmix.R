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

  crucial <- c(common_crucial_market_share_columns(), "technology_share")
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share")

  abort_if_has_zero_rows(data, env = env)

  enforce_single_value <- c("sector", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)

  abort_if_multiple_scenarios(data, env = env)

  invisible(data)
}

abort_if_multiple_scenarios <- function(data, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  scenarios <- extract_scenarios(data$metric)
  n <- length(scenarios)

  if (n == 0L) {
    abort(c(
      glue("`{.data}$metric` must have one scenario."),
      x = "It has none."
    ))
  }

  if (n > 1L) {
    example <- c(setdiff(unique(data$metric), scenarios), first(scenarios))
    abort(c(
      glue("`{.data}$metric` must have a single scenario not {n}."),
      i = glue(
        "Do you need to pick one scenario? E.g. pick '{scenarios[[1]]}' with: \\
        `subset({.data}, metric %in% {fmt_vector(fmt_string(example))})`."
      ),
      x = glue("Provided: {toString(scenarios)}.")
    ))
  }

  invisible(data)
}

prep_techmix <- function(data) {
  data %>%
    common_prep() %>%
    filter(.data$year %in% c(min(.data$year), max(.data$year))) %>%
    # TODO: Should we make this in qplot_()?
    mutate(
      label = recode_metric(.data$label),
      label = to_title(.data$label),
      label = sub("target_", "", .data$label)
    ) %>%
    # TODO: Should we instead facet by year?
    mutate(label = paste0(.data$label, "_", .data$year)) %>%
    mutate(value = .data$technology_share, sector = recode_sector(.data$sector))
}

plot_techmix_impl <- function(data) {
  colours <- semi_join(technology_colours, data, by = c("sector", "technology"))
  labels <- rev(unique(data$label))

  ggplot() +
    geom_bar(
      data = data,
      aes(
        x = factor(.data$label, levels = labels),
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
    scale_x_discrete(labels = labels) +
    scale_fill_manual(
      labels = colours$label,
      values = colours$hex
    ) +
    coord_flip() +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE, reverse = TRUE)) +
    theme_2dii() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(legend.position = "bottom") +
    xlab("") +
    ylab("")
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

extract_scenarios <- function(x) {
  unique(x[startsWith(x, "target_")])
}
