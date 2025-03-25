#' Create a techmix plot
#'
#' @param data A data frame like the output of `prep_techmix()`.
#'
#' @seealso [market_share_demo].
#'
#' @return An object of class "ggplot".
#'
#' @export
#' @examples
#' # plot with `qplot_techmix()` parameters
#' data <- subset(
#'   market_share_demo,
#'   scenario_source == "demo_2020" &
#'     sector == "power" &
#'     region == "global" &
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#' )
#' data <- prep_techmix(
#'   data,
#'   span_5yr = TRUE,
#'   convert_label = recode_metric_techmix,
#'   convert_tech_label = spell_out_technology
#' )
#'
#' plot_techmix(data)
plot_techmix <- function(data) {
  env <- list(data = substitute(data))
  check_plot_techmix(data, env = env)
  colours <- get_technology_colours(data)
  labels <- techmix_labels(data)

  data <- data %>%
    group_by(.data$year) %>%
    mutate(
      n_datapoints = n()
    ) %>%
    ungroup() %>%
    mutate(
      scaling_factor = .data$n_datapoints / max(.data$n_datapoints)
    )

  ggplot(
    data = data,
    aes(
      x = factor(.data$label, levels = labels),
      y = .data$value,
      fill = factor(.data$technology, levels = colours$technology),
      width = .data$scaling_factor * 0.6
    )
  ) +
    geom_bar(
      position = "fill",
      stat = "identity"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = c(0, 0),
      sec.axis = dup_axis()
    ) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(
      labels = colours$label_tech,
      values = colours$hex
    ) +
    coord_flip() +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE, reverse = TRUE)) +
    theme_2dii() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(legend.position = "bottom") +
    xlab("") +
    ylab("") +
    facet_wrap(~year, nrow = 2, strip.position = "right", scales = "free_y")
}

check_plot_techmix <- function(data, env) {
  stopifnot(is.data.frame(data))

  crucial <- c(
    common_crucial_market_share_columns(),
    "technology_share",
    "label",
    "label_tech"
  )
  hint_if_missing_names(abort_if_missing_names(data, crucial), "market_share_demo")

  abort_if_has_zero_rows(data, env = env)

  enforce_single_value <- c("sector", "region", "scenario_source")
  abort_if_multiple(data, enforce_single_value, env = env)
  abort_if_wrong_number_of_scenarios(data, env = env)

  invisible(data)
}

abort_if_wrong_number_of_scenarios <- function(data, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  scen <- extract_scenarios(data$metric)
  n <- length(scen)

  if (n == 0L) {
    abort(c(
      glue("`{.data}$metric` must have one scenario."),
      x = "It has none."
    ))
  }

  if (n > 1L) {
    example <- c(setdiff(unique(data$metric), scen), first(scen))
    abort(c(
      glue("`{.data}$metric` must have a single scenario not {n}."),
      i = glue(
        "Do you need to pick one scenario? E.g. pick '{first(scen)}' with: \\
        `subset({.data}, metric %in% {fmt_vector(fmt_string(example))})`."
      ),
      x = glue("Provided: {toString(scen)}.")
    ))
  }

  invisible(data)
}

techmix_labels <- function(data) {
  metrics_other <- data %>%
    filter(
      .data$metric != "projected",
      !is_scenario(.data$metric)
    ) %>%
    pull(.data$metric) %>%
    unique()
  scenario <- data %>%
    filter(is_scenario(.data$metric)) %>%
    pull(.data$metric) %>%
    unique()
  metrics_order <- c("projected", metrics_other, scenario)

  labels <- data %>%
    arrange(factor(.data$metric, levels = metrics_order)) %>%
    pull(.data$label) %>%
    unique() %>%
    rev()

  names(labels) <- rev(metrics_order)
  labels
}

get_technology_colours <- function(data) {
  colours <- semi_join(technology_colours, data, by = c("sector", "technology")) %>%
    left_join(
      data %>%
        select("technology", "label_tech") %>%
        unique(),
      by = "technology"
    )
}

add_label_tech_if_missing <- function(data) {
  if (has_name(data, "label_tech")) {
    return(data)
  }

  data$label_tech <- data$technology
  data
}
