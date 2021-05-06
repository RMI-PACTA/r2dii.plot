#' Create a techmix chart in a ggplot object
#'
#' The function returns a ggplot object containing a stacked bar chart showing a
#' technology mix for different categories (portfolio, scenario, benchmark,
#' etc.).
#'
#' @param data Filtered input data, with columns: technology, metric_type,
#'   metric and value.
#' @param metric_type_order Vector with the order of bars based on 'metric_type'
#'   values.
#' @param metric_type_labels Vector with labels for bars. Order must follow that
#'   in 'metric type order'.
#' @param tech_colours Dataframe containing colours per technology, with
#'   columns: technology, colour, label(optional).
#'
#' @export
#' @examples
#' data <- prepare_for_techmix_chart(
#'   process_input_data(get_example_data()),
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_to_plot = "technology_share"
#' )
#'   metric_type_order = c(
#'     "portfolio_2020",
#'     "benchmark_2020",
#'     "portfolio_2025",
#'     "benchmark_2025",
#'     "scenario_2025"
#'   )
#'   metric_type_label = c(
#'     "Portfolio 2020",
#'     "Benchmark 2020",
#'     "Portfolio 2025",
#'     "Benchmark 2025",
#'     "Target SDS 2025"
#'   )
#'
#' print(
#'   plot_techmix(
#'     data,
#'     metric_type_order = metric_type_order,
#'     metric_type_label = metric_type_label
#'   )
#' )
plot_techmix <- function(data,
                         metric_type_order = NULL,
                         metric_type_labels = NULL,
                         tech_colours = NULL) {

  metric_type_order <- metric_type_order %||% unique(data$metric_type)
  metric_type_labels <-
    metric_type_labels %||% guess_label_metric_type(metric_type_order)

  sector <- data %>%
    slice_head(n = 1) %>%
    pull(.data$sector)

  if (is.null(tech_colours)) {
    tech_colours <- get_r2dii_technology_colours(sector)
  }

  check_tech_colours(data, tech_colours)

  if (!("label" %in% names(tech_colours))) {
    tech_colours <- tech_colours %>%
      mutate(label = guess_label_tech(.data$technology))
  }

  data_colours <- dplyr::semi_join(tech_colours, data, by = "technology")

  data <- data %>%
    filter(.data$metric_type %in% metric_type_order)

  p_techmix <- ggplot() +
    theme_2dii_ggplot() +
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
      values = data_colours$colour
    ) +
    coord_flip() +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.y = element_blank())

    p_techmix <- p_techmix +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(ncol = 3, byrow = TRUE))

  p_techmix
}

check_tech_colours <- function(data, tech_colours) {

  if (!all(c("technology", "colour") %in% names(tech_colours))) {
    msg <- sprintf(
      "'tech_colours' must have columns 'technology' and 'colour'.
      * The columns in 'tech_colours' given are: %s.",
      toString(names(tech_colours))
    )
    stop(msg, call. = FALSE)
  }

  if (!all(unique(data$technology) %in% unique(tech_colours$technology))) {
    msg <- sprintf(
      "All technologies in input data must have a colour in 'tech_colours'.
      * The 'technology' in data missing from tech_colours are: %s.
      Note: if not given by the user, 'tech_colours' are specified internally based on 'sector'",
      toString(setdiff(unique(data$technology), unique(tech_colours$technology)))
    )
    stop(msg, call. = FALSE)
  }

  invisible(data)
}

guess_label_tech <- function(string) {
  string <- stringr::str_to_title(string)
  string <- stringr::str_replace(string, "cap$", " Capacity")
}

guess_label_metric_type <- function(string) {
  string <- stringr::str_to_title(string)
  string <- stringr::str_replace(string, "_", " ")
}
