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
#' data <- prep_techmix(
#'   market_share,
#'   sector_filter = "power",
#'   years_filter = c(2020, 2025),
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   scenario_filter = "sds",
#'   value_to_plot = "technology_share"
#' )
#' print(
#'   plot_techmix(data)
#' )
plot_techmix <- function(data,
                         metric_type_order = NULL,
                         metric_type_labels = NULL,
                         tech_colours = NULL) {
  metric_type_order <- metric_type_order %||% unique(data$metric_type)
  metric_type_labels <-
    metric_type_labels %||% to_title(metric_type_order)

  sector <- data %>%
    pull(.data$sector) %>%
    unique() %>%
    guess_sector()

  check_input_parameters_plot_techmix(
    data,
    metric_type_order,
    metric_type_labels,
    sector,
    tech_colours
  )

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

check_input_parameters_plot_techmix <- function(data,
                                                metric_type_order,
                                                metric_type_labels,
                                                sector,
                                                tech_colours) {
  if (!all(metric_type_order %in% unique(data$metric_type))) {
    abort(glue(
      "'metric_type_order' elements must be found in 'metric_type' column of input data.
      * Possible 'metric_type' in data are: {toString(unique(data$metric_type))}.
      * You submitted: {toString(metric_type_order)}."
    ))
  }

  if (length(metric_type_order) != length(metric_type_labels)) {
    abort(glue(
      "'metric_type_labels' must be of the same length (and order) as 'metric_type_order'.
      * 'metric_type_order' has length {length(metric_type_order)} and elements: {toString(metric_type_order)}.
      * You submitted 'metric_type_labels' of legth {length(metric_type_labels)} and elements: {toString(metric_type_labels)}."
    ))
  }

  if (length(sector) > 1) {
    abort(glue(
      "Input data must have only one 'sector'.
      * You submitted data with {length(sector)} sectors: {toString(sector)}."
    ))
  }

  if (!(sector %in% c("power", "automotive", "oil&gas", "fossil fuels"))) {
    if (is.null(tech_colours)) {
      abort(glue(
        "Input data 'sector' not found in standard chart sectors.
        * Standard sectors are: power, automotive, oil&gas, fossil fuels.
        * You submitted data with sector: {sector}.
        Please use data from a known sector or specify technology colours in 'tech_colours' parameters."
      ))
    }
  }
}

check_tech_colours <- function(data, tech_colours) {
  if (!is.data.frame(tech_colours)) {
    abort(glue(
      "'tech_colours' must be a dataframe.
      * You've supplied a {typeof(tech_colours)}."
    ))
  }

  if (!all(c("technology", "colour") %in% names(tech_colours))) {
    abort(glue(
      "'tech_colours' must have columns 'technology' and 'colour'.
      * The columns in 'tech_colours' given are: {toString(names(tech_colours))}."
    ))
  }

  if (!all(unique(data$technology) %in% unique(tech_colours$technology))) {
    abort(glue(
      "All technologies in input data must have a colour in 'tech_colours'.
      * The 'technology' in data missing from tech_colours are: {toString(setdiff(unique(data$technology), unique(tech_colours$technology)))}.
      Note: if not given by the user, 'tech_colours' are specified internally based on 'sector'"
    ))
  }

  invisible(data)
}

guess_label_tech <- function(string) {
  string <- to_title(string)
  string <- gsub("cap$", " Capacity", string)
  string
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
