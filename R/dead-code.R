#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     dplyr::between(year, 2020, 2025),
#'     scenario_source == "demo_2020",
#'     sector == "power",
#'     region == "global",
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#'   )
#'
#' prep <- prep_techmixB(data)
prep_techmixB <- function(data, value = "technology_share", metric = "metric") {
  check_prep_techmixB(data, value)

  data %>%
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

check_prep_techmixB <- function(data, value) {
  crucial <- c("metric", "year", "scenario_source", "region", value)
  abort_if_missing_names(data, crucial)

  cols <- c("scenario_source", "sector", "region")
  lapply(cols, function(x) abort_multiple(data, x))

  abort_bad_metric(data$metric)

  invisible(data)
}

#' @noRd
#' @examples
#' library(dplyr)
#'
#' data <- market_share %>%
#'   filter(
#'     technology == "oilcap",
#'     region == "global",
#'     scenario_source == "demo_2020",
#'     year <= 2025,
#'     sector == "power"
#'   )
#'
#' prep_trajectoryB(data)
prep_trajectoryB <- function(data,
                             value = "production",
                             metric = "metric",
                             normalize = TRUE) {
  check_prep_trajectoryB(data, value, normalize)
  data <- recode_metric_and_metric_type(data, metric)

  cols <- c("year", "metric_type", "metric", "technology", "value")
  out <- data %>%
    mutate(value = .data[[value]]) %>%
    select(all_of(cols))

  if (!normalize) {
    return(out)
  }

  left_join(
    out, filter(out, .data$year == min(.data$year)),
    by = c("metric_type", "metric")
  ) %>%
    mutate(
      value = .data$value.x / .data$value.y,
      year = .data$year.x,
      technology = .data$technology.x
    ) %>%
    select(all_of(cols))
}

check_prep_trajectoryB <- function(data, value, normalize) {
  crucial <- c(
    "metric", "sector", "technology", "region", "year", "scenario_source", value
  )
  abort_if_missing_names(data, crucial)

  abort_if_invalid_length(normalize)
  stopifnot(is.logical(normalize))

  cols <- c("sector", "technology", "region", "scenario_source")
  lapply(cols, function(x) abort_multiple(data, x))

  invisible(data)
}

#' @noRd
#' @description
#' * `plot_timelineB()` plots `line_name` "as is". You may recode `line_name`
#' before passing the `data` with, for example, [dplyr::recode()].
#'
#' @family plotting functions
#'
#' @export
#' @examples
#'
#' # `plot_timelineB()` ------------------------------------------------------
#'
#' data <- sda %>%
#'   filter(sector == "aviation") %>%
#'   prep_timeline()
#' plot_timelineB(data)
#'
#' # Recode `line_name` with `dplyr::recode()`
#' data %>%
#'   mutate(line_name = recode(line_name,
#'     "corporate_economy" = "Corp. economy",
#'     "projected" = "Proj.",
#'     "target_demo" = "Target (demo)",
#'     "adjusted_scenario_demo" = "Adj. Scenario (demo)",
#'   )) %>%
#'   plot_timelineB()
plot_timelineB <- function(data) {
  abort_if_missing_names(data, "line_name")

  line_names <- unique(data$line_name)
  labels <- line_names
  specs <- tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineA(data = data, specs = specs)
}

#' @noRd
#' @description
#' * `plot_timelineC()` defaults to plotting a title case version of `line_name`
#' values, and allows none or other recoding via the argument `recode` (see
#' Arguments).
#' @param recode One of the following:
#' * A function/lambda to apply to `data$line_name`.
#' * A named vector to recode `data$line_name`.
#' * A logical vector of length 1. `TRUE` recodes `data$line_name` to title
#' case. `FALSE` does no recoding and plots `data$line_name` as is.
#' @seealso [dplyr::recode()].
#' @export
#' @examples
#'
#' # `plot_timelineC()` ------------------------------------------------------
#'
#' data <- sda %>%
#'   filter(sector == "aviation") %>%
#'   prep_timeline()
#'
#' unique(data$line_name)
#'
#' # Recode to title case
#' plot_timelineC(data, recode = TRUE)
#'
#' # Don't recode
#' plot_timelineC(data, recode = FALSE)
#'
#' # Recode to title case
#' unique(data$line_name)
#' plot_timelineC(data)
#'
#' # Recode using a function
#' plot_timelineC(data, recode = toupper)
#'
#' # Recode using a formula giving a lambda function
#' plot_timelineC(data, recode = ~ toupper(gsub("_", " ", .x)))
#'
#' # Recode via a named vector
#' legend <- c(
#'   "projected" = "Proj.",
#'   "corporate_economy" = "Corp. Economy",
#'   "target_demo" = "Target (demo)",
#'   "adjusted_scenario_demo" = "Adj. Scenario (demo)"
#' )
#' plot_timelineC(data, recode = legend)
plot_timelineC <- function(data, recode = TRUE) {
  if (!is.null(recode)) data$line_name <- recode_lines(recode, data)
  plot_timelineB(data)
}

recode_lines <- function(recode, data) {
  UseMethod("recode_lines")
}
recode_lines.default <- function(recode, data) {
  abort(glue("Can't handle `recode` of class: {class(recode)}"))
}
recode_lines.function <- function(recode, data) {
  recode(data$line_name)
}
recode_lines.formula <- function(recode, data) {
  f <- rlang::as_function(recode)
  f(data$line_name)
}
recode_lines.character <- function(recode, data) {
  dplyr::recode(data$line_name, !!!recode)
}
recode_lines.logical <- function(recode, data) {
  out <- data$line_name
  if (recode) out <- to_title(data$line_name)
  out
}

#' @noRd
#' @description
#' * `plot_trajectoryB()` derives the main and additional lines as well as
#' scenario order from the data. The lines are plotted according to the order of
#' the input data. The scenario order is inferred from the order of values on
#' the last year. For the labels the `data` column `metric` is used. You may
#' recode `metric` before passing the data with, for example, `dplyr::recode()`.
#'
#' @param main_line String of length 1. The `metric` to plot as the line with
#'   the most visual salience (solid black line). `NULL` defaults to
#'   "projected".
#'
#' @family plotting functions
#'
#' @export
#' @examples
#'
#' # `plot_trajectoryB()` ------------------------------------------------------
#' library(dplyr)
#'
#' data <- prep_trajectory(
#'   market_share,
#'   sector_filter = "power",
#'   technology_filter = "renewablescap",
#'   region_filter = "global",
#'   scenario_source_filter = "demo_2020",
#'   value = "production"
#' )
#'
#' plot_trajectoryB(data, main_line = "projected")
#'
#' # You may recode `metric` with `dplyr::recode()`
#' recoded <- data %>%
#'   mutate(
#'     metric = recode(
#'       .data$metric,
#'       "projected" = "Projected",
#'       "corporate_economy" = "Corporate Economy",
#'       "sds" = "SDS",
#'       "sps" = "SPS",
#'       "cps" = "CPS"
#'     )
#'   )
#'
#' plot_trajectoryB(recoded, main_line = "Projected")
plot_trajectoryB <- function(data, main_line = NULL) {
  abort_if_invalid_scenarios_number(data)

  main_line <- main_line %||%
    (data %>%
      filter(.data$metric_type != "scenario") %>%
      slice_head(n = 1) %>%
      pull(.data$metric))
  abort_if_invalid_main_line(data, main_line)

  # plot scenario areas
  scenario_specs_areas <- get_ordered_scenario_specsB(data)
  data_scenarios <- get_scenario_data(data, scenario_specs_areas)
  p_trajectory <- ggplot() +
    geom_ribbon(
      data = data_scenarios,
      aes(
        x = .data$year,
        ymin = .data$value_low,
        ymax = .data$value,
        fill = .data$metric,
        alpha = 0.9
      )
    ) +
    scale_fill_manual(values = scenario_specs_areas$colour)

  # plot trajectory and scenario lines
  scenario_specs_lines <- scenario_specs_areas %>%
    filter(.data$scenario != "worse")
  data_lines <- order_for_trajectoryB(data, scenario_specs_lines, main_line)

  n_scenarios <- nrow(scenario_specs_lines)
  n_lines_traj <- length(unique(data_lines$metric)) - n_scenarios
  linetypes_trajectory <- c("solid", "dashed", "solid", "solid", "twodash")
  linecolours_trajectory <- c("black", "black", "gray", "grey46", "black")
  line_types <- c(linetypes_trajectory[1:n_lines_traj], rep("solid", n_scenarios))
  line_colours <- c(linecolours_trajectory[1:n_lines_traj], scenario_specs_lines$colour)

  p_trajectory <- p_trajectory +
    geom_line(
      data = data_lines,
      aes(
        x = .data$year,
        y = .data$value,
        linetype = .data$metric,
        color = .data$metric
      )
    )

  p_trajectory <- p_trajectory +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_linetype_manual(values = line_types) +
    scale_color_manual(values = line_colours)

  # annotate trajectory and scenario lines
  last_year <- max(data$year)
  value_span <- max(data_scenarios$value) - min(data_scenarios$value_low)

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type != "scenario"
        ),
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$metric
      ),
      direction = "y",
      size = 3.5,
      nudge_x = 0.15,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0,
      xlim = c(min(data$year), last_year + 3)
    )

  p_trajectory <- p_trajectory +
    ggrepel::geom_text_repel(
      data = data_lines %>%
        filter(
          .data$year == last_year,
          .data$metric_type == "scenario"
        ),
      aes(
        x = .data$year,
        y = .data$value,
        label = .data$metric,
        segment.color = .data$metric
      ),
      direction = "y",
      color = "black",
      size = 3.5,
      alpha = 1,
      nudge_x = 0.6,
      nudge_y = 0.01 * value_span,
      hjust = 0,
      segment.size = 0.3,
      xlim = c(min(data$year), last_year + 5)
    ) +
    scale_fill_manual(
      aesthetics = "segment.color",
      values = scenario_specs_lines$colour
    )

  p_trajectory <- p_trajectory +
    theme_2dii() +
    theme(
      axis.line = element_blank(),
      legend.position = "none"
    ) %+replace%
    theme(
      plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm")
    )

  p_trajectory
}

abort_if_invalid_scenarios_number <- function(data) {
  unique_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    pull(.data$metric) %>%
    unique()

  if (length(unique_scenarios) > 4) {
    rlang::abort(glue(
      "Scenario number for plotting must be between 1 and 4. \\
      You provided {nrow(scenario_specs)} scenarios in 'scenario_specs'."
    ))
  }

  invisible(data)
}

abort_if_invalid_main_line <- function(data, main_line) {
  abort_if_invalid_length(main_line)

  metrics <- unique(data$metric)
  if (!main_line %in% metrics) {
    rlang::abort(glue(
      "`main_line` must be one value of `data$metric`.
      * Valid values: {toString(metrics)}.
      * You provided: {toString(main_line)}."
    ))
  }

  invisible(data)
}

order_for_trajectoryB <- function(data, scenario_specs, main_line) {
  order_add_lines <- data %>%
    filter(
      .data$metric_type != "scenario",
      .data$metric != .env$main_line
    ) %>%
    pull(.data$metric) %>%
    unique() %>%
    as.character()

  order_scenarios <- scenario_specs$scenario

  data_ordered <- data %>%
    mutate(metric = factor(
      .data$metric,
      levels = c(main_line, order_add_lines, order_scenarios)
    )) %>%
    arrange(.data$year, .data$metric)

  data_ordered
}

get_area_borders <- function(data) {
  lower_area_border <- 0.9 * min(data$value)
  upper_area_border <- 1.1 * max(data$value)
  value_span <- upper_area_border - lower_area_border

  start_value_portfolio <- data %>%
    filter(.data$year == min(.data$year)) %>%
    filter(.data$metric_type == "portfolio") %>%
    pull(.data$value)

  perc_distance_upper_border <-
    (upper_area_border - start_value_portfolio) / value_span
  perc_distance_lower_border <-
    (start_value_portfolio - lower_area_border) / value_span

  # adjusting the area border to center the starting point of the lines
  max_delta_distance <- 0.1
  delta_distance <- abs(perc_distance_upper_border - perc_distance_lower_border)
  if (delta_distance > max_delta_distance) {
    if (perc_distance_upper_border > perc_distance_lower_border) {
      lower_area_border <-
        start_value_portfolio - perc_distance_upper_border * value_span
      value_span <- upper_area_border - lower_area_border
    } else {
      upper_area_border <-
        perc_distance_lower_border * value_span + start_value_portfolio
      value_span <- upper_area_border - lower_area_border
    }
  }

  area_borders <- list(lower = lower_area_border, upper = upper_area_border)
  area_borders
}

get_ordered_scenario_specsB <- function(data) {
  ordered_scenarios <- data %>%
    filter(.data$metric_type == "scenario") %>%
    filter(.data$year == max(.data$year)) %>%
    arrange(desc(.data$value)) %>%
    pull(.data$metric) %>%
    as.character()
  num_scen_areas <- length(ordered_scenarios) + 1
  scenario_colours <- get_ordered_scenario_colours(num_scen_areas)

  green_or_brown <- r2dii.data::green_or_brown
  tech_green_or_brown <- green_or_brown %>%
    filter(.data$technology == data$technology[1]) %>%
    pull(.data$green_or_brown)

  if (tech_green_or_brown == "brown") {
    ordered_scenarios_good_to_bad <- tibble(
      scenario = rev(c("worse", ordered_scenarios)),
      colour = scenario_colours$hex
    )
    scenario_specs <- ordered_scenarios_good_to_bad
  } else if (tech_green_or_brown == "green") {
    ordered_scenarios_good_to_bad <- tibble(
      scenario = c(ordered_scenarios, c("worse")),
      colour = scenario_colours$hex
    )
    scenario_specs <- reverse_rows(ordered_scenarios_good_to_bad)
  }
  scenario_specs
}

reverse_rows <- function(x) {
  x[sort(rownames(x), decreasing = TRUE), , drop = FALSE]
}

get_ordered_scenario_colours <- function(n) {
  pick <- function(cols) filter(scenario_colours, .data$label %in% cols)
  switch(as.character(n),
    "2" = pick(c("light_green", "red")),
    "3" = pick(c("light_green", "light_yellow", "red")),
    "4" = pick(c("light_green", "dark_yellow", "light_yellow", "red")),
    "5" = assert_5_rows(scenario_colours)
  )
}

assert_5_rows <- function(data) {
  stopifnot(nrow(data) == 5L)
  invisible(data)
}
