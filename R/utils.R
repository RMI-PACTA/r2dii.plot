abort_if_multiple <- function(data, x, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  do_it_once <- function(x) {
    .x <- unique(data[[x]])
    if (length(.x) > 1L) {
      abort(c(
        glue("`{.data}` must have a single value of `{x}`."),
        i = glue(
          "Do you need to pick one value? E.g. pick '{first(.x)}' with: \\
          `subset({.data}, {x} == '{first(.x)}')`."
        ),
        x = glue("Provided: {toString(.x)}.")
      ))
    }
    invisible(x)
  }
  lapply(x, do_it_once)

  invisible(data)
}

# Backport `base::deparse1()` to R < 4.0.0
deparse_1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

abort_if_has_zero_rows <- function(data, env) {
  .data <- deparse_1(substitute(data, env = env))
  if (nrow(data) == 0L) {
    abort(c(
      glue("`{.data}` must have some rows."),
      x = glue("`{.data}` has zero rows.")
    ))
  }

  invisible(data)
}

hint_if_missing_names <- function(expr, like) {
  withCallingHandlers(
    expr,
    missing_names = function(err) {
      abort(
        c(conditionMessage(err), i = glue("Is your data `{like}`-like?")),
        class = "hint_missing_names",
        parent = err
      )
    }
  )
}

common_crucial_market_share_columns <- function() {
  c(
    "metric",
    "region",
    "scenario_source",
    "sector",
    "technology",
    "year"
  )
}

#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::abort_if_missing_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' abort_if_missing_names(x, "a")
#' try(abort_if_missing_names(x, "bad"))
#' @noRd
abort_if_missing_names <- function(data, expected_names) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_names))

  if (!all(unique(expected_names) %in% names(data))) {
    missing_names <- sort(setdiff(expected_names, names(data)))
    abort(
      c(
        "`data` must have all the expected names.",
        x = glue("Missing names: {toString(missing_names)}.")
      ),
      class = "missing_names"
    )
  }

  invisible(data)
}

fmt_string <- function(x) {
  toString(paste0("'", x, "'"))
}

fmt_vector <- function(x) {
  paste0("c(", x, ")")
}

example_market_share <- function(...) {
  filter(market_share_demo, .data$technology == first(.data$technology), ...)
}

example_tech_mix <- function(...) {
  out <- filter(market_share_demo, .data$sector == first(.data$sector), ...)

  filter(out, .data$metric %in% c("projected", "target_sds", "corporate_economy"))
}

r_version_is_older_than <- function(major) {
  as.integer(R.version$major) < major
}

#' The metric to plot most saliently
#'
#' The concept of "main line" is not obvious from the literal "projected" and
#' we reuse it in multiple places, so it seems worth to capture the concept
#' in this function's name.
#' @examples
#' main_line()
#' @noRd
main_line <- function() "projected"

#' r2dii.plot options
#'
#' * `r2dii.plot.quiet`: `TRUE` suppresses user-facing messages.
#'
#' @noRd
quiet <- function() getOption("r2dii.plot.quiet") %||% FALSE

get_projected_start_year <- function(data) {
  data %>%
    filter(
      !!(as.name(metric(data))) == main_line()
    ) %>%
    pull(.data$year) %>%
    min()
}

#' The name of the column holding metrics such as projected, corporate_economy
#'
#' @examples
#' metric(sda_demo)
#' metric(market_share_demo)
#' @noRd
metric <- function(data) {
  extract_names(data, metric_names())
}

#' Names of columns holding metrics such as projected, corporate_economy
#'
#' The column holding metrics such as "projected" and "corporate_economy" may
#' have a different name in different datasets. This function outputs all the
#' possible names. Eventually the difference may disappear (r2dii.analysis#313)
#' and this function should help make the transition smooth.
#'
#' @examples
#' metric_names()
#' @noRd
metric_names <- function() c("metric", "emission_factor_metric")

#' Extract names matching `possible_names`
#'
#' @examples
#' extract_names(sda_demo, metric_names())
#' extract_names(market_share_demo, metric_names())
#' extract_names(mtcars, c("mpg", "bad", "disp"))
#' @noRd
extract_names <- function(data, possible_names) {
  doit_once <- function(x) grep(x, names(data), value = TRUE)

  x <- anchor(possible_names)
  unlist(lapply(x, doit_once))
}

anchor <- function(x) paste0("^", x, "$")

drop_before_projected_start_year <- function(data) {
  start_year <- get_projected_start_year(data)
  if (!min(data$year, na.rm = TRUE) < start_year) {
    return(data)
  }

  if (!quiet()) {
    inform(glue(
      "Removing data before {start_year} -- the start year of 'projected'."
    ))
  }
  filter(data, .data$year >= start_year)
}

abort_if_too_many_lines <- function(data, max) {
  metrics <- unique(data[[metric(data)]])
  n <- length(metrics)
  if (n > max) {
    abort(
      # c(glue("Can't plot more than {max} lines in one plot."),
      c(glue("The number of lines to plot must be {max} or less."),
        i = "Do you need to split the data over multiple plots?",
        x = glue("Found {n} lines: {toString(metrics)}.")
      )
    )
  }

  invisible(data)
}

is_scenario <- function(x) grepl("^target", x, ignore.case = TRUE)
is_portfolio <- function(x) grepl("^projected", x, ignore.case = TRUE)

beautify <- function(data, x) {
  mutate(data, "{x}" := to_title(.data[[x]]))
}

# PACTA results are conventionally shown over a time period of 5 years
span_5yr <- function(data) {
  min_year <- get_projected_start_year(data)
  filter(data, .data$year <= min_year + 5L)
}

add_label_if_missing <- function(data) {
  if (has_name(data, "label")) {
    return(data)
  }

  data$label <- data[[metric(data)]]
  data
}

#' A place to DRY common preparation steps
#' @noRd
prep_common <- function(data) {
  data %>%
    drop_before_projected_start_year() %>%
    add_label_if_missing() %>%
    factor_label_if_factored_metric()
}

factor_label_if_factored_metric <- function(data) {
  if (is.factor(data[[metric(data)]])) {
    arranged_factors <- arrange(
      unique(data[c(metric(data), "label")])
    )

    data$label <- factor(
      data$label,
      levels = arranged_factors$label,
      ordered = TRUE
    )
  }

  data
}

abort_if_unknown_values <- function(value, data, column) {
  if (is.null(value)) {
    return(invisible(value))
  }

  .value <- deparse_1(substitute(value))
  .data <- deparse_1(substitute(data))
  .column <- deparse_1(substitute(column))

  valid <- unique(data[[column]])
  if (!all(value %in% valid)) {
    msg <- c(
      glue("Each value of `{.value}` must be one of these:\n{toString(valid)}."),
      x = glue("You passed: {toString(value)}."),
      i = glue("Do you need to see valid values in this dataset?:\n{.data}")
    )
    abort(msg, class = "unknown_value")
  }

  invisible(value)
}

r2dii_pal_impl <- function(x, data, column) {
  x <- x %||% data[[column]]
  values <- as_tibble_col(x, column) %>%
    inner_join(data, by = column) %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

# source: https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

api_warning_details <- function(prep_fn_name, plot_fn_name) {
  c(
    glue("From the next release you will need to call `r2dii.plot::{prep_fn_name}(data)`
             prior to calling `r2dii.plot::{plot_fn_name}()`."),
    "Alternatively custom data preparation will also become possible."
  )
}

get_ordered_scenarios <- function(data) {
  ordered_scenarios <- data %>%
    filter(is_scenario(.data$metric), .data$year == max(data$year)) %>%
    arrange(desc(.data$value)) %>%
    pull(.data$metric) %>%
    as.character()

  ordered_scenarios
}

extract_scenarios <- function(x) {
  unique(x[startsWith(x, "target_")])
}

abort_if_metric_has_no_projected <- function(data) {
  if (!any(data[["metric"]] %in% "projected")) {
    abort(
      message = c(
        "The column `metric` has no value 'projected' .",
        i = "Did you accidentally filter out the 'projected' values?"
      ),
      class = "no_projected"
    )
  }

  invisible(data)
}
