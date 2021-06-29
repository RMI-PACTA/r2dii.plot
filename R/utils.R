#' Convert a string to title case
#'
#' This function replaces a sequence of non alpha-numeric characters to a single
#' space, and applies title case to the remaining words.
#'
#' @param x A character vector.
#'
#' @return A character vector.
#' @keywords internal
#' @examples
#' to_title(c("a.string", "ANOTHER_string"))
#' to_title(c("a.string", "another_string", "b.STRING"))
#' @noRd
to_title <- function(x) {
  to_title_one <- function(x) {
    words <- tolower(unlist(strsplit(x, "[^[:alnum:]]+")))
    # `toTitleCase()` with "a" returns "a", not "A" (a bug in this context)
    words <- capitalize_single_letters(tools::toTitleCase(words))
    paste(words, collapse = " ")
  }

  unlist(lapply(x, to_title_one))
}

capitalize_single_letters <- function(words) {
  out <- words
  out[which(nchar(out) == 1L)] <- toupper(out[which(nchar(out) == 1L)])
  out
}

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

abort_if_has_zero_rows <- function(data, env = parent.frame()) {
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
  rlang::with_handlers(
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
    abort(c(
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
  filter(market_share, .data$technology == first(.data$technology), ...)
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

get_common_start_year <- function(data) {
  data %>%
    group_by(.data[[metric(data)]]) %>%
    summarise(year = min(.data$year)) %>%
    pull(.data$year) %>%
    max()
}

#' The name of the column holding metrics such as projected, corporate_economy
#'
#' @examples
#' metric(sda)
#' metric(market_share)
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
#' extract_names(sda, metric_names())
#' extract_names(market_share, metric_names())
#' extract_names(mtcars, c("mpg", "bad", "disp"))
#' @noRd
extract_names <- function(data, possible_names) {
  doit_once <- function(x) grep(x, names(data), value = TRUE)

  x <- anchor(possible_names)
  unlist(lapply(x, doit_once))
}

anchor <- function(x) paste0("^", x, "$")

drop_before_start_year <- function(data) {
  start_year <- get_common_start_year(data)
  if (!min(data$year) < start_year) {
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

recode_metric <- function(x) {
  case_when(
    x == "projected" ~ "portfolio",
    startsWith(x, "target") ~ "scenario",
    TRUE ~ "benchmark"
  )
}

# PACTA results are conventionally shown over a time period of 5 years
span_5yr <- function(data) {
  min_year <- get_common_start_year(data)
  filter(data, .data$year <= min_year + 5L)
}

spell_out_technology <- function(technology) {
  label <- to_title(technology)
  label <- sub("^(?i)ice", "ICE", label)
  label <- sub("cap$", " Capacity", label)
  label <- sub("_hdv$", "Heavy Duty Vehicles", label)
  label
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
    drop_before_start_year() %>%
    add_label_if_missing()
}


