`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

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

recode_metric_and_metric_type <- function(data, metric) {
  data %>%
    mutate(metric_type = recode_portfolio_benchmark_scenario(.data[[metric]])) %>%
    mutate(metric = sub("target_", "", .data[[metric]]))
}

recode_portfolio_benchmark_scenario <- function(x) {
  case_when(
    x == "projected" ~ "portfolio",
    startsWith(x, "target") ~ "scenario",
    TRUE ~ "benchmark"
  )
}

abort_if_invalid_length <- function(x, valid = 1L) {
  .x <- deparse_1(substitute(x))
  if (!length(x) == valid) {
    abort(
      class = "invalid_length",
      glue("`{.x}` must be of length {valid}, not {length(x)}.")
    )
  }

  invisible(x)
}

abort_if_multiple <- function(data, x, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  do_it_once <- function(x) {
    .x <- unique(data[[x]])
    if (length(.x) > 1L) {
      abort(glue(
        "`{.data}` must have a single value of `{x}` but has: {toString(.x)}.
        Pick one value, e.g. '{first(.x)}', with:
          dplyr::filter({.data}, {x} == '{first(.x)}')"
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

abort_if_has_cero_rows <- function(data) {
  .data <- deparse_1(substitute(data, env = parent.frame()))
  if (nrow(data) == 0L) {
    abort(glue("`{.data}` must have some rows but has none."))
  }

  invisible(data)
}

hint_if_missing_names <- function(expr) {
  .expr <- deparse_1(substitute(expr))
  fun <- format_plot_function_name(.expr)
  kind <- ifelse(grepl("timeline", fun), "sda", "market_share")

  rlang::with_handlers(
    expr,
    missing_names = function(e) {
      abort(class = "hint_missing_names",
        glue(
        "{conditionMessage(e)}
        Is your data `{kind}`-like?"
      ))
    }
  )

  invisible(expr)
}

format_plot_function_name <- function(.expr) {
  # "fun_name(...)" -> "fun_name"
  fun <- gsub("(.*)\\(.*", "\\1", .expr)
  # "fun_nameZ" -> "_name"
  fun <- gsub(".*_(.*)[A-Z]", "\\1", fun)
  fun <- glue("plot_{fun}")
  fun
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
abort_if_missing_names <- function(x, expected_names) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(expected_names))

  if (!all(unique(expected_names) %in% names(x))) {
    missing_names <- sort(setdiff(expected_names, names(x)))
    abort(
      class = "missing_names",
      glue(
        "Must have missing names:
        {toString(missing_names)}"
      )
    )
  }

  invisible(x)
}

fmt_string <- function(x) {
  toString(paste0("'", x, "'"))
}

fmt_vector <- function(x) {
  paste0("c(", x, ")")
}

expect_no_error <- function(...) {
  expect_error(..., NA)
}

example_market_share <- function(...) {
  filter(market_share, .data$technology == first(.data$technology), ...)
}

r_version_is_older_than <- function(major) {
  as.integer(R.version$major) < major
}
