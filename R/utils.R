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

abort_if_bad_metric <- function(x) {
  has_projected <- "projected" %in% x
  if (!has_projected) abort("Can't find values to recode as 'portfolio'.")
  has_scenarios <- any(startsWith(x, "target"))
  if (!has_scenarios) abort("Can't find values to recode as scenarios.")

  invisible(x)
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

abort_if_multiple <- function(data, x) {
  # FIXME: Pretty labels must work beyond the top level. May need to pass `env`
  .data <- deparse_1(substitute(data, env = parent.frame()))

  .x <- unique(data[[x]])
  if (length(.x) > 1L) {
    abort(glue(
      "`{.data}` must have a single value of `{x}` but has: {toString(.x)}.
      Pick one value, e.g. '{first(.x)}', with:
        dplyr::filter({.data}, {x} == '{first(.x)}')"
    ))
  }

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
      abort(glue(
        "{conditionMessage(e)}
        Is your data `{kind}`-like?"
      ))
    }
  )

  invisible(expr)
}

format_plot_function_name <- function(.expr) {
  # Matches "fun_name" from "fun_name(...)"
  fun <- gsub("(.*)\\(.*", "\\1", .expr)
  # Matches "_name" from "fun_nameZ"
  fun <- gsub(".*_(.*)[A-Z]", "\\1", fun)
  fun <- glue("plot_{fun}")
}

some_crucial_market_share_columns <- function() {
  c(
    "metric",
    "region",
    "scenario_source",
    "sector",
    "technology",
    "year"
  )

}
