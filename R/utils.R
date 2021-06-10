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

abort_bad_metric <- function(x) {
  has_projected <- "projected" %in% x
  if (!has_projected) abort("Can't find values to recode as 'portfolio'.")
  has_scenarios <- any(startsWith(x, "target"))
  if (!has_scenarios) abort("Can't find values to recode as scenarios.")

  invisible(x)
}

abort_if_invalid_length <- function(x, valid = 1L) {
  .x <- as.character(substitute(x))
  long <- !length(x) == valid
  if (long) {
    abort(
      class = "invalid_length",
      glue("`{.x}` must be of length {valid}, not {length(x)}.")
    )
  }

  invisible(x)
}

abort_if_multiple <- function(data, x) {
  .data <- deparse1(substitute(data, env = parent.frame()))

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
