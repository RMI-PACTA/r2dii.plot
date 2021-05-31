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

#' Performs the initial processing on raw input data in banks' format
#'
#' @param data Raw input data in the format of banks' output.
#'
#' @description This function processes the data in banks' format so that it can
#'   be used later in data filtering functions for charts.
#'
#' @return A dataframe with additional column `metric_type` and modified
#'   `metric`.
#'
#' @examples
#' process_input_data(market_share)
#' @keywords internal
#' @noRd
process_input_data <- function(data) {
  data %>%
    add_metric_type() %>%
    mutate(metric = sub("target_", "", .data$metric))
}

add_metric_type <- function(data) {
  check_crucial_names(data, "metric")

  mutate(
    data,
    metric_type = case_when(
      .data$metric == "projected" ~ "portfolio",
      startsWith(.data$metric, "target") ~ "scenario",
      TRUE ~ "benchmark"
    )
  )
}
