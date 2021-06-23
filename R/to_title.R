#' Format legend labels
#'
#' @description
#' `format_label()` formats legend labels from the expected values of metrics in
#' `sda`-like and `market_share`-like datasets:
#' * Scenarios are stripped of the suffix "target_" and converted to upper case.
#' * Other metrics are converted to title case.
#' * For all metrics any sequence of non alpha-numeric characters is replaced
#' with a single space.
#' `recode_label()` recoding metric values and outputs them in title case.
#'
#' @param x A character vector.
#'
#' @family helpers
#'
#' @return A character vector.
#'
#' @export
#' @examples
#' format_label(c("corporate_economy", "target_sds"))
#' format_label(c("corporate_._economy", "target_sds_abc"))
format_label <- function(x) {
  out <- sub("target_", "", x)
  out <- to_title(out)
  if_else(is_scenario(x), toupper(out), out)
}

#' @rdname format_label
#' @export
#' @examples
#' recode_label(c("projected", "corporate_economy", "target_sds"))
recode_label <- function(x) {
  # TODO: Do we need to recode? Why not use the metric values as we do
  # in the other plots?
  # styler: off
  to_title(case_when(
    x == "projected"        ~ "portfolio",
    startsWith(x, "target") ~ "scenario",
    TRUE                    ~ "benchmark"
  ))
  # styler: on
}

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
