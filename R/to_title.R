#' Functions to replicate labels produced with `qplot_*()` functions
#'
#' * `to_title()` converts labels like [qplot_emission_intensity()].
#' * `format_label()` converts labels like [qplot_trajectory()].
#' * `format_label_techmix()` converts labels like [qplot_techmix()].
#' * `spell_out_technology()` converts technology labels like [qplot_techmix()].
#'
#' @param x A character vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' to_title(c("a.string", "another_string", "b.STRING"))
#'
#' format_label(c("projected", "target_xyz", "whatever"))
#'
#' format_label_techmix(c("projected", "target_xyz", "whatever"))
#'
#' spell_out_technology(c("gas", "coalcap"))
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

#' @rdname to_title
#' @export
format_label <- function(x) {
  out <- sub("target_", "", x)
  out <- to_title(out)
  if_else(is_scenario(x), toupper(out), out)
}

#' @rdname to_title
#' @export
format_label_techmix <- function(x) {
  out <- recode_metric(x)
  out <- to_title(out)
  out
}

recode_metric <- function(x) {
  case_when(
    x == "projected" ~ "portfolio",
    startsWith(x, "target") ~ "scenario",
    TRUE ~ "benchmark"
  )
}

#' @rdname to_title
#' @export
spell_out_technology <- function(x) {
  label <- to_title(x)
  label <- sub("^(?i)ice", "ICE", label)
  label <- sub("cap$", " Capacity", label)
  label <- sub("_hdv$", "Heavy Duty Vehicles", label)
  label
}
