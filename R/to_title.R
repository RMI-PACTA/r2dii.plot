#' Replicate labels produced with `qplot_*()` functions
#'
#' * `to_title()` converts labels like [qplot_emission_intensity()].
#' * `format_metric()` converts labels like [qplot_trajectory()].
#' * `recode_metric_techmix()` converts labels like [qplot_techmix()].
#' * `spell_out_technology()` converts technology labels like [qplot_techmix()].
#'
#' @param x A character vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' to_title(c("a.string", "another_STRING"))
#'
#' metric <- c("projected", "corporate_economy", "target_xyz", "else")
#' format_metric(metric)
#'
#' recode_metric_techmix(metric)
#'
#' spell_out_technology(c("gas", "ice", "coalcap", "hdv"))
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
format_metric <- function(x) {
  out <- sub("target_", "", x)
  out <- to_title(out)
  if_else(is_scenario(x), toupper(out), out)
}

#' @rdname to_title
#' @export
recode_metric_techmix <- function(x) {
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
  label <- sub("Hdv$", "Heavy Duty Vehicles", label)
  label
}
