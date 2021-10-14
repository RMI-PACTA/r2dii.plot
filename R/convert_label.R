#' Functions to replicate labels produced with `qplot_*()` functions
#'
#' * `to_title()` converts a string to title case. It's similar to but not
#' identical to [tools::toTitleCase()]. It replaces a sequence of non
#' alpha-numeric characters to a single space, and applies title case to the
#' remaining words.
#' * `spell_out_technology()` replaces technology abbreviations with their
#' spelled-out version.
#' * `format_label_techmix()` replaces "projected" with "portfolio",
#' "targets*" (e.g. "target_sds") with "Scenario", and everything else with
#' "Benchmark".
#'
#' @param x A character vector.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' to_title(c("a.string", "ANOTHER_string"))
#' to_title(c("a.string", "another_string", "b.STRING"))
#'
#' spell_out_technology(c("FIXME_hdv", "FIXMEcap", "FIXMEice"))
#'
#' format_label_techmix(c("projected", "target_xyz", "whatever"))
#' # Weird case
#' format_label_techmix(c("corporate_._economy", "target_sds_abc"))
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
spell_out_technology <- function(x) {
  label <- to_title(x)
  label <- sub("^(?i)ice", "ICE", label)
  label <- sub("cap$", " Capacity", label)
  label <- sub("_hdv$", "Heavy Duty Vehicles", label)
  label
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
