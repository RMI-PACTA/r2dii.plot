#' Replicate labels produced with `qplot_*()` functions
#'
#' * `to_title()` converts labels like [qplot_emission_intensity()].
#' * `recode_metric_trajectory()` converts labels like [qplot_trajectory()].
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
#' recode_metric_trajectory(metric)
#'
#' recode_metric_techmix(metric)
#'
#' spell_out_technology(c("gas", "ice", "coalcap", "hdv"))
to_title <- function(x) {
  to_title_one <- function(x) {
    words <- unlist(strsplit(x, "[^[:alnum:]]+"))
    words <- tolower_unless_all_uppercase(words)
    # `toTitleCase()` with "a" returns "a", not "A" (a bug in this context)
    words <- capitalize_single_letters(tools::toTitleCase(words))
    paste(words, collapse = " ")
  }

  x_fctr <- factor(x)
  levels(x_fctr) <- vapply(levels(x_fctr), to_title_one, character(1))
  as.character(x_fctr)
}

tolower_unless_all_uppercase <- function(x) {
  out <- if_else(stringr::str_count(x, "[A-Z]") < length(x), tolower(x), x)
  out
}

capitalize_single_letters <- function(words) {
  out <- words
  out[which(nchar(out) == 1L)] <- toupper(out[which(nchar(out) == 1L)])
  out
}

#' @rdname to_title
#' @export
recode_metric_techmix <- function(x) {
  out <- case_when(
    tolower(x) == "projected" ~ "portfolio",
    startsWith(tolower(x), "target") ~ recode_scenario(x),
    TRUE ~ "benchmark"
  )
  to_title(out)
}

#' @rdname to_title
#' @export
recode_metric_trajectory <- function(x) {
  out <- case_when(
    tolower(x) == "projected" ~ "portfolio",
    startsWith(tolower(x), "target") ~ recode_scenario(x),
    TRUE ~ x
  )
  out <- sub("scenario", "", out)
  out <- trimws(out)
  to_title(out)
}

recode_scenario <- function(x) {
  out <- sub("target_", "", x)
  out <- toupper(out)
  out <- paste(out, "scenario", sep = " ")
  out
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
