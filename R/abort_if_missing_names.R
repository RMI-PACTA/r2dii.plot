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
      glue("Must have missing names:
      {toString(missing_names)}")
    )
  }

  invisible(x)
}
