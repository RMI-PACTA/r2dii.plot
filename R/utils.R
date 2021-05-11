`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

to_title <- function(x) {
  to_title_one <- function(x) {
    words <- unlist(strsplit(x, "[^[:alnum:]]+"))
    paste(tools::toTitleCase(words), collapse = " ")
  }

  unlist(lapply(x, to_title_one))
}
