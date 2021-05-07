#' Creates the default specs data frame for [plot_timeline()]
#'
#' @inheritParams plot_timeline
#'
#' @seealso plot_timeline
#' @return
#' @export
#' @examples
#' data <- prepare_for_timeline(sda_target)
#' timeline_specs(data)
timeline_specs <- function(data) {
  check_crucial_names(data, "line_name")

  to_title <- function(x) {
    paste(tools::toTitleCase(unlist(strsplit(x, "_"))), collapse = " ")
  }

  line_names <- unique(data$line_name)
  labels <- unlist(lapply(line_names, to_title))
  tibble(line_name = line_names, label = labels) %>%
    stop_if_too_many_lines() %>%
    add_r2dii_colours()
}

stop_if_too_many_lines <- function(data) {
  n_lines <- nrow(data)
  n_colours <- nrow(r2dii_palette_colours())
  if (n_lines > n_colours) {
    msg <- sprintf(
        "Can't plot more than %s lines. Found %s lines:\n%s",
        n_colours,
        n_lines,
        toString(data$line_name)
      )
    rlang::abort(class = "too_many_lines", msg)
  }

  invisible(data)
}
