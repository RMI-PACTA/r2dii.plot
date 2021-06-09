#' Creates the default specs data frame for `plot_timelineY()`
#'
#' @inheritParams plot_timelineY
#'
#' @seealso plot_timelineA
#'
#' @return A data frame useful as the `specs` argument of [plot_timelineY()].
#'
#' @export
#' @examples
#' data <- prep_timelineY(sda, sector_filter = "cement")
#' timeline_specs(data)
timeline_specs <- function(data) {
  abort_if_missing_names(data, "line_name")

  to_title <- function(x) {
    paste(tools::toTitleCase(unlist(strsplit(x, "_"))), collapse = " ")
  }

  line_names <- unique(data$line_name)
  labels <- unlist(lapply(line_names, to_title))
  tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()
}

abort_if_too_many_lines <- function(data) {
  n_lines <- nrow(data)
  n_colours <- nrow(palette_colours)
  if (n_lines > n_colours) {
    abort(glue(
      "Can't plot more than {n_colours} lines. Found {n_lines} lines:
        {toString(data$line_name)}."
    ))
  }

  invisible(data)
}

add_r2dii_colours <- function(specs) {
  n <- seq_len(nrow(specs))
  specs$r2dii_colour_name <- palette_colours$label[n]

  specs %>%
    left_join(palette_colours, by = c("r2dii_colour_name" = "label")) %>%
    select(-.data$r2dii_colour_name)
}
