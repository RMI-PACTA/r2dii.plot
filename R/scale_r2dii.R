#' Custom 2DII colour and fill scales
#'
#' A custom discrete colour and fill scales with colours from 2DII palette.
#'
#' @param labels A character vector. Specifies colour labels to use and their
#'   order. Run `unique(r2dii.plot:::palette_colours$label)` to see available
#'   labels. Similar to `value` parameter in `ggplot::scale_colour_manual()`.
#' @param ... Other parameters passed on to `ggplot2::discrete_scale()`.
#'
#' @return An object of class "ScaleDiscrete".
#' @export
#'
#' @aliases scale_color_r2dii
#'
#' @examples
#' library(ggplot2, warn.conflicts = FALSE)
#'
#' ggplot(data = mpg) +
#'  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'  scale_colour_r2dii()
#'
#' ggplot(data = mpg) +
#'  geom_histogram(mapping = aes(x = cyl, fill = class), position = "dodge", bins = 5) +
#'  scale_fill_r2dii()
scale_colour_r2dii <- function(labels = NULL, ...) {
  discrete_scale("colour", "r2dii", r2dii_pal(labels), ...)
}

#' @rdname scale_colour_r2dii
#' @export
scale_fill_r2dii <- function(labels = NULL, ...) {
  discrete_scale("fill", "r2dii", r2dii_pal(labels), ...)
}

#' @noRd
r2dii_pal <- function(labels = NULL) {
  check_labels(labels)
  r2dii_pal_impl(labels, column = "label", data = palette_colours)
}

check_labels <- function(labels) {
  available_labels <- unique(palette_colours$label)
  if(!is.null(labels)) {
    if (!all((labels %in% available_labels))) {
      bad_labels <- sort(setdiff(labels, available_labels))
      abort(
        c(glue("`labels` must be in palette_colours data set."),
          i = glue("Run `unique(r2dii.plot:::palette_colours$label)` to see all available labels:
            {toString(available_labels)}."),
          x = glue("You passed: {toString(bad_labels)}.")
        )
      )
    }
  }
}
