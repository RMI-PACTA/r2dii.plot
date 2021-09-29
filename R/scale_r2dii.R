scale_colour_r2dii <- function(r2dii_labels = NULL, ...) {
  discrete_scale("colour", "r2dii", r2dii_pal(r2dii_labels), ...)
}

scale_color_r2dii <- scale_colour_r2dii

scale_fill_r2dii <- function(r2dii_labels = NULL, ...) {
  discrete_scale("fill", "r2dii", r2dii_pal(r2dii_labels), ...)
}

r2dii_pal <- function(r2dii_labels = NULL) {
  r2dii_labels <- r2dii_labels %||% r2dii.plot:::palette_colours$label
  values <- tibble(label = r2dii_labels) %>%
    inner_join(r2dii.plot:::palette_colours, by = "label") %>%
    pull(hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}
