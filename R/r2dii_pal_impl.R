r2dii_pal_impl <- function(thing, data, column) {
  thing <- thing %||% data[[column]]
  values <- as_tibble_col(thing, column) %>%
    inner_join(data, by = column) %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}
