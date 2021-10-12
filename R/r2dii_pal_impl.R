r2dii_pal_impl <- function(x, data, column) {
  x <- x %||% data[[column]]
  values <- as_tibble_col(x, column) %>%
    inner_join(data, by = column) %>%
    pull(.data$hex)
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}
