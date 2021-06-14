plot_timelineB <- function(data) {
  abort_if_missing_names(data, "line_name")

  line_names <- unique(data$line_name)
  labels <- line_names
  specs <- tibble(line_name = line_names, label = labels) %>%
    abort_if_too_many_lines() %>%
    add_r2dii_colours()

  plot_timelineY(data = data, specs = specs)
}

plot_timelineC <- function(data, recode = TRUE) {
  if (!is.null(recode)) data$line_name <- recode_lines(recode, data)
  plot_timelineB(data)
}
