qplot_trajectory <- function(data) {
  check_plot_trajectory(data)

  data <- data %>%
    mutate(label = sub("target_", "", .data$metric),
      label = case_when(
        is_scenario(.data$metric) ~ toupper(as.character(.data$label)),
        TRUE ~ to_title(as.character(.data$label))
      )
    ) %>%
    restrict_to_5_years()

  base_size = 12
  plot_trajectory(data) +
    # TODO: move to theme_2dii()
    theme(
      plot.subtitle = element_text(
        hjust = 0.5, vjust = 0.5,
        size = base_size * 10 / 12
      )
    ) +
    labs(
      title = glue("Production trajectory of {to_pretty_label(data$technology[1])} technology
                   in the {to_title(data$sector[1])} sector"),
      subtitle = glue("The coloured areas indicate trajectories in reference to a scenario.
                      The red area indicates trajectories not aligned with any sustainble scenario."),
      x = "Year",
      y = glue("Production rate (normalized to {min_year})")
    )
}

restrict_to_5_years <- function(data) {
  min_year <- get_common_start_year(data)
  PACTA_int <- 5 #PACTA results are conventionally shown over a time period of 5 years
  data %>% filter(.data$year <= min_year + PACTA_int)

  data
}

to_pretty_label <- function(technology) {
  label <- to_title(technology)
  label <- sub("cap$", " Capacity", label)
  label <- sub("_hdv$", "Heavy Duty Vehicles", label)
}
