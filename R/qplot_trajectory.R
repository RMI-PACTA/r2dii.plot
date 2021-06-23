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
