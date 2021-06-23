ggplot_trajectory <- function(data,
                              convert_label = identity,
                              env = parent.frame()) {
  check_plot_trajectory(data, env = parent.frame())
  prep <- prep_trajectory(data, convert_label = convert_label)
  plot_trajectory_impl(prep)
}
