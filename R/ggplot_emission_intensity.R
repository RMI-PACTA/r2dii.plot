ggplot_emission_intensity <- function(data,
                                      convert_label = identity,
                                      env = parent.frame()) {
  check_plot_emission_intensity(data, env = env)
  prep <- prep_emission_intensity(data, convert_label = convert_label)
  plot_emission_intensity_impl(prep)
}
