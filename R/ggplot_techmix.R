ggplot_techmix <- function(data,
                           convert_label = identity,
                           env = parent.frame()) {
  check_plot_techmix(data, parent.frame())
  prep <- prep_techmix(data, convert_label = convert_label)
  plot_techmix_impl(prep)
}
