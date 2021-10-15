#' Create a quick techmix plot
#'
#' @inherit plot_techmix
#' @seealso plot_techmix
#'
#' @description
#' Compared to [plot_techmix()] this function:
#' * is restricted to plotting future as 5 years from the start year,
#' * outputs pretty bar labels, based on metric column,
#' * outputs pretty legend labels, based on technology column,
#' * outputs a title.
#'
#' @export
#' @examples
#' # `data` must meet documented "Requirements"
#' data <- subset(
#'   market_share,
#'   sector == "power" &
#'     region == "global" &
#'     scenario_source == "demo_2020" &
#'     metric %in% c("projected", "corporate_economy", "target_sds")
#' )
#'
#' qplot_techmix(data)
qplot_techmix <- function(data) {
  env <- list(data = substitute(data))
  check_plot_techmix(data, env = env)

  data %>%
    prep_techmix(
      convert_label = recode_metric_techmix,
      span_5yr = TRUE,
      convert_tech_label = spell_out_technology,
      env = env
    ) %>%
    plot_techmix_impl() %>%
    labs_techmix()
}

labs_techmix <- function(p) {
  sector <- to_title(p[["data"]][["sector"]][[1]])

  p +
    labs(
      title = glue("Current and Future Technology Mix for the {sector} Sector")
    )
}
