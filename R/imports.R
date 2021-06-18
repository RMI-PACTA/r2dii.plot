#' @import ggplot2
#' @importFrom dplyr arrange case_when filter group_by if_else left_join mutate
#' @importFrom dplyr select tribble summarise ungroup pull distinct right_join
#' @importFrom dplyr desc lead tibble lag slice_head across bind_rows between
#' @importFrom dplyr all_of rename first
#' @importFrom rlang .env abort warn inform %||%
#' @importFrom glue glue
NULL

utils::globalVariables(c(
  "market_share",
  "palette_colours",
  "scenario_colours",
  "sector_colours",
  "technology_colours",
  "where",
  ":="
))
