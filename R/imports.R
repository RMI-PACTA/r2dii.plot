#' @import ggplot2
#' @importFrom dplyr arrange case_when filter group_by if_else left_join mutate
#' @importFrom dplyr select tribble summarise ungroup pull distinct right_join
#' @importFrom dplyr desc lead tibble lag slice_head across bind_rows between
#' @importFrom dplyr all_of rename first semi_join inner_join
#' @importFrom rlang .env abort warn inform %||% has_name
#' @importFrom glue glue
#' @importFrom scales manual_pal
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

# Re-implement tibble::as_tibble_col to avoid adding tibble to Imports
as_tibble_col <- function(x, column_name = "value") {
  tibble(`:=`(!!column_name, x))
}
