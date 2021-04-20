fake_data <- function(sector = "automotive",
                      technology = "electric",
                      year = 2020L,
                      region = "global",
                      scenario_source = "demo_2020",
                      metric = "projected",
                      production = 1,
                      technology_share = 0.2,
                      ...) {
  tibble(
    sector = sector,
    technology = technology,
    year = year,
    region = region,
    scenario_source = scenario_source,
    metric = metric,
    production = production,
    technology_share = technology_share,
    ...
  )
}

lines_specs <- function(...) {
  dplyr::tibble(
    line_name = c(
      "projected",
      "corporate_economy",
      "target_demo",
      "adjusted_scenario_demo"
    ),
    # Not DRYing (see r2dii.plot.static/pull/91#pullrequestreview-639054150)
    label = c(
      "Projected",
      "Corporate Economy",
      "Target Demo",
      "Adjusted Scenario Demo"
    ),
    ...
  )
}

r_version_is_older_than <- function(major) {
  as.integer(R.version$major) < major
}
