fake_data <- function() {
  data.frame(
    sector = "automotive",
    technology = "electric",
    year = 2020L,
    region = "global",
    scenario_source = "demo_2020",
    metric = "projected",
    production = 1,
    technology_share = 0.2,
    stringsAsFactors = FALSE
  )
}

lines_specs <- function(...) {
  dplyr::tibble(
    label = c(
      "Projected",
      "Corporate Economy",
      "Target Demo",
      "Adjusted Scenario Demo"
    ),
    line_name = gsub(" ", "_", tolower(label)),
    ...
  )
}

skip_if_r_version_is_older_than <- function(major) {
  as.integer(R.version$major) < major
}
