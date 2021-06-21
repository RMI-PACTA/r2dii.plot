add_fake_metrics_market_share <- function(data, n, prefix = "") {
  sector <- data$sector[1]
  technology <- data$technology[1]
  region <- data$region[1]
  scenario_source <- data$scenario_source[1]
  min_year <- min(data$year)
  max_year <- max(data$year)
  for (i in 1:n) {
    fake_data <- tibble(
      sector = rep(sector, 2),
      technology = rep(technology, 2),
      region = rep(region, 2),
      scenario_source = rep(scenario_source, 2),
      year = c(min_year, max_year),
      metric = glue("{prefix}{letters[i]}"),
      production = 100,
      technology_share = 0.1
    )

    data <- rbind(data, fake_data)
  }
  data
}

expect_no_error <- function(...) {
  testthat::expect_error(..., NA)
}

expect_no_message <- function(...) {
  testthat::expect_message(..., NA)
}
