test_that("outputs pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- qplot_emission_intensity(data)

  metrics <- unique(p$data$label)
  pretty <- c("Projected", "Corporate Economy")
  expect_equal(pretty, metrics)
})

test_that("Prints title as expected", {
  data <- filter(sda, sector == "automotive")
  p <- qplot_emission_intensity(data)

  expect_snapshot_output(p$labels$title)
})

test_that("Prints axis labels as expected", {
  data <- filter(sda, sector == "automotive")
  p <- qplot_emission_intensity(data)

  expect_equal(p$labels$x, "Year")
  expect_snapshot_output(p$labels$x)

  expect_match(p$labels$y, "[Tt]ons of CO2 per ton")
  expect_snapshot_output(p$labels$y)
})

test_that("Plots a data set with maximum time horizon of 5 years", {
  data <- filter(sda, sector == "automotive")
  p <- qplot_emission_intensity(data)

  expect_true(lubridate::year(max(p$data$year)) - lubridate::year(min(p$data$year)) <= 5)
})