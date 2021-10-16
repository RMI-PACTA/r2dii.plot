test_that("Plots a data set with maximum time horizon of 5 years", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- qplot_techmix(data)
  expect_true(diff(year_range(p)) <= 5)
})

test_that("Outputs pretty labels", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- qplot_techmix(data)

  metrics <- unique(p$data$label)
  pretty <- c("Portfolio", "Benchmark", "Scenario")
  expect_equal(metrics, pretty)
})

test_that("Has the title as expected", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- qplot_techmix(data)

  expect_snapshot_output(p$labels$title)
})

test_that("Outputs pretty legend labels", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- qplot_techmix(data)

  metrics <- unique(p$data$label_tech)
  pretty <- c("Coal Capacity", "Gas Capacity", "Hydro Capacity")
  expect_equal(metrics[1:3], pretty)

  data <- filter(
    market_share,
    sector == "automotive",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- qplot_techmix(data)

  metrics <- unique(p$data$label_tech)
  pretty <- c("Electric", "Hybrid", "ICE")
  expect_equal(metrics, pretty)
})
