# prep_techmixB() ----

test_that("prep_techmixB() outputs the expected snapshot", {
  data <- market_share %>%
    filter(
      dplyr::between(year, 2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  out <- prep_techmixB(data, value = "technology_share")
  skip("Dead code")
  expect_snapshot(out)
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "missing_names", prep_techmixB(bad))

  bad <- select(data, -technology_share)
  expect_error(class = "missing_names", prep_techmixB(bad, "technology_share"))

  bad <- select(data, -year)
  expect_error(class = "missing_names", prep_techmixB(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "missing_names", prep_techmixB(bad))
})

test_that("with more than one value of some columns errors gracefully", {
  long_source <- mutate(head(market_share, 2), scenario_source = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_source))

  long_sector <- mutate(head(market_share, 2), sector = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_sector))

  long_region <- mutate(head(market_share, 2), region = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_region))
})

test_that("with bad metric errors gracefully", {
  bad_metric <- mutate(head(market_share), metric = "bad")
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(bad_metric))
})

# prep_trajectoryB() ----

test_that("prep_trajectoryB() outputs the expected snapshot", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      year <= 2025,
      sector == "power"
    )
  out <- prep_trajectoryB(data)

  skip("Dead code")
  expect_snapshot(out)
})

test_that("with multiple distinct values in some columns errors gracefully", {
  long_sector <- mutate(head(market_share, 2), sector = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_sector))

  long_tech <- mutate(head(market_share, 2), technology = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_tech))

  long_region <- mutate(head(market_share, 2), region = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_region))

  long_source <- mutate(head(market_share, 2), scenario_source = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_source))
})

test_that("if `normalize` isn't length-1 errors gracefully", {
  skip("Dead code")
  expect_snapshot_error(
    prep_trajectoryB(market_share, normalize = c(TRUE, TRUE))
  )
})

test_that("if `normalize` isn't logical errors gracefully", {
  skip("Dead code")
  expect_snapshot_error(
    prep_trajectoryB(market_share, normalize = "a")
  )
})

test_that("is sensitive to `normalize`", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      sector == "power"
    )

  expect_false(
    identical(
      prep_trajectoryB(data, normalize = TRUE),
      prep_trajectoryB(data, normalize = FALSE)
    )
  )
})

test_that("with missing crucial names errors gracefully", {
  # crucial <- c("metric", "sector", "technology", "region", "year")

  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -sector)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -technology)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -region)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -year)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -production)
  expect_error(class = "missing_names", prep_trajectoryB(bad))
})

test_that("integrates with plot_trajectory()", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      year <= 2025,
      sector == "power"
    )

  out <- prep_trajectoryB(data)
  expect_no_error(plot_trajectoryB(out))
})
