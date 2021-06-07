# prep_techmixB() ----

test_that("outputs the expected snapshot", {
  data <- market_share %>%
    filter(
      dplyr::between(year, 2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  out <- prep_techmixB(data, value = "technology_share")
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
  expect_snapshot_error(prep_techmixB(long_source))

  long_sector <- mutate(head(market_share, 2), sector = 1:2)
  expect_snapshot_error(prep_techmixB(long_sector))

  long_region <- mutate(head(market_share, 2), region = 1:2)
  expect_snapshot_error(prep_techmixB(long_region))
})

test_that("with bad metric errors gracefully", {
  bad_metric <- mutate(head(market_share), metric = "bad")
  expect_snapshot_error(prep_techmixB(bad_metric))
})
