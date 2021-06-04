test_that("outputs a data.frame", {
  out <- prep_techmix(
    market_share,
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value = "technology_share"
  )

  expect_s3_class(out, "data.frame")
})

test_that("returns visibly", {
  expect_visible(
    prep_techmix(
      market_share,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value = "technology_share"
    )
  )
})

test_that("with bad `sector_filter` errors gracefully", {
  expect_error(
    regexp = "arg.*should be one of",
    prep_techmix(
      market_share,
      sector_filter = "bad",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value = "technology_share"
    )
  )
})

test_that("with bad `years_filter` errors gracefully", {
  expect_error(
    regexp = "years_filter.*must be.*vector of numbers.",
    prep_techmix(
      market_share,
      sector_filter = "power",
      years_filter = c("not", "good"),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value = "technology_share"
    )
  )
})

test_that("with bad `region_filter` errors gracefully", {
  expect_error(
    regexp = "region_filter.*must be.*in.*input data.*region.",
    prep_techmix(
      market_share,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "bad",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value = "technology_share"
    )
  )
})

test_that("with bad `scenario_source_filter` errors gracefully", {
  expect_error(
    regexp = "scenario_source_filter.*must be.*in.*input data.*scenario_source",
    prep_techmix(
      market_share,
      scenario_source_filter = "bad",
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_filter = "sds",
      value = "technology_share"
    )
  )
})

test_that("with bad `scenario_filter` errors gracefully", {
  expect_error(
    regexp = "scenario_filter.*must.*in.*input data.*metric",
    prep_techmix(
      market_share,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "bad",
      value = "technology_share"
    )
  )
})

test_that("adds the column `value` from the column named in `value`", {
  out <- prep_techmix(
    market_share,
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value = "production"
  )

  expect_true(rlang::has_name(out, "value"))
  expect_type(out$value, "double")
})



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
