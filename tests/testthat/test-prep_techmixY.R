test_that("outputs a data.frame", {
  out <- prep_techmixY(
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
    prep_techmixY(
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
    prep_techmixY(
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
    prep_techmixY(
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
    prep_techmixY(
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
    prep_techmixY(
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
    prep_techmixY(
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
  out <- prep_techmixY(
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
