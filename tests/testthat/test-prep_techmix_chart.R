test_that("outputs a data.frame", {
  data <- process_input_data(example_data)

  out <- prep__techmix(
    data,
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  expect_s3_class(out, "data.frame")
})

test_that("returns visibly", {
  data <- process_input_data(example_data)

  expect_visible(
    prep__techmix(
      data,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_to_plot = "technology_share"
    )
  )
})

test_that("with bad `sector_filter` errors gracefully", {
  expect_error(
    regexp = "arg.*should be one of",
    prep__techmix(
      process_input_data(example_data),
      sector_filter = "bad",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_to_plot = "technology_share"
    )
  )
})

test_that("with bad `years_filter` errors gracefully", {
  expect_error(
    regexp = "years_filter.*must be.*vector of numbers.",
    prep__techmix(
      process_input_data(example_data),
      sector_filter = "power",
      years_filter = "bad",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_to_plot = "technology_share"
    )
  )
})

test_that("with bad `region_filter` errors gracefully", {
  expect_error(
    regexp = "region_filter.*must be.*in.*input data.*region.",
    prep__techmix(
      process_input_data(example_data),
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "bad",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_to_plot = "technology_share"
    )
  )
})

test_that("with bad `scenario_source_filter` errors gracefully", {
  expect_error(
    regexp = "scenario_source_filter.*must be.*in.*input data.*scenario_source",
    prep__techmix(
      process_input_data(example_data),
      scenario_source_filter = "bad",
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_filter = "sds",
      value_to_plot = "technology_share"
    )
  )
})

test_that("with bad `scenario_filter` errors gracefully", {
  expect_error(
    regexp = "scenario_filter.*must.*in.*input data.*metric",
    prep__techmix(
      process_input_data(example_data),
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "bad",
      value_to_plot = "technology_share"
    )
  )
})

test_that("adds the column `value` from the column named in `value_to_plot`", {
  out <- prep__techmix(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "production"
  )

  expect_true(rlang::has_name(out, "value"))
  expect_type(out$value, "double")
})
