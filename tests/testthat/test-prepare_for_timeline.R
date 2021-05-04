test_that("w/ R 3.6 & extrapolate_missing_values = TRUE outputs a data.frame", {
  out <- prepare_for_timeline(
    sda_target,
    sector_filter = "automotive",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value",
    extrapolate_missing_values = TRUE
  )

  expect_s3_class(out, "data.frame")
})

test_that("outputs a data.frame", {
  out <- prepare_for_timeline(sda_target,
    sector_filter = "automotive",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  expect_s3_class(out, "data.frame")
})

test_that("with bad `sector_filter` errors gracefully", {
  expect_error(
    regexp = "arg.*should be one of",
    prepare_for_timeline(sda_target,
      sector_filter = "bad",
      year_start = 2020,
      year_end = 2026,
      column_line_names = "emission_factor_metric",
      value_to_plot = "emission_factor_value"
    )
  )
})

test_that("with bad `year_start` errors gracefully", {
  expect_error(
    regexp = "year_start.*must be.*number.",
    prepare_for_timeline(sda_target,
      sector_filter = "automotive",
      year_start = "bad",
      year_end = 2026,
      column_line_names = "emission_factor_metric",
      value_to_plot = "emission_factor_value"
    )
  )
})

test_that("with bad `year_end` errors gracefully", {
  expect_error(
    regexp = "year_end.*must be.*number.",
    prepare_for_timeline(sda_target,
      sector_filter = "automotive",
      year_start = 2020,
      year_end = "bad",
      column_line_names = "emission_factor_metric",
      value_to_plot = "emission_factor_value"
    )
  )
})

test_that("with bad `column_line_names` errors gracefully", {
  expect_error(
    regexp = "column_line_names.*must be.*in.*input data.",
    prepare_for_timeline(
      sda_target,
      column_line_names = "bad",
      sector_filter = "automotive",
      year_start = 2020,
      year_end = 2026,
      value_to_plot = "emission_factor_value"
    )
  )
})

test_that("with bad `value_to_plot` errors gracefully", {
  expect_error(
    regexp = "'value_to_plot' must be one of column names in the input data.",
    prepare_for_timeline(
      sda_target,
      sector_filter = "automotive",
      year_start = 2020,
      year_end = 2026,
      column_line_names = "emission_factor_metric",
      value_to_plot = "bad"
    )
  )
})

test_that("with bad `extrapolate_missing_values` errors gracefully", {
  expect_error(
    regexp = "extrapolate_missing_values.*must be.*logical",
    prepare_for_timeline(sda_target,
      sector_filter = "automotive",
      year_start = 2020,
      year_end = 2026,
      column_line_names = "emission_factor_metric",
      value_to_plot = "emission_factor_value",
      extrapolate_missing_values = "bad"
    )
  )
})
