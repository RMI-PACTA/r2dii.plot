test_that("w/ R 3.6 & extrapolate_missing_values = TRUE outputs a data.frame", {
  out <- prep_timelineA(
    sda,
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
  out <- prep_timelineA(sda,
    sector_filter = "automotive",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  expect_s3_class(out, "data.frame")
})

test_that("with bad `sector_filter` errors gracefully", {
  expect_snapshot_error(
    prep_timelineA(sda, sector_filter = "bad")
  )
})

test_that("with bad `year_start` errors gracefully", {
  expect_error(
    regexp = "year_start.*must be.*number.",
    prep_timelineA(sda,
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
    prep_timelineA(sda,
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
    regexp = "column_line_names.*must be.*in.*data",
    prep_timelineA(
      sda,
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
    regexp = "value_to_plot.*must be.*in.*data.",
    prep_timelineA(
      sda,
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
    prep_timelineA(sda,
      sector_filter = "automotive",
      year_start = 2020,
      year_end = 2026,
      column_line_names = "emission_factor_metric",
      value_to_plot = "emission_factor_value",
      extrapolate_missing_values = "bad"
    )
  )
})

test_that("with a `sector_filter` of lengh > 1 throws an error", {
  too_long <- c("steel", "power")
  expect_error(
    prep_timelineA(sda, sector_filter = too_long),
    "must be of length 1"
  )
})

test_that("without `sector_filter` throws an error", {
  expect_error(
    prep_timelineA(sda),
    "must be.*length 1"
  )
})

test_that("w/ a single-sector dataset and it's selected, throws no warning", {
  data <- filter(sda, sector == "steel")

  expect_warning(
    prep_timelineA(data, sector_filter = "steel"),
    NA
  )
})

test_that("w/ a single-sector dataset and it's not selected, throws warning", {
  data <- filter(sda, sector == dplyr::first(sector))

  expect_warning(
    class = "missing_sector",
    prep_timelineA(data, sector_filter = "steel")
  )
})

test_that("works with sectors in title case", {
  data <- sda
  data$sector <- tools::toTitleCase(data$sector)

  expect_no_error(prep_timelineA(data, "Aviation"))

  out <- prep_timelineA(data, "aviation")
  expect_true(nrow(out) > 0L)
})

test_that("outputs column `sector`", {
  out <- prep_timelineA(sda, sector_filter = "aviation")
  expect_true(hasName(out, "sector"))
})



# prep_timelineB() -----------------------------------------------------

test_that("preserves sectors", {
  data <- sda
  out <- prep_timelineB(data)
  expect_equal(unique(data$sector), unique(out$sector))
})

test_that("outputs the expected snapshot", {
    expect_snapshot(prep_timelineB(sda))
})
