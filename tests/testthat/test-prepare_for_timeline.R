test_that("w/ R 3.6 & extrapolate_missing_values = TRUE outputs a data.frame", {
  out <- prep__timelineA(
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
  out <- prep__timelineA(sda_target,
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
    prep__timelineA(sda_target, sector_filter = "bad")
  )
})

test_that("with bad `year_start` errors gracefully", {
  expect_error(
    regexp = "year_start.*must be.*number.",
    prep__timelineA(sda_target,
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
    prep__timelineA(sda_target,
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
    prep__timelineA(
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
    regexp = "value_to_plot.*must be.*in.*data.",
    prep__timelineA(
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
    prep__timelineA(sda_target,
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
    prep__timelineA(sda_target, sector_filter = too_long),
    "must be of length 1"
  )
})

test_that("without `sector_filter` throws an error", {
  expect_error(
    prep__timelineA(sda_target),
    "must be.*length 1"
  )
})

test_that("w/ a single-sector dataset and it's selected, throws no warning", {
  data <- filter(sda_target, sector == "steel")

  expect_warning(
    prep__timelineA(data, sector_filter = "steel"),
    NA
  )
})

test_that("w/ a single-sector dataset and it's not selected, throws warning", {
  data <- filter(sda_target, sector == dplyr::first(sector))

  expect_warning(
    class = "missing_sector",
    prep__timelineA(data, sector_filter = "steel")
  )
})

test_that("works with sectors in title case", {
  data <- sda_target
  data$sector <- tools::toTitleCase(data$sector)

  expect_no_error(prep__timelineA(data, "Aviation"))

  out <- prep__timelineA(data, "aviation")
  expect_true(nrow(out) > 0L)
})

test_that("outputs column `sector`", {
  out <- prep__timelineA(sda_target, sector_filter = "aviation")
  expect_true(hasName(out, "sector"))
})



# prep__timelineB() -----------------------------------------------------

test_that("preserves sectors", {
  data <- sda_target
  out <- prep__timelineB(data)
  expect_equal(unique(data$sector), unique(out$sector))
})

test_that("outputs the expected snapshot", {
    expect_snapshot(prep__timelineB(sda_target))
})
