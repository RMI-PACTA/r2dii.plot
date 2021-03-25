test_that("outputs a data.frame", {
  out <- prepare_for_timeline(sda_target,
                              sector_filter = "automotive",
                              year_start = 2020,
                              year_end = 2026,
                              column_line_names = "emission_factor_metric",
                              value_to_plot = "emission_factor_value")

  expect_s3_class(out, "data.frame")
})

test_that("with bad `sector_filter` errors gracefully", {
  bad <- "bad"
  graceful_message <- "'arg' should be one of "

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = bad,
                          year_start = 2020,
                          year_end = 2026,
                          column_line_names = "emission_factor_metric",
                          value_to_plot = "emission_factor_value"),
    graceful_message
  )
})

test_that("with bad `year_start` errors gracefully", {
  bad <- "bad"
  graceful_message <- "'year_start' should be a number."

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = "automotive",
                          year_start = bad,
                          year_end = 2026,
                          column_line_names = "emission_factor_metric",
                          value_to_plot = "emission_factor_value"),
    graceful_message
  )
})

test_that("with bad `year_end` errors gracefully", {
  bad <- "bad"
  graceful_message <- "'year_end' should be a number."

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = "automotive",
                          year_start = 2020,
                          year_end = bad,
                          column_line_names = "emission_factor_metric",
                          value_to_plot = "emission_factor_value"),
    graceful_message
  )
})

test_that("with bad `column_line_names` errors gracefully", {
  bad <- "bad"
  graceful_message <- "not found as a column in the input data. Please pass an"

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = "automotive",
                          year_start = 2020,
                          year_end = 2026,
                          column_line_names = bad,
                          value_to_plot = "emission_factor_value"),
    graceful_message
  )
})

test_that("with bad `value_to_plot` errors gracefully", {
  bad <- "bad"
  graceful_message <- "not found as a column in the input data. Please pass an"

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = "automotive",
                          year_start = 2020,
                          year_end = 2026,
                          column_line_names = "emission_factor_metric",
                          value_to_plot = bad),
    graceful_message
  )
})

test_that("with bad `extrapolate_missing_values` errors gracefully", {
  bad <- "bad"
  graceful_message <- "argument is not interpretable as logical"

  expect_error(
    prepare_for_timeline(sda_target,
                          sector_filter = "automotive",
                          year_start = 2020,
                          year_end = 2026,
                          column_line_names = "emission_factor_metric",
                          value_to_plot = "emission_factor_value",
                          extrapolate_missing_values = bad),
    graceful_message
  )
})
