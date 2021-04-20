test_that("prints output ggplot object without error", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  plot <- plot_timeline(data_sda_cement)
  expect_error(
    {
      path <- tempfile(fileext = ".pdf")
      pdf(path)
      print(plot)
      dev.off()
    },
    NA
  )
})

test_that("with bad 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  expect_error(
    plot_timeline(data_sda_cement, lines_specs = "bad"),
    "'line_specs' must be a dataframe."
  )
})

test_that("with bad column names in 'lines_specs' errors gracefully", {
  skip_if(r_version_is_older_than(4))

  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- dplyr::rename(lines_specs(), bad = label)

  expect_error(
    plot_timeline(data_sda_cement, lines_specs = bad_lines_specs),
    "'line_specs' must have columns 'line_name' and 'label'."
  )
})

test_that("with unmatching entries in 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- lines_specs()
  bad_lines_specs$line_name[1] <- "bad"

  expect_error(
    plot_timeline(data_sda_cement, lines_specs = bad_lines_specs),
    "Can't find `line_name` values from 'lines_specs' in the data."
  )
})

test_that("with bad colour names in 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- lines_specs(
    r2dii_colour_name = c("dark_blue", "green", "bad", "orange")
  )

  expect_error(
    plot_timeline(data_sda_cement, lines_specs = bad_lines_specs),
    "Colour names specified in 'lines_specs' must match "
  )
})

test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  extrapolated <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value",
    extrapolate_missing_values = TRUE
  )
  lines_specs <- lines_specs(
    r2dii_colour_name = c("dark_blue", "green", "grey", "orange")
  )

  out <- unclass(plot_timeline(extrapolated, lines_specs = lines_specs))
  out$plot_env <- NULL
  expect_snapshot(out)
})

test_that("with malformed lines_specs errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  too_many_rows <- Reduce(
    dplyr::bind_rows,
    list(
      lines_specs(),
      lines_specs(),
      lines_specs()
    )
  )
  expect_error(
    plot_timeline(data_sda_cement, lines_specs = too_many_rows),
    "lines.*must be.*lower"
  )
})

test_that("handles lines_specs with factors", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2050,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  specs <- dplyr::mutate(lines_specs(), label = as.factor(label))
  expect_no_error(plot_timeline(data_sda_cement, lines_specs = specs))
})
