# Create a file from code and return a path (?expect_snapshot_file).
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  path
}

test_that("prints the expected .png", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  path <- save_png(plot_timeline(data_sda_cement))
  expect_snapshot_file(path, paste0("plot_timeline_sda_cement", ".png"))
})

test_that("prints output ggplot object without error", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  plot <- plot_timeline(data_sda_cement)
  expect_error(print(plot), NA)
})

test_that("with bad 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad <- "bad"
  graceful_message <- "'line_specs' must be a dataframe."

  expect_error(plot_timeline(data_sda_cement, lines_specs = bad), graceful_message)
})

test_that("with bad column names in 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- data.frame(
    "line_name" = c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo"),
    "bad" = c("Projected", "Corporate Economy", "Target Demo", "Adjusted Scenario Demo")
  )
  graceful_message <- "'line_specs' must have columns 'line_name' and 'label'."

  expect_error(plot_timeline(data_sda_cement, lines_specs = bad_lines_specs), graceful_message)
})

test_that("with unmatching entries in 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- data.frame(
    "line_name" = c("projected", "corporate_economy", "bad", "adjusted_scenario_demo"),
    "label" = c("Projected", "Corporate Economy", "Target Demo", "Adjusted Scenario Demo")
  )
  graceful_message <- "Can't find `line_name` values from 'lines_specs' in the data."

  expect_error(plot_timeline(data_sda_cement, lines_specs = bad_lines_specs), graceful_message)
})

test_that("with bad colour names in 'lines_specs' errors gracefully", {
  data_sda_cement <- prepare_for_timeline(sda_target,
    sector_filter = "cement",
    year_start = 2020,
    year_end = 2026,
    column_line_names = "emission_factor_metric",
    value_to_plot = "emission_factor_value"
  )

  bad_lines_specs <- data.frame(
    "line_name" = c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo"),
    "label" = c("Projected", "Corporate Economy", "Target Demo", "Adjusted Scenario Demo"),
    "r2dii_colour_name" = c("dark_blue", "green", "bad", "orange")
  )
  graceful_message <- "Colour names specified in 'lines_specs' must match "

  expect_error(plot_timeline(data_sda_cement, lines_specs = bad_lines_specs), graceful_message)
})
