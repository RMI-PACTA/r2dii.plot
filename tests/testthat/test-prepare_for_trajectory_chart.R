test_that("outputs a data.frame", {
  data <- process_input_data(get_example_data())

  out <- prepare_for_trajectory_chart(
    data,
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production"
  )

  expect_s3_class(out, "data.frame")
})

# FIXME: This is likely a mistake
test_that("by default outputs invisibly", {
  data <- process_input_data(get_example_data())

  expect_invisible(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production"
    )
  )
})

test_that("with `normalize_to_start_year = FALSE` outputs visibly", {
  dont_normalize <- FALSE

  data <- process_input_data(get_example_data())

  expect_visible(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production",
      normalize_to_start_year = dont_normalize
    )
  )
})

