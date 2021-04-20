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

test_that("returns visibly", {
  data <- process_input_data(get_example_data())

  expect_visible(
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

# FIXME: Should this become an error?
test_that("with bad `sector_filter` warns gracefully", {
  # Catch bubbling wranings from lower-levels.
  suppressWarnings(
    expect_warning(
      prepare_for_trajectory_chart(
        process_input_data(get_example_data()),
        sector_filter = "bad",
        technology_filter = "oilcap",
        region_filter = "global",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      ),
      "sector_filter.*bad"
    )
  )
})

# FIXME: We could throw a more graceful warning.
test_that("with bad `technology_filter` warns ungracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  ungraceful_message <- "no non-missing arguments to min"

  expect_warning(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = bad,
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production"
    ),
    ungraceful_message
  )
})

# FIXME: We could throw a more graceful warning.
test_that("with bad `region_filter` warns ungracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  ungraceful_message <- "no non-missing arguments to min"

  expect_warning(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = bad,
      scenario_source_filter = "demo_2020",
      value_name = "production"
    ),
    ungraceful_message
  )
})

# FIXME: We could throw a more graceful warning.
test_that("with bad `scenario_source_filter` warns ungracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  ungraceful_message <- "no non-missing arguments to min"

  expect_warning(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = bad,
      value_name = "production"
    ),
    ungraceful_message
  )
})

# FIXME: We could throw a more graceful error.
test_that("with bad `value_name` errors ungracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  ungraceful_message <- "Can't subset columns that don't exist"

  expect_error(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = bad
    ),
    ungraceful_message
  )
})

# FIXME: This may need an error or warning
test_that("with bad `end_year_filter` throws no error", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  no_error <- NA

  expect_error(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production",
      end_year_filter = bad
    ),
    no_error
  )
})

test_that("with bad `normalize_to_start_year` errors gracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"

  expect_error(
    prepare_for_trajectory_chart(
      data,
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production",
      normalize_to_start_year = bad
    ),
    "not.*logical"
  )
})

# FIXME: The error message could be more graceful, maybe with this or similar:
# r2dii.utils::check_crucial_names()
# TODO: Do this with all crucial names that the function expects in the input
test_that("with input missing crucial columns errors ungracefully", {
  bad <- select(process_input_data(get_example_data()), -sector)

  expect_error(
    # Catch irrelevant, bubbling warnings
    suppressWarnings(
      prepare_for_trajectory_chart(
        bad,
        sector_filter = "power",
        technology_filter = "oilcap",
        region_filter = "global",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      )
    ),
    "Problem with.*filter"
  )
})

test_that("outputs data starting at the start of 'projected' or later", {
  data <- process_input_data(get_example_data())

  year_start_projected <- data %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  out <- prepare_for_trajectory_chart(
    data,
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production"
  )

  expect_true(min(out$year) >= year_start_projected)
})
