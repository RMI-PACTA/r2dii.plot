test_that("outputs a data.frame", {
  data <- process_input_data(example_data)

  out <- prepare_for_trajectory(
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
  data <- process_input_data(example_data)

  expect_visible(
    prepare_for_trajectory(
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
  data <- process_input_data(example_data)

  expect_visible(
    prepare_for_trajectory(
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
  suppressWarnings(
    expect_warning(
      regexp = "bad.*matches.*no",
      prepare_for_trajectory(
        process_input_data(example_data),
        sector_filter = "bad",
        technology_filter = "oilcap",
        region_filter = "global",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      )
    )
  )
})

# FIXME: Should this become an error?
test_that("with bad `technology_filter` warns gracefully", {
  suppressWarnings(
    expect_warning(
      regexp = "bad.*matches.*no",
      prepare_for_trajectory(
        process_input_data(example_data),
        sector_filter = "power",
        technology_filter = "bad",
        region_filter = "global",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      )
    )
  )
})

# FIXME: Should this become an error?
test_that("with bad `region_filter` warns gracefully", {
  suppressWarnings(
    expect_warning(
      regexp = "bad.*matches.*no",
      prepare_for_trajectory(
        process_input_data(example_data),
        sector_filter = "power",
        technology_filter = "oilcap",
        region_filter = "bad",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      )
    )
  )
})

# FIXME: Should this become an error?
test_that("with bad `scenario_source_filter` warns gracefully", {
  suppressWarnings(
    expect_warning(
      regexp = "bad.*matches.*no",
      prepare_for_trajectory(
        process_input_data(example_data),
        sector_filter = "power",
        technology_filter = "oilcap",
        region_filter = "global",
        scenario_source_filter = "bad",
        value_name = "production"
      )
    )
  )
})

# FIXME: Is this what `value_name` means? What are valid options other than
# "production"? That information is not documented in the description of the
# argument `value_name` not via examples or README.
test_that("adds the column `value` from the column named in `value_name`", {
  out <- prepare_for_trajectory(
    process_input_data(example_data),
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production"
  )

  expect_true(rlang::has_name(out, "value"))
  expect_type(out$value, "double")
})

# FIXME: Do we need an error or warning?
test_that("with bad `end_year_filter` throws no error", {
  expect_no_error(
    prepare_for_trajectory(
      process_input_data(example_data),
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production",
      end_year_filter = "bad"
    )
  )
})

test_that("with bad `normalize_to_start_year` errors gracefully", {
  expect_error(
    regexp = "not.*logical",
    prepare_for_trajectory(
      process_input_data(example_data),
      sector_filter = "power",
      technology_filter = "oilcap",
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      value_name = "production",
      normalize_to_start_year = "bad"
    )
  )
})

test_that("with missing crucial columns errors gracefully", {
  suppressWarnings(
    expect_snapshot_error(
      prepare_for_trajectory(
        bad <- select(process_input_data(example_data), -sector),
        sector_filter = "power",
        technology_filter = "oilcap",
        region_filter = "global",
        scenario_source_filter = "demo_2020",
        value_name = "production"
      )
    )
  )
})

test_that("outputs data starting at the start of 'projected' or later", {
  data <- process_input_data(example_data)

  year_start_projected <- data %>%
    filter(.data$metric == "projected") %>%
    pull(.data$year) %>%
    min()

  out <- prepare_for_trajectory(
    data,
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production"
  )

  expect_true(min(out$year) >= year_start_projected)
})
