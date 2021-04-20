test_that("outputs a data.frame", {
  data <- process_input_data(get_example_data())

  out <- prepare_for_techmix_chart(
    data,
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_name = "technology_share"
  )

  expect_s3_class(out, "data.frame")
})

test_that("returns visibly", {
  data <- process_input_data(get_example_data())

  expect_visible(
    prepare_for_techmix_chart(
      data,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_name = "technology_share"
    )
  )
})

# FIXME: We should throw a graceful warning.
test_that("with bad `sector_filter` returns a data.frame with no rows", {
  data <- process_input_data(get_example_data())
  bad <- "bad"

  expect_equal(
    nrow(prepare_for_techmix_chart(
      data,
      sector_filter = bad,
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_name = "technology_share"
    )),
    0L
  )
})

# FIXME: We should throw a graceful warning.
test_that("with bad `years_filter` returns a data.frame with no rows", {
  data <- process_input_data(get_example_data())
  bad <- "bad"

  expect_equal(
    nrow(prepare_for_techmix_chart(
      data,
      sector_filter = "power",
      years_filter = bad,
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_name = "technology_share"
    )),
    0L
  )
})

# FIXME: We should throw a graceful warning.
test_that("with bad `region_filter` returns a data.frame with no rows", {
  data <- process_input_data(get_example_data())
  bad <- "bad"

  expect_equal(
    nrow(prepare_for_techmix_chart(
      data,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = bad,
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_name = "technology_share"
    )),
    0L
  )
})

# FIXME: We should throw a graceful warning.
test_that("with bad `scenario_source_filter` returns a 0-rows data.frame", {
  data <- process_input_data(get_example_data())

  out <- prepare_for_techmix_chart(
    data,
    scenario_source_filter = "bad",
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_filter = "sds",
    value_name = "technology_share"
  )

  expect_equal(nrow(out), 0L)
})

# FIXME: We should throw a graceful warning.
test_that("with bad `scenario_filter` returns a data.frame", {
  data <- process_input_data(get_example_data())
  bad <- "bad"

  expect_s3_class(
    prepare_for_techmix_chart(
      data,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = bad,
      value_name = "technology_share"
    ),
    "data.frame"
  )
})

# FIXME: We could throw a more graceful error.
test_that("with bad `value_name` errors ungracefully", {
  data <- process_input_data(get_example_data())
  bad <- "bad"
  ungraceful_message <- "Can't subset columns that don't exist"

  expect_error(
    prepare_for_techmix_chart(
      data,
      sector_filter = "power",
      years_filter = c(2020, 2025),
      region_filter = "global",
      scenario_source_filter = "demo_2020",
      scenario_filter = "sds",
      value_name = bad
    ),
    ungraceful_message
  )
})
