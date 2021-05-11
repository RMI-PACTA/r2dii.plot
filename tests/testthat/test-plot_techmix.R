test_that("with the simplest call outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  out <- plot_techmix(data)

  expect_s3_class(out, "ggplot")
  out <- unclass(out)
  out$plot_env <- NULL
  expect_snapshot(out)
})

test_that("with bad 'metric_type_order' errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  expect_error(
    regexp = "metric_type_order.*must be.*in.*metric_type.*data.",
    plot_techmix(data, metric_type_order = "bad")
  )
})

test_that("with bad 'metric_type_labels' errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  expect_error(
    regexp = "metric_type_labels.*must be.*same length.*metric_type_order.",
    plot_techmix(data, metric_type_labels = "bad")
  )
})

test_that("with more than one sector in data errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )
  data <- data %>%
    dplyr::add_row(sector = "automotive", technology = "electric",
            metric_type = data$metric_type[1], metric = data$metric[1],
            value = data$value[1], scenario_source = data$scenario_source[1])

  expect_error(
    regexp = "Input data.*must.*one.*sector.",
    plot_techmix(data)
  )
})

test_that("with bad sector errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  ) %>%
    mutate(sector = "bad")

  expect_error(
    regexp = "Input data.*sector.*not found.",
    plot_techmix(data)
  )
})

test_that("with bad 'tech_colours' errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  expect_error(
    regexp = "tech_colours.*must.*dataframe",
    plot_techmix(data, tech_colours = "bad")
  )
})

test_that("with bad column in 'tech_colours' errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  tech_colours <- get_r2dii_technology_colours("power")
  colnames(tech_colours)[1] <- "bad"

  expect_error(
    regexp = "tech_colours.*must.*columns.*technology.*colour.",
    plot_techmix(data, tech_colours = tech_colours)
  )
})

test_that("with bad technology in 'tech_colours' errors gracefully", {
  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_to_plot = "technology_share"
  )

  tech_colours <- get_r2dii_technology_colours("power")
  tech_colours$technology <- "bad"

  expect_error(
    regexp = ".*technologies.*input data.*must.*colour.*tech_colours.",
    plot_techmix(data, tech_colours = tech_colours)
  )
})
