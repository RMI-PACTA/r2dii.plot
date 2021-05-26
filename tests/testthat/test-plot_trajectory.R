test_that("with wrong number of scenarios errors gracefully", {
  example_data <- process_input_data(example_data)

  data <- prepare_for_trajectory_chart(
    example_data, sector_filter = "power", technology_filter = "oilcap",
    region_filter = "global", scenario_source_filter = "demo_2020",
    value_name = "production", end_year_filter = 2025,
    normalize_to_start_year = TRUE
  )

  scenario_specs <- data.frame(
    scenario = c("sds", "sps", "cps", "sce4", "sce5"),
    label = c("SDS", "STEPS", "CPS", "Scenario 4", "Scenario 5")
  )

  main_line_metric <- data.frame(metric = "projected", label = "Portfolio")

  expect_snapshot_error(plot_trajectory(data,
    scenario_specs_good_to_bad = scenario_specs,
    main_line_metric = main_line_metric
  ))
})
