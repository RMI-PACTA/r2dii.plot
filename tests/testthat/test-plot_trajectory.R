test_that("snapshot", {
  data <- prepare_for_trajectory_chart(
    process_input_data(example_data),
    sector_filter = "power",
    technology_filter = "renewablescap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production"
  )

  scenario_specs <- dplyr::tibble(
    scenario = c("sds", "sps", "cps", "worse"),
    color = c("#9CAB7C", "#FFFFCC", "#FDE291", "#E07B73"),
    label = c("SDS", "STEPS", "CPS", "worse")
  )

  main_line_metric <-
    dplyr::tibble(metric = "projected", label = "Portfolio")

  additional_line_metrics <- dplyr::tibble(metric = "corporate_economy",
                                           label = "Corporate Economy")

  p <- suppressWarnings(plot_trajectory(data,
                       scenario_specs_good_to_bad = scenario_specs,
                       main_line_metric = main_line_metric))
  p$plot_env <- NULL
  expect_snapshot(str(p))
})
