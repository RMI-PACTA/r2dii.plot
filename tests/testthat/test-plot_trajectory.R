test_that("with wrong number of scenarios errors gracefully", {
  data <- prep_trajectory(
    market_share,
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value = "production",
    end_year_filter = 2025,
    normalize = TRUE
  )

  scenario_specs <- tibble(
    scenario = c("sds", "sps", "cps", "sce4", "sce5"),
    label = c("SDS", "STEPS", "CPS", "Scenario 4", "Scenario 5")
  )

  main_line_metric <- tibble(metric = "projected", label = "Portfolio")

  expect_snapshot_error(plot_trajectory(data,
    scenario_specs_good_to_bad = scenario_specs,
    main_line_metric = main_line_metric
  ))
})

test_that("outputs the expected snapshot", {
  data <- prep_trajectory(
    market_share,
    sector_filter = "power",
    technology_filter = "renewablescap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value = "production"
  )

  scenario_specs <- dplyr::tibble(scenario = c("sds", "sps", "cps"),
                                  label = c("SDS", "STEPS", "CPS"))

  main_line_metric <-
    dplyr::tibble(metric = "projected", label = "Portfolio")

  additional_line_metrics <- dplyr::tibble(metric = "corporate_economy",
                                           label = "Corporate Economy")

  p <- plot_trajectoryA(
    data,
    scenario_specs_good_to_bad = scenario_specs,
    main_line_metric = main_line_metric,
    additional_line_metrics = additional_line_metrics
  )

  p$plot_env <- NULL
  expect_snapshot(str(p))
})
