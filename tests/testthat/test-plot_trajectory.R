test_that("works with custom `additional_line_metrics` and brown technology", {
  skip_if_not_local()

  data <- prepare_for_trajectory_chart(
    process_input_data(get_example_data()),
    sector_filter = "power",
    technology_filter = "oilcap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production",
    end_year_filter = 2025,
    normalize_to_start_year = TRUE
  )
  specs <- dplyr::tibble(
    scenario = c("sds", "sps", "cps", "worse"),
    color = c("#9CAB7C", "#FFFFCC", "#FDE291", "#E07B73"),
    label = scenario
  )
  main_line_metric <- dplyr::tibble(metric = "projected", label = "Portfolio")
  additional_line_metrics <- dplyr::tibble(
    label = "Corporate Economy",
    metric = tolower(label)
  )
  out <- plot_trajectory(
    data,
    scenario_specs_good_to_bad = specs,
    main_line_metric = main_line_metric,
    additional_line_metrics = additional_line_metrics
  )

  expect_s3_class(out, "ggplot")
  out <- unclass(out)
  # Changes in every run
  out$plot_env <- NULL
  expect_snapshot(out)
})

test_that("works with `annotate_data`", {
  data <- prepare_for_trajectory_chart(
    process_input_data(get_example_data()),
    sector_filter = "power",
    technology_filter = "renewablescap",
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    value_name = "production",
    end_year_filter = 2025,
    normalize_to_start_year = TRUE
  )
  specs <- dplyr::tibble(
    scenario = c("sds", "sps", "cps", "worse"),
    color = c("#9CAB7C", "#FFFFCC", "#FDE291", "#E07B73"),
    label = scenario
  )
  main_line_metric <- dplyr::tibble(metric = "projected", label = "Portfolio")
  additional_line_metrics <- dplyr::tibble(
    label = "Corporate Economy",
    metric = tolower(label)
  )

  out <- plot_trajectory(
    data,
    annotate_data = TRUE,
    scenario_specs_good_to_bad = specs,
    main_line_metric = main_line_metric,
    additional_line_metrics = additional_line_metrics
  )
  out <- unclass(out)
  # Changes in every run
  out$plot_env <- NULL
  expect_snapshot(out)
})
