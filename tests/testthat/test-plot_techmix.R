test_that("with the simplest call outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_name = "technology_share"
  )

  specs <- dplyr::tibble(
    label = c(
      "Portfolio 2020",
      "Benchmark 2020",
      "Portfolio 2025",
      "Benchmark 2025",
      "Target SDS 2025"
    ),
    # Not DRYing (see r2dii.plot.static/pull/91#discussion_r615990701)
    metric_type = c(
      "portfolio_2020",
      "benchmark_2020",
      "portfolio_2025",
      "benchmark_2025",
      "target_sds_2025"
    )
  )

  out <- plot_techmix(
    data,
    df_tech_colours = get_r2dii_technology_colours("power"),
    df_bar_specs = specs
  )

  expect_s3_class(out, "ggplot")
  out <- unclass(out)
  out$plot_env <- NULL
  expect_snapshot(out)
})

test_that("with `show_legend = FALSE` outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  data <- prepare_for_techmix_chart(
    process_input_data(example_data),
    sector_filter = "power",
    years_filter = c(2020, 2025),
    region_filter = "global",
    scenario_source_filter = "demo_2020",
    scenario_filter = "sds",
    value_name = "technology_share"
  )

  specs <- dplyr::tibble(
    label = c(
      "Portfolio 2020",
      "Benchmark 2020",
      "Portfolio 2025",
      "Benchmark 2025",
      "Target SDS 2025"
    ),
    metric_type = gsub(" ", "_", tolower(label))
  )

  out <- plot_techmix(
    data,
    show_legend = FALSE,
    df_tech_colours = get_r2dii_technology_colours("power"),
    df_bar_specs = specs
  )

  out <- unclass(out)
  out$plot_env <- NULL
  expect_snapshot(out)
})
