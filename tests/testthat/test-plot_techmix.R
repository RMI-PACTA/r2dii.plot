test_that("with the simplest call outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  data <- prepare_for_techmix_chart(
    process_input_data(get_example_data()),
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
