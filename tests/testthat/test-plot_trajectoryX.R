test_that("outputs a ggplot", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020"
    )

  p <- plot_trajectoryX(data, normalize = TRUE)
  expect_s3_class(p, "ggplot")
})
