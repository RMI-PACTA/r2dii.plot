test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- market_share %>%
    filter(
      sector == first(sector),
      technology == first(technology)
    )
  p <- plot_trajectoryX(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("without a data frame errors gracefully", {
  expect_snapshot_error(plot_trajectoryX(1))
})

test_that("without market_share-like data errors gracefully", {
  bad <- sda
  expect_snapshot_error(plot_trajectoryX(bad))
})

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
