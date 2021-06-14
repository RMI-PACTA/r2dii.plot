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

test_that("with cero-row data errors gracefully", {
  cero_row <- market_share[0L, ]
  expect_snapshot_error(
    plot_trajectoryX(cero_row)
  )
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

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(plot_trajectoryX(bad_sector))
})

test_that("with too many technologies errors gracefully", {
  bad_tech <- head(market_share, 2L)
  bad_tech$technology <- c("a", "b")
  expect_snapshot_error(plot_trajectoryX(bad_tech))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(
    plot_trajectoryX(bad_region)
  )
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(
    plot_trajectoryX(bad_scenario_source)
  )
})

test_that("with inexistent `main_line` errors gracefully", {
  data <- head(market_share, 1L)
  expect_snapshot_error(plot_trajectoryX(data, main_line = "bad"))
})

