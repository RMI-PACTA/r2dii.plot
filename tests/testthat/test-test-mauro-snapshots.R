test_that("plot_timeline outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- head(filter(sda, sector == "cement"))
  p <- plot_timeline(data)
  p$plot_env <- NULL

  expect_snapshot(str(p))
})

test_that("plot_techmix outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- example_market_share(
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- plot_techmix(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("plot_trajectory outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  p <- plot_trajectory(example_market_share())
  p$plot_env <- NULL

  expect_snapshot(str(p))
})
