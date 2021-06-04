test_that("outputs a ggplot", {
  data <- filter(market_share, region == "global", sector == "power")
  p <- plot_techmixX(data)
  expect_s3_class(p, "ggplot")
})
