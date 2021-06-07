test_that("with too many scenarios errors gracefully", {
  too_many <- head(market_share, 4L)
  too_many$metric <- c("projected", "corporate_economy", "target_a", "target_b")

  expect_error(
    # class = "invalid_length",
    plot_techmixX(too_many),
    "must be.*1.*not 2"
  )
})

test_that("with too many scenarios errors gracefully", {
  too_few <- head(market_share, 2L)
  too_few$metric <- c("projected", "corporate_economy")

  expect_error(
    plot_techmixX(too_few),
    "Can't find.*scenarios"
  )
})

test_that("outputs a ggplot", {
  data <- head(market_share, 3)
  p <- plot_techmixX(data)
  expect_s3_class(p, "ggplot")
})
