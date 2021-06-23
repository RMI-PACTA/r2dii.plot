test_that("does not modify `metric`", {
  data <- example_market_share()
  metrics <- sort(unique(data$metric))

  p <- ggplot_trajectory(data)
  out <- sort(as.character(unique(p$layers[[2]]$data$metric)))
  expect_equal(out, metrics)
})
test_that("outputs vanilla labels", {
  data <- example_market_share()
  p <- ggplot_trajectory(data)

  vanilla <- sort(unique(data$metric))
  labels <- sort(as.character(unique(p$layers[[2]]$data$label)))
  expect_equal(labels, vanilla)
})
