test_that("outputs vanilla labels", {
  data <- filter(example_market_share(), metric %in% unique(metric)[1:3])

  vanilla <- sort(unique(data$metric))
  p <- ggplot_techmix(data)
  labels <- p[["layers"]][[1]][["data"]][["label"]]
  labels <- sort(unique(sub("(.*)_202.$", "\\1", labels)))

  expect_equal(labels, vanilla)
})
