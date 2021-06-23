test_that("outputs vanilla labels", {
  data <- filter(sda, sector == "automotive")
  p <- ggplot_emission_intensity(data)

  vanilla <- sort(unique(data[[metric(data)]]))
  labels <- sort(unique(p[["layers"]][[1]][["data"]][["label"]]))
  expect_equal(labels, vanilla)
})
