test_that("outputs pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- qplot_emission_intensity(data)

  metrics <- unique(p$data$label)
  pretty <- c("Projected", "Corporate Economy")
  expect_equal(pretty, metrics)
})
