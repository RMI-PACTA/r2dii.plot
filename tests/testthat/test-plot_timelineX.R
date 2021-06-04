test_that("outputs a ggplot", {
  data <- filter(sda, sector == "cement")
  p <- plot_timelineX(data)
  expect_s3_class(p, "ggplot")
})
