test_that("outputs a ggplot", {
  data <- head(filter(sda, sector == "cement"))
  p <- plot_timelineX(data)
  expect_s3_class(p, "ggplot")
})

test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- head(filter(sda, sector == "cement"))
  p <- plot_timelineX(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("with data other than sda errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_timelineX(bad))
})

test_that("with data other than sda errors gracefully", {
  bad <- head(market_share, 1L)
  expect_error(
    regexp = "sda.*data",
    plot_timelineX(bad)
  )
})

test_that("with cero-row data errors gracefully", {
  cero_row <- sda[0L, ]
  expect_snapshot_error(plot_timelineX(cero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_timelineX(data))
})

