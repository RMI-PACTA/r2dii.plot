test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- head(filter(sda, sector == "cement"))
  p <- plot_timelineX(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("outputs a ggplot", {
  data <- head(filter(sda, sector == "cement"))
  p <- plot_timelineX(data)
  expect_s3_class(p, "ggplot")
})

test_that("without a data frame errors gracefully", {
  expect_snapshot_error(plot_timelineX(1))
})

test_that("without sda-like data errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_timelineX(bad))
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

test_that("with bad `extrapolate errors gracefully", {
  data <- head(filter(sda, sector == "cement"))
  expect_snapshot_error(plot_timelineX(data, extrapolate = 1))
})

test_that("is sensitive to extrapolate", {
  data <- filter(sda, sector == "cement")
  pull_extrapolated <- function(p) p$layers[[1]]$data$extrapolated

  p <- plot_timelineX(data, extrapolate = TRUE)
  expect_true(any(pull_extrapolated(p)))

  q <- plot_timelineX(data, extrapolate = FALSE)
  expect_false(any(pull_extrapolated(q)))
})
