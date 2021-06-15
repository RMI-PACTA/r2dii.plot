test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_timeline(1))
})

test_that("if `data` is not sda-like errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_timeline(bad))
})

test_that("if `data` has cero rows errors gracefully", {
  cero_row <- sda[0L, ]
  expect_snapshot_error(plot_timeline(cero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_timeline(data))
})

test_that("with bad `extrapolate` errors gracefully", {
  data <- head(filter(sda, sector == "cement"))
  expect_snapshot_error(plot_timeline(data, extrapolate = 1))
})

test_that("is sensitive to extrapolate", {
  data <- filter(sda, sector == "cement")
  pull_extrapolated <- function(p) p$layers[[1]]$data$extrapolated

  p <- plot_timeline(data, extrapolate = TRUE)
  expect_true(any(pull_extrapolated(p)))

  q <- plot_timeline(data, extrapolate = FALSE)
  expect_false(any(pull_extrapolated(q)))
})

test_that("outputs an object with no factor-columns derived from `specs`", {
  data <- head(filter(sda, sector == "cement"))

  p <- plot_timeline(data)
  p_data <- p$layers[[1]]$data
  specs_cols <- c("line_name", "label", "hex")
  has_factors <- any(unlist(lapply(p_data[specs_cols], is.factor)))

  expect_false(has_factors)
})

test_that("outputs pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- plot_timeline(data)

  get_line_name <- function(p) unique(p$layers[[1]]$data$line_name)
  expect_equal(get_line_name(p), c("Projected", "Corporate Economy"))
})
